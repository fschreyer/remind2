#' @title Parse per-iteration non-convergence reasons from a GAMS listing file
#' @description Extract, for each Nash iteration, the block of diagnostic text
#'   that REMIND writes to the GAMS listing file (\code{full.lst}) between the
#'   lines "Reasons for non-convergence in this iteration" and "See the
#'   indicators below to dig deeper ...". Each block is associated with the
#'   iteration number printed just before it (\code{PARAMETER o_iterationNumber}).
#'
#'   The listing file can be several hundred MB. When the command line tools
#'   \code{grep} and \code{sed} are available, they are used to scan the file and
#'   extract only the relevant lines, which is markedly faster than reading the
#'   whole file into R. Otherwise the function falls back to a pure-R
#'   implementation. Both paths yield identical results, so the function is
#'   portable to systems without these tools (e.g. Windows).
#'
#' @param lstFile \code{character(1)}. Path to the GAMS listing file.
#' @param timeLimit \code{numeric(1)}. Maximum time in seconds allowed for
#'   parsing. If exceeded, the function stops and fails gracefully by returning
#'   \code{NULL}. Default 90.
#' @return A \code{data.frame} with integer column \code{iteration} and character
#'   column \code{text}, one row per iteration ordered by iteration, or
#'   \code{NULL} if \code{lstFile} does not exist, the time limit is exceeded, or
#'   parsing otherwise fails.
#'
#' @author Gabriel Abrahão
#'
#' @export
parseLSTReasons <- function(lstFile, timeLimit = 90) {
  if (!file.exists(lstFile)) {
    return(NULL)
  }

  start <- Sys.time()
  remaining <- function() {
    as.numeric(timeLimit) - as.numeric(difftime(Sys.time(), start, units = "secs"))
  }

  # Overall time budget: fail gracefully (return NULL) if parsing exceeds it.
  # setTimeLimit guards R-side work; the external tools are bounded via
  # system2(timeout=) with the time still remaining in the budget.
  setTimeLimit(elapsed = timeLimit, transient = TRUE)
  on.exit(setTimeLimit())

  tryCatch({
    # the fast external-tool path returns NULL if the tools are missing or
    # anything unexpected happens, in which case we fall back to the pure-R path.
    fast <- .parseLSTReasonsFast(lstFile, timeout = max(remaining(), 0))
    if (!is.null(fast)) {
      fast
    } else if (remaining() <= 0) {
      stop("time limit reached before completing")
    } else {
      .parseLSTReasonsSlow(lstFile)
    }
  }, error = function(e) {
    warning("parseLSTReasons: parsing failed or exceeded timeLimit (",
            timeLimit, "s): ", conditionMessage(e))
    NULL
  })
}

# markers delimiting the blocks of interest in the listing file
.lstIterMarker  <- "PARAMETER o_iterationNumber"
.lstStartMarker <- "Reasons for non-convergence in this iteration"
.lstEndMarker   <- "See the indicators below to dig deeper"

.emptyReasons <- function() {
  data.frame(iteration = integer(0), text = character(0), stringsAsFactors = FALSE)
}

# extract the iteration number from an "o_iterationNumber ... = <value>" line
.lstIterValue <- function(x) {
  as.integer(as.numeric(sub("^.*=\\s*([0-9.]+).*$", "\\1", x)))
}

# Given the line numbers of the three markers, build a data.frame with the
# iteration number and the first/last line of each block's text. Blocks with no
# preceding o_iterationNumber (e.g. the source-code echo at the top of the
# listing) or no closing marker are dropped.
.assembleReasonBlocks <- function(startIdx, endIdx, iterIdx, iterVal) {
  rows <- lapply(startIdx, function(s) {
    e <- endIdx[endIdx > s][1]
    if (is.na(e)) {
      return(NULL)
    }
    pre <- iterIdx[iterIdx < s]
    if (length(pre) == 0) {
      return(NULL)
    }
    iter <- iterVal[which(iterIdx == pre[length(pre)])[1]]
    if (is.na(iter)) {
      return(NULL)
    }
    data.frame(iteration = iter, first = s + 1L, last = e - 1L, stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows)) {
    return(data.frame(iteration = integer(0), first = integer(0), last = integer(0)))
  }
  rows[order(rows$iteration), , drop = FALSE]
}

# TRUE if a system2 call reported an error or timeout. grep uses status 1 to
# signal "no match", which is not a failure here, so only status > 1 counts.
.system2Failed <- function(x) {
  st <- attr(x, "status")
  !is.null(st) && st > 1
}

# fast path: use grep to locate the markers (one scan) and sed to extract the
# block lines (one scan), bringing only the small result into R. The external
# calls are bounded by `timeout` seconds so they cannot hang past the budget.
.parseLSTReasonsFast <- function(lstFile, timeout = 90) {
  grepBin <- Sys.which("grep")
  sedBin <- Sys.which("sed")
  if (!nzchar(grepBin) || !nzchar(sedBin)) {
    return(NULL)
  }
  to <- if (is.finite(timeout) && timeout > 0) timeout else 0

  tryCatch({
    pattern <- paste(.lstIterMarker, .lstStartMarker, .lstEndMarker, sep = "|")
    matched <- suppressWarnings(
      system2(grepBin, c("-nE", shQuote(pattern), shQuote(lstFile)),
              stdout = TRUE, stderr = FALSE, timeout = to)
    )
    if (.system2Failed(matched)) {
      return(NULL)
    }
    if (length(matched) == 0) {
      return(.emptyReasons())
    }

    lineNo <- as.integer(sub(":.*$", "", matched))
    content <- sub("^[0-9]+:", "", matched)

    isIter  <- grepl(.lstIterMarker, content, fixed = TRUE)
    isStart <- grepl(.lstStartMarker, content, fixed = TRUE)
    isEnd   <- grepl(.lstEndMarker, content, fixed = TRUE)

    blocks <- .assembleReasonBlocks(
      startIdx = lineNo[isStart], endIdx = lineNo[isEnd],
      iterIdx = lineNo[isIter], iterVal = .lstIterValue(content[isIter])
    )
    if (nrow(blocks) == 0) {
      return(.emptyReasons())
    }

    texts <- rep("(empty)", nrow(blocks))
    hasLines <- blocks$last >= blocks$first
    if (any(hasLines)) {
      ranges <- paste0(blocks$first[hasLines], ",", blocks$last[hasLines], "p")
      out <- suppressWarnings(
        system2(sedBin, c("-n", shQuote(paste(ranges, collapse = ";")), shQuote(lstFile)),
                stdout = TRUE, stderr = FALSE, timeout = to)
      )
      if (.system2Failed(out)) {
        return(NULL)
      }
      lens <- blocks$last[hasLines] - blocks$first[hasLines] + 1L
      if (length(out) != sum(lens)) {
        return(NULL) # unexpected extraction size -> fall back to the pure-R path
      }
      ends <- cumsum(lens)
      starts <- ends - lens + 1L
      idx <- which(hasLines)
      for (j in seq_along(idx)) {
        texts[idx[j]] <- paste(out[starts[j]:ends[j]], collapse = "\n")
      }
    }

    data.frame(iteration = blocks$iteration, text = texts, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# fallback path: read the whole file into R and slice it there.
.parseLSTReasonsSlow <- function(lstFile) {
  lines <- readLines(lstFile, warn = FALSE)

  iterIdx  <- grep(.lstIterMarker, lines, fixed = TRUE)
  startIdx <- grep(.lstStartMarker, lines, fixed = TRUE)
  endIdx   <- grep(.lstEndMarker, lines, fixed = TRUE)
  if (length(startIdx) == 0) {
    return(.emptyReasons())
  }

  blocks <- .assembleReasonBlocks(startIdx, endIdx, iterIdx, .lstIterValue(lines[iterIdx]))
  if (nrow(blocks) == 0) {
    return(.emptyReasons())
  }

  texts <- vapply(seq_len(nrow(blocks)), function(k) {
    if (blocks$last[k] >= blocks$first[k]) {
      paste(lines[blocks$first[k]:blocks$last[k]], collapse = "\n")
    } else {
      "(empty)"
    }
  }, character(1))

  data.frame(iteration = blocks$iteration, text = texts, stringsAsFactors = FALSE)
}
