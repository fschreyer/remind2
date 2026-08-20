#' @title Nash Convergence Report
#' @description Create plots visualizing nash convergence of a given REMIND run
#'
#' @author Falk Benke and Renato Rodrigues
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @importFrom rmarkdown render
#'
#' @export

nashConvergenceReport <- function(gdx = "fulldata.gdx", outputDir = getwd()) {
  yamlParams <- list(gdx = normalizePath(gdx, mustWork = TRUE))

  # This report only supports nash optimization runs; for other optimization
  # realisations (e.g. negishi, testOneRegi) the nash convergence parameters do not
  # exist, so we do not attempt to render a (broken) report. Individual criterion pages
  # are gated further down by activeConvMessage80, so an inactive criterion (e.g. a
  # carbonprice / regipol realisation that does not use it) simply produces no page.
  m2r <- suppressWarnings(gdx::readGDX(gdx, "module2realisation", restore_zeros = FALSE))
  optiReali <- if (is.null(m2r)) NA_character_ else m2r[m2r$modules == "optimization", "*"]
  if (length(optiReali) == 0 || is.na(optiReali) || optiReali != "nash") {
    warning("nashConvergenceReport only supports nash optimization runs (found: ",
            if (length(optiReali) == 0 || is.na(optiReali)) "unknown" else optiReali,
            "). No report generated.")
    return(invisible(NULL))
  }

  # create convergence criteria reports
  activeReports <- .getActiveReports(gdx) # active reports in the current gdx

  # create html report index
  .createIndexHTML(active_reports = activeReports, output_dir = outputDir)

  # create markdown documents
  for (report in activeReports) {
    mkdFile <- system.file("markdown", paste0("nashConvergence-", report, ".Rmd"), package = "remind2")
    if (mkdFile != "") {
      rmarkdown::render(
        mkdFile,
        output_dir = outputDir,
        intermediates_dir = outputDir,
        output_file = paste0("nashConvergence-", report, ".html"),
        output_format = "html_document",
        params = yamlParams
      )
    }
  }
}

.getActiveReports <- function(gdx_file) {
  # all reports
  reports <- c(
    "infes" = "overview", "surplus" = "trade", "DevPriceAnticip" = "priceAnticipation", "taxconv" = "taxConv", "globalbudget" = "emiTarget",
    "regiTarget" = "regiTarget", "implicitEnergyTarget" = "qttyTarget", "cm_implicitPriceTarget" = "priceTarget", "cm_implicitPePriceTarget" = "pePriceTarget", "damage" = "damage",
    "peakbudget" = "co2price"
  )

  # active convergence criteria
  activeCriteria <- suppressWarnings(gdx::readGDX(gdx_file, "activeConvMessage80"))
  if (is.null(activeCriteria)) activeCriteria <- gdx::readGDX(gdx_file, "convMessage80") # fallback for runs before activeConvMessage80 implementation

  # active reports
  d <- unname(reports[names(reports) %in% activeCriteria])

  return(d)
}

.createIndexHTML <- function(active_reports, output_dir) {
  titles <- c(
    "overview" = "Overview", "trade" = "Trade Surplus", "priceAnticipation" = "Price Anticipation", "taxConv" = "Tax Convergence", "emiTarget" = "Emission target",
    "regiTarget" = "Regional target", "qttyTarget" = "Quantity target", "priceTarget" = "Price target", "pePriceTarget" = "PE Price target", "damage" = "Damage",
    "co2price" = "CO2 price convergence"
  )

  indexHTML <- '<!DOCTYPE html>\n<html>\n<head>\n<meta name="viewport" content="width=device-width, initial-scale=1">'
  indexHTML <- paste0(indexHTML, '\n<style> body { font-family: "Lato", sans-serif; } .sidenav { height: 100%; width: 160px; position: fixed; z-index: 1; top: 0; left: 0; background-color: #111; overflow-x: hidden; padding-top: 20px; } .sidenav a { padding: 6px 8px 6px 16px; text-decoration: none; font-size: 15px; color: #818181; display: block; } .sidenav a:hover { color: #a1a1a1; } .selected { color: #f1f1f1 !important; } .content { width: 100%; margin-left: 160px; /* Same as the width of the sidenav */ } @media screen and (max-height: 450px) { .sidenav {padding-top: 15px;} .sidenav a {font-size: 18px;} } iframe { position: fixed; height: 100%; width: calc(100vw - 160px); top: 0; left: 160px; } .hide { display: none; } .disabled { pointer-events: none; color: #434343 !important; } .lds-ring { /* change color here */ color: #1c4c5b; margin-left: 160px; } .lds-ring, .lds-ring div { box-sizing: border-box; } .lds-ring { display: inline-block; position: relative; width: 80px; height: 80px; } .lds-ring div { box-sizing: border-box; display: block; position: absolute; width: 64px; height: 64px; margin: 8px; border: 8px solid currentColor; border-radius: 50%; animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite; border-color: currentColor transparent transparent transparent; } .lds-ring div:nth-child(1) { animation-delay: -0.45s; } .lds-ring div:nth-child(2) { animation-delay: -0.3s; } .lds-ring div:nth-child(3) { animation-delay: -0.15s; } @keyframes lds-ring { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }</style>')
  indexHTML <- paste0(indexHTML, '\n</head>\n<body>\n  <div id="sideBar" class="sidenav">')
  for (report in active_reports) {
    indexHTML <- paste0(indexHTML, '\n    <a href="#', report, '" class="sidenavLink', ifelse(report == "overview", " selected", ""), '">', titles[report], "</a>")
  }
  indexHTML <- paste0(indexHTML, '\n  </div>\n<div id="content">\n<div class="lds-ring"><div></div><div></div><div></div><div></div></div>')
  for (report in active_reports) {
    indexHTML <- paste0(indexHTML, '\n    <div id="', report, '"', ifelse(report == "overview", "", ' class="hide"'), '><iframe data-src="nashConvergence-', report, '.html" loading="lazy" src="nashConvergence-', report, '.html"></iframe></div>')
  }
  indexHTML <- paste0(indexHTML, "\n</div>")
  indexHTML <- paste0(indexHTML, '\n<script type="text/javascript"> const addClassList = (element) => { Object.values(element.children).forEach((e) => { e.classList.add("hide"); }); }; document.addEventListener("click", (e) => { const { target } = e; if (!target.classList.contains("sidenavLink")) { return; } Object.values(document.getElementById("sideBar").getElementsByTagName("a")).forEach((e) => { e.classList.remove("selected"); }); target.classList.add("selected"); addClassList(document.getElementById("content")); const hash = target.hash.replace("#",""); selected = document.getElementById(hash); selected.classList.remove("hide"); });</script>')
  indexHTML <- paste0(indexHTML, "\n</body>\n</html>")

  # write html to file
  fileConn <- file(file.path(output_dir, "nashConvergence-index.html"))
  writeLines(indexHTML, fileConn)
  close(fileConn)
}
