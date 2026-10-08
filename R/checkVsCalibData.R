#' Render checkVsCalibData
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @author Falk Benke
#' @examples
#' \dontrun{
#' # Simple use. Creates PDF:
#' checkVsCalibData(
#'   gdx = "path/to/data.gdx",
#'   outputDir = "path/to/output/directory",
#'   outputFile = "Check_vs_CalibData_Example.pdf"
#' )
#' }
#' @export
checkVsCalibData <- function(gdx, outputDir = getwd(), outputFile = "Check_vs_CalibData.pdf") {
  yamlParams <- list(gdx = normalizePath(gdx, mustWork = TRUE))

  tmpFolder <- file.path(outputDir, "checkVsCalibrationData")

  dir.create(tmpFolder)

  file.copy(
    from = piamutils::getSystemFile("markdown", "checkVsCalibrationData.Rmd", package = "remind2"),
    to = tmpFolder, recursive = TRUE, copy.mode = FALSE
  )

  # avoid potential problems with write permissions on the copied files
  Sys.chmod(list.files(tmpFolder, full.names = TRUE))

  rmarkdown::render(
    file.path(tmpFolder, "checkVsCalibrationData.Rmd"),
    output_dir = outputDir,
    output_file = outputFile,
    intermediates_dir = tmpFolder,
    output_format = "pdf_document",
    params = yamlParams
  )

  unlink(tmpFolder, recursive = TRUE)
}
