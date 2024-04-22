#' match object dimensions of a magclass object
#'
#' A helper that restricts and expands a magclass object x to the size of a
#' magclass object ref.
#'
#' @param x a magclass object to be modified
#' @param ref a magclass object used as a reference for the modification
#' @param fill value to be set in new dimensions
#' @importFrom magclass ndim
#'
#' @export
matchDim <- function(x, ref, fill = 0) {

  if (ndim(x, dim = 3) != ndim(ref, dim = 3)) {
    stop(
      "Unsupported case: magclass objects x and ref have different number of ",
      "subdimensions in third dimension."
    )
  }

  # extend the object to the union of both objects
  r <- new.magpie(
    cells_and_regions = union(getRegions(x), getRegions(ref)),
    years = union(getYears(x), getYears(ref)),
    names = union(getNames(x), getNames(ref)),
    fill = fill,
    sets = names(dimnames(ref))
  )

  # copy over values from x
  r[getRegions(x), getYears(x), getNames(x)] <- x

  # restrict object to dimensions of ref
  r <- r[getRegions(ref), getYears(ref), getNames(ref)]

  return(r)
}
