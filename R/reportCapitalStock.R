#' Read in GDX and calculate capital stocks, used in convGDX2MIF.R for the reporting
#'
#' Read in capital stock information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#' @param gdx a GDX object as created by gdx2::readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_ref a GDX object as created by gdx2::readGDX, or the path to a gdx of the reference run.
#' It is used to guarantee consistency before 'cm_startyear' for capacity variables
#' using time averaging.
#'
#' @return MAgPIE object - contains the capital stock variables
#' @author Lavinia Baumstark; Michaja Pehl
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportCapitalStock(gdx)
#' }
#'
#' @export
#' @importFrom magclass getYears mbind setNames
#' @importFrom dplyr tribble
reportCapitalStock <- function(gdx,
                               regionSubsetList = NULL,
                               t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                               gdx_ref = NULL) {

  module2realisation <- gdx2::readGDX(gdx, "module2realisation", react = "silent")
  tran_mod <- module2realisation[module2realisation$modules == "transport", 2]

  pm_conv_cap_2_MioLDV <- 650  # The world has ~715million cars in 2005 (IEA TECO2)

  # read sets
  teall2rlf <- gdx2::readGDX(gdx, name = c("te2rlf", "teall2rlf"), format = "first_found")
  teue2rlf <- gdx2::readGDX(gdx, name = c("teue2rlf", "tees2rlf"), format = "first_found")

  # read variables
  vm_cap <- gdx2::readGDX(gdx, name = "vm_cap", select = list("_field" = "level"), restoreZeros = FALSE)
  vm_deltaCap <- gdx2::readGDX(gdx, name = "vm_deltaCap", select = list("_field" = "level"),
                               restoreZeros = FALSE)

  vm_costTeCapital <- gdx2::readGDX(gdx, name = c("vm_costTeCapital", "v_costTeCapital"), select = list("_field" = "level"),
                                    format = "first_found")
  vm_cesIO <- gdx2::readGDX(gdx, name = "vm_cesIO", select = list("_field" = "level"))

  # read parameters
  ppfKap_Ind <- gdx2::readGDX(gdx, name = "ppfkap_industry_dyn37", react = "silent")
  steel_process_based <- "steel" %in% gdx2::readGDX(gdx, "secInd37Prc", react = "silent")

  # calculate maximal temporal resolution
  y <- Reduce(intersect, list(getYears(vm_cap), getYears(vm_costTeCapital)))
  vm_cap <- vm_cap[, y, ]
  vm_deltaCap <- vm_deltaCap[, y, ]
  vm_costTeCapital <- vm_costTeCapital[, y, ]

  if (!is.null(gdx_ref)) {
    cm_startyear <- as.integer(gdx2::readGDX(gdx, name = "cm_startyear", format = "simplest"))
    vm_deltaCapRef <- gdx2::readGDX(gdx_ref, name = "vm_deltaCap",
                                    select = list("_field" = "level"), restoreZeros = FALSE)[, y, ]
    vm_deltaCap <- modifyInvestmentVariables(vm_deltaCap, vm_deltaCapRef, cm_startyear)
  } else {
    vm_deltaCap <- modifyInvestmentVariables(vm_deltaCap)
  }

  tmp <- NULL

  # ---- report transport capital stocks ----
  if (tran_mod == "complex") {
    LDV35 <- gdx2::readGDX(gdx, name = "LDV35")
    tmp <- mbind(tmp, setNames(dimSums((vm_cap * vm_costTeCapital)[teue2rlf],
                                       dim = c(3.1, 3.2)) * 1000, "Est Capital Stock|ESM|Transp vehic (billion US$2017)"))
    tmp <- mbind(tmp, setNames(dimSums((vm_cap * vm_costTeCapital)[teall2rlf][, , LDV35],
                                       dim = c(3.1, 3.2)) * 1000, "Est Capital Stock|ESM|Pet/EV LDV (billion US$2017)"))

    tmp <- mbind(tmp,
                 setNames(
                   dimSums(mbind(vm_cap * vm_costTeCapital),
                           dim = 3) * 1000,
                   "Estimated Capital Stock|ESM (billion US$2017)"
                 )
    )

    # prepare variables
    vm_cap      <- vm_cap[teall2rlf]
    vm_deltaCap <- vm_deltaCap[teall2rlf]

    # build reporting
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , LDV35] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est LDV Stock (million vehicles)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apCarElT"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est EV LDV Stock (million vehicles)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apCarH2T"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est H2 LDV Stock (million vehicles)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apCarPeT"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est ICE LDV Stock (million vehicles)"))

    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , c("apCarDiT", "apcarDiEffT", "apcarDiEffH2T")], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Stock|uedit (arbitrary unit)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apCarDiT"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Stock|apCarDiT (arbitrary unit)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apcarDiEffT"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Stock|apcarDiEffT (arbitrary unit)"))
    tmp <- mbind(tmp, setNames(dimSums(vm_cap[, , "apcarDiEffH2T"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Stock|apcarDiEffH2T (arbitrary unit)"))

    tmp2 <- NULL

    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , LDV35] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est LDV Sales (million vehicles)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apCarElT"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est EV LDV Sales (million vehicles)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apCarH2T"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est H2 LDV Sales (million vehicles)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apCarPeT"] * pm_conv_cap_2_MioLDV, dim = c(3.1, 3.2)), "Est ICE LDV Sales (million vehicles)"))

    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , c("apCarDiT", "apcarDiEffT", "apcarDiEffH2T")], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Sales|uedit (arbitrary unit)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apCarDiT"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Sales|apCarDiT (arbitrary unit)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apcarDiEffT"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Sales|apcarDiEffT (arbitrary unit)"))
    tmp2 <- mbind(tmp2, setNames(dimSums(vm_deltaCap[, , "apcarDiEffH2T"], dim = c(3.1, 3.2)), "Services and Products|Transport|non-LDV|Sales|apcarDiEffH2T (arbitrary unit)"))

    tmp <- mbind(tmp, tmp2)

    ## add global values
    tmp <- mbind(tmp, dimSums(tmp, dim = 1))
  }

  # ---- report industry energy efficiency capital stocks ----
  if (!is.null(ppfKap_Ind) && 0 < length(ppfKap_Ind)) {
    mixer <- tribble(
      ~pf,                     ~name,
      "kap_cement",            "Cement",
      "kap_chemicals",         "Chemicals",
      "kap_otherInd",          "other")

    if (!steel_process_based) {
      mixer <- bind_rows(mixer, tribble(
        ~pf,                     ~name,
        "kap_steel_primary",     "Primary Steel",
        "kap_steel_secondary",   "Secondary Steel"))
    }

    if (0 != length(setdiff(ppfKap_Ind, mixer$pf))) {
      warning(paste("Unknown ppfKap_industry_dyn37 entity.",
                    "Adjust remind2::reportCapitalStock()"))
    }

    if (0 != length(setdiff(mixer$pf, ppfKap_Ind))) {
      warning(paste("Missing ppfKap_industry_dyn37 entity.",
                    "Adjust remind2::reportCapitalStock()"))
    }

    eek_Ind <- setNames(vm_cesIO[, y, ppfKap_Ind],
                        paste0("Capital|Energy Efficiency|Industry|",
                               mixer[mixer$pf %in% ppfKap_Ind, ][["name"]],
                               " (billion US$2017)"))
    # add industry EEK and global totals
    tmp <- mbind(tmp, mbind(eek_Ind, dimSums(eek_Ind, dim = 1)))
  }

  # ---- add region aggregates ----
  if (!is.null(regionSubsetList))
    tmp <- mbind(tmp, calcRegionSubsetSums(tmp, regionSubsetList))

  getSets(tmp)[3] <- "variable"

  return(tmp)
}
