#' Read in GDX and calculate policy costs, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in GDX and calculate policy costs functions
#'
#'
#' @param gdx a GDX as created by gdx2::readGDX, or the file name of a gdx
#' @param gdx_ref a reference GDX as created by gdx2::readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' reportPolicyCosts(gdx)
#' }
#'
#' @export

#' @importFrom magclass getYears mbind setNames
reportPolicyCosts <- function(gdx, gdx_ref, regionSubsetList = NULL,
                              t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {

  ## sets
  trade <- gdx2::readGDX(gdx, name = "trade")

  ## parameter
  pm_pvp_bau <- gdx2::readGDX(gdx_ref, name = "pm_pvp")
  pm_pvp <- gdx2::readGDX(gdx, name = "pm_pvp")

  ## variables
  cons_bau <- gdx2::readGDX(gdx_ref, name = "vm_cons", select = list("_field" = "level"),
                            restoreZeros = FALSE)
  gdp_bau <- gdx2::readGDX(gdx_ref, name = "vm_cesIO", select = list("_field" = "level"),
                           restoreZeros = FALSE)[, , "inco"]
  Xport_bau <- gdx2::readGDX(gdx_ref, name = "vm_Xport", select = list("_field" = "level"))
  Mport_bau <- gdx2::readGDX(gdx_ref, name = "vm_Mport", select = list("_field" = "level"))
  cons <- gdx2::readGDX(gdx, name = "vm_cons", select = list("_field" = "level"), restoreZeros = FALSE)
  gdp <- gdx2::readGDX(gdx, name = "vm_cesIO", select = list("_field" = "level"), restoreZeros = FALSE)[, , "inco"]
  Xport <- gdx2::readGDX(gdx, name = "vm_Xport", select = list("_field" = "level"))
  Mport <- gdx2::readGDX(gdx, name = "vm_Mport", select = list("_field" = "level"))
  v_costfu_bau <- gdx2::readGDX(gdx_ref, name = c("v_costFu", "v_costfu"),
                                select = list("_field" = "level"), restoreZeros = FALSE,
                                format = "first_found")
  v_costom_bau <- gdx2::readGDX(gdx_ref, name = c("v_costOM", "v_costom"),
                                select = list("_field" = "level"), format = "first_found")
  v_costin_bau <- gdx2::readGDX(gdx_ref, name = c("v_costInv", "v_costin"),
                                select = list("_field" = "level"), format = "first_found")
  v_costfu <- gdx2::readGDX(gdx, name = c("v_costFu", "v_costfu"),
                            select = list("_field" = "level"), restoreZeros = FALSE,
                            format = "first_found")
  v_costom <- gdx2::readGDX(gdx, name = c("v_costOM", "v_costom"),
                            select = list("_field" = "level"), format = "first_found")
  v_costin <- gdx2::readGDX(gdx, name = c("v_costInv", "v_costin"),
                            select = list("_field" = "level"), format = "first_found")

  ####### calculate minimal temporal and regional resolution #####
  y <- Reduce(intersect, list(getYears(cons), getYears(gdp), getYears(Xport),
                              getYears(Mport), getYears(pm_pvp)))
  cons_bau <- cons_bau[, y, ]
  gdp_bau <- gdp_bau[, y, ]
  Xport_bau <- Xport_bau[, y, ]
  Mport_bau <- Mport_bau[, y, ]
  pm_pvp_bau <- pm_pvp_bau[, y, ]
  cons <- cons[, y, ]
  gdp <- gdp[, y, ]
  Xport <- Xport[, y, ]
  Mport <- Mport[, y, ]
  pm_pvp <- pm_pvp[, y, ]
  v_costfu_bau <- v_costfu_bau[, y, ]
  v_costin_bau <- v_costin_bau[, y, ]
  v_costom_bau <- v_costom_bau[, y, ]
  v_costfu <- v_costfu[, y, ]
  v_costin <- v_costin[, y, ]
  v_costom <- v_costom[, y, ]
  ####### add global values
  cons_bau <- mbind(cons_bau, dimSums(cons_bau, dim = 1))
  gdp_bau <- mbind(gdp_bau, dimSums(gdp_bau, dim = 1))
  Xport_bau <- mbind(Xport_bau, dimSums(Xport_bau, dim = 1))
  Mport_bau <- mbind(Mport_bau, dimSums(Mport_bau, dim = 1))
  v_costfu_bau <- mbind(v_costfu_bau, dimSums(v_costfu_bau, dim = 1))
  v_costin_bau <- mbind(v_costin_bau, dimSums(v_costin_bau, dim = 1))
  v_costom_bau <- mbind(v_costom_bau, dimSums(v_costom_bau, dim = 1))
  cons <- mbind(cons, dimSums(cons, dim = 1))
  gdp <- mbind(gdp, dimSums(gdp, dim = 1))
  Xport <- mbind(Xport, dimSums(Xport, dim = 1))
  Mport <- mbind(Mport, dimSums(Mport, dim = 1))
  v_costfu <- mbind(v_costfu, dimSums(v_costfu, dim = 1))
  v_costin <- mbind(v_costin, dimSums(v_costin, dim = 1))
  v_costom <- mbind(v_costom, dimSums(v_costom, dim = 1))
  ####### some pre-calculations
  currAcc_bau <- dimSums((Xport_bau[, , trade] - Mport_bau[, , trade]) * pm_pvp_bau[, , trade] / setNames(pm_pvp_bau[, , "good"], NULL), dim = 3)
  currAcc <- dimSums((Xport[, , trade] - Mport[, , trade]) * pm_pvp[, , trade] / setNames(pm_pvp[, , "good"], NULL), dim = 3)
  ####### calculate reporting parameters ############
  tmp <- NULL
  tmp <- mbind(tmp, setNames((cons_bau - cons) * 1000, "Policy Cost|Consumption Loss (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames((cons_bau - cons) / (cons_bau + 1e-10) * 100, "Policy Cost|Consumption Loss|Relative to Reference Consumption (%)"))
  tmp <- mbind(tmp, setNames((gdp_bau - gdp) * 1000, "Policy Cost|GDP Loss (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames((gdp_bau - gdp) / (gdp_bau + 1e-10) * 100, "Policy Cost|GDP Loss|Relative to Reference GDP (%)"))
  tmp <- mbind(tmp, setNames((v_costfu + v_costin + v_costom - (v_costfu_bau + v_costin_bau + v_costom_bau)) * 1000, "Policy Cost|Additional Total Energy System Cost (billion US$2017/yr)"))
  # Policy costs calculated as consumption losses net the effect of climate-policy induced changes in the current account
  tmp <- mbind(tmp, setNames(((cons_bau + currAcc_bau) - (cons + currAcc)) * 1000, "Policy Cost|Consumption + Current Account Loss (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(((cons_bau + currAcc_bau) - (cons + currAcc)) / (cons_bau + currAcc_bau + 1e-10) * 100, "Policy Cost|Consumption + Current Account Loss|Relative to Reference Consumption + Current Account (%)"))

  # add other region aggregations
  if (!is.null(regionSubsetList)) {
    tmp <- mbind(tmp, calcRegionSubsetSums(tmp, regionSubsetList))
  }

  getSets(tmp)[3] <- "variable"
  return(tmp)
}
