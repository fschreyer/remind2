#' Read in GDX and calculate trade, used in convGDX2MIF.R for the reporting
#'
#' Read in trade information from GDX file, information used in convGDX2MIF.R
#' for the reporting
#'
#'
#' @param gdx a GDX object as created by gdx2::readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @return MAgPIE object - contains the price variables
#' @author Lavinia Baumstark, Christoph Bertram, Anselm Schultes
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportTrade(gdx)
#' }
#'
#' @export
#' @importFrom magclass getYears setNames mbind

reportTrade <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {

  ####### get realisations #########
  module2realisation <- gdx2::readGDX(gdx, "module2realisation", stringsAsFactors = FALSE)
  rownames(module2realisation) <- module2realisation$modules

  TWa_2_EJ <- 3600 * 24 * 365 / 1e6
  C_2_CO2 <- 44 / 12
  sm_tdptwyr2dpgj <- 31.71 # TerraDollar per TWyear to Dollar per GJ

  pm_eta_conv <- gdx2::readGDX(gdx, name = "pm_eta_conv")
  p_costsPEtradeMp <- gdx2::readGDX(gdx, name = c("pm_costsPEtradeMp", "p_costsPEtradeMp"), format = "first_found")
  pm_pvp <- gdx2::readGDX(gdx, name = "pm_pvp")
  Xport <- gdx2::readGDX(gdx, name = "vm_Xport", select = list("_field" = "level"))
  Mport <- gdx2::readGDX(gdx, name = "vm_Mport", select = list("_field" = "level"))

  # calculate maximal temporal resolution
  y <- Reduce(intersect, list(getYears(pm_pvp), getYears(Xport)))
  Xport <- Xport[, y, ]
  Mport <- Mport[, y, ]
  pm_pvp <- pm_pvp[, y, ]
  pm_eta_conv <- pm_eta_conv[, y, ]

  # AJS for current account
  set_trade <- gdx2::readGDX(gdx, name = "trade", type = "Set")

  tradeMacro <- gdx2::readGDX(gdx, "tradeMacro")
  tradePe <- gdx2::readGDX(gdx, "tradePe")
  tradeSe <- gdx2::readGDX(gdx, "tradeSe")


  # calculate variables
  trade <- Xport - Mport
  trade_net <- Xport - (1 - p_costsPEtradeMp) * Mport
  price <- pm_pvp / setNames(pm_pvp[, , "good"], NULL) # so in TeraDollar per either TWyr (pecoal,pegas,peoil,pebiolc), Gt C (perm), and Mt Uran (peUr) respectively

  # build reporting
  tmp <- NULL
  tmp <- mbind(tmp, setNames(trade_net[, , "pecoal"] * TWa_2_EJ, "Trade|Coal (EJ/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "pecoal"]) * Mport[, , "pecoal"] * TWa_2_EJ, "Trade|Imports|Coal (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "pecoal"] * TWa_2_EJ, "Trade|Exports|Coal (EJ/yr)"))
  tmp <- mbind(tmp, setNames(trade_net[, , "pegas"] * TWa_2_EJ, "Trade|Gas (EJ/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "pegas"]) * Mport[, , "pegas"] * TWa_2_EJ, "Trade|Imports|Gas (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "pegas"] * TWa_2_EJ, "Trade|Exports|Gas (EJ/yr)"))
  tmp <- mbind(tmp, setNames(trade_net[, , "peoil"] * TWa_2_EJ, "Trade|Oil (EJ/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "peoil"]) * Mport[, , "peoil"] * TWa_2_EJ, "Trade|Imports|Oil (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "peoil"] * TWa_2_EJ, "Trade|Exports|Oil (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "peur"] * 1000, "Trade|Uranium|Mass (ktU/yr)"))
  tmp <- mbind(tmp, setNames(trade_net[, , "peur"] * pm_eta_conv[, , "tnrs"] * TWa_2_EJ, "Trade|Uranium|seelEquivalent (EJ/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "peur"]) * Mport[, , "peur"] * pm_eta_conv[, , "tnrs"] * TWa_2_EJ, "Trade|Imports|Uranium (ktU/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "peur"] * pm_eta_conv[, , "tnrs"] * TWa_2_EJ, "Trade|Exports|Uranium (ktU/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "good"] * 1000, "Trade|Goods (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "good"]) * Mport[, , "good"] * 1000, "Trade|Imports|Goods (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "good"] * 1000, "Trade|Exports|Goods (billion US$2017/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "pebiolc"] * TWa_2_EJ, "Trade|Biomass (EJ/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "pebiolc"]) * Mport[, , "pebiolc"] * TWa_2_EJ, "Trade|Imports|Biomass (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "pebiolc"] * TWa_2_EJ, "Trade|Exports|Biomass (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "perm"] * C_2_CO2 * 1000, "Trade|Emi Allowances|Volume (Mt CO2-equiv/yr)"))
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "perm"]) * Mport[, , "perm"] * C_2_CO2 * 1000, "Trade|Imports|Emi Allowances|Volume (Mt CO2-equiv/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "perm"] * C_2_CO2 * 1000, "Trade|Exports|Emi Allowances|Volume (Mt CO2-equiv/yr)"))

  # SE Trade
  tmp <- mbind(tmp, setNames(trade_net[, , "seh2"] * TWa_2_EJ, "Trade|SE|Hydrogen (EJ/yr)"))
  # for now p_costsPEtradeMp is zero for SE trade, adapt once SE trade transport cost implemented
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "seh2"]) * Mport[, , "seh2"] * TWa_2_EJ, "Trade|Imports|SE|Hydrogen (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "seh2"] * TWa_2_EJ, "Trade|Exports|SE|Hydrogen (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "seel"] * TWa_2_EJ, "Trade|SE|Electricity (EJ/yr)"))
  # for now p_costsPEtradeMp is zero for SE trade, adapt once SE trade transport cost implemented
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "seel"]) * Mport[, , "seel"] * TWa_2_EJ, "Trade|Imports|SE|Electricity (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "seel"] * TWa_2_EJ, "Trade|Exports|SE|Electricity (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "seliqsyn"] * TWa_2_EJ, "Trade|SE|Liquids|Hydrogen (EJ/yr)"))
  # for now p_costsPEtradeMp is zero for SE trade, adapt once SE trade transport cost implemented
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "seliqsyn"]) * Mport[, , "seliqsyn"] * TWa_2_EJ, "Trade|Imports|SE|Liquids|Hydrogen (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "seliqsyn"] * TWa_2_EJ, "Trade|Exports|SE|Liquids|Hydrogen (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "seliqbio"] * TWa_2_EJ, "Trade|SE|Liquids|Biomass (EJ/yr)"))
  # for now p_costsPEtradeMp is zero for SE trade, adapt once SE trade transport cost implemented
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "seliqbio"]) * Mport[, , "seliqbio"] * TWa_2_EJ, "Trade|Imports|SE|Liquids|Biomass (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "seliqbio"] * TWa_2_EJ, "Trade|Exports|SE|Liquids|Biomass (EJ/yr)"))

  tmp <- mbind(tmp, setNames(trade_net[, , "segasyn"] * TWa_2_EJ, "Trade|SE|Gases|Hydrogen (EJ/yr)"))
  # for now p_costsPEtradeMp is zero for SE trade, adapt once SE trade transport cost implemented
  tmp <- mbind(tmp, setNames((1 - p_costsPEtradeMp[, , "segasyn"]) * Mport[, , "segasyn"] * TWa_2_EJ, "Trade|Imports|SE|Gases|Hydrogen (EJ/yr)"))
  tmp <- mbind(tmp, setNames(Xport[, , "segasyn"] * TWa_2_EJ, "Trade|Exports|SE|Gases|Hydrogen (EJ/yr)"))

  # add global values
  tmp <- mbind(tmp, dimSums(tmp, dim = 1))
  trade <- mbind(trade, dimSums(trade, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList)) {
    tmp <- mbind(tmp, calcRegionSubsetSums(tmp, regionSubsetList))
    trade <- mbind(trade, calcRegionSubsetSums(trade, regionSubsetList))
  }

  # values use global prices
  tmp <- mbind(tmp, setNames(trade[, , "pecoal"] * price[, , "pecoal"] * 1000, "Trade|Primary Energy|Coal|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(trade[, , "pegas"] * price[, , "pegas"] * 1000, "Trade|Primary Energy|Gas|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(trade[, , "peoil"] * price[, , "peoil"] * 1000, "Trade|Primary Energy|Oil|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(trade[, , "peur"] * price[, , "peur"] * 1000, "Trade|Uranium|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(trade[, , "pebiolc"] * price[, , "pebiolc"] * sm_tdptwyr2dpgj, "Trade|Biomass|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(trade[, , "perm"] * price[, , "perm"] * 1000, "Trade|Emi Allowances|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(dimSums(trade[, , set_trade] * price[, , set_trade], dim = 3) * 1000, "Trade|All|Value (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(dimSums(trade[, , set_trade] * price[, , set_trade], dim = 3), "Current Account (billion US$2017/yr)"))

  # rename dimensions in the magpie object
  getSets(tmp)[3] <- "variable"
  return(tmp)
}
