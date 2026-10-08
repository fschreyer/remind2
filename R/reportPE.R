#' Read in GDX and calculate primary energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in primary energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by gdx2::readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Lavinia Baumstark, Fabrice Lécuyer
#' @examples
#' \dontrun{
#' reportPE(gdx)
#' }
#'
#' @export
#' @importFrom magclass mselect getYears getNames<- mbind setNames matchDim

reportPE <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {
  ####### conversion factors ##########
  TWa_2_EJ <- 3600 * 24 * 365 / 1e6
  ####### read in needed data #########
  ## sets
  teCCS    <- gdx2::readGDX(gdx, "teCCS") # technologies with carbon capture
  teNoCCS  <- gdx2::readGDX(gdx, "teNoCCS") # technologies without CCS
  entySe   <- gdx2::readGDX(gdx, "entySe") # secondary energy types
  peFos    <- gdx2::readGDX(gdx, "peFos") # primary energy fossil fuels
  peBio    <- gdx2::readGDX(gdx, "peBio") # biomass primary energy types
  # map primary energy carriers to secondary
  pe2se    <- gdx2::readGDX(gdx, "pe2se", uniqueStyle = "classic", stringsAsFactors = FALSE)
  # prod couple: mapping for own consumption and co-production of technologies
  pc2te    <- gdx2::readGDX(gdx, "pc2te", uniqueStyle = "classic", stringsAsFactors = FALSE)
  pc2te    <- pc2te[(pc2te$all_enty1 %in% entySe) & (pc2te$all_enty2 %in% entySe), ] # ensure main and couple product are valid entySe

  seLiq    <- intersect(c("seliqfos", "seliqbio"), entySe)
  seGas    <- intersect(c("segafos", "segabio"), entySe)
  seSol    <- intersect(c("sesofos", "sesobio"), entySe)

  ## variables
  demPE  <- gdx2::readGDX(gdx, name = "vm_demPe", select = list("_field" = "level"), restoreZeros = FALSE, uniqueStyle = "classic")
  prodSE <- gdx2::readGDX(gdx, name = "vm_prodSe", select = list("_field" = "level"), restoreZeros = FALSE, uniqueStyle = "classic")
  y <- Reduce(intersect, list(getYears(demPE), getYears(prodSE))) # calculate minimal temporal resolution
  demPE  <- demPE[pe2se][, y, ] * TWa_2_EJ
  prodSE <- mselect(prodSE, all_enty1 = entySe)[, y, ] * TWa_2_EJ
  fuExtr <- gdx2::readGDX(gdx, "vm_fuExtr", select = list("_field" = "level"))[, y, ] * TWa_2_EJ
  Mport  <- gdx2::readGDX(gdx, "vm_Mport", select = list("_field" = "level"))[, y, ] * TWa_2_EJ
  Xport  <- gdx2::readGDX(gdx, "vm_Xport", select = list("_field" = "level"))[, y, ] * TWa_2_EJ

  ## parameters
  prodCouple_tmp <- gdx2::readGDX(gdx, "pm_prodCouple", restoreZeros = FALSE, uniqueStyle = "classic") # share of couple production
  prodCouple <- magclass::matchDim(prodCouple_tmp, prodSE, dim = 1, fill = 0) # adjust regional dimension
  getSets(prodCouple) <- getSets(prodCouple_tmp) # necessary because matchDim overwrites sets names of dim 3
  prodCouple[prodCouple < 0] <- 0 # ignore negative values (own consumption of technologies)

  pm_costsPEtradeMp <- gdx2::readGDX(gdx, "pm_costsPEtradeMp", restoreZeros = FALSE)


  ####### internal functions for reporting ###########
  get_demPE <- function(PEcarrier, SEcarrier = entySe, te = pe2se$all_te, name = NULL) {
    # Compute the PE of technologies that have SEcarrier as their main product
    PE <- dimSums(mselect(demPE, all_enty = PEcarrier, all_enty1 = SEcarrier, all_te = te), dim = 3)

    # Some technologies output a couple of SE carriers: the main product (all_enty1), and the couple product (all_enty2)
    pc2te_subset <- pc2te[(pc2te$all_enty %in% PEcarrier) & (pc2te$all_te %in% te), ]

    # Compute the share of a particular SE in the output of each technology by summing over its couple products (dim 3.4)
    coupleContribution <- demPE[pc2te_subset] * prodCouple[pc2te_subset] / (1 + dimSums(prodCouple[pc2te_subset], dim = 3.4))
    # Add contribution of technologies that have SEcarrier as their couple product (all_enty2) and another main product
    PE <- PE + dimSums(mselect(coupleContribution, all_enty2 = SEcarrier), dim = 3)
    # Subtract contribution of technologies that have SEcarrier as their main product (all_enty1) and have couple products
    PE <- PE - dimSums(mselect(coupleContribution, all_enty1 = SEcarrier), dim = 3)

    if (!is.null(name)) magclass::getNames(PE) <- name
    return(PE)
  }

  get_prodSE <- function(PEcarrier, SEcarrier = entySe, te = pe2se$all_te, name) {
    setNames(dimSums(mselect(prodSE, all_enty = PEcarrier, all_enty1 = SEcarrier, all_te = te), dim = 3), name)
  }

  ####### calculate reporting parameters ############
  out <- mbind(
    # fossil fuels
    get_demPE(peFos,                                                 name = "PE|Fossil (EJ/yr)"),
    get_demPE(peFos, te = teCCS,                                     name = "PE|Fossil|+|w/ CC (EJ/yr)"),
    get_demPE(peFos, te = teNoCCS,                                   name = "PE|Fossil|+|w/o CC (EJ/yr)"),

    get_demPE("pecoal",                                              name = "PE|+|Coal (EJ/yr)"),
    get_demPE("pecoal", te = teCCS,                                  name = "PE|Coal|++|w/ CC (EJ/yr)"),
    get_demPE("pecoal", te = teNoCCS,                                name = "PE|Coal|++|w/o CC (EJ/yr)"),
    get_demPE("pecoal", "seel",                                      name = "PE|Coal|+|Electricity (EJ/yr)"),
    get_demPE("pecoal", "seel", teCCS,                               name = "PE|Coal|Electricity|+|w/ CC (EJ/yr)"),
    get_demPE("pecoal", "seel", teNoCCS,                             name = "PE|Coal|Electricity|+|w/o CC (EJ/yr)"),
    get_demPE("pecoal", seGas,                                       name = "PE|Coal|+|Gases (EJ/yr)"),
    get_demPE("pecoal", seGas, teCCS,                                name = "PE|Coal|Gases|+|w/ CC (EJ/yr)"),
    get_demPE("pecoal", seGas, teNoCCS,                              name = "PE|Coal|Gases|+|w/o CC (EJ/yr)"),
    get_demPE("pecoal", seLiq,                                       name = "PE|Coal|+|Liquids (EJ/yr)"),
    get_demPE("pecoal", seLiq, teCCS,                                name = "PE|Coal|Liquids|+|w/ CC (EJ/yr)"),
    get_demPE("pecoal", seLiq, teNoCCS,                              name = "PE|Coal|Liquids|+|w/o CC (EJ/yr)"),
    get_demPE("pecoal", "seh2",                                      name = "PE|Coal|+|Hydrogen (EJ/yr)"),
    get_demPE("pecoal", "seh2", teCCS,                               name = "PE|Coal|Hydrogen|+|w/ CC (EJ/yr)"),
    get_demPE("pecoal", "seh2", teNoCCS,                             name = "PE|Coal|Hydrogen|+|w/o CC (EJ/yr)"),
    get_demPE("pecoal", "sehe",                                      name = "PE|Coal|+|Heat (EJ/yr)"),
    get_demPE("pecoal", seSol,                                       name = "PE|Coal|+|Solids (EJ/yr)"),

    get_demPE("peoil",                                               name = "PE|+|Oil (EJ/yr)"),
    get_demPE("peoil", te = teCCS,                                   name = "PE|Oil|++|w/ CC (EJ/yr)"),
    get_demPE("peoil", te = teNoCCS,                                 name = "PE|Oil|++|w/o CC (EJ/yr)"),
    get_demPE("peoil", "seel",                                       name = "PE|Oil|+|Electricity (EJ/yr)"),
    get_demPE("peoil", seLiq,                                        name = "PE|Oil|+|Liquids (EJ/yr)"),

    get_demPE("pegas",                                               name = "PE|+|Gas (EJ/yr)"),
    get_demPE("pegas", te = teCCS,                                   name = "PE|Gas|++|w/ CC (EJ/yr)"),
    get_demPE("pegas", te = teNoCCS,                                 name = "PE|Gas|++|w/o CC (EJ/yr)"),
    get_demPE("pegas", "seel",                                       name = "PE|Gas|+|Electricity (EJ/yr)"),
    get_demPE("pegas", "seel", teCCS,                                name = "PE|Gas|Electricity|+|w/ CC (EJ/yr)"),
    get_demPE("pegas", "seel", teNoCCS,                              name = "PE|Gas|Electricity|+|w/o CC (EJ/yr)"),
    get_demPE("pegas", seGas,                                        name = "PE|Gas|+|Gases (EJ/yr)"),
    get_demPE("pegas", seLiq,                                        name = "PE|Gas|+|Liquids (EJ/yr)"),
    get_demPE("pegas", seLiq, teCCS,                                 name = "PE|Gas|Liquids|+|w/ CC (EJ/yr)"),
    get_demPE("pegas", seLiq, teNoCCS,                               name = "PE|Gas|Liquids|+|w/o CC (EJ/yr)"),
    get_demPE("pegas", "sehe",                                       name = "PE|Gas|+|Heat (EJ/yr)"),
    get_demPE("pegas", "seh2",                                       name = "PE|Gas|+|Hydrogen (EJ/yr)"),
    get_demPE("pegas", "seh2", teCCS,                                name = "PE|Gas|Hydrogen|+|w/ CC (EJ/yr)"),
    get_demPE("pegas", "seh2", teNoCCS,                              name = "PE|Gas|Hydrogen|+|w/o CC (EJ/yr)"),

    # biomass
    get_demPE(peBio,                                                 name = "PE|+|Biomass (EJ/yr)"),
    get_demPE(peBio, te = teCCS,                                     name = "PE|Biomass|++|w/ CC (EJ/yr)"),
    get_demPE(peBio, te = teNoCCS,                                   name = "PE|Biomass|++|w/o CC (EJ/yr)"),
    get_demPE(c("pebioil", "pebios"),                                name = "PE|Biomass|+++|1st Generation (EJ/yr)"),
    get_demPE(peBio, te = "biotr",                                   name = "PE|Biomass|++++|Traditional (EJ/yr)"),
    get_demPE(peBio, "seel",                                         name = "PE|Biomass|+|Electricity (EJ/yr)"),
    get_demPE(peBio, "seel", teCCS,                                  name = "PE|Biomass|Electricity|+|w/ CC (EJ/yr)"),
    get_demPE(peBio, "seel", teNoCCS,                                name = "PE|Biomass|Electricity|+|w/o CC (EJ/yr)"),
    get_demPE(peBio, seGas,                                          name = "PE|Biomass|+|Gases (EJ/yr)"),
    get_demPE(peBio, seGas, teCCS,                                   name = "PE|Biomass|Gases|+|w/ CC (EJ/yr)"),
    get_demPE(peBio, seGas, teNoCCS,                                 name = "PE|Biomass|Gases|+|w/o CC (EJ/yr)"),
    get_demPE(peBio, "seh2",                                         name = "PE|Biomass|+|Hydrogen (EJ/yr)"),
    get_demPE(peBio, "seh2", teCCS,                                  name = "PE|Biomass|Hydrogen|+|w/ CC (EJ/yr)"),
    get_demPE(peBio, "seh2", teNoCCS,                                name = "PE|Biomass|Hydrogen|+|w/o CC (EJ/yr)"),
    get_demPE(peBio, seLiq,                                          name = "PE|Biomass|+|Liquids (EJ/yr)"),
    get_demPE(peBio, seLiq, teCCS,                                   name = "PE|Biomass|Liquids|+|w/ CC (EJ/yr)"),
    get_demPE(peBio, seLiq, teNoCCS,                                 name = "PE|Biomass|Liquids|+|w/o CC (EJ/yr)"),
    get_demPE("pebiolc", seLiq,                                      name = "PE|Biomass|Liquids|Cellulosic (EJ/yr)"),
    get_demPE("pebiolc", seLiq, teCCS,                               name = "PE|Biomass|Liquids|Cellulosic|+|w/ CC (EJ/yr)"),
    get_demPE("pebiolc", seLiq, teNoCCS,                             name = "PE|Biomass|Liquids|Cellulosic|+|w/o CC (EJ/yr)"),
    get_demPE(c("pebioil", "pebios"), seLiq,                         name = "PE|Biomass|Liquids|Non-Cellulosic (EJ/yr)"),
    get_demPE("pebios", seLiq,                                       name = "PE|Biomass|Liquids|Conventional Ethanol (EJ/yr)"),
    get_demPE(peBio, seLiq, c("bioftrec", "bioftcrec", "biodiesel", "biopyrliq"), name = "PE|Biomass|Liquids|Biodiesel (EJ/yr)"),
    get_demPE(peBio, seLiq, c("bioftcrec"),                          name = "PE|Biomass|Liquids|Biodiesel|+|w/ CC (EJ/yr)"),
    get_demPE(peBio, seLiq, c("bioftrec", "biodiesel", "biopyrliq"), name = "PE|Biomass|Liquids|Biodiesel|+|w/o CC (EJ/yr)"),
    get_demPE(peBio, seSol,                                          name = "PE|Biomass|+|Solids (EJ/yr)"),
    get_demPE(peBio, "sebiochar",                                    name = "PE|Biomass|+|Biochar (EJ/yr)"),
    get_demPE(peBio, "sehe",                                         name = "PE|Biomass|+|Heat (EJ/yr)"),

    # renewables and nuclear
    # using prodSE rather than demPE to follow the "direct method" for primary energy:
    # https://ourworldindata.org/energy-substitution-method
    get_prodSE(c("pegeo", "pehyd", "pewin", "pesol"),                name = "PE|Non-Biomass Renewables (EJ/yr)"),
    get_prodSE("pegeo",                                              name = "PE|+|Geothermal (EJ/yr)"),
    get_prodSE("pegeo", "sehe",                                      name = "PE|Geothermal|+|Heat (EJ/yr)"),
    get_prodSE("pegeo", "seel",                                      name = "PE|Geothermal|+|Electricity (EJ/yr)"),
    get_prodSE("peur",                                               name = "PE|+|Nuclear (EJ/yr)"),
    get_prodSE("pehyd",                                              name = "PE|+|Hydro (EJ/yr)"),
    get_prodSE("pewin",                                              name = "PE|+|Wind (EJ/yr)"),
    get_prodSE("pewin", te = "windon",                               name = "PE|Wind|+|Onshore (EJ/yr)"),
    get_prodSE("pewin", te = "windoff",                              name = "PE|Wind|+|Offshore (EJ/yr)"),
    get_prodSE("pesol",                                              name = "PE|+|Solar (EJ/yr)"),
    get_prodSE("pesol", "sehe",                                      name = "PE|Solar|+|Heat (EJ/yr)"),
    get_prodSE("pesol", "seel",                                      name = "PE|Solar|+|Electricity (EJ/yr)")
  )

  # advanced calculation
  out <- mbind(out,
    setNames(dimSums(demPE[, , c(peFos, peBio)], dim = 3)
           + dimSums(prodSE[, , c("pegeo", "pehyd", "pewin", "pesol", "peur")], dim = 3), "PE (EJ/yr)"),
    setNames(dimSums(demPE[, , peBio], dim = 3)
           - dimSums(mselect(demPE, all_enty = peBio, all_te = "biotr"), dim = 3),  "PE|Biomass|++++|Modern (EJ/yr)"),
    setNames(fuExtr[, , "pebiolc.2"], "PE|Biomass|+++|Residues (EJ/yr)"),
    setNames((fuExtr[, , "pebiolc.1"]
              + (1 - pm_costsPEtradeMp[, , "pebiolc"]) * Mport[, , "pebiolc"]
              - Xport[, , "pebiolc"]), "PE|Biomass|+++|Energy Crops (EJ/yr)")
  )

  # add global values
  out <- mbind(out, dimSums(out, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calcRegionSubsetSums(out, regionSubsetList))

  getSets(out)[3] <- "variable"
  return(out)
}
