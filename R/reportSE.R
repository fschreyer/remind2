#' Read in GDX and calculate secondary energy, used in convGDX2MIF.R for the
#' reporting
#'
#' Read in secondary energy information from GDX file, information used in
#' convGDX2MIF.R for the reporting
#'
#'
#' @param gdx a GDX as created by readGDX, or the file name of a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Gunnar Luderer, Lavinia Baumstark, Felix Schreyer, Falk Benke
#' @examples
#' \dontrun{
#' reportSE(gdx)
#' }
#'
#' @export
#' @importFrom gdx readGDX
#' @importFrom magclass mselect getSets getSets<- getYears getNames<- mbind
#' @importFrom abind abind
#' @importFrom rlang sym

reportSE <- function(gdx, regionSubsetList = NULL, t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {
  ####### get realisations #########
  module2realisation <- readGDX(gdx, "module2realisation")
  rownames(module2realisation) <- module2realisation$modules

  ####### conversion factors ##########
  TWa_2_EJ <- 3600 * 24 * 365 / 1e6
  s_tBC_2_TWa <- readGDX(gdx, name = "sm_tBC_2_TWa", format = "first_found", react = "silent") # Biochar calorific value
  if (is.null(s_tBC_2_TWa)){
    s_tBC_2_TWa <- 1          # necessary to avoid division by zero for versions preceding biochar introduction; to be removed with v360 (TD)
  }

  ####### read in needed data #########
  ## sets
  teChp     <- readGDX(gdx, "teChp") # technologies that produce seel as main output und sehe as secondary output
  teReNoBio <- readGDX(gdx, "teReNoBio") # renewable technologies except for biomass
  teCCS     <- readGDX(gdx, "teCCS") # technologies with carbon capture
  teNoCCS   <- readGDX(gdx, "teNoCCS") # technologies without CCS
  teccsinje <- readGDX(gdx, "teccsinje", react = "silent") # CCS injection technologies
  entyPe    <- readGDX(gdx, "entyPe") # primary energy types (PE)
  entySe    <- readGDX(gdx, "entySe") # secondary energy types
  peFos     <- readGDX(gdx, "peFos") # primary energy fossil fuels
  peBio     <- readGDX(gdx, "pebio") # biomass primary energy types
  pe2se     <- readGDX(gdx, "pe2se") # map primary energy carriers to secondary
  se2se     <- readGDX(gdx, "se2se") # map secondary energy to secondary energy using a technology
  pc2te     <- readGDX(gdx, "pc2te") # prod couple: mapping for own consumption of technologies

  seLiq <- intersect(c("seliqfos", "seliqbio", "seliqsyn"), entySe)
  seGas <- intersect(c("segafos", "segabio", "segasyn"), entySe)
  seSol <- intersect(c("sesofos", "sesobio"), entySe)

  # necessary to avoid errors for versions having only a single CCS injection technology; to be removed with release 3.6.0
  if(is.null(teccsinje)){
    teccsinje <- "ccsinje"
  }

  ## variables
  prodSE <- readGDX(gdx, name = "vm_prodSe", field = "l", restore_zeros = FALSE) * TWa_2_EJ
  prodSE <- mselect(prodSE, all_enty1 = entySe)
  storLoss <- readGDX(gdx, name = "v32_storloss", # total energy loss from storage for a given technology [TWa]
    field = "l", restore_zeros = TRUE) * TWa_2_EJ

  # calculate minimal temporal resolution #####
  y <- Reduce(intersect, list(getYears(prodSE), getYears(storLoss)))

  v_macBase <- readGDX(gdx, name = c("v_macBase", "vm_macBase"), field = "l", restore_zeros = FALSE, format = "first_found") * TWa_2_EJ
  v_macBase <- v_macBase[, y, ]
  vm_emiMacSector <- readGDX(gdx, name = c("vm_emiMacSector"), field = "l", restore_zeros = FALSE, format = "first_found") * TWa_2_EJ
  vm_emiMacSector <-   vm_emiMacSector[, y, ]
  ####### set temporal resolution #####
  prodSE    <- prodSE[, y, ]
  storLoss  <- storLoss[, y, ]

  #### adjust regional dimension of prodCouple (own consumption of technologies)
  prodCouple_tmp <- readGDX(gdx, "pm_prodCouple", restore_zeros = FALSE, format = "first_found")
  prodCouple_tmp[is.na(prodCouple_tmp)] <- 0
  prodCouple <- new.magpie(getRegions(prodSE), getYears(prodCouple_tmp), magclass::getNames(prodCouple_tmp), fill = 0)
  prodCouple[getRegions(prodCouple_tmp), , ] <- prodCouple_tmp
  getSets(prodCouple) <- getSets(prodCouple_tmp)
  prodCouple[prodCouple < 0] <- 0 # fix negative values to 0

  ####### internal function for reporting ###########
  get_prodSE <- function(inputCarrier, outputCarrier, te = c(pe2se$all_te, se2se$all_te), name = NULL, storageLossOnly = FALSE) {
    # test if inputs make sense
    if (length(setdiff(inputCarrier, abind::abind(entyPe, entySe))) > 0) {
      warning(paste("inputCarrier ", setdiff(inputCarrier, abind::abind(entyPe, entySe)), " is not element of entyPe or entySe"))
    }

    if (length(setdiff(outputCarrier, abind::abind(entyPe, entySe))) > 0) {
      warning(paste("outputCarrier ", setdiff(outputCarrier, abind::abind(entyPe, entySe)), " is not element of entyPe or entySe"))
    }

    ## storage losses
    pe2se_tmp <- pe2se[(pe2se$all_enty %in% inputCarrier) & (pe2se$all_enty1 %in% outputCarrier) & (pe2se$all_te %in% te), ]
    storageLosses <- dimSums(storLoss[pe2se_tmp], dim = 3, na.rm = TRUE)
    if (storageLossOnly) {
      out <- storageLosses
    } else {
      # Compute the SE of technologies that have outputCarrier as their main product
      SE <- dimSums(mselect(prodSE, all_enty = inputCarrier, all_enty1 = outputCarrier, all_te = te), dim = 3, na.rm = TRUE)

      # Some technologies output a couple of SE carriers: the main product (all_enty1), and the couple product (all_enty2)
      # Add the couple SE of technologies that have outputCarrier as their couple product (and another carrier as main product)
      pc2te_couple <- pc2te[(pc2te$all_enty %in% inputCarrier) & (pc2te$all_enty2 %in% outputCarrier) & (pc2te$all_te %in% te), ]
      SE <- SE + dimSums(prodSE[pc2te_couple] * prodCouple[pc2te_couple], dim = 3, na.rm = TRUE)
      # (as prodSE represents the energy produced for the main SE carrier, there is no need to
      # subtract the couple SE of technologies that have outputCarrier as their main product, but also have a couple product)
      out <- SE - storageLosses
    }

    if (!is.null(name)) magclass::getNames(out) <- name
    return(out)
  }

  get_lossSE <- function(...) { ### ... means that any arguments are passed directly to get_prodSE
    return(get_prodSE(..., storageLossOnly = TRUE))
  }

  ## reporting should adhere to the following logic:
  ## if a category has more than one subcategory, the subcategories should be reported *explicitly*.

  out <- get_prodSE(abind(entyPe, entySe), entySe, name = "SE (EJ/yr)")

  ## Biomass (SE|Biomass includes all secondary energy produced from biomass)
  out <- mbind(out,
    get_prodSE(peBio, entySe, name = "SE|Biomass (EJ/yr)")
  )


  ## Electricity
  out <- mbind(out,
    get_prodSE(append(entyPe, "seh2"), "seel",   name = "SE|Electricity (EJ/yr)"), # seh2 to account for se2se production once we add h2 to elec technology
    get_prodSE(entyPe, "seel", te = teChp,       name = "SE|Electricity|Combined Heat and Power w/o CC (EJ/yr)"),

    get_prodSE(peBio, "seel",                    name = "SE|Electricity|+|Biomass (EJ/yr)"),
    get_prodSE(peBio, "seel", te = teCCS,        name = "SE|Electricity|Biomass|+|w/ CC (EJ/yr)"),
    get_prodSE(peBio, "seel", te = teNoCCS,      name = "SE|Electricity|Biomass|+|w/o CC (EJ/yr)"),
    get_prodSE(peBio, "seel", te = "bioigccc",   name = "SE|Electricity|Biomass|++|Gasification Combined Cycle w/ CC (EJ/yr)"),
    get_prodSE(peBio, "seel", te = "bioigcc",    name = "SE|Electricity|Biomass|++|Gasification Combined Cycle w/o CC (EJ/yr)"),
    get_prodSE(peBio, "seel", te = "biochp",     name = "SE|Electricity|Biomass|++|Combined Heat and Power w/o CC (EJ/yr)"),
    get_prodSE(peBio, "seel", te = c("biopyrchp"),   name = "SE|Electricity|Biomass|++|Pyrolysis (EJ/yr)"),
    get_prodSE(peBio, "seel", te = setdiff(pe2se$all_te, c("bioigccc", "bioigcc", "biochp", "biopyrchp")),
                                                 name = "SE|Electricity|Biomass|++|Other (EJ/yr)"),

    get_prodSE("pecoal", "seel",                 name = "SE|Electricity|+|Coal (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = teCCS,     name = "SE|Electricity|Coal|+|w/ CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = teNoCCS,   name = "SE|Electricity|Coal|+|w/o CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = "igcc",    name = "SE|Electricity|Coal|++|Gasification Combined Cycle w/o CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = "igccc",   name = "SE|Electricity|Coal|++|Gasification Combined Cycle w/ CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = "pc",      name = "SE|Electricity|Coal|++|Pulverised Coal w/o CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = "coalchp", name = "SE|Electricity|Coal|++|Combined Heat and Power w/o CC (EJ/yr)"),
    get_prodSE("pecoal", "seel", te = setdiff(pe2se$all_te, c("igcc", "igccc", "pc", "coalchp")),
                                                 name = "SE|Electricity|Coal|++|Other (EJ/yr)"),

    get_prodSE("pegas", "seel",                  name = "SE|Electricity|+|Gas (EJ/yr)"),
    get_prodSE("pegas", "seel", te = teCCS,      name = "SE|Electricity|Gas|+|w/ CC (EJ/yr)"),
    get_prodSE("pegas", "seel", te = teNoCCS,    name = "SE|Electricity|Gas|+|w/o CC (EJ/yr)"),
    get_prodSE("pegas", "seel", te = "ngcc",     name = "SE|Electricity|Gas|++|Combined Cycle w/o CC (EJ/yr)"),
    get_prodSE("pegas", "seel", te = "ngccc",    name = "SE|Electricity|Gas|++|Combined Cycle w/ CC (EJ/yr)"),
    get_prodSE("pegas", "seel", te = "ngt",      name = "SE|Electricity|Gas|++|Gas Turbine (EJ/yr)"),
    get_prodSE("pegas", "seel", te = "gaschp",   name = "SE|Electricity|Gas|++|Combined Heat and Power w/o CC (EJ/yr)"),

    get_prodSE("seh2", "seel",                   name = "SE|Electricity|+|Hydrogen (EJ/yr)"),

    get_prodSE("peoil", "seel",                  name = "SE|Electricity|+|Oil (EJ/yr)"),
    get_prodSE("peoil", "seel", te = teNoCCS,    name = "SE|Electricity|Oil|w/o CC (EJ/yr)"),
    get_prodSE("peoil", "seel", te = "dot",      name = "SE|Electricity|Oil|DOT (EJ/yr)"),

    get_prodSE(entyPe, "seel", te = teReNoBio,   name = "SE|Electricity|Non-Biomass Renewables (EJ/yr)"),

    get_prodSE("peur", "seel",                   name = "SE|Electricity|+|Nuclear (EJ/yr)"),

    get_prodSE("pegeo", "seel",                  name = "SE|Electricity|+|Geothermal (EJ/yr)"),

    get_prodSE("pehyd", "seel",                  name = "SE|Electricity|+|Hydro (EJ/yr)"),

    get_prodSE(c("pewin", "pesol"), "seel",      name = "SE|Electricity|WindSolar (EJ/yr)"),
    get_prodSE("pesol", "seel",                  name = "SE|Electricity|+|Solar (EJ/yr)"),
    get_prodSE("pesol", "seel", te = "csp",      name = "SE|Electricity|Solar|+|CSP (EJ/yr)"),
    get_prodSE("pesol", "seel", te = "spv",      name = "SE|Electricity|Solar|+|PV (EJ/yr)"),

    get_lossSE(c("pewin", "pesol"), "seel",      name = "SE|Electricity|Curtailment (EJ/yr)"),
    get_lossSE("pesol", "seel",                  name = "SE|Electricity|Curtailment|+|Solar (EJ/yr)"),
    get_lossSE("pesol", "seel", te = "csp",      name = "SE|Electricity|Curtailment|Solar|+|CSP (EJ/yr)"),
    get_lossSE("pesol", "seel", te = "spv",      name = "SE|Electricity|Curtailment|Solar|+|PV (EJ/yr)"),

    get_prodSE("pewin", "seel",                  name = "SE|Electricity|+|Wind (EJ/yr)"),
    get_prodSE("pewin", "seel", te = "windon",   name = "SE|Electricity|Wind|+|Onshore (EJ/yr)"),
    get_prodSE("pewin", "seel", te = "windoff",  name = "SE|Electricity|Wind|+|Offshore (EJ/yr)"),

    get_lossSE("pewin", "seel",                  name = "SE|Electricity|Curtailment|+|Wind (EJ/yr)"),
    get_lossSE("pewin", "seel", te = "windon",   name = "SE|Electricity|Curtailment|Wind|+|Onshore (EJ/yr)"),
    get_lossSE("pewin", "seel", te = "windoff",  name = "SE|Electricity|Curtailment|Wind|+|Offshore (EJ/yr)")
  )


  ## Gases
  if (!(is.null(v_macBase) & is.null(vm_emiMacSector))) {
    ## exogenous variable for representing reused gas from waste landfills (accounted in the model as segabio)
    MtCH4_2_TWa <- readGDX(gdx, "sm_MtCH4_2_TWa", react = "silent")
    if (is.null(MtCH4_2_TWa)) {
      MtCH4_2_TWa <- 0.001638
    }
    out <- mbind(out,
      setNames(MtCH4_2_TWa * (v_macBase[, , "ch4wstl"] - vm_emiMacSector[, , "ch4wstl"]), "SE|Gases|Biomass|Waste (EJ/yr)")
    )
  } else {
    out <- mbind(out,
      setNames(new.magpie(cells_and_regions = getRegions(prodCouple), years = y, fill = 0), "SE|Gases|Biomass|Waste (EJ/yr)")
    )
  }

  out <- mbind(out,
    get_prodSE(c(entyPe, "seh2"), seGas,        name = "SE|Gases (EJ/yr)"),
    get_prodSE(peBio, seGas,                    name = "SE|Gases|+|Biomass (EJ/yr)"),
    get_prodSE(peBio, seGas, te = teCCS,        name = "SE|Gases|Biomass|+|w/ CC (EJ/yr)"),
    get_prodSE(peBio, seGas, te = teNoCCS,      name = "SE|Gases|Biomass|+|w/o CC (EJ/yr)"),
    get_prodSE("seh2", seGas,                   name = "SE|Gases|+|Hydrogen (EJ/yr)"),
    get_prodSE(peFos, seGas,                    name = "SE|Gases|+|Fossil (EJ/yr)"),
    get_prodSE("pegas", seGas,                  name = "SE|Gases|Fossil|+|Natural Gas (EJ/yr)"),
    get_prodSE("pecoal", seGas,                 name = "SE|Gases|Fossil|+|Coal (EJ/yr)"),
    get_prodSE("pecoal", seGas, te = teCCS,     name = "SE|Gases|Fossil|Coal|+|w/ CC (EJ/yr)"),
    get_prodSE("pecoal", seGas, te = teNoCCS,   name = "SE|Gases|Fossil|Coal|+|w/o CC (EJ/yr)")
  )

  ## Heat
  out <- mbind(out,
    get_prodSE(entyPe,    "sehe",                 name = "SE|Heat (EJ/yr)"),
    get_prodSE(peBio,     "sehe",                 name = "SE|Heat|+|Biomass (EJ/yr)"),
    get_prodSE("pecoal",  "sehe",                 name = "SE|Heat|+|Coal (EJ/yr)"),
    get_prodSE("pegas",   "sehe",                 name = "SE|Heat|+|Gas (EJ/yr)"),
    get_prodSE("pegeo",   "sehe",                 name = "SE|Heat|+|Geothermal (EJ/yr)"), # same as SE|Heat|Electricity|Heat Pump
    get_prodSE("pegeo",   "sehe",                 name = "SE|Heat|Electricity|Heat Pump (EJ/yr)"), # same as SE|Heat|+|Geothermal
    get_prodSE("pesol",   "sehe",                 name = "SE|Heat|+|Solar (EJ/yr)"),
    get_prodSE(peFos,     "sehe",                 name = "SE|Heat|Fossil (EJ/yr)"),
    get_prodSE(entyPe,    "sehe", te = teChp,     name = "SE|Heat|Combined Heat and Power (EJ/yr)"),
    get_prodSE("pecoal",  "sehe", te = "coalchp", name = "SE|Heat|Coal|Combined Heat and Power (EJ/yr)"),
    get_prodSE("pegas",   "sehe", te = "gaschp",  name = "SE|Heat|Gas|Combined Heat and Power (EJ/yr)"),
    get_prodSE(peBio,     "sehe", te = "biochp",  name = "SE|Heat|Biomass|+|Combined Heat and Power (EJ/yr)"),
    get_prodSE(peBio,     "sehe", te = "biohp",   name = "SE|Heat|Biomass|+|Heat (EJ/yr)"),
    get_prodSE(peBio,     "sehe", te = c("biopyrchp", "biopyrhe"),    name = "SE|Heat|Biomass|+|Pyrolysis (EJ/yr)")
  )

  ## Hydrogen
  out <- mbind(out,
    get_prodSE(c(entyPe, entySe), "seh2",      name = "SE|Hydrogen (EJ/yr)"),
    get_prodSE(peBio, "seh2",                  name = "SE|Hydrogen|+|Biomass (EJ/yr)"),
    get_prodSE(peBio, "seh2", te = teCCS,      name = "SE|Hydrogen|Biomass|+|w/ CC (EJ/yr)"),
    get_prodSE(peBio, "seh2", te = teNoCCS,    name = "SE|Hydrogen|Biomass|+|w/o CC (EJ/yr)"),
    get_prodSE("pecoal", "seh2",               name = "SE|Hydrogen|+|Coal (EJ/yr)"),
    get_prodSE("pecoal", "seh2", te = teCCS,   name = "SE|Hydrogen|Coal|+|w/ CC (EJ/yr)"),
    get_prodSE("pecoal", "seh2", te = teNoCCS, name = "SE|Hydrogen|Coal|+|w/o CC (EJ/yr)"),
    get_prodSE("pegas", "seh2",                name = "SE|Hydrogen|+|Gas (EJ/yr)"),
    get_prodSE("pegas", "seh2", te = teCCS,    name = "SE|Hydrogen|Gas|+|w/ CC (EJ/yr)"),
    get_prodSE("pegas", "seh2", te = teNoCCS,  name = "SE|Hydrogen|Gas|+|w/o CC (EJ/yr)"),
    get_prodSE("seel", "seh2",                 name = "SE|Hydrogen|+|Electricity (EJ/yr)"),
    get_prodSE("seel", "seh2", te = "elh2",    name = "SE|Hydrogen|Electricity|+|Standard Electrolysis (EJ/yr)"),
    get_prodSE("seel", "seh2", te = "elh2VRE", name = "SE|Hydrogen|Electricity|+|VRE Storage Electrolysis (EJ/yr)"),
    get_prodSE(peFos, "seh2",                  name = "SE|Hydrogen|Fossil (EJ/yr)"),
    get_prodSE(peFos, "seh2", te = teCCS,      name = "SE|Hydrogen|Fossil|+|w/ CC (EJ/yr)"),
    get_prodSE(peFos, "seh2", te = teNoCCS,    name = "SE|Hydrogen|Fossil|+|w/o CC (EJ/yr)")
  )

  ## Liquids
  out <- mbind(out,
    get_prodSE(c(entyPe, "seh2"), seLiq,        name = "SE|Liquids (EJ/yr)"),
    get_prodSE(peBio, seLiq,                    name = "SE|Liquids|+|Biomass (EJ/yr)"),
    get_prodSE(peBio, seLiq, te = teCCS,        name = "SE|Liquids|Biomass|+|w/ CC (EJ/yr)"),
    get_prodSE(peBio, seLiq, te = teNoCCS,      name = "SE|Liquids|Biomass|+|w/o CC (EJ/yr)"),
    get_prodSE("pebiolc", seLiq,                name = "SE|Liquids|Biomass|++|Cellulosic (EJ/yr)"),
    get_prodSE("pebiolc", seLiq, teCCS,         name = "SE|Liquids|Biomass|Cellulosic|+|w/ CC (EJ/yr)"),
    get_prodSE("pebiolc", seLiq, teNoCCS,       name = "SE|Liquids|Biomass|Cellulosic|+|w/o CC (EJ/yr)"),
    get_prodSE(c("pebioil", "pebios"), seLiq,   name = "SE|Liquids|Biomass|++|Non-Cellulosic (EJ/yr)"),
    get_prodSE(c("pebioil", "pebios"), seLiq,   name = "SE|Liquids|Biomass|1st Generation (EJ/yr)"),
    get_prodSE("pebios", seLiq,                 name = "SE|Liquids|Biomass|Conventional Ethanol (EJ/yr)"),
    get_prodSE(peBio, seLiq, "bioftcrec",       name = "SE|Liquids|Biomass|BioFTR|w/ CC (EJ/yr)"),
    get_prodSE(peBio, seLiq, "bioftrec",        name = "SE|Liquids|Biomass|BioFTR|w/o CC (EJ/yr)"),
    get_prodSE(peBio, seLiq, "biopyrliq",       name = "SE|Liquids|Biomass|BioFTR|w/ pyrolysis (EJ/yr)"),
    get_prodSE(peBio, seLiq, "biodiesel",       name = "SE|Liquids|Biomass|Biodiesel (EJ/yr)"),
    get_prodSE(peBio, seLiq, "bioethl",         name = "SE|Liquids|Biomass|Lignocellulosic Ethanol (EJ/yr)"),
    get_prodSE("pebioil", seLiq,                name = "SE|Liquids|Biomass|Non-Cellulosic|+|Oil-based (EJ/yr)"),
    get_prodSE("pebios", seLiq,                 name = "SE|Liquids|Biomass|Non-Cellulosic|+|Sugar and Starch (EJ/yr)"),
    get_prodSE("seh2", seLiq,                   name = "SE|Liquids|+|Hydrogen (EJ/yr)"),
    get_prodSE(peFos, seLiq,                    name = "SE|Liquids|+|Fossil (EJ/yr)"),
    get_prodSE("pecoal", seLiq,                 name = "SE|Liquids|Fossil|+|Coal (EJ/yr)"),
    get_prodSE("pecoal", seLiq, te = teCCS,     name = "SE|Liquids|Fossil|Coal|+|w/ CC (EJ/yr)"),
    get_prodSE("pecoal", seLiq, te = teNoCCS,   name = "SE|Liquids|Fossil|Coal|+|w/o CC (EJ/yr)"),
    get_prodSE("pegas", seLiq,                  name = "SE|Liquids|Fossil|+|Gas (EJ/yr)"),
    get_prodSE("pegas", seLiq, te = teCCS,      name = "SE|Liquids|Fossil|Gas|+|w/ CC (EJ/yr)"),
    get_prodSE("pegas", seLiq, te = teNoCCS,    name = "SE|Liquids|Fossil|Gas|+|w/o CC (EJ/yr)"),
    get_prodSE("peoil", seLiq,                  name = "SE|Liquids|Fossil|+|Oil (EJ/yr)"),
    get_prodSE(peFos, seLiq, te = teCCS,        name = "SE|Liquids|Fossil|++|w/ CC (EJ/yr)"),
    get_prodSE(peFos, seLiq, te = teNoCCS,      name = "SE|Liquids|Fossil|++|w/o CC (EJ/yr)"),
    get_prodSE(peFos, seLiq, te = teNoCCS,      name = "SE|Liquids|Fossil|w/ oil|w/o CC (EJ/yr)")
  )

  ## Solids
  out <- mbind(out,
    get_prodSE(entyPe, seSol,               name = "SE|Solids (EJ/yr)"),
    get_prodSE("pecoal", seSol,             name = "SE|Solids|+|Coal (EJ/yr)"),
    # SE|Solids|Biomass is supposed to exclude traditional biomass
    get_prodSE(peBio, seSol, te = setdiff(pe2se$all_te, "biotr"),
                                            name = "SE|Solids|+|Biomass (EJ/yr)"),
    get_prodSE(peBio, seSol, te = "biotr",  name = "SE|Solids|+|Traditional Biomass (EJ/yr)")
  )

  ## Biochar
  if("sebiochar" %in% entySe){  # for backwards compatibility, to be removed with v360 (TD)
    out <- mbind(out,
      get_prodSE(entyPe, "sebiochar",                    name = "SE|Biochar (EJ/yr)"),
      get_prodSE(entyPe, "sebiochar", te = "biopyronly", name = "SE|Biochar|+|w/o co-product (EJ/yr)"),
      get_prodSE(entyPe, "sebiochar", te = "biopyrhe",   name = "SE|Biochar|+|w/ heat (EJ/yr)"),
      get_prodSE(entyPe, "sebiochar", te = "biopyrchp",  name = "SE|Biochar|+|w/ heat and power (EJ/yr)"),
      get_prodSE(entyPe, "sebiochar", te = "biopyrliq",  name = "SE|Biochar|+|w/ liquids (EJ/yr)")
    )} else {
    out <- mbind(out,
      setNames(new.magpie(getRegions(out), getYears(out), fill = 0), "SE|Biochar (EJ/yr)"),
      setNames(new.magpie(getRegions(out), getYears(out), fill = 0), "SE|Biochar|+|w/o co-product (EJ/yr)"),
      setNames(new.magpie(getRegions(out), getYears(out), fill = 0), "SE|Biochar|+|w/ heat (EJ/yr)"),
      setNames(new.magpie(getRegions(out), getYears(out), fill = 0), "SE|Biochar|+|w/ heat and power (EJ/yr)"),
      setNames(new.magpie(getRegions(out), getYears(out), fill = 0), "SE|Biochar|+|w/ liquids (EJ/yr)")
    )}

    out <- mbind(out,
      setNames(out[, , "SE|Biochar (EJ/yr)"] / TWa_2_EJ / s_tBC_2_TWa * 10^-6, "SE|Biochar [Mt] (Mt/yr)"),
      setNames(out[, , "SE|Biochar|+|w/o co-product (EJ/yr)"] / TWa_2_EJ / s_tBC_2_TWa / 10^6, "SE|Biochar [Mt]|+|w/o co-product (Mt/yr)"),
      setNames(out[, , "SE|Biochar|+|w/ heat (EJ/yr)"] / TWa_2_EJ / s_tBC_2_TWa / 10^6, "SE|Biochar [Mt]|+|w/ heat (Mt/yr)"),
      setNames(out[, , "SE|Biochar|+|w/ heat and power (EJ/yr)"] / TWa_2_EJ / s_tBC_2_TWa / 10^6, "SE|Biochar [Mt]|+|w/ heat and power (Mt/yr)"),
      setNames(out[, , "SE|Biochar|+|w/ liquids (EJ/yr)"] / TWa_2_EJ / s_tBC_2_TWa / 10^6, "SE|Biochar [Mt]|+|w/ liquids (Mt/yr)")
    )

  ## Trade
  if (module2realisation["trade", 2] == "se_trade") {

    vm_Mport <- readGDX(gdx, "vm_Mport", field = "l", restore_zeros = FALSE)[, t, ]
    vm_Xport <- readGDX(gdx, "vm_Xport", field = "l", restore_zeros = FALSE)[, t, ]

    out <- mbind(out,
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seh2") * TWa_2_EJ,
        "SE|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seel") * TWa_2_EJ,
        "SE|Electricity|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "seliqsyn") * TWa_2_EJ,
        "SE|Liquids|Hydrogen|Net Imports (EJ/yr)"),
      setNames(mselect(vm_Mport - vm_Xport, all_enty = "segasyn") * TWa_2_EJ,
        "SE|Gases|Hydrogen|Net Imports (EJ/yr)"))
  }

  ## SE Demand Flows ----
  # SE|Input|X|Y variables denote the demand of energy carrier X
  # flowing into sector/production of Y.

  vm_demFeSector <- readGDX(gdx, "vm_demFeSector", field = "l", restore_zeros = FALSE)[, y, ] * TWa_2_EJ
  vm_demFeSector[is.na(vm_demFeSector)] <- 0

  ## Ensure backwards compatibility for release version 3.6.0 (can be removed with 3.7.0)
  getNames(vm_demFeSector, dim = 3) <- tolower(getNames(vm_demFeSector, dim = 3))
  ## End backwards compatibility

  # SE demand
  vm_demSe <- readGDX(gdx, "vm_demSe", field = "l", restore_zeros = FALSE)[, y, ] * TWa_2_EJ
  # SE demand of specific energy system technologies
  v_demSeOth <- readGDX(gdx, c("v_demSeOth", "vm_demSeOth"), field = "l", restore_zeros = FALSE)[, y, ] * TWa_2_EJ
  # conversion efficiency
  pm_eta_conv <- readGDX(gdx, "pm_eta_conv", field = "l", restore_zeros = FALSE)[, y, ]

  # hydrogen used for electricity production via H2 turbines
  out <- mbind(out,
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel"), dim = 3), "SE|Input|Hydrogen|Electricity (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turb"), dim = 3), "SE|Input|Hydrogen|Electricity|+|Normal Turbines (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel", all_te = "h2turbVRE"), dim = 3), "SE|Input|Hydrogen|Electricity|+|Forced VRE Turbines (EJ/yr)")
  )

  # hydrogen used for synthetic fuels
  out <- mbind(out,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn", "segasyn"), all_te = c("MeOH", "h22ch4")), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seliqsyn", all_te = "MeOH"), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels|+|Liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "segasyn", all_te = "h22ch4"), dim = 3), "SE|Input|Hydrogen|Synthetic Fuels|+|Gases (EJ/yr)")
  )

  # hydrogen used for other energy system technologies subsumed in v_demSeOth
  # e.g. co-firing of h2 in csp
  seh2 <- setNames(dimSums(mselect(v_demSeOth, all_enty = "seh2"), dim = 3),
                   "SE|Input|Hydrogen|Other Energy System Consumption (EJ/yr)")
  seh2 <- magclass::matchDim(seh2, out, dim = 1, fill = NA)
  out <- mbind(out, seh2)

  # SE electricity use

  ### calculation of electricity use for own consumption of energy system
  vm_prodFe <- readGDX(gdx, "vm_prodFe", field = "l", restore_zeros = FALSE) * TWa_2_EJ  # all energy values are first converted to EJ
  vm_co2CCS <- readGDX(gdx, "vm_co2CCS", field = "l", restore_zeros = FALSE)

  # filter for coupled production coefficents which consume seel
  # (have all_enty2=seel and are negative)
  teprodCoupleSeel <- getNames(mselect(prodCouple_tmp, all_enty2 = "seel"), dim = 3)
  CoeffOwnConsSeel <- prodCouple_tmp[, , teprodCoupleSeel]
  CoeffOwnConsSeel[CoeffOwnConsSeel > 0] <- 0
  CoeffOwnConsSeel_woCCS <- CoeffOwnConsSeel[, , teccsinje, invert = TRUE]

  # FE and SE production that has own consumption of electricity
  # calculate prodSE back to TWa (was in EJ before), but prod couple coefficient is defined in TWa(input)/Twa(output)
  prodOwnCons <- mbind(vm_prodFe / TWa_2_EJ, prodSE / TWa_2_EJ)[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)]

  out <- mbind(out, setNames(
    -TWa_2_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS * prodOwnCons[, , getNames(CoeffOwnConsSeel_woCCS, dim = 3)], dim = 3, na.rm = TRUE) +
        dimSums(CoeffOwnConsSeel[, , teccsinje] * vm_co2CCS[, , teccsinje], dim = 3,  na.rm = TRUE)),
    "SE|Input|Electricity|Self Consumption Energy System (EJ/yr)"))

  # electricity for central ground heat pumps
  out <- mbind(out, setNames(
    -TWa_2_EJ *
      (dimSums(CoeffOwnConsSeel_woCCS[, , "geohe"] * prodOwnCons[, , "geohe"], dim = 3)),
    "SE|Input|Electricity|Self Consumption Energy System|Central Ground Heat Pump (EJ/yr)"))

  # electricity for fuel extraction, e.g. electricity used for oil and gas extraction

  # read in with restore_zero = F first, to get non-zero third dimension
  pm_fuExtrOwnCons_reduced <- readGDX(gdx, "pm_fuExtrOwnCons", restore_zeros = FALSE)
  # read in again with restore_zero = T to get all regions in case the parameter is zero for some regions
  pm_fuExtrOwnCons <- readGDX(gdx, "pm_fuExtrOwnCons", restore_zeros = TRUE)[, , getNames(pm_fuExtrOwnCons_reduced)]
  vm_fuExtr <- readGDX(gdx, "vm_fuExtr", field = "l", restore_zeros = FALSE)[, y, ]

  # calculate electricity for fuel extraction as in q32_balSe
  # by multiplying fuel consumption of extraction with extraction quantities
  out <- mbind(out,
                setNames(
                  # sum over all PE carriers and extraction grades
                  dimSums(
                    # sum over pm_fuExtrOwnCons to reduce all_enty dimensions
                    dimSums(mselect(pm_fuExtrOwnCons, all_enty = "seel"), dim = 3.1)
                    * vm_fuExtr[, , getNames(pm_fuExtrOwnCons, dim = 2)], dim = 3)
                    * TWa_2_EJ,
                  "SE|Input|Electricity|PE Production (EJ/yr)"))

  # set to zero in 2005 as the fuel production electricity demand is not included in the SE balance equation in this year
  # due to incompatibilities with the InitialCap module
  out[, "y2005", "SE|Input|Electricity|PE Production (EJ/yr)"] <- 0

  # share of electrolysis H2 in total H2
  p_shareElec_H2 <- collapseNames(out[, , "SE|Hydrogen|+|Electricity (EJ/yr)"] / out[, , "SE|Hydrogen (EJ/yr)"])
  p_shareElec_H2[is.na(p_shareElec_H2)] <- 0


  # share of domestically produced H2 (only not 1 if se trade module on and hydrogen can be imported/exported)
  if (module2realisation["trade", 2] == "se_trade") {
    p_share_H2DomProd <-  collapseNames(out[, , "SE|Hydrogen (EJ/yr)"] / (out[, , "SE|Hydrogen|Net Imports (EJ/yr)"] + out[, , "SE|Hydrogen (EJ/yr)"]))
  } else {
    p_share_H2DomProd <- out[, , "SE|Hydrogen (EJ/yr)"]
    p_share_H2DomProd[] <- 1
  }

  out <- mbind(out,
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "build"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|Buildings (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "indst"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|Industry (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feelt", emi_sectors = "trans"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdelt"),
    "SE|Input|Electricity|Transport (EJ/yr)"),
    setNames(dimSums(mselect(vm_demFeSector, all_enty = "seel", all_enty1 = "feels", emi_sectors = "cdr"), dim = 3) /
      mselect(pm_eta_conv, all_te = "tdels"),
    "SE|Input|Electricity|CDR (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2"), dim = 3),
      "SE|Input|Electricity|Hydrogen (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2"), dim = 3),
      "SE|Input|Electricity|Hydrogen|+|Standard Electrolysis (EJ/yr)"),
    setNames(dimSums(mselect(vm_demSe, all_enty = "seel", all_enty1 = "seh2", all_te = "elh2VRE"), dim = 3),
      "SE|Input|Electricity|Hydrogen|+|VRE Storage (EJ/yr)"),
    setNames(dimSums(vm_demSe[,,"feels"] + vm_demSe[,,"feelt"], dim = 3) -
               dimSums(vm_prodFe[,,"feels"] + vm_prodFe[,,"feelt"], dim = 3),
             "SE|Input|Electricity|T&D losses (EJ/yr)"),
    setNames(dimSums(vm_demSe[,,"feels"] * (1 - pm_eta_conv[,,"tdels"]) , dim = 3) +
               dimSums(vm_demSe[,,"feelt"] * (1 - pm_eta_conv[,,"tdelt"]) , dim = 3),
             "SE|Electricity|Transmission Losses (EJ/yr)")  # this variable name was used before and might still be needed by some templates
  )

  # electricity for specific H2 usages
  out <- mbind(
    out,
    # calculate electricity going into domestic (!) H2 production for direct H2 use (FE Hydrogen)
    setNames(
      dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("feh2s", "feh2t")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|direct FE H2 (EJ/yr)"
    ),
    # calculate electricity used for storage of electricity
    setNames(
      dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = "seel"), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Electricity Storage (EJ/yr)"
    )
  )

  # electricity used for domestic (!) synfuel production
  out <- mbind(out,
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn", "segasyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("seliqsyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Liquids (EJ/yr)"),
      setNames(dimSums(mselect(vm_demSe, all_enty = "seh2", all_enty1 = c("segasyn")), dim = 3) * p_share_H2DomProd *
        p_shareElec_H2 / mselect(pm_eta_conv, all_te = "elh2"),
      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Gases (EJ/yr)"))

  # other t&d losses
  out <- mbind(out,
      setNames(dimSums(vm_demSe[,,"fegas"] + vm_demSe[,,"fegat"] - vm_prodFe[,,"fegas"] - vm_prodFe[,,"fegat"], dim = 3),
           "SE|Input|Gases|T&D losses (EJ/yr)"),
      setNames(dimSums(vm_demSe[,,"fehes"] - vm_prodFe[,,"fehes"], dim = 3),
               "SE|Input|Heat|T&D losses (EJ/yr)"),
      setNames(dimSums(vm_demSe[,,"fehos"] + vm_demSe[,,"fepet"] + vm_demSe[,,"fedie"]
                     - vm_prodFe[,,"fehos"] - vm_prodFe[,,"fepet"] - vm_prodFe[,,"fedie"], dim = 3),
               "SE|Input|Liquids|T&D losses (EJ/yr)"),
      setNames(dimSums(vm_demSe[,,"feh2s"] + vm_demSe[,,"feh2t"] - vm_prodFe[,,"feh2s"] - vm_prodFe[,,"feh2t"], dim = 3),
               "SE|Input|Hydrogen|T&D losses (EJ/yr)"),
      setNames(dimSums(vm_demSe[,,"fesos"] - vm_prodFe[,,"fesos"], dim = 3),
               "SE|Input|Solids|T&D losses (EJ/yr)")
  )

  # add global values
  out <- mbind(out, dimSums(out, dim = 1))
  # add other region aggregations
  if (!is.null(regionSubsetList))
    out <- mbind(out, calc_regionSubset_sums(out, regionSubsetList))

  getSets(out)[3] <- "variable"
  return(out)
}
