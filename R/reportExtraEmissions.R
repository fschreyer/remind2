#' Report Extra Emissions
#'
#' Calulate additonal emissions based on CEDS 2020 emissions and
#' REMIND/EDGE-Transport Data
#'
#' @param mif a mif file with reported variables from remind2 and EDGE-Transport
#' @param extraData path to extra data files to be used in the reporting
#' @param gdx a GDX as created by readGDX, or the file name of a gdx, needed to define region subsets
#' @author Gabriel Abrahão, Falk Benke
#' @export
reportExtraEmissions <- function(mif, extraData, gdx) {
  # Define region subsets ----
  regionSubsetList <- toolRegionSubsets(gdx)

  # ADD EU-27 region aggregation if possible
  if ("EUR" %in% names(regionSubsetList)) {
    regionSubsetList <- c(regionSubsetList, list(
      "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW")
    ))
  }

  # Read in selected mif variables for calculation of extra emissions ----
  reportVars <- c(
    "ES|Transport|Bunkers|Freight",
    "ES|Transport|Pass|Aviation",
    "FE|Buildings|Gases|Fossil",
    "FE|Buildings|Gases",
    "FE|Buildings|Liquids",
    "FE|Buildings|Solids",

    # Use net CO2 emissions for aviation and shipping instead of tailpipe
    # emissions as activity variables. Net CO2 emissions only include fossil
    # fuel combustion, which seems more adequate for scaling airpollutants,
    # since combustion of biofuels and synfuels has typically smaller
    # airpollutant emissions per unit of CO2 emitted.
    "Emi|CO2|Energy|Demand|Transport|Pass|Domestic Aviation",
    "Emi|CO2|Energy|Demand|Transport|Bunkers|Pass|International Aviation",
    "Emi|CO2|Energy|Demand|Transport|Bunkers|Freight|International Shipping"
  )

  report <- quitte::read.quitte(mif, check.duplicates = FALSE) %>%
    deletePlus()

  model <- unique(report$model)[grepl("REMIND", unique(report$model))][1] # Deals with REMIND-MAgPIE mifs
  scenario <- unique(report$scenario)[1]

  reportCalc <- report %>%
    filter(.data$variable %in% reportVars, .data$region != "World") %>%
    as.magpie() %>%
    collapseDim()


  # Read in additional reporting files ----

  if (!file.exists(file.path(extraData, "p_emissions4ReportExtraCEDS.cs4r"))) {
    stop("Auxiliary file 'p_emissions4ReportExtraCEDS.cs4r' not found")
  }

  if (!file.exists(file.path(extraData, "p_emissions4ReportExtraIAMC.cs4r"))) {
    stop("Auxiliary file 'p_emissions4ReportExtraIAMC.cs4r' not found")
  }

  if (!file.exists(file.path(extraData, "emi2020_sectNOGAINS_sourceCEDS.cs4r"))) {
    stop("Auxiliary file 'emi2020_sectNOGAINS_sourceCEDS.cs4r' not found")
  }

  cedsceds <- read.magpie(file.path(extraData, "p_emissions4ReportExtraCEDS.cs4r"))
  cedsiamc <- read.magpie(file.path(extraData, "p_emissions4ReportExtraIAMC.cs4r"))
  cedsairpoll <- read.magpie(file.path(extraData, "emi2020_sectNOGAINS_sourceCEDS.cs4r"))


  if (!is.null(regionSubsetList)) {
    cedsceds <- mbind(cedsceds, calc_regionSubset_sums(cedsceds, regionSubsetList))
    cedsiamc <- mbind(cedsiamc, calc_regionSubset_sums(cedsiamc, regionSubsetList))
    cedsairpoll <- mbind(cedsairpoll, calc_regionSubset_sums(cedsairpoll, regionSubsetList))
  }

  # check if regional resolution matches
  if (length(setdiff(getItems(reportCalc, dim = 1), getItems(cedsceds, dim = 1))) != 0 ||
    length(setdiff(getItems(cedsceds, dim = 1), getItems(reportCalc, dim = 1))) != 0
  ) {
    stop("Regional resolution in 'p_emissions4ReportExtraCEDS.cs4r' and gdx do not match.")
  }

  if (length(setdiff(getItems(reportCalc, dim = 1), getItems(cedsiamc, dim = 1))) != 0 ||
    length(setdiff(getItems(cedsiamc, dim = 1), getItems(reportCalc, dim = 1))) != 0
  ) {
    stop("Regional resolution in 'p_emissions4ReportExtraIAMC.cs4r' and gdx do not match.")
  }

  if (length(setdiff(getItems(reportCalc, dim = 1), getItems(cedsairpoll, dim = 1))) != 0 ||
    length(setdiff(getItems(cedsairpoll, dim = 1), getItems(reportCalc, dim = 1))) != 0
  ) {
    stop("Regional resolution in 'emi2020_sectNOGAINS_sourceCEDS.cs4r' and gdx do not match.")
  }


  # 1. Calculate CH4 and N2O emissions that are based on emission factors.  ----
  # Derive EFs based on CEDS 2020 emissions and REMIND 2020 activities
  .deriveEF <- function(emirefyear, actreffull, refyear = 2020, convyear = NULL) {
    # EF in the reference year
    ef2020 <- setYears(emirefyear / actreffull[, refyear, ], NULL)

    # Preallocate with actreffull to ensure they will multiply nicely later
    ef <- actreffull
    ef[, , ] <- ef2020

    # If convyear was given, assume linear convergence towards
    # the global emission factor in convyear
    if (!is.null(convyear)) {
      gef <- as.numeric(
        dimSums(emirefyear, dim = 1) / dimSums(actreffull[, refyear, ], dim = 1)
      )
      ef <- convergence(
        ef, gef,
        start_year = 2020, end_year = convyear, type = "linear"
      )
    }
    return(ef)
  }

  MtN_to_ktN2O <- 44 / 28 * 1000 # conversion from MtN to ktN2O

  out <- NULL

  # N2O from international shipping
  # Converge to global EF in 2060
  ef <- .deriveEF(
    dimReduce(cedsiamc[, 2020, "International Shipping.n2o_n"]),
    reportCalc[, , "ES|Transport|Bunkers|Freight"],
    refyear = 2020,
    convyear = 2060
  )

  out <- mbind(
    out,
    setNames(
      reportCalc[, , "ES|Transport|Bunkers|Freight"] * ef * MtN_to_ktN2O,
      "Emi|N2O|Extra|Transport|Bunkers|Freight (kt N2O/yr)"
    )
  )
  # CH4 from international shipping (should be very small)
  # Converge to global EF in 2060
  ef <- .deriveEF(
    dimReduce(cedsiamc[, 2020, "International Shipping.ch4"]),
    reportCalc[, , "ES|Transport|Bunkers|Freight"],
    refyear = 2020,
    convyear = 2060
  )


  out <- mbind(
    out,
    setNames(
      reportCalc[, , "ES|Transport|Bunkers|Freight"] * ef,
      "Emi|CH4|Extra|Transport|Bunkers|Freight (Mt CH4/yr)"
    )
  )
  # N2O from domestic+international aviation.
  # Converge to global EF in 2060
  ef <- .deriveEF(
    dimReduce(cedsiamc[, 2020, "Aircraft.n2o_n"]),
    reportCalc[, , "ES|Transport|Pass|Aviation"],
    refyear = 2020,
    convyear = 2060
  )

  out <- mbind(
    out,
    setNames(
      reportCalc[, , "ES|Transport|Pass|Aviation"] * ef * MtN_to_ktN2O,
      "Emi|N2O|Extra|Transport|Pass|Aviation (kt N2O/yr)"
    )
  )
  # CH4 from residential+commercial, assume most of it is from incomplete biomass/solids burning. Requires CEDS detail
  # Don't assume convergence, as Global South EFs may be more representative of solids burning
  ef <- .deriveEF(
    dimReduce(cedsceds[, 2020, "1A4a_Commercial-institutional.ch4"] + cedsceds[, 2020, "1A4b_Residential.ch4"]),
    reportCalc[, , "FE|Buildings|Solids"],
    refyear = 2020,
    convyear = NULL
  )
  out <- mbind(
    out,
    setNames(
      reportCalc[, , "FE|Buildings|Solids"] * ef,
      "Emi|CH4|Extra|Buildings|Solids (Mt CH4/yr)"
    )
  )
  # N2O from residential+commercial. Requires CEDS detail, assume it's all from fuel burning byproducts.
  # Common solid fuels tend to have higher N2O EFs than common gaseous and liquid fuels, but here
  # we are implicitly assuming the 2020 mix Solids+Liquids+Gases determines the EF.
  # See https://www.epa.gov/system/files/documents/2024-02/ghg-emission-factors-hub-2024.pdf
  # Don't assume convergence, as Global South EFs may be more representative of solids burning
  tmp <- dimSums(reportCalc[, , c("FE|Buildings|Gases", "FE|Buildings|Liquids", "FE|Buildings|Solids")], dim = 3)
  ef <- .deriveEF(
    dimReduce(cedsceds[, 2020, "1A4a_Commercial-institutional.n2o_n"] + cedsceds[, 2020, "1A4b_Residential.n2o_n"]),
    tmp,
    refyear = 2020,
    convyear = NULL
  )
  out <- mbind(
    out,
    setNames(
      tmp * ef * MtN_to_ktN2O,
      "Emi|N2O|Extra|Buildings (kt N2O/yr)"
    )
  )

  # 2. Calculate AP emissions that are based on ratio to CO2 emissions.  ----

  airpollutants <- c("BC", "CO", "NH3", "NOx", "OC", "SO2", "VOC")

  # Derive emission ratio (ER) based on CEDS 2020 AP emissions and REMIND 2020 CO2 emissions
  .deriveER <- function(emiAPrefyear, emico2, refyear = 2020, convyear = "never") {
    # regional ER in the reference year
    rer2020 <- emiAPrefyear / emico2[, refyear, ]
    # global ER in the reference year
    ger2020 <- dimSums(emiAPrefyear, dim = 1) / dimSums(emico2[, refyear, ], dim = 1)
    # Preallocate with emico2 to ensure they will multiply nicely later
    er <- emico2

    if (convyear == "immediate") {
      # If convyear is set to immediate, always use global emission ratio
      er[, , ] <- ger2020
    } else if (convyear == "never") {
      # If convyear is set to never, always use regional emission ratio
      er[, , ] <- rer2020
    } else {
      # If another value for convyear is given, assume linear convergence towards
      # the global emission ratio in convyear
      er[, , ] <- rer2020
      er <- convergence(
        er, as.numeric(ger2020),
        start_year = 2020, end_year = convyear, type = "linear"
      )
    }
    return(er)
  }

  # Air Pollutants from Domestic Aviation
  # Converge to global ER in 2060
  for (spec in airpollutants) {
    er <- .deriveER(
      dimReduce(cedsairpoll[, 2020, paste0("NOGAINS Domestic Aviation.", spec)]),
      reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Pass|Domestic Aviation"],
      refyear = 2020,
      convyear = 2060
    )
    out <- mbind(
      out,
      setNames(
        reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Pass|Domestic Aviation"] * er,
        paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Pass|Domestic Aviation (Mt ", spec, "/yr)")
      )
    )
  }

  # Air Pollutants from International Aviation
  # Always use global emissions ratio since CEDS2025 emissions are only available as global total (and regional
  # disaggregation in REMIND was uniform)
  for (spec in airpollutants) {
    er <- .deriveER(
      dimReduce(cedsairpoll[, 2020, paste0("NOGAINS International Aviation.", spec)]),
      reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Bunkers|Pass|International Aviation"],
      refyear = 2020,
      convyear = "immediate"
    )
    out <- mbind(
      out,
      setNames(
        reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Bunkers|Pass|International Aviation"] * er,
        paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers|Pass|International Aviation (Mt ", spec, "/yr)")
      )
    )
  }

  # Aggregation: Domestic Aviation + International Aviation
  for (spec in airpollutants) {
    out <- mbind(
      out,
      setNames(
        dimSums(out[
          , ,
          c(
            paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers|Pass|International Aviation (Mt ", spec, "/yr)"),
            paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Pass|Domestic Aviation (Mt ", spec, "/yr)")
          )
        ], dim = 3),
        paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Pass|Aviation (Mt ", spec, "/yr)")
      )
    )
  }

  # Air Pollutants from International Shipping
  # Calculate global AP emissions based on ratio to global CO2 emissions.
  for (spec in airpollutants) {
    er <- .deriveER(
      dimReduce(cedsairpoll[, 2020, paste0("NOGAINS International Shipping.", spec)]),
      reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Bunkers|Freight|International Shipping"],
      refyear = 2020,
      convyear = "immediate"
    )
    out <- mbind(
      out,
      setNames(
        reportCalc[, , "Emi|CO2|Energy|Demand|Transport|Bunkers|Freight|International Shipping"] * er,
        paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers|Freight|International Shipping (Mt ", spec, "/yr)")
      )
    )
  }

  # Aggregation: International Shipping + International Aviation
  for (spec in airpollutants) {
    out <- mbind(
      out,
      setNames(
        dimSums(out[
          , ,
          c(
            paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers|Pass|International Aviation (Mt ", spec, "/yr)"),
            paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers|Freight|International Shipping (Mt ", spec, "/yr)")
          )
        ], dim = 3),
        paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers (Mt ", spec, "/yr)")
      )
    )
  }

  # Aggregate to global. Since all variables are emissions, we can just sum them
  out <- mbind(out, setItems(dimSums(out, dim = 1), dim = 1, value = "World"))

  # 3. Aggregate AP emissions with variables from reportAirPollutantEmissions----

  # Read variables reported in reportAirPollutantEmissions
  reportAPvars <- c(
    paste0("Emi|", airpollutants, "|Energy|Demand|Industry"),
    paste0("Emi|", airpollutants, "|Energy|Demand|Buildings"),
    paste0("Emi|", airpollutants, "|Energy|Demand|Transport|Ground"),
    paste0("Emi|", airpollutants, "|Energy|Supply"),
    paste0("Emi|", airpollutants, "|Industrial Processes"),
    paste0("Emi|", airpollutants, "|Product Use|Solvents"),
    paste0("Emi|", airpollutants, "|Waste"),
    paste0("Emi|", airpollutants, "|AFOLU")
  )
  reportAgg <- report %>%
    filter(.data$variable %in% reportAPvars) %>%
    as.magpie(spatial = "region", temporal = "period") %>%
    collapseDim()

  # Aggregate for all airpollutants
  for (spec in airpollutants) {
    # w/o Bunkers|Energy|Demand|Transport
    out <- mbind(
      out,
      setNames(
        reportAgg[, , paste0("Emi|", spec, "|Energy|Demand|Transport|Ground.Mt ", spec, "/yr")] +
          out[, , paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Pass|Domestic Aviation (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/o Bunkers|Energy|Demand|Transport (Mt ", spec, "/yr)")
      )
    )
    # w/ Bunkers|Energy|Demand|Transport
    out <- mbind(
      out,
      setNames(
        dimSums(out[, , c(
          paste0("Emi|", spec, "|w/o Bunkers|Energy|Demand|Transport (Mt ", spec, "/yr)"),
          paste0("Emi|", spec, "|Extra|Energy|Demand|Transport|Bunkers (Mt ", spec, "/yr)")
        )], dim = 3),
        paste0("Emi|", spec, "|w/ Bunkers|Energy|Demand|Transport (Mt ", spec, "/yr)")
      )
    )
    # w/o Bunkers|Energy|Demand
    out <- mbind(
      out,
      setNames(
        dimSums(reportAgg[, , c(
          paste0("Emi|", spec, "|Energy|Demand|Industry.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|Energy|Demand|Buildings.Mt ", spec, "/yr")
        )], dim = 3) +
          out[, , paste0("Emi|", spec, "|w/o Bunkers|Energy|Demand|Transport (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/o Bunkers|Energy|Demand (Mt ", spec, "/yr)")
      )
    )
    # w/ Bunkers|Energy|Demand
    out <- mbind(
      out,
      setNames(
        dimSums(reportAgg[, , c(
          paste0("Emi|", spec, "|Energy|Demand|Industry.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|Energy|Demand|Buildings.Mt ", spec, "/yr")
        )], dim = 3) +
          out[, , paste0("Emi|", spec, "|w/ Bunkers|Energy|Demand|Transport (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/ Bunkers|Energy|Demand (Mt ", spec, "/yr)")
      )
    )
    # w/o Bunkers|Energy
    out <- mbind(
      out,
      setNames(
        reportAgg[, , paste0("Emi|", spec, "|Energy|Supply.Mt ", spec, "/yr")] +
          out[, , paste0("Emi|", spec, "|w/o Bunkers|Energy|Demand (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/o Bunkers|Energy (Mt ", spec, "/yr)")
      )
    )
    # w/ Bunkers|Energy
    out <- mbind(
      out,
      setNames(
        reportAgg[, , paste0("Emi|", spec, "|Energy|Supply.Mt ", spec, "/yr")] +
          out[, , paste0("Emi|", spec, "|w/ Bunkers|Energy|Demand (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/ Bunkers|Energy (Mt ", spec, "/yr)")
      )
    )
    # w/o Bunkers|Energy and Industrial Processes
    out <- mbind(
      out,
      setNames(
        reportAgg[, , paste0("Emi|", spec, "|Industrial Processes.Mt ", spec, "/yr")] +
          out[, , paste0("Emi|", spec, "|w/o Bunkers|Energy (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/o Bunkers|Energy and Industrial Processes (Mt ", spec, "/yr)")
      )
    )
    # w/ Bunkers|Energy and Industrial Processes
    out <- mbind(
      out,
      setNames(
        reportAgg[, , paste0("Emi|", spec, "|Industrial Processes.Mt ", spec, "/yr")] +
          out[, , paste0("Emi|", spec, "|w/ Bunkers|Energy (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/ Bunkers|Energy and Industrial Processes (Mt ", spec, "/yr)")
      )
    )
    # w/o Bunkers
    out <- mbind(
      out,
      setNames(
        dimSums(reportAgg[, , c(
          paste0("Emi|", spec, "|Product Use|Solvents.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|Waste.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|AFOLU.Mt ", spec, "/yr")
        )], dim = 3) +
          out[, , paste0("Emi|", spec, "|w/o Bunkers|Energy and Industrial Processes (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/o Bunkers (Mt ", spec, "/yr)")
      )
    )
    # w/ Bunkers
    out <- mbind(
      out,
      setNames(
        dimSums(reportAgg[, , c(
          paste0("Emi|", spec, "|Product Use|Solvents.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|Waste.Mt ", spec, "/yr"),
          paste0("Emi|", spec, "|AFOLU.Mt ", spec, "/yr")
        )], dim = 3) +
          out[, , paste0("Emi|", spec, "|w/ Bunkers|Energy and Industrial Processes (Mt ", spec, "/yr)")],
        paste0("Emi|", spec, "|w/ Bunkers (Mt ", spec, "/yr)")
      )
    )
  }

  # Convert to quitte and ensure it has the same model and scenario as the original report
  out <- as.quitte(out)
  out$model <- model
  out$scenario <- scenario

  return(out)
}
