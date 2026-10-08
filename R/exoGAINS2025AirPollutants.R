#' Calculate projected air pollutant emissions at the level of GAINS sectors
#' based on REMIND activities, GAINS emission factors, and baseyear (2020)
#' emissions
#'
#' @param remind_output a magpie object containing all needed REMIND activities
#' @param gains_emifacs a magpie object containing historic and projected
#'                      GAINS emission factors for all 35 GAINS sectors
#' @param baseyear_emis_2020 a magpie object containing baseyear (2020)
#'                      emissions for all 35 GAINS sectors
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param logging       Boolean to activate logging
#'
#' @author Laurin Köhler-Schindler
#' @examples
#' \dontrun{
#' exoGAINS2025AirPollutants(remind_output, gains_emifacs, baseyear_emis_2020)
#' }
#' @export

exoGAINS2025AirPollutants <- function(remind_output, gains_emifacs, baseyear_emis_2020,
                                      t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                                      logging = FALSE) {
  if (logging) {
    cat("\nstarting exoGAINS2025AirPollutants.R ...\n\n")
  }
  # 1. Prepare REMIND activities -----------------------------------------------

  # Delete "+", "++" and "+++" from variable names
  remind_output <- piamutils::deletePlus(remind_output)

  # Get mapping from GAINS2025 sectors to REMIND activities
  map_GAINS2REMIND <- toolGetMapping(type = "sectoral", name = "mappingGAINS2025toREMIND.csv", where = "remind2")

  # End_Use_Services_Coal is mapped to an activity that is so far not existing in the REMIND reporting:
  # FE|Solids without BioTrad (EJ/yr) = Final Energy|Solids (EJ/yr) -  Final Energy|Solids|Biomass|Traditional (EJ/yr)
  # Thus, add this new activity
  remind_output <- add_columns(remind_output, addnm = "FE|Solids without BioTrad (EJ/yr)", dim = 3.1)
  remind_output[, , "FE|Solids without BioTrad (EJ/yr)"] <- remind_output[, , "FE|Solids (EJ/yr)"] - remind_output[, , "FE|Solids|Biomass|Traditional (EJ/yr)"]

  # Select REMIND activity (RA) data according to order in mapping
  RA <- collapseNames(remind_output[, , map_GAINS2REMIND$REMINDactivity])

  # Compute relative regional share of global total
  RA_regishare_2020 <- RA[, 2020, ] / RA["GLO", 2020, ]

  # Set activities to NA that account for less than 0.1% of global activity in 2020
  # since huge relative changes in region x sector combinations with initially very low activity
  # can lead to very high emissions that are not realistic
  RA_regishare_2020[RA_regishare_2020 < 0.001] <- NA

  mask <- setYears(RA_regishare_2020, NULL)
  mask[!is.na(mask)] <- 1
  RA <- RA * mask

  # Remove global dimension from RA
  RA <- RA["GLO", , invert = TRUE]

  # Logging missing sectors
  if (logging) {
    cat("List of sectors that are not in the GAINS2025toREMIND mapping because there is no emission and/or activity data.\nThese sectors will be omitted in the calculations!\n")
    missing_sectors <- setdiff(getNames(gains_emifacs, dim = 1), map_GAINS2REMIND$GAINSsector)
    cat(missing_sectors, sep = "\n")
  }

  # 2. Select GAINS data -------------------------------------------------------

  # Select GAINS data according to order in mapping and bring regions into same (alphabetically sorted) order as RA
  emifacs <- gains_emifacs[getItems(RA, dim = 1), , map_GAINS2REMIND$GAINSsector]
  emis <- baseyear_emis_2020[getItems(RA, dim = 1), , map_GAINS2REMIND$GAINSsector]

  # Rename REMIND activities to GAINS sectors to make them compatible for calculation
  # IMPORTANT: before renaming, order of REMIND sectors must be identical to order of GAINS sectors, otherwise data would be mixed up
  # This was already taken care of by selecting both REMIND and GAINS data using the map_GAINS2REMIND (see above)
  getItems(RA, dim = 3) <- getItems(emifacs, dim = 3.1)
  getSets(RA) <- c("region", "year", "sector")

  # create magpie object of the structure of RA, fill with elasticity data from map_GAINS2REMIND
  elasticity <- RA * 0
  tmp <- as.magpie(map_GAINS2REMIND$elasticity)
  getNames(tmp) <- getNames(RA)
  elasticity[, , ] <- tmp

  # create magpie object of the structure of emifacs, fill with constantef data from map_GAINS2REMIND
  constantef <- emifacs * 0
  tmp <- as.magpie(map_GAINS2REMIND$constantef)
  getNames(tmp) <- getNames(emifacs, dim = "sector")
  tmp <- add_dimension(tmp, add = "species", nm = getNames(emifacs, dim = "species"), dim = 3.2)
  constantef[, , ] <- tmp

  # create magpie object of the structure of emifacs, fill with constantemi data from map_GAINS2REMIND
  constantemi <- emifacs * 0
  tmp <- as.magpie(map_GAINS2REMIND$constantemi)
  getNames(tmp) <- getNames(emifacs, dim = "sector")
  tmp <- add_dimension(tmp, add = "species", nm = getNames(emifacs, dim = "species"), dim = 3.2)
  constantemi[, , ] <- tmp

  # 3. Calculate emissions -----------------------------------------------------

  # Compute relative change in activitiy compared to 2020
  RA_change <- RA / (setYears(RA[, 2020, ]))
  # Compute relative change in emission factor compared to 2020
  emifacs_change <- emifacs / (setYears(emifacs[, 2020, ]))
  # Compute emissions with formula: emis = emifac/emifac(2020) * emis(2020) * (RA/RA(2020))^elasticity
  emis_projected <- setYears(emis) * emifacs_change * (RA_change)^elasticity

  # Special case 1: constant emission factor
  # Not yet needed since no sector is marked as constantef in the mapping
  # emis_projected_constantef <-  setYears(emis) * 1 * (RA_change)^elasticity
  # emis_projected[emis_projected_constantef == 1] <- emis_projected_constantef

  # Special case 2: constant emissions
  # Not yet needed since no sector is marked as constantemi in the mapping

  # Logging species x sector x region combinations for which baseyear emissions (2020) are positive but projected emissions are NA
  # this happens if RA in 2020 is zero (division by zero in RA_change) or if emifacs in 2020 is zero (division by zero in emifacs_change)
  if (logging) {
    cat("\nList of species x sector x region combinations for which projected emissions are NA but baseyear emissions (2020) are positive.
  This can happen if the REMIND activity in 2020 is zero or the emission factor in 2020 is zero.\n")
    for (species in getNames(emis_projected, dim = "species")) {
      for (sector in getNames(emis_projected, dim = "sector")) {
        missing_combinations <- which(is.na(emis_projected[, 2020, species][, , sector]) & (emis[, 2020, species][, , sector] > 0))
        if (length(missing_combinations) > 0) {
          cat(paste0("Region(s) with missing projected emissions for species ", species, " and sector ", sector, ":"))
          cat(paste0(getItems(emis_projected, dim = 1)[missing_combinations]))
          cat("\n")
        }
      }
    }
  }

  # Fill NA values with zero
  emis_projected[is.na(emis_projected)] <- 0

  # Add global dimension
  emis_projected_GLO <- dimSums(emis_projected, dim = 1)
  getItems(emis_projected_GLO, dim = 1) <- "GLO"
  emis_projected <- mbind(emis_projected, emis_projected_GLO)
  if (logging) {
    cat("\nexoGAINS2025AirPollutants.R finished.\n")
  }
  return(emis_projected)
}
