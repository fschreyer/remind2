#' Report investments
#'
#' Report investments
#'
#' @param gdx a GDX object as created by readGDX, or the path to a gdx
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#' @param gdx_ref a GDX object as created by readGDX, or the path to a gdx of the reference run.
#' It is used to guarantee consistency before 'cm_startyear' for investment variables
#' using time averaging.
#'
#' @return magclass object
#' @seealso \code{\link{convGDX2MIF}}
#' @examples
#' \dontrun{
#' reportInvestments(gdx)
#' }
#' @export
#'
reportInvestments <- function(gdx,
                              regionSubsetList = NULL,
                              t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                              gdx_ref = NULL) {
  # Load in investment variables
  vm_invMacro <- gdx::readGDX(gdx, "vm_invMacro", field = "l")
  v01_invMacroAdj <- gdx::readGDX(gdx, "v01_invMacroAdj", field = "l")
  v_costInv <- gdx::readGDX(gdx, "v_costInv", field = "l")
  vm_costInvTeDir <- gdx::readGDX(gdx, "vm_costInvTeDir", field = "l")
  vm_costInvTeAdj <- gdx::readGDX(gdx, "vm_costInvTeAdj", field = "l")
  vm_costAddTeInv <- gdx::readGDX(gdx, "vm_costAddTeInv", field = "l")
  vm_costCESMkup <- gdx::readGDX(gdx, "vm_costCESMkup", field = "l")

  # Apply 'modifyInvestmentVariables' to shift from the model-internal time coverage (deltacap and investment
  # variables for step t represent the average of the years from t-4years to t) to the general convention for
  # the reporting template (all variables represent the average of the years from t-2.5years to t+2.5years).
  # This function requires the variable to be defined over ttot.
  ttot <- gdx::readGDX(gdx, "ttot") |> as.numeric()
  cm_startyear <- gdx::readGDX(gdx, "cm_startyear") |> as.integer()

  if (!is.null(gdx_ref)) {
    vm_invMacro_ref <- gdx::readGDX(gdx_ref, "vm_invMacro", field = "l")[, ttot, ]
    v01_invMacroAdj_ref <- gdx::readGDX(gdx_ref, "v01_invMacroAdj", field = "l")[, ttot, ]
    v_costInv_ref <- gdx::readGDX(gdx_ref, "v_costInv", field = "l")[, ttot, ]
    vm_costInvTeDir_ref <- gdx::readGDX(gdx_ref, "vm_costInvTeDir", field = "l")[, ttot, ]
    vm_costInvTeAdj_ref <- gdx::readGDX(gdx_ref, "vm_costInvTeAdj", field = "l")[, ttot, ]
    vm_costAddTeInv_ref <- gdx::readGDX(gdx_ref, "vm_costAddTeInv", field = "l")[, ttot, ]
    vm_costCESMkup_ref <- gdx::readGDX(gdx_ref, "vm_costCESMkup", field = "l")[, ttot, ]
  } else {
    vm_invMacro_ref <- NULL
    v01_invMacroAdj_ref <- NULL
    v_costInv_ref <- NULL
    vm_costInvTeDir_ref <- NULL
    vm_costInvTeAdj_ref <- NULL
    vm_costAddTeInv_ref <- NULL
    vm_costCESMkup_ref <- NULL
  }

  vm_invMacro <- modifyInvestmentVariables(vm_invMacro[, ttot, ],
                                           ref = vm_invMacro_ref,
                                           startYear = cm_startyear)
  v01_invMacroAdj <- modifyInvestmentVariables(v01_invMacroAdj[, ttot, ],
                                               ref = v01_invMacroAdj_ref,
                                               startYear = cm_startyear)
  v_costInv <- modifyInvestmentVariables(v_costInv[, ttot, ],
                                         ref = v_costInv_ref,
                                         startYear = cm_startyear)
  vm_costInvTeDir <- modifyInvestmentVariables(vm_costInvTeDir[, ttot, ],
                                               ref = vm_costInvTeDir_ref,
                                               startYear = cm_startyear)
  vm_costInvTeAdj <- modifyInvestmentVariables(vm_costInvTeAdj[, ttot, ],
                                               ref = vm_costInvTeAdj_ref,
                                               startYear = cm_startyear)
  vm_costAddTeInv <- modifyInvestmentVariables(vm_costAddTeInv[, ttot, ],
                                               ref = vm_costAddTeInv_ref,
                                               startYear = cm_startyear)
  vm_costCESMkup <- modifyInvestmentVariables(vm_costCESMkup[, ttot, ],
                                              ref = vm_costCESMkup_ref,
                                              startYear = cm_startyear)

  # Load in sets used to filter the investment variables
  ppfKap <- gdx::readGDX(gdx, "ppfKap") |> as.character()
  en2en <- gdx::readGDX(gdx, "en2en") |> dplyr::rename("en_in" = "all_enty", "en_out" = "all_enty1", "te" = "all_te")
  teStor <- gdx::readGDX(gdx, "teStor") |> as.character()
  teGrid <- gdx::readGDX(gdx, "teGrid") |> as.character()
  teNoTransform <- gdx::readGDX(gdx, "teNoTransform") |> as.character()
  teCCS <- gdx::readGDX(gdx, "teCCS") |> as.character()
  peFos <- gdx::readGDX(gdx, "peFos") |> as.character()
  peBio <- gdx::readGDX(gdx, "peBio") |> as.character()
  peRe <- gdx::readGDX(gdx, "peRe") |> as.character()
  entyFe <- gdx::readGDX(gdx, "entyFe") |> as.character()
  entyPe <- gdx::readGDX(gdx, "entyPe") |> as.character()
  entySe <- gdx::readGDX(gdx, "entySe") |> as.character()
  sector2te_addTDCost <- gdx::readGDX(gdx, "sector2te_addTDCost") |>
    tidyr::unite("x", c("all_te", "emi_sectors"), sep = ".") |>
    dplyr::pull()
  ppfen_CESMkup <- gdx::readGDX(gdx, "ppfen_CESMkup") |> as.character()
  tePrc <- gdx::readGDX(gdx, "tePrc") |> as.character()


  # Perform some helpful calculations, motivated by the fact that the GAMS variables do not align with what we want to
  # report, i.e. vm_invMacro also contains investments do not increase the macro-economic capita stock, and
  # vm_costInvTeDir contains both energy supply and industry steel investments.
  # Also: filter by t, and convert to billion $

  # Get macro investments.
  inv_m <- vm_invMacro[, t, "kap"] * 1e3

  # Get energy supply and industry steel investments.
  ## Combine the direct investment- and adjustment costs for the different energy techs. These contain the
  ## energy supply investments (inv_es) and the industry steel investments (inv_st).
  ## Filtering by te_used is necessary, as inv still contains non-zero values for "wind", "storwind" and "gridwind".
  inv_es_st <- (vm_costInvTeDir[, t, ] + vm_costInvTeAdj[, t, ]) * 1e3
  te_used <- c(en2en$te, teNoTransform)
  te_steel <- tePrc
  # Carbon-removal technologies (enhanced weathering, ocean alkalinity enhancement) are energy-consuming CDR
  # techs. They are reported on the demand side (Investment|Energy Demand|Carbon Management), not as energy
  # supply.
  te_cdr_demand <- intersect(c("weathering", "oae_ng", "oae_el"), te_used)
  te_energy_supply <- te_used[!te_used %in% c(te_steel, te_cdr_demand)]
  inv_es <- inv_es_st[, , te_energy_supply]
  inv_st <- inv_es_st[, , te_steel]
  inv_cdr <- inv_es_st[, , te_cdr_demand]

  # Get total investments.
  inv_tot <- inv_m + dimSums(inv_es) + dimSums(inv_st) + dimSums(inv_cdr)

  # Get other quantities that appear in the GAMS investment variables v_costInv, and vm_invMacro, but that we do
  # not count as investments. These will only appear as INTERNAL variables. Should probably get cleaned up in the
  # future.
  ## H2 T&D Phase-In Cost
  inv_other_1 <- dimSums(vm_costAddTeInv[, t, sector2te_addTDCost]) * 1e3
  ## CES Markup Costs
  inv_other_2 <- dimSums(vm_costCESMkup[, t, ppfen_CESMkup]) * 1e3
  ## Industry Energy Efficiency Capital
  inv_other_3 <- dimSums(vm_invMacro[, t, ppfKap[ppfKap != "kap"]]) * 1e3
  ## Total other
  inv_other <- (inv_other_1 + inv_other_2 + inv_other_3)


  # Build reporting variables

  # Total investments split into macro, energy supply and industry steel
  tmp <- setNames(inv_tot, "Investment (billion US$2017/yr)")
  tmp <- mbind(tmp, setNames(inv_m, "Investment|+|Macroeconomy (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(dimSums(inv_es), "Investment|+|Energy Supply (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(dimSums(inv_st), "Investment|+|Industry|Steel (billion US$2017/yr)"))

  # Demand-side carbon-removal investments (enhanced weathering, OAE)
  if (length(te_cdr_demand) > 0) {
    tmp <- mbind(tmp, setNames(
      dimSums(inv_cdr),
      "Investment|+|Energy Demand|Carbon Management (billion US$2017/yr)"
    ))
    if ("weathering" %in% te_cdr_demand) {
      tmp <- mbind(tmp, setNames(
        dimSums(inv_cdr[, , "weathering"]),
        "Investment|Energy Demand|Carbon Management|+|Enhanced Weathering (billion US$2017/yr)"
      ))
    }
    te_oae <- intersect(c("oae_ng", "oae_el"), te_cdr_demand)
    if (length(te_oae) > 0) {
      tmp <- mbind(tmp, setNames(
        dimSums(inv_cdr[, , te_oae]),
        "Investment|Energy Demand|Carbon Management|+|OAE (billion US$2017/yr)"
      ))
      if ("oae_ng" %in% te_oae) {
        tmp <- mbind(tmp, setNames(
          dimSums(inv_cdr[, , "oae_ng"]),
          "Investment|Energy Demand|Carbon Management|OAE|+|Traditional Calciner (billion US$2017/yr)"
        ))
      }
      if ("oae_el" %in% te_oae) {
        tmp <- mbind(tmp, setNames(
          dimSums(inv_cdr[, , "oae_el"]),
          "Investment|Energy Demand|Carbon Management|OAE|+|Electric Calciner (billion US$2017/yr)"
        ))
      }
    }
  }

  # Report other "investments" for internal checks. Should probably be improved in the future!
  # (Ideally we would have no unattributed quantities in the GAMS investment variables.)
  tmp <- mbind(tmp, setNames(inv_other, "Internal|Investment|Other (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(inv_other_1, "Internal|Investment|Other|+|H2 T&D Phase-In Cost (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(inv_other_2, "Internal|Investment|Other|+|CES Markup Costs (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(
    inv_other_3,
    "Internal|Investment|Other|+|Industry Energy Efficiency Capital (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(inv_other / inv_tot, "Internal|Investment|Ratio Other/Total (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(
    (inv_other_1 + inv_other_2) / dimSums(v_costInv[, t, ] * 1e3),
    "Internal|Investment|Unattributed share of v_costInv (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    inv_other_3 / dimSums(vm_invMacro[, t, ] * 1e3),
    "Internal|Investment|Unattributed share of vm_invMacro (billion US$2017/yr)"
  ))
  # Add internal variable on ratio of macro investment adjustment costs over total
  tmp <- mbind(tmp, setNames(
    v01_invMacroAdj[, t, "kap"] / vm_invMacro[, t, "kap"],
    "Internal|Investment|Ratio 'kap' v01_invMacroAdj/vm_invMacro (billion US$2017/yr)"
  ))

  # Energy supply investments (rename inv_es to inv)
  inv <- inv_es

  # Total energy supply investments split into fossil and non-fossil
  te_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_fossil]), "Investment|Energy Supply|++|Fossil (billion US$2017/yr)"))
  tmp <- mbind(tmp, setNames(
    tmp[, , "Investment|+|Energy Supply (billion US$2017/yr)"] -
      tmp[, , "Investment|Energy Supply|++|Fossil (billion US$2017/yr)"],
    "Investment|Energy Supply|++|Non-Fossil (billion US$2017/yr)"
  ))

  # In the following the supply side investments are split into SE carriers
  # Investments into electricity split by generation vs grid&storage
  te_el_generation <- dplyr::filter(en2en, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_generation]),
    "Investment|Energy Supply|Electricity|+++|Generation (billion US$2017/yr)"
  ))
  ## Add grid techs to transmission and distribution
  te_td <- c(teGrid, "tdels", "tdelt")
  te_td_storage <- c(te_td, teStor)
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_td_storage]),
    "Investment|Energy Supply|Electricity|+++|TD&Storage (billion US$2017/yr)"
  ))

  ## Total electricity investments
  te_el <- c(te_el_generation, te_td_storage)
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el]),
    "Investment|Energy Supply|+|Electricity (billion US$2017/yr)"
  ))

  # Investments into electricity generation split by primary energy
  ## Coal
  te_coal <- dplyr::filter(en2en, .data$en_in == "pecoal", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal]),
    "Investment|Energy Supply|Electricity|+|Coal (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal[te_coal %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Coal|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_coal[!te_coal %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Coal|+|w/o CCS (billion US$2017/yr)"
  ))

  ## Gas
  te_gas <- dplyr::filter(en2en, .data$en_in == "pegas", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas]),
    "Investment|Energy Supply|Electricity|+|Gas (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas[te_gas %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Gas|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gas[!te_gas %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Gas|+|w/o CCS (billion US$2017/yr)"
  ))

  ## Oil
  te_oil <- dplyr::filter(en2en, .data$en_in == "peoil", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil]),
    "Investment|Energy Supply|Electricity|+|Oil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil[te_oil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Oil|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_oil[!te_oil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Oil|+|w/o CCS (billion US$2017/yr)"
  ))

  ## Biomass
  te_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio]),
    "Investment|Energy Supply|Electricity|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio[te_bio %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Biomass|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_bio[!te_bio %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Biomass|+|w/o CCS (billion US$2017/yr)"
  ))

  ## Nuclear
  te_nuclear <- dplyr::filter(en2en, .data$en_in == "peur", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nuclear]),
    "Investment|Energy Supply|Electricity|+|Nuclear (billion US$2017/yr)"
  ))

  ## Solar
  te_solar <- dplyr::filter(en2en, .data$en_in == "pesol", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_solar]),
    "Investment|Energy Supply|Electricity|+|Solar (billion US$2017/yr)"
  ))

  ## Wind
  te_wind <- dplyr::filter(en2en, .data$en_in == "pewin", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_wind]),
    "Investment|Energy Supply|Electricity|+|Wind (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "windon"]),
    "Investment|Energy Supply|Electricity|Wind|+|Onshore (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "windoff"]),
    "Investment|Energy Supply|Electricity|Wind|+|Offshore (billion US$2017/yr)"
  ))

  ## Hydro
  te_hydro <- dplyr::filter(en2en, .data$en_in == "pehyd", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydro]),
    "Investment|Energy Supply|Electricity|+|Hydro (billion US$2017/yr)"
  ))

  ## Geothermal
  te_geo <- dplyr::filter(en2en, .data$en_in == "pegeo", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_geo]),
    "Investment|Energy Supply|Electricity|+|Geothermal (billion US$2017/yr)"
  ))

  ## Hydrogen
  te_hydrogen <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydrogen]),
    "Investment|Energy Supply|Electricity|+|Hydrogen (billion US$2017/yr)"
  ))

  # Investments into electricity generation split by fossil, non-fossil, bio-renew and non-bio-renew pes.
  ## Fossil (coal, gas and oil)
  te_el_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil]),
    "Investment|Energy Supply|Electricity|++|Fossil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil[te_el_fossil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Fossil|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_el_fossil[!te_el_fossil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Fossil|+|w/o CCS (billion US$2017/yr)"
  ))
  ## Non-Fossil (Add the T&D techs here)
  te_nonfossil <- dplyr::filter(en2en, !.data$en_in %in% peFos, .data$en_out == "seel")$te
  te_nonfossil <- c(te_nonfossil, te_td_storage)
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil]),
    "Investment|Energy Supply|Electricity|++|Non-Fossil (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil[te_nonfossil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Non-Fossil|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonfossil[!te_nonfossil %in% teCCS]]),
    "Investment|Energy Supply|Electricity|Non-Fossil|+|w/o CCS (billion US$2017/yr)"
  ))
  ## Bio-renewables
  te_biorenew <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_biorenew]),
    "Investment|Energy Supply|Electricity|Non-Fossil|++|Bio Re (billion US$2017/yr)"
  ))
  ## Non-bio-renewables and nuclear
  pe_nonbiorenew <- peRe[!peRe %in% peBio]
  te_nonbiorenew <- dplyr::filter(en2en, .data$en_in %in% pe_nonbiorenew, .data$en_out == "seel")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nonbiorenew]),
    "Investment|Energy Supply|Electricity|Non-Fossil|++|Non-Bio Re (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_nuclear]),
    "Investment|Energy Supply|Electricity|Non-Fossil|++|Nuclear (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_hydrogen]),
    "Investment|Energy Supply|Electricity|Non-Fossil|++|Hydrogen (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_td_storage]),
    "Investment|Energy Supply|Electricity|Non-Fossil|++|TD&Storage (billion US$2017/yr)"
  ))

  # Investments into electricity grid and storage split by specific types
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , teStor]),
    "Investment|Energy Supply|Electricity|+|Storage (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_td]),
    "Investment|Energy Supply|Electricity|+|Transmission and Distribution (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , teGrid]),
    "Investment|Energy Supply|Electricity|Transmission and Distribution|+|VRE support (billion US$2017/yr)"
  ))
  ## Split out the amount of "normal" grid, the additional grid/charger investments due to electric vehicles,
  ## and the additional grid investments needed for better pooling of VRE. For BEVs, the assumption is to only
  ## take the share of tdelt costs that are higher than the tdels costs
  pm_data <- gdx::readGDX(gdx, "pm_data")[, , c("inco0.tdelt", "inco0.tdels")]
  cr <- pm_data[, , "inco0.tdelt"] / pm_data[, , "inco0.tdels"]
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "tdelt"]) * (cr - 1) / cr,
    "Investment|Energy Supply|Electricity|Transmission and Distribution|+|BEV Chargers (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    (dimSums(inv[, , "tdelt"]) / cr + dimSums(inv[, , "tdels"])),
    "Investment|Energy Supply|Electricity|Transmission and Distribution|+|Normal (billion US$2017/yr)"
  ))

  # Investments into hydrogen
  te_h2 <- dplyr::filter(en2en, (.data$en_in == "seh2" & .data$en_out %in% entyFe) | .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_h2]), "Investment|Energy Supply|+|Hydrogen (billion US$2017/yr)"))

  te_h2_pe <- dplyr::filter(en2en, .data$en_in %in% entyPe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_pe]),
    "Investment|Energy Supply|Hydrogen|++|PE (billion US$2017/yr)"
  ))

  te_h2_se <- dplyr::filter(en2en, .data$en_in %in% entySe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_se]),
    "Investment|Energy Supply|Hydrogen|++|se2se (billion US$2017/yr)"
  ))

  te_h2_fe <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fe]),
    "Investment|Energy Supply|Hydrogen|++|se2fe (billion US$2017/yr)"
  ))

  te_h2_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fossil]),
    "Investment|Energy Supply|Hydrogen|+|Fossil (billion US$2017/yr)"
  ))

  te_h2_re <- dplyr::filter(en2en, .data$en_in %in% peRe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_re]),
    "Investment|Energy Supply|Hydrogen|+|RE (billion US$2017/yr)"
  ))

  te_h2_se <- dplyr::filter(en2en, .data$en_in %in% entySe, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_se]),
    "Investment|Energy Supply|Hydrogen|+|Electrolysis (billion US$2017/yr)"
  ))

  te_h2_fe <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_fe]),
    "Investment|Energy Supply|Hydrogen|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  te_h2_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "seh2")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_h2_bio]),
    "Investment|Energy Supply|Hydrogen|Bio (billion US$2017/yr)"
  ))

  # Investments into gases
  se_gas <- c("segafos", "segabio", "segasyn")
  te_gases <- dplyr::filter(en2en, .data$en_in %in% se_gas | .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_gases]), "Investment|Energy Supply|+|Gases (billion US$2017/yr)"))

  te_gases_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_fossil]),
    "Investment|Energy Supply|Gases|+|Fossil (billion US$2017/yr)"
  ))

  te_gases_h2 <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_h2]),
    "Investment|Energy Supply|Gases|+|Hydrogen (billion US$2017/yr)"
  ))

  te_gases_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out %in% se_gas)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio]),
    "Investment|Energy Supply|Gases|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio[te_gases_bio %in% teCCS]]),
    "Investment|Energy Supply|Gases|Biomass|+|CCS (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_bio[!te_gases_bio %in% teCCS]]),
    "Investment|Energy Supply|Gases|Biomass|+|w/o CCS (billion US$2017/yr)"
  ))

  te_gases_td <- dplyr::filter(en2en, .data$en_in %in% se_gas, .data$en_out %in% entyFe)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_gases_td]),
    "Investment|Energy Supply|Gases|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into heat
  te_heat <- dplyr::filter(en2en, .data$en_in == "sehe" | .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_heat]), "Investment|Energy Supply|+|Heat (billion US$2017/yr)"))

  te_heat_pump <- dplyr::filter(en2en, .data$en_in == "pegeo", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_pump]),
    "Investment|Energy Supply|Heat|+|Heat Pump (billion US$2017/yr)"
  ))

  te_heat_gas <- dplyr::filter(en2en, .data$en_in == "pegas", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_gas]),
    "Investment|Energy Supply|Heat|+|Gas (billion US$2017/yr)"
  ))

  te_heat_coal <- dplyr::filter(en2en, .data$en_in == "pecoal", .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_coal]),
    "Investment|Energy Supply|Heat|+|Coal (billion US$2017/yr)"
  ))

  te_heat_fossil <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_fossil]),
    "Investment|Energy Supply|Heat|Fossil (billion US$2017/yr)"
  ))

  te_heat_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out == "sehe")$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_heat_bio]),
    "Investment|Energy Supply|Heat|+|Biomass (billion US$2017/yr)"
  ))
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , "tdhes"]),
    "Investment|Energy Supply|Heat|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into liquids
  se_liq <- c("seliqfos", "seliqbio", "seliqsyn")
  te_liq <- dplyr::filter(en2en, .data$en_in %in% se_liq | .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq]),
    "Investment|Energy Supply|+|Liquids (billion US$2017/yr)"
  ))

  te_liq_fos <- dplyr::filter(en2en, .data$en_in %in% peFos, .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_fos]),
    "Investment|Energy Supply|Liquids|+|Fossil (billion US$2017/yr)"
  ))

  te_liq_or <- dplyr::filter(en2en, .data$en_in == "peoil", .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_or]),
    "Investment|Energy Supply|Liquids|Fossil|+|Oil Ref (billion US$2017/yr)"
  ))

  te_liq_fosno <- dplyr::filter(en2en, .data$en_in %in% c("pecoal", "pegas"), .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_fosno]),
    "Investment|Energy Supply|Liquids|Fossil|+|w/o oil (billion US$2017/yr)"
  ))

  te_liq_bio <- dplyr::filter(en2en, .data$en_in %in% peBio, .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_bio]),
    "Investment|Energy Supply|Liquids|+|Bio (billion US$2017/yr)"
  ))

  te_liq_h2 <- dplyr::filter(en2en, .data$en_in == "seh2", .data$en_out %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_h2]),
    "Investment|Energy Supply|Liquids|+|Hydrogen (billion US$2017/yr)"
  ))

  te_liq_td <- dplyr::filter(en2en, .data$en_in %in% se_liq)$te
  tmp <- mbind(tmp, setNames(
    dimSums(inv[, , te_liq_td]),
    "Investment|Energy Supply|Liquids|+|Transmission and Distribution (billion US$2017/yr)"
  ))

  # Investments into solids
  se_sol <- c("sesofos", "sesobio")
  te_sol <- dplyr::filter(en2en, .data$en_in %in% se_sol | .data$en_out %in% se_sol)$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_sol]), "Investment|Energy Supply|+|Solids (billion US$2017/yr)"))

  # Investments into biochar
  te_biochar <- dplyr::filter(en2en, .data$en_out == "sebiochar")$te
  tmp <- mbind(tmp, setNames(dimSums(inv[, , te_biochar]), "Investment|Energy Supply|+|Biochar (billion US$2017/yr)"))

  # Investments into DAC
  tmp <- mbind(tmp, setNames(dimSums(inv[, , "dac"]), "Investment|Energy Supply|+|DAC (billion US$2017/yr)"))

  if ("ccsinje" %in% getItems(inv, dim = "all_te")) {
    # Investments into CCS Trans and Stor (Onshore)
    tmp <- mbind(tmp, setNames(
      dimSums(inv[, , "ccsinje"]),
      "Investment|Energy Supply|+|CO2 Trans&Stor (billion US$2017/yr)"
    ))

  } else {
    # Investments into CCS Trans and Stor (Onshore)
    tmp <- mbind(tmp, setNames(
      dimSums(inv[, , "ccsinjeon"]),
      "Investment|Energy Supply|CO2 Trans&Stor|+|Onshore (billion US$2017/yr)"
    ))

    # Investments into CCS Trans and Stor (Offshore)
    tmp <- mbind(tmp, setNames(
      dimSums(inv[, , "ccsinjeoff"]),
      "Investment|Energy Supply|CO2 Trans&Stor|+|Offshore (billion US$2017/yr)"
    ))

    # Investments into CCS Trans and Stor (Total)
    tmp <- mbind(tmp, setNames(
      tmp[, , "Investment|Energy Supply|CO2 Trans&Stor|+|Onshore (billion US$2017/yr)"] +
      tmp[, , "Investment|Energy Supply|CO2 Trans&Stor|+|Offshore (billion US$2017/yr)"],
      "Investment|Energy Supply|+|CO2 Trans&Stor (billion US$2017/yr)"
    ))
  }

  # Add non-electricity aggregate
  tmp <- mbind(tmp, setNames(
    tmp[, , "Investment|+|Energy Supply (billion US$2017/yr)"] -
      tmp[, , "Investment|Energy Supply|+|Electricity (billion US$2017/yr)"],
    "Investment|Energy Supply|Non-Electricity (billion US$2017/yr)"
  ))

  # Add global values
  tmp <- mbind(tmp, dimSums(tmp, dim = 1))
  tmp["GLO", , getNames(tmp)[grepl("Ratio|share", getNames(tmp))]] <- 0

  # Add other region aggregations
  if (!is.null(regionSubsetList)) {
    tmp <- mbind(tmp, calc_regionSubset_sums(tmp, regionSubsetList))
  }

  getSets(tmp)[3] <- "variable"
  tmp
}
