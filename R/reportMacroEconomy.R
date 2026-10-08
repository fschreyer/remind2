#' Read in GDX and calculate macro economy values, used in convGDX2MIF.R for
#' the reporting
#'
#' Read in macro economy information from GDX file, information used in
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
#' @author Lavinia Baumstark, Anselm Schultes
#' @examples
#' \dontrun{
#' reportMacroEconomy(gdx)
#' }
#'
#' @export
reportMacroEconomy <- function(gdx,
                               regionSubsetList = NULL,
                               t = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)) {
  TWa_2_EJ <- 31.536
  sm_DpGJ_2_TDpTWa <- gdx2::readGDX(gdx, "sm_DpGJ_2_TDpTWa")

  # Read in data from GDX
  vm_cesIO <- gdx2::readGDX(gdx, "vm_cesIO", select = list("_field" = "level"))[, t, ]
  pm_pvp <- gdx2::readGDX(gdx, "pm_pvp")[, , "good"]
  ppfen_ind <- gdx2::readGDX(gdx, "ppfen_industry_dyn37", react = "silent")
  ppfkap_ind <- gdx2::readGDX(gdx, "ppfKap_industry_dyn37", react = "silent")
  inputs <- gdx2::readGDX(gdx, "in")


  steel_process_based <- "steel" %in% gdx2::readGDX(gdx, "secInd37Prc", react = "silent")
  cons <- setNames(
    gdx2::readGDX(gdx, name = "vm_cons", select = list("_field" = "level"))[, t, ] * 1000,
    "Consumption (billion US$2017/yr)"
  )
  gdp <- setNames(vm_cesIO[, , "inco"] * 1000, "GDP|MER (billion US$2017/yr)")
  shPPPMER <- gdx2::readGDX(gdx, c("pm_shPPPMER", "p_ratio_ppp"), format = "first_found")
  gdp_ppp <- setNames(gdp / shPPPMER, "GDP|PPP (billion US$2017/yr)")
  pop <- setNames(
    gdx2::readGDX(gdx, name = c("pm_pop", "pm_datapop"), format = "first_found")[, t, ] * 1000,
    "Population (million)"
  )


  damageFactor <- setNames(
    gdx2::readGDX(gdx,
      name = c("vm_damageFactor", "vm_damage"), select = list("_field" = "level"),
      format = "first_found"
    ),
    "Damage factor (1)"
  )

  # Calculate net GDP using the damage factors
  tintersect <- intersect(getYears(gdp), getYears(damageFactor))
  gdp_net <- setNames(gdp[, tintersect, ] * damageFactor[, tintersect, ],
                      "GDP|MER|Net_afterDamages (billion US$2017/yr)")
  gdp_ppp_net <- setNames(gdp_ppp[, tintersect, ] * damageFactor[, tintersect, ],
                          "GDP|PPP|Net_afterDamages (billion US$2017/yr)")

  ies <- gdx2::readGDX(gdx, c("pm_ies", "p_ies"), format = "first_found")
  c_damage <- gdx2::readGDX(gdx, c("cm_damage", "c_damage"), format = "first_found", react = "silent")
  if (is.null(c_damage)) c_damage <- 0
  forcOs <- gdx2::readGDX(gdx, "vm_forcOs", select = list("_field" = "level"), react = "silent")[, t, ]
  if (is.null(forcOs)) forcOs <- 0

  # CES derivatives aka CES prices, marginal products
  o01_CESderivatives <- gdx2::readGDX(gdx, "o01_CESderivatives", restoreZeros = FALSE, react = "silent", uniqueStyle = "classic")
  # marginal rate of substitution (ratio of CES prices)
  o01_CESmrs <- gdx2::readGDX(gdx, "o01_CESmrs", restoreZeros = FALSE, react = "silent")

  # add NAs in first years from 2005 for o01_CESderivatives and o01_CESmrs
  o01_CESderivatives_w0 <- new.magpie(getItems(o01_CESderivatives, dim = 1), t, getNames(o01_CESderivatives),
                                      fill = NA)
  o01_CESmrs_w0 <- new.magpie(getItems(o01_CESmrs, dim = 1), t, getNames(o01_CESmrs), fill = NA)

  o01_CESderivatives_w0[, getYears(o01_CESderivatives), ] <- o01_CESderivatives
  o01_CESmrs_w0[, getYears(o01_CESmrs), ] <- o01_CESmrs

  getSets(o01_CESderivatives_w0) <- getSets(o01_CESderivatives)
  getSets(o01_CESmrs_w0) <- getSets(o01_CESmrs)

  o01_CESderivatives <- o01_CESderivatives_w0
  o01_CESmrs <- o01_CESmrs_w0

  #### ---- main variables -----

  welf <- cons
  getNames(welf) <- "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)"
  for (regi in getItems(cons, dim = 1)) {
    if (ies[regi, , ] == 1) {
      welf[regi, , ] <- setNames(
        pop[regi, , ] * log(1000 * cons[regi, , ] * (1 - (c_damage * forcOs)) / pop[regi, , ]),
        "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)"
      )
      # in REMIND (without factor 1000, which was added here to prevent negative numbers):
      # (log((vm_cons(ttot,regi)*(1-c_damage*vm_forcOs(ttot)*vm_forcOs(ttot))) / pm_pop(ttot,regi)))$(pm_ies(regi) eq 1)
    } else {
      welf[regi, , ] <- setNames(
        pop[regi, , ] *
          ((1000 * cons[regi, , ] * (1 - (c_damage * forcOs)) / pop[regi, , ])**(1 - 1 / ies[regi, , ]) - 1) /
          (1 - 1 / ies[regi, , ]),
        "Welfare|Real and undiscounted|Yearly (arbitrary unit/yr)"
      )
      # in REMIND (without factor 1000, which was added here to prevent negative numbers):
      # ((( (vm_cons(ttot,regi)*(1-c_damage*vm_forcOs(ttot)*vm_forcOs(ttot)))/pm_pop(ttot,regi))**(1-1/pm_ies(regi))-1)/
      # (1-1/pm_ies(regi)) )$(pm_ies(regi) ne 1)
    }
  }

  cap <- setNames(vm_cesIO[, , "kap"] * 1000, "Capital Stock|Non-ESM (billion US$2017)")

  vm_welfare <- gdx2::readGDX(gdx, c("v02_welfare", "v_welfare", "vm_welfare"),
                              select = list("_field" = "level"), format = "first_found",
    react = "warning"
  )
  if (!is.null(vm_welfare)) {
    setNames(vm_welfare, "Welfare|Real (1)")
  }

  ces <- NULL


  # macro
  ces <- mbind(ces, setNames(vm_cesIO[, , "kap"] * 1000, "CES_input|kap (billion US$2017)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "feelb"] * TWa_2_EJ, "CES_input|feelb (EJ/yr)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "fegab"] * TWa_2_EJ, "CES_input|fegab (EJ/yr)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "fehob"] * TWa_2_EJ, "CES_input|fehob (EJ/yr)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "fesob"] * TWa_2_EJ, "CES_input|fesob (EJ/yr)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "feheb"] * TWa_2_EJ, "CES_input|feheb (EJ/yr)"))
  ces <- mbind(ces, setNames(vm_cesIO[, , "feh2b"] * TWa_2_EJ, "CES_input|feh2b (EJ/yr)"))


  ces <- mbind(
    ces,
    mbind(
      lapply(
        ppfkap_ind,
        function(x) {
          setNames(
            vm_cesIO[, , x] * 1000,
            paste0("CES_input|", x, " (billion US$2017)")
          )
        }
      )
    ),
    mbind(lapply(
      ppfen_ind,
      function(x) {
        setNames(
          vm_cesIO[, , x] * TWa_2_EJ,
          paste0("CES_input|", x, " (EJ/yr)")
        )
      }
    ))
  )


  #### ---- additional variables for CES function reporting ----

  # (internal variables that should usually not be reported to external projects
  #  but that serve as diagnostic output for understanding REMIND results better)

  # check that CES derivatives output parameter exists in GDX
  if ((length(o01_CESmrs > 0)) && (length(o01_CESderivatives > 0))) {
    ## 1.) CES Prices (CES Derivatives)

    # remove inco from inputs as no CES derivative exists
    inputs <- inputs[inputs != "inco"]
    # get sets of CES inputs which are FEs and those which are no FEs
    inputs.fe <- grep("fe", inputs, value = TRUE)
    inputs.nofe <- setdiff(inputs, inputs.fe)

    # CES prices of inputs are derivatives of inco (GDP) w.r.t to input
    CES.price <- collapseNames(mselect(o01_CESderivatives, all_in = "inco", all_in1 = inputs))

    # convert FE input prices to USD/GJ (like PE, SE, FE Prices)
    CES.price.fe <- setNames(
      CES.price[, , inputs.fe] / as.numeric(sm_DpGJ_2_TDpTWa),
      paste0(
        "Internal|CES Function|CES Price|",
        getNames(CES.price[, , inputs.fe]), " (US$2017/GJ)"
      )
    )

    # leave non-FE input prices generally as they are in trUSD/CES input
    CES.price.nofe <- setNames(
      CES.price[, , inputs.nofe],
      paste0(
        "Internal|CES Function|CES Price|",
        getNames(CES.price[, , inputs.nofe]), " (trUS$2017/Input)"
      )
    )

    # bind FE and non-FE CES prices to one array
    CES.price <- mbind(CES.price.fe, CES.price.nofe)


    ## 2.) Marginal rates of substitution

    # equals ratio of CES prices
    # corresponds to amount of second input needed to substitute the first input to generate the same economic value
    # only needed for certain FE inputs to assess substitution/rebound effect from price changes

    # add MRS from o01_CESmrs parameter
    mrs.report <- c(
      "feelhpb.fehob",
      "feelhpb.fesob",
      "feelhpb.feheb",
      "feelhpb.feelrhb",
      "feelhpb.feelrhcob",
      "feh2b.fegab",
      "feelhth_otherInd.fega_otherInd",
      "feelhth_otherInd.feli_otherInd",
      "feelhth_otherInd.feso_otherInd",
      "feelhth_chemicals.fega_chemicals",
      "feelhth_chemicals.feli_chemicals",
      "feh2_otherInd.fega_otherInd",
      "feh2_otherInd.feli_otherInd",
      "feh2_otherInd.feso_otherInd",
      "feh2_chemicals.fega_chemicals",
      "feh2_chemicals.feli_chemicals",
      "feh2_cement.fega_cement",
      "feh2_cement.feso_cement",
      "feh2_cement.feli_cement"
    )

    if (!steel_process_based) {
      mrs.report <- append(
        mrs.report,
        "feh2_steel.feso_steel"
      )
    }

    # Filter to only variables that exist (to ensure backwards compatibility for new buildings CES tree)
    availableMrs <- getItems(o01_CESmrs, 3)
    mrs.report <- mrs.report[mrs.report %in% availableMrs]

    CES.mrs <- collapseDim(setNames(
      o01_CESmrs[, , mrs.report],
      paste0("Internal|CES Function|MRS|", gsub("\\.", "\\|", mrs.report), " (ratio)")
    ))

    # add further MRS by calculating price ratio for inputs which are not in one nest

    CES.mrs <- mbind(
      CES.mrs,
      setNames(
        CES.price[, , "Internal|CES Function|CES Price|feelhpb (US$2017/GJ)"] /
          CES.price[, , "Internal|CES Function|CES Price|fegab (US$2017/GJ)"],
        "Internal|CES Function|MRS|feelhpb|fegab (ratio)"
      ),
      setNames(
        CES.price[, , "Internal|CES Function|CES Price|feelhpb (US$2017/GJ)"] /
          CES.price[, , "Internal|CES Function|CES Price|feh2b (US$2017/GJ)"],
        "Internal|CES Function|MRS|feelhpb|feh2b (ratio)"
      )
    )

    if (!steel_process_based) {
      CES.mrs <- mbind(
        CES.mrs,
        setNames(
          CES.price[, , "Internal|CES Function|CES Price|feel_steel_secondary (US$2017/GJ)"] /
            CES.price[, , "Internal|CES Function|CES Price|feso_steel (US$2017/GJ)"],
          "Internal|CES Function|MRS|feel_steel_secondary|feso_steel (ratio)"
        )
      )
    }


    ## 3.) Value generated by CES Inputs (price * quantity)
    CES.value <- setNames(
      mselect(o01_CESderivatives, all_in = "inco", all_in1 = inputs) *
        vm_cesIO[, getYears(o01_CESderivatives), inputs] * 1000,
      paste0("Internal|CES Function|Value|", inputs, " (billion US$2017)")
    )

    # bind all CES variables together in one array
    ces <- mbind(ces, CES.price, CES.mrs, CES.value)
  }
  #### end CES function reporting


  # define list of variables that will be exported:
  varlist <- list(cons, gdp, gdp_ppp, gdp_net, gdp_ppp_net, pop, cap, ces, welf) # , damageFactor) # ,curracc)
  # use the same temporal resolution for all variables
  # calculate minimal temporal resolution
  tintersect <- Reduce(intersect, lapply(varlist, getYears))
  varlist <- lapply(varlist, function(v) v[, tintersect, ])

  # put all together
  out <- Reduce(mbind, varlist)

  # calculate global aggregation for the damage factor, weighted by MER GDP
  mapping <- data.frame(region = getItems(out, dim = 1), world = "GLO", stringsAsFactors = FALSE)
  glo_damageFactor <- toolAggregate(damageFactor[, tintersect, ], rel = mapping, weight = gdp[, tintersect, ])

  # add global region aggregation
  out <- mbind(out, dimSums(out, dim = 1))
  # add damageFactor, which was aggregated differently
  out <- mbind(out, mbind(damageFactor[, tintersect, ], glo_damageFactor))
  # add other region aggregations
  if (!is.null(regionSubsetList)) {
    out <- mbind(out, calcRegionSubsetSums(out, regionSubsetList))
  }

  # remove regional aggregations for CES Prices and CES MRS
  vars.remove.agg <- c(
    grep("Internal\\|CES Function\\|MRS", getNames(out), value = TRUE),
    grep("Internal\\|CES Function\\|CES Price", getNames(out), value = TRUE)
  )


  if ((length(o01_CESmrs > 0)) && (length(o01_CESderivatives > 0))) {
    # set to zero instead of NA because NA erases the labels in compare scenario 2
    out[setdiff(getItems(out, dim = 1), getItems(CES.price, dim = 1)), , vars.remove.agg] <- 0
  }

  # calculate interest rate
  inteRate <- new.magpie(getItems(out, dim = 1),
    getYears(out),
    c("Interest Rate (t+1)/(t-1)|Real (unitless)", "Interest Rate t/(t-1)|Real (unitless)"),
    fill = NA
  )

  for (t in getYears(out[, which(getYears(out, as.integer = TRUE) > 2005 &
                                 getYears(out, as.integer = TRUE) < max(getYears(out, as.integer = TRUE))), ])) {
    inteRate[, t, "Interest Rate (t+1)/(t-1)|Real (unitless)"] <-
      (1
       - ((setYears(pm_pvp[, (which(getYears(pm_pvp) == t) + 1), ], t) /
             setYears(pm_pvp[, (which(getYears(pm_pvp) == t) - 1), ], t))
          ^(1 / (getYears(pm_pvp[, (which(getYears(pm_pvp) == t) + 1), ], as.integer = TRUE) -
                   getYears(pm_pvp[, (which(getYears(pm_pvp) == t) - 1), ], as.integer = TRUE))))
      )
    inteRate[, t, "Interest Rate t/(t-1)|Real (unitless)"] <-
      (1
       - ((pm_pvp[, t, ] / setYears(pm_pvp[, (which(getYears(pm_pvp) == t) - 1), ], t))
          ^(1 / (getYears(pm_pvp[, t, ], as.integer = TRUE) - getYears(pm_pvp[, (which(getYears(pm_pvp) == t) - 1), ],
                                                                       as.integer = TRUE
          )))
       )
      )
  }
  # add interest rate
  out <- mbind(out, inteRate)
  getSets(out)[3] <- "variable"
  return(out)
}
