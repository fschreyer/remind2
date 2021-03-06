#' Reporting for the coupled EDGE-T Transport Sector Model (REMIND Module edge_esm)
#'
#' Data is loaded from the EDGE-T subfolder in the output folder.
#' The input files can be (re-) generated calling
#' `Rscript EDGETransport.R --reporting`
#' from the output folder.
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables and replaces the "_withoutPlus" version.
#'
#' Region subsets are obtained from fulldata.gdx
#'
#' @param output_folder path to the output folder, default is current folder.
#' @param remind_root path to the REMIND root directory, defaults to two levels up from output_folder.
#' @author Alois Dirnaichner Marianna Rottoli
#'
#' @importFrom rmndt approx_dt readMIF writeMIF
#' @importFrom gdxdt readgdx
#' @importFrom data.table fread fwrite rbindlist copy CJ
#' @export

reportEDGETransport <- function(output_folder=".",
                                remind_root=NULL) {

  if(is.null(remind_root)){
    remind_root <- file.path(output_folder, "../..")
  }
  gdx <- file.path(output_folder, "fulldata.gdx")

  ## check the regional aggregation
  regionSubsetList <- toolRegionSubsets(gdx)

  sub_folder = "EDGE-T/"

  ## NULL Definitons for codeCheck compliance
  RegionCode <- CountryCode <- cfg <- `.` <- sector <- subsector_L3 <- region <- year <- NULL
  subsector_L2 <- subsector_L1 <- aggr_mode <- vehicle_type <- det_veh <- aggr_nonmot <- NULL
  demand_F <- demand_EJ <- remind_rep <- V25 <- aggr_veh <- technology <- NULL
  ttot <- se_share <- fe_demand <- variable <- value <- demand_VKM <- loadFactor <- NULL
  all_enty <- ef <- variable_agg <- model <- scenario <- period <- NULL
  Region <- Variable <- co2 <- co2val <- elh2 <- fe  <- NULL
  int <- se <- sec  <- sharesec <- te  <- tech <-  val <- share <- NULL
  eff <- sharebio <- sharesyn <- totseliq <- type <- ven <- NULL

  load(file.path(output_folder, "config.Rdata"))

  datapath <- function(fname){
    file.path(output_folder, sub_folder, fname)
  }

  ## load input data from last EDGE run
  demand_km <- readRDS(datapath(fname = "demandF_plot_pkm.RDS"))[
    , demand_F := demand_F * 1e-3] ## million -> billion pkm
  load_factor <- readRDS(datapath(fname = "loadFactor.RDS"))
  annual_mileage <- readRDS(datapath(fname = "annual_mileage.RDS"))
  demand_vkm <- merge(demand_km, load_factor, by=c("year", "region", "vehicle_type"))
  demand_vkm[, demand_VKM := demand_F/loadFactor] ## billion vkm

  demand_ej <- readRDS(datapath(fname = "demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ

  name_mif = list.files(output_folder, pattern = "REMIND_generic", full.names = F)
  name_mif = file.path(output_folder, name_mif[!grepl("withoutPlu", name_mif)])

  stopifnot(typeof(name_mif) == "character")
  miffile <- readMIF(name_mif)

  ## ES and FE Demand

  reportingESandFE <- function(datatable, mode){

    datatable[, sector := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "Pass", "Freight")]

    ## attribute aggregated mode and vehicle names for plotting purposes, and aggregate
    ## Large Categories (as used by REMIND)
    datatable[subsector_L2 == "trn_pass_road_LDV",
              aggr_mode := "Pass|Road|LDV"]
    datatable[subsector_L2 != "trn_pass_road_LDV" & sector == "Pass",
              aggr_mode := "Pass|non-LDV"]
    datatable[is.na(aggr_mode), aggr_mode := "Freight"]

    ## A little more detail: Vehicle Aggregates
    datatable[grepl("^Truck", vehicle_type), aggr_veh := "Freight|Road"]
    datatable["Freight Rail_tmp_vehicletype" == vehicle_type, aggr_veh := "Freight|Rail"]
    ## there seem to be no passenger ships in EDGE-T!
    datatable[subsector_L3 == "International Ship", aggr_veh := "Freight|International Shipping"]
    datatable[subsector_L3 == "Domestic Ship", aggr_veh := "Freight|Navigation"]

    datatable[grepl("Bus", vehicle_type), aggr_veh := "Pass|Road|Bus"]


    if(mode == "ES")
      datatable[grepl("Cycle|Walk", subsector_L3), aggr_nonmot := "Pass|Road|Non-Motorized"]

    ## Rail & Aviation
    datatable[grepl("Passenger Rail|HSR", vehicle_type), aggr_veh := "Pass|Rail"]
    datatable[subsector_L3 == "Domestic Aviation", aggr_veh := "Pass|Aviation|Domestic"]
    datatable[subsector_L3 == "International Aviation", aggr_veh := "Pass|Aviation|International"]

    ## Freight
    datatable[grepl("^Truck", vehicle_type), det_veh := sprintf("Freight|Road|%s", vehicle_type)]

    ## High Detail: Ecoinvent-Compatible Output
    datatable[grepl("Subcompact", vehicle_type),
              det_veh := "Pass|Road|LDV|Small"]
    datatable[grepl("Mini", vehicle_type),
              det_veh := "Pass|Road|LDV|Mini"]
    datatable[vehicle_type == "Compact Car", det_veh := "Pass|Road|LDV|Medium"]
    datatable[grepl("Large Car|Midsize Car", vehicle_type), det_veh := "Pass|Road|LDV|Large"]
    datatable[grepl("SUV", vehicle_type),
              det_veh := "Pass|Road|LDV|SUV"]
    datatable[grepl("Van|Multipurpose", vehicle_type),
              det_veh := "Pass|Road|LDV|Van"]

    datatable[grepl("Motorcycle|Scooter|Moped", vehicle_type),
              det_veh := "Pass|Road|LDV|Two-Wheelers"]

    prepare4MIF <- function(dt, remind_unit, valcol, varcol){
      ## REMIND years, loading from MIF File would take too long
      yrs <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
      remind_scenario <- cfg$title

      prefix <- switch(mode,
                       "FE" = "FE|Transport|",
                       "ES" = "ES|Transport|",
                       "VKM" = "ES|Transport|VKM|")

      ## we only care for non-NA variables (NA is basically *all others*)
      toadd = dt[!is.na(get(varcol)), .(model=cfg$model_name, scenario=remind_scenario, region,
                                        variable=paste0(prefix, get(varcol)),
                                        unit=remind_unit, period=year,
                                        value=get(valcol))]

      toadd <- approx_dt(toadd, yrs, xcol="period", ycol = "value",
                         idxcols=colnames(toadd)[1:5],
                         extrapolate=T)
      return(toadd)
    }

    if(mode == "ES"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(aggr_nonmot), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_nonmot")],
          "bn pkm/yr", "V1", "aggr_nonmot"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      report <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "det_veh")], "bn vkm/yr", "V1", "det_veh")))
    }else{
      report <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode")],
          "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh")],
          "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh")],
          "EJ/yr", "V1", "det_veh")))
    }

    ## with energy carrier

    ## remove cycling and walking placeholder techs for ESs
    techmap <- data.table(technology=unique(datatable$technology),
                          key="technology")[!c("Cycle_tmp_technology", "Walk_tmp_technology")]

    if(mode == "ES"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "bn pkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                   "bn tkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_F, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "bn pkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_F, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                              "bn pkm/yr", "V1", "det_veh")))
    }else if(mode == "VKM"){
      ## for energy services, it is better to refer to the actual technologies
      ## and not the fuel types (-> LCA)
      techmap[, remind_rep := technology]
      techmap["LA-BEV", remind_rep := "BEV"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[sector == "Pass", sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "bn vkm/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[sector == "Freight" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
          "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[sector == "Pass" & !is.na(aggr_veh), sum(demand_VKM, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "bn vkm/yr", "V1", "aggr_veh"),
        prepare4MIF(datatable[!is.na(det_veh), sum(demand_VKM, na.rm=T),
                              by = c("region", "year", "det_veh", "remind_rep")
                              ][, det_veh := paste0(det_veh, "|", remind_rep)],
                              "bn vkm/yr", "V1", "det_veh")))
    }else{
      techmap["BEV", remind_rep := "Electricity"]
      techmap["Electric", remind_rep := "Electricity"]
      techmap["FCEV", remind_rep := "Hydrogen"]
      techmap["Hydrogen", remind_rep := "Hydrogen"]
      techmap["Liquids", remind_rep := "Liquids"]
      techmap["NG", remind_rep := "Gases"]

      datatable <- datatable[techmap, on="technology"]

      report_tech <- rbindlist(list(
        prepare4MIF(
          datatable[!is.na(aggr_mode), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_mode", "remind_rep")
                    ][, aggr_mode := paste0(aggr_mode, "|", remind_rep)],
                    "EJ/yr", "V1", "aggr_mode"),
        prepare4MIF(
          datatable[!is.na(aggr_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "aggr_veh", "remind_rep")
                    ][, aggr_veh := paste0(aggr_veh, "|", remind_rep)],
                    "EJ/yr", "V1", "aggr_veh"),
        prepare4MIF(
          datatable[!is.na(det_veh), sum(demand_EJ, na.rm=T),
                    by = c("region", "year", "det_veh", "remind_rep")
                    ][, det_veh := paste0(det_veh, "|", remind_rep)],
                    "EJ/yr", "V1", "det_veh")))

    }
    ## add World
    report_w = report[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report = rbind(report, report_w)

    report_tech_w = report_tech[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    report_tech = rbind(report_tech, report_tech_w)

    ## in case the aggregation features EU sub-regions, include also the aggregated versions
    return(rbindlist(list(report, report_tech)))
  }


  ## Demand emissions
  reportingEmi <- function(repFE, gdx){

    ## load emission factors for fossil fuels
    p_ef_dem <- readgdx(gdx, "p_ef_dem")[all_enty %in% c("fepet", "fedie", "fegas")]  ## MtCO2/EJ
    p_ef_dem[all_enty == "fegas", all_enty := "fegat"]
    setnames(p_ef_dem, old = c("value", "all_regi"), new = c("ef", "region"))
    ## attribute explicitly fuel used to the FE values
    emidem = repFE[grepl("Liquids|Gases|Hydrogen|Electricity", variable) & region != "World"]   ## EJ
    emidem[, all_enty := ifelse(grepl("Liquids", variable), "fedie", NA)]
    emidem[, all_enty := ifelse(grepl("LDV.+Liquids", variable), "fepet", all_enty)]
    emidem[, all_enty := ifelse(grepl("Gases", variable), "fegat", all_enty)]
    emidem[, all_enty := ifelse(grepl("Electricity", variable), "feelt", all_enty)]
    emidem[, all_enty := ifelse(grepl("Hydrogen", variable), "feh2t", all_enty)]
    ## merge with emission factors
    emidem = emidem[p_ef_dem, on = c("all_enty","region")]
    ## calculate emissions and attribute variable and unit names
    emidem[, value := value*ef][, c("variable", "unit") := list(gsub("FE", "Emi\\|CO2", variable), "Mt CO2/yr")]

    emi = rbind(copy(emidem)[, c("type", "variable") := list("tailpipe", paste0(variable, "|Tailpipe"))],
                   copy(emidem)[, c("type", "variable") := list("demand", paste0(variable, "|Demand"))])

    prodFe <- readgdx(gdx, "vm_prodFE")[, ttot := as.numeric(ttot)]
    setnames(prodFe,
             c("period", "region", "se", "all_enty", "te", "fe_demand"))
    prodFe[, se_share := fe_demand/sum(fe_demand), by=c("period", "region", "all_enty")]
    prodFe <- prodFe[all_enty %in% c("fedie", "fepet", "fegat") & se %in% c("segafos", "seliqfos")][, c("se", "te", "fe_demand") := NULL]

    emi <- prodFe[emi, on=c("period", "region", "all_enty")]
    ## in case no fossil fuels are used (e.g. 100% biodiesel), the value in se_share results NA. set the NA value to 0
    emi[is.na(se_share), se_share := 0]
    emi <- emi[all_enty %in% c("fedie", "fepet", "fegat") & type == "demand", value := value*se_share]

    emi[, c("se_share", "type", "ef", "all_enty") := NULL]

    ## aggregate removing the fuel dependency
    emi[, variable_agg := gsub("\\|Liquids|\\|Electricity|\\|Hydrogen|\\|Gases", "", variable)]
    emi = emi[, .(value = sum(value)), by = c("model", "scenario", "region", "unit", "period", "variable_agg")]
    setnames(emi, old = "variable_agg", new = "variable")
    emi = emi[, .(model, scenario, region, variable, unit, period, value)]

    ## add World
    emi_w = emi[,.(value = sum(value), region = "World"), by = .(model, scenario, variable, unit, period)]
    emi = rbind(emi, emi_w)

    return(emi)
  }

  reportingVehNum <- function(demand_vkm, annual_mileage){
    venum <- copy(demand_vkm)
    ## merge annual mileage
    anmil <- copy(annual_mileage)
    anmil[grepl("Subcompact", vehicle_type),
          variable := "Pass|Road|LDV|Small"]
    anmil[grepl("Mini", vehicle_type),
          variable := "Pass|Road|LDV|Mini"]
    anmil[vehicle_type == "Compact Car", variable := "Pass|Road|LDV|Medium"]
    anmil[grepl("Large Car|Midsize Car", vehicle_type), variable := "Pass|Road|LDV|Large"]
    anmil[grepl("SUV", vehicle_type),
          variable := "Pass|Road|LDV|SUV"]
    anmil[grepl("Van|Multipurpose", vehicle_type),
          variable := "Pass|Road|LDV|Van"]
    anmil[grepl("Motorcycle|Scooter|Moped", vehicle_type),
          variable := "Pass|Road|LDV|Two-Wheelers"]
    anmil[grepl("^Truck", vehicle_type),
          variable := sprintf("Freight|Road|%s", vehicle_type)]
    anmil[grepl("Bus", vehicle_type),
          variable := "Pass|Road|Bus"]

    anmil <- anmil[,.(region, period = year, variable, annual_mileage)]

    anmil <- approx_dt(anmil, unique(demand_vkm$period), xcol = "period", ycol = "annual_mileage", idxcols = c("region", "variable"), extrapolate = T)
    anmil<- unique(anmil[,c("period", "region", "variable", "annual_mileage")])
    anmil <- anmil[, variable := paste0("ES|Transport|VKM|", variable)]
    venum <- merge(demand_vkm, anmil, by = c("variable", "region", "period"))
    venum[, ven := value/annual_mileage] # billion vehicle-km -> thousand vehicles

    venum <- venum[!is.na(ven)]
    venum[, variable := gsub("|VKM", "|VNUM", variable, fixed=TRUE)][, c("value", "annual_mileage") := NULL]
    venum[, unit := "tsd veh"]
    setnames(venum, "ven", "value")
    venum = venum[,.(model, scenario, region, variable, unit, period, value)]
    return(venum)
  }

  reportStockAndSales <- function(){
    vintages_file <- file.path(output_folder, "vintcomp.csv")
    if(!file.exists(vintages_file)){
      print("EDGE-T Reporting: No vintages file found.")
      return(NULL)
    }

    year_c <- construction_year <- Stock <- Sales <- vintage_demand_vkm <- fct <- NULL
    vintgs <- fread(vintages_file)

    ## backward compat. fix
    fct <- 1.
    if("variable" %in% colnames(vintgs)){
      fct <- 1e-6
      setnames(vintgs, "variable", "construction_year")
    }
 
    vintgs[, year_c := as.numeric(gsub("C_", "", construction_year))]

    ## stock is the full stock up to the end of the current year
    ## sales are the sales of the current year

    setnames(vintgs, "full_demand_vkm", "Stock")
    vintgs[, Stock := Stock * fct]
    vintgs[, Sales := Stock - sum(vintage_demand_vkm), by=.(year, region, vehicle_type, technology)]
    vintgs[, c("construction_year", "vintage_demand_vkm", "year_c") := NULL]
    vintgs <- unique(vintgs)

    vintgs <- data.table::melt(vintgs, measure.vars = c("Stock", "Sales"))
    ## vkm -> v-num
    vintgs[, value := value / 15000]
    vintgs[, variable := sprintf("%s|Transport|LDV|%s|%s", variable, vehicle_type, technology)]

    vintgs[, c("vehicle_type", "technology") := NULL]
    vintgs <- vintgs[!is.na(value)]

    setnames(vintgs, "year", "period")
    vintgs <- vintgs[CJ(period=c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                        region=vintgs$region, variable=vintgs$variable, unique=TRUE),
                     on=c("period", "region", "variable")]

    vintgs[, `:=`(model=cfg$model_name, scenario=cfg$title, unit="Million vehicles")]

    return(vintgs)
    
  }

  repFE <- reportingESandFE(
    demand_ej,
    mode ="FE")
  repVKM <- reportingESandFE(
    datatable=demand_vkm,
    mode="VKM")

  toMIF <- rbindlist(list(
    repFE,
    repVKM,
    reportingESandFE(
      datatable=demand_km,
      mode="ES"),
    reportingVehNum(repVKM,
                    annual_mileage),
    reportingEmi(repFE = repFE,
                 gdx = gdx)
  ))


  ## add Road Totals
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|Pass|Road",
            unit="bn pkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Pass|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="ES|Transport|VKM|Pass|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|Pass\\|Road\\|[A-Za-z-]+$", variable),
          .(variable="FE|Transport|Pass|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  ## VKM totals, Road and Rail
  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Road$", variable),
          .(variable="ES|Transport|VKM|Road",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("ES\\|Transport\\|VKM\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="ES|Transport|VKM|Rail",
            unit="bn vkm/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]
    ), use.names = TRUE)

  toMIF <- rbindlist(list(
    toMIF,
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Road$", variable),
          .(variable="FE|Transport|Road",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Road|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Tailpipe$", variable),
          .(variable="Emi|CO2|Transport|Rail|Tailpipe",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Road\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Road|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("Emi\\|CO2\\|Transport\\|(Pass|Freight)\\|Rail\\|Demand$", variable),
          .(variable="Emi|CO2|Transport|Rail|Demand",
            unit="Mt CO2/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")],
    toMIF[grep("FE\\|Transport\\|(Pass|Freight)\\|Rail$", variable),
          .(variable="FE|Transport|Rail",
            unit="EJ/yr", value=sum(value)),
          by=c("model", "scenario", "region", "period")]), use.names = TRUE)

  toMIF <- rbindlist(list(toMIF, reportStockAndSales()), use.names=TRUE)


  if (!is.null(regionSubsetList)){
    toMIF <- rbindlist(list(
      toMIF,
      toMIF[region %in% regionSubsetList[[1]],.(value = sum(value), region = "EUR"), by = .(model, scenario, variable, unit, period)],
      toMIF[region %in% regionSubsetList[[2]],.(value = sum(value), region = "NEU"), by = .(model, scenario, variable, unit, period)]
    ), use.names=TRUE)
  }


  ## Make sure there are no duplicates!
  idx <- anyDuplicated(toMIF, by = c("region", "variable", "period"))
  if(idx){
    warning(paste0("Duplicates found in EDGE-T reporting output:",
                   capture.output(toMIF[idx]), collapse="\n"))
  }

  toMIF <- data.table::dcast(toMIF, ... ~ period, value.var="value")
  setnames(toMIF, colnames(toMIF)[1:5], c("Model", "Scenario", "Region", "Variable", "Unit"))

  writeMIF(toMIF, name_mif, append=T)
  deletePlus(name_mif, writemif=T)
}
