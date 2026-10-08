#' Modify Investment Variables
#'
#' This function transforms investment variables to the normal reporting convention.
#'
#' Years represented by investment variables in the energy system
#' ('vm_deltaCap', 'vm_costInvTeDir' and 'vm_costInvTeAdj') are different from the normal
#' reporting convention. In the current REMIND version, vm_deltacap(t) represents
#' the average of the years t-4..t, while in the general reporting
#' convention it represents the average of t-2.5..t+2.5 (for 5 year time steps).
#'
#' See also: https://github.com/remindmodel/remind/pull/1238.
#'
#'
#' @param x a magclass object to be manipulated
#' @param ref an optional magclass object to be used for fixing values before 'startYear'
#' @param startYear years before will be overwritten with values from 'ref'
#'
#'
#' @author Falk Benke
#'
#'
modifyInvestmentVariables <- function(x, ref = NULL, startYear = NULL) {

  # generate mapping from REMIND timesteps to yearly timesteps for REMIND investment variables (vm_deltaCap etc.)
  # REMIND investment variables are defined to cover years between the last and the current REMIND time step.
  # Example: vm_deltaCap(2020) refers to annual capacity additions from 2016 to 2020.
  investTs <- rbind(
    # first year is 1898, as 1990 represents 1898 to 1902 in REMIND
    data.frame(year = seq(1898, 2110, 1)) %>%
      mutate("period" = ifelse(
        .data$year <= 2060,
        ifelse(.data$year %% 5 == 0, .data$year, .data$year + 5 - (.data$year %% 5)),
        ifelse(.data$year %% 10 == 0, .data$year, .data$year + 10 - (.data$year %% 10))
      )),
    # highest year is 2167, as 2150 represents 2140 - 2167 in REMIND
    data.frame(year = seq(2111, 2167, 1)) %>%
      mutate("period" = ifelse(.data$year <= 2130, 2130, 2150))
  )

  # generate mapping from yearly timesteps to REMIND reporting timesteps.
  # Reported variables are defined as averages between t-2 and t+2 around the central year t.
  # Example: New Cap|XYZ(2020) refers to average annual capacity additions of technology XYZ in 2018 to 2022.
  remindTs <-
    rbind(
      # mapping for 2005 - 2150 from quitte
      quitte::remind_timesteps,
      # enhance the logic to cover all years starting from 1898 (e.g. period 2000 represents 1998 - 2002)
      data.frame(year = seq(1898, 2002, 1), weight = 1) %>%
        mutate("period" = ifelse(.data$year %% 10 %in% c(1, 2, 8, 9), round(.data$year, digits = -1),
          ifelse(.data$year %% 10 %in% c(6, 7), .data$year - (.data$year %% 5),
            ifelse(.data$year %% 10 %in% c(3, 4), .data$year + 5 - (.data$year %% 5), .data$year)
          )
        ))
    )

  investTs <- investTs %>%
    filter(.data$period %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      "year" = paste0("y", .data$year),
      "period" = paste0("y", .data$period),
    )

  # map variables from model timesteps to yearly timesteps (e.g. 2020 -> 2016-2020)
  x <- toolAggregate(x, dim = 2, rel = investTs, from = "period", to = "year", verbosity = 2)

  remindTs <- remindTs %>%
    filter(.data$year %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      "year" = paste0("y", .data$year),
      "period" = paste0("y", .data$period),
    )

  w <- remindTs %>%
    select(-"period") %>%
    # period x weight combos should be distinct for this to work
    distinct() %>%
    as.magpie()

  # Average variables with yearly timesteps to 5-year reporting time steps defined
  # around center year (e.g. 2018-2022 average -> 2020)
  x <- toolAggregate(x, dim = 2, rel = remindTs, weight = w, from = "year", to = "period") %>%
    magpiesort()

  if (!is.null(ref)) {

    ref <- magpiesort(ref)

    fixedYears <- getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) < startYear]
    if (length(fixedYears) == 0) {
      return(x)
    }

    ref <- modifyInvestmentVariables(ref)
    if (!is.null(getNames(x))) {
      ref <- magclass::matchDim(ref, x, dim = 3, fill = 0)
    }
    x[, fixedYears, ] <- ref[, fixedYears, ]
  }

  return(magclass::magpiesort(x))
}
