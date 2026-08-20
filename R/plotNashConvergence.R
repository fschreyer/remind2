#' @title Creates a REMIND convergence overview
#'
#' @param gdx GDX file
#' @author Renato Rodrigues, Falk Benke
#'
#' @examples
#'
#'   \dontrun{
#'     plotNashConvergence(gdx="fulldata.gdx")
#'   }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr summarise group_by mutate filter distinct case_when
#' @importFrom quitte as.quitte
#' @importFrom data.table :=
#' @importFrom mip plotstyle
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous scale_y_discrete scale_fill_manual scale_color_manual
#' @importFrom ggplot2 coord_cartesian aes_ geom_rect theme geom_point geom_hline
#' @importFrom plotly ggplotly config hide_legend subplot layout
#'
#' @export
plotNashConvergence <- function(gdx) { # nolint cyclocomp_linter

  .generatePlots <- function(gdx) {

    lastIteration <- readGDX(gdx, name = "o_iterationNumber", react = "error")[[1]]

    aestethics <- list(
      "alpha" = 0.6,
      "line" = list("size" = 2 / 3.78),
      "point" = list("size" = 2 / 3.78)
    )

    booleanColor <- c("yes" = "#00BFC4", "no" = "#F8766D")

    activeCriteriaColor <- c("true" = "#000000", "false" = "#cccccc")

    subplots <- list()

    activeCriteria <- suppressWarnings(gdx::readGDX(gdx, "activeConvMessage80"))

    # convergence tolerances, read from the gdx with fallbacks to the historical
    # hardcoded defaults for older gdx files (see main.gms / nash postsolve.gms)
    objValConvTol <- suppressWarnings(as.numeric(readGDX(gdx, "cm_nashObjVal_tolerance", react = "silent")))
    if (length(objValConvTol) == 0 || is.na(objValConvTol)) objValConvTol <- 1e-4

    devPriceAnticipTolFactor <- suppressWarnings(as.numeric(readGDX(gdx, "cm_DevPriceAnticip_tolFactor", react = "silent")))
    if (length(devPriceAnticipTolFactor) == 0 || is.na(devPriceAnticipTolFactor)) devPriceAnticipTolFactor <- 0.1

    taxConvTol <- suppressWarnings(as.numeric(readGDX(gdx, "cm_TaxConv_tolerance", react = "silent")))
    if (length(taxConvTol) == 0 || is.na(taxConvTol)) taxConvTol <- 0.001

    # Feasibility -----
    p80RepyIteration <- readGDX(gdx, name = "p80_repy_iteration", restore_zeros = FALSE, react = "error") %>%
      as.quitte() %>%
      select(c("solveinfo80", "region", "iteration", "value")) %>%
      tidyr::pivot_wider(names_from = "solveinfo80") %>%
      mutate(
        "iteration" := as.numeric(as.character(.data$iteration)),
        "convergence" := case_when(
          .data$modelstat == 1 & .data$solvestat == 1 ~ "optimal",
          .data$modelstat == 2 & .data$solvestat == 1 ~ "optimal",
          .data$modelstat == 7 & .data$solvestat == 4 ~ "feasible",
          is.na(.data$modelstat) & is.na(.data$solvestat) ~ "skipped",
          .default = "infeasible"
        )
      )

    data <- p80RepyIteration %>%
      group_by(.data$iteration, .data$convergence) %>%
      mutate("details" = paste0("Iteration: ", .data$iteration,
                                "<br>region: ", paste0(.data$region, collapse = ", "))) %>%
      ungroup()

    data$convergence <- factor(data$convergence, levels = c("infeasible", "feasible", "skipped", "optimal"))

    convergencePlot <-
      suppressWarnings(ggplot(mapping = aes_(~iteration, ~convergence, text = ~details))) +
      geom_line(
        data = data,
        linetype = "dashed",
        aes_(group = ~region, color = ~region),
        alpha = aestethics$alpha,
        linewidth = aestethics$line$size
      ) +
      geom_point(
        data = select(data, c("iteration", "convergence", "details")) %>% distinct(),
        aes_(fill = ~convergence),
        size = 2,
        alpha = aestethics$alpha
      ) +
      scale_fill_manual(values = c("optimal" = "#00BFC4", "skipped" = "#fcae1e", "feasible" = "#ffcc66", "infeasible" = "#F8766D")) +
      scale_color_manual(values = plotstyle(as.character(unique(data$region)))) +
      scale_y_discrete(breaks = c("infeasible", "feasible", "skipped", "optimal"), drop = FALSE) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_text(colour = ifelse(("infes" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    convergencePlotPlotly <- ggplotly(convergencePlot, tooltip = c("text"))
    subplots <- append(subplots, list(convergencePlotPlotly))

    # Optimality / Objective Deviation ----

    p80ConvNashObjValIter <- readGDX(gdx, name = "p80_convNashObjVal_iter", react = "error") %>%
      as.quitte() %>%
      select(c("region", "iteration", "objvalDifference" = "value")) %>%
      mutate("iteration" := as.numeric(as.character(.data$iteration))) %>%
      filter(.data$iteration <= lastIteration)

    p80RepyIteration <- readGDX(gdx, name = "p80_repy_iteration", restore_zeros = FALSE, react = "error") %>%
      as.quitte() %>%
      select(c("solveinfo80", "region", "iteration", "value")) %>%
      mutate("iteration" := as.numeric(as.character(.data$iteration))) %>%
      tidyr::pivot_wider(names_from = "solveinfo80")

    p80RepyIteration <- p80RepyIteration %>%
      left_join(p80ConvNashObjValIter, by = c("region", "iteration")) %>%
      group_by(.data$region) %>%
      mutate(
        "objvalCondition" = ifelse(.data$modelstat == "2", TRUE,
          ifelse(.data$modelstat == "7" & is.na(.data$objvalDifference), FALSE,
            ifelse(.data$modelstat == "7" & .data$objvalDifference < -objValConvTol, FALSE, TRUE)
          )
        )
      ) %>%
      ungroup() %>%
      group_by(.data$iteration) %>%
      mutate("objvalConverge" = all(.data$objvalCondition)) %>%
      ungroup()

    data <- p80RepyIteration %>%
      select("iteration", "objvalConverge") %>%
      distinct() %>%
      mutate(
        "objVarCondition" := ifelse(.data$objvalConverge, "yes", "no"),
        "tooltip" := paste0("Iteration: ", .data$iteration, "<br>Converged")
      )

    for (iter in unique(data$iteration)) {
      current <- p80RepyIteration %>% filter(.data$iteration == iter, !is.na(.data$objvalCondition))

      if (!all(current$objvalCondition)) {
        tooltip <- NULL
        current <- filter(current, .data$objvalCondition == FALSE)

        for (reg in current$region) {
          diff <- current[current$region == reg, ]$objvalDifference
          tooltip <- paste0(tooltip, "<br> ", reg, "   |     ", round(diff, 5))
        }
        tooltip <- paste0(
          "Iteration: ", iter, "<br>Not converged",
          "<br>Region | Deviation", tooltip, "<br>The deviation limit is +- ", objValConvTol
        )
        data[which(data$iteration == iter), ]$tooltip <- tooltip
      }
    }

    objVarSummary <- suppressWarnings(ggplot(data, aes_(
      x = ~iteration, y = "Objective\nDeviation",
      fill = ~objVarCondition, text = ~tooltip
    ))) +      theme_minimal() +
      geom_point(size = 2, alpha = aestethics$alpha) +
      scale_fill_manual(values = booleanColor) +
      scale_y_discrete(breaks = c("Objective\nDeviation"), drop = FALSE) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_text(colour = ifelse(("nonopt" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    objVarSummaryPlotly <- ggplotly(objVarSummary, tooltip = c("text"))
    subplots <- append(subplots, list(objVarSummaryPlotly))


    # Trade goods surplus detail ----

    surplus <- readGDX(gdx, name = "p80_surplusMax_iter", restore_zeros = FALSE, react = "error")[, c(2100, 2150), ] %>%
      as.quitte() %>%
      select(c("period", "value", "all_enty", "iteration")) %>%
      mutate(
        "iteration" := as.numeric(as.character(.data$iteration)),
        "value" := ifelse(is.na(.data$value), 0, .data$value),
        "type" := case_when(
          .data$all_enty == "good" ~ "Goods trade surplus",
          .data$all_enty == "perm" ~ "Permits",
          TRUE ~ "Primary energy trade surplus"
        )
      )

    p80SurplusMaxTolerance <- readGDX(gdx, name = "p80_surplusMaxTolerance", restore_zeros = FALSE, react = "error") %>%
      as.quitte() %>%
      select(c("maxTol" = 7, "all_enty" = 8))

    surplus <- left_join(surplus, p80SurplusMaxTolerance, by = "all_enty") %>%
      mutate(
        "maxTol" := ifelse(.data$period == 2150, .data$maxTol * 10, .data$maxTol),
        "withinLimits" := ifelse(.data$value > .data$maxTol, "no", "yes")
      )

    data <- surplus

    data$tooltip <- paste0(
      ifelse(data$withinLimits == "no",
        paste0(data$all_enty, " trade surplus (", data$value,
               ") is greater than maximum tolerance (", data$maxTol, ")."),
        paste0(data$all_enty, " trade surplus (", data$value,
               ") is within tolerance (", data$maxTol, ").")
      ),
      "<br>Iteration: ", data$iteration
    )

    limits <- surplus %>%
      group_by(.data$type, .data$period, .data$iteration) %>%
      mutate("withinLimits" = ifelse(all(.data$withinLimits == "yes"), "yes", "no")) %>%
      ungroup() %>%
      select("type", "period", "iteration", "maxTol", "withinLimits") %>%
      distinct() %>%
      mutate(
        "rectXmin" = .data$iteration - 0.5,
        "rectXmax" = .data$iteration + 0.5,
        "tooltip" = paste0(
          .data$type,
          ifelse(.data$withinLimits == "no",
            " outside tolerance limits.",
            " within tolerance limits."
          )
        )
      )

    surplusColor <- c(
      peoil = "#cc7500",
      pegas = "#999959",
      pecoal = "#0c0c0c",
      peur = "#EF7676",
      pebiolc = "#005900",
      good = "#00BFC4"
    )

    surplusConvergence <- ggplot() +
      suppressWarnings(geom_rect(
        data = limits,
        aes_(
          xmin = ~rectXmin, xmax = ~rectXmax,
          ymin = 0, ymax = ~maxTol,
          fill = ~withinLimits, text = ~tooltip
        ),
        inherit.aes = FALSE,
        alpha = aestethics$alpha
      )) +
      suppressWarnings(geom_line(
        data = data,
        aes_(
          x = ~iteration, y = ~value, color = ~all_enty,
          group = ~all_enty, text = ~tooltip
        ),
        alpha = aestethics$alpha,
        linewidth = aestethics$line$size
      )) +
      theme_minimal() +
      facet_grid(type ~ period, scales = "free_y") +
      scale_color_manual(values = surplusColor) +
      scale_fill_manual(values = booleanColor) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.y = element_text(colour = ifelse(("surplus" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    surplusConvergencePlotly <- ggplotly(surplusConvergence, tooltip = c("text"), height = 700) %>%
      hide_legend() %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) %>%
      layout(hovermode = "closest")

    # Trade surplus summary ----

    surplusCondition <- surplus %>%
      group_by(.data$iteration) %>%
      summarise(withinLimits = ifelse(all(.data$withinLimits == "yes"), "yes", "no")) %>%
      mutate("tooltip" = paste0("Iteration: ", .data$iteration, "<br>Converged"))

    for (iter in surplusCondition$iteration) {
      if (surplusCondition[which(surplusCondition$iteration == iter), ]$withinLimits == "no") {
        tooltip <- NULL
        for (period in unique(surplus$period)) {
          for (good in unique(surplus$all_enty)) {
            currSurplus <- surplus[which(surplus$iteration == iter & surplus$period == period &
                                           surplus$all_enty == good), ]
            withinLimits <- ifelse(currSurplus$value > currSurplus$maxTol,
                                   "no", ifelse(currSurplus$value < -currSurplus$maxTol, "no", "yes"))
            if (withinLimits == "no") {
              tooltip <- paste0(tooltip, "<br> ", period, " | ", good, " | ",
                                ifelse(currSurplus$value > currSurplus$maxTol,
                                       paste0(round(currSurplus$value, 5), " > ", currSurplus$maxTol),
                                       paste0(round(currSurplus$value, 5), " < ", -currSurplus$maxTol)))
            }
          }
        }
        tooltip <- paste0(
          "Iteration: ", iter, "<br>Not converged",
          "<br>Period | Trade | Surplus", tooltip
        )
        surplusCondition[which(surplusCondition$iteration == iter), ]$tooltip <- tooltip
      }
    }

    surplusSummary <- suppressWarnings(ggplot(surplusCondition,
                                              aes_(x = ~iteration, y = "Max. Trade\nSurplus",
                                                   fill = ~withinLimits, text = ~tooltip))) +      theme_minimal() +
      geom_point(size = 2, alpha = aestethics$alpha) +
      scale_fill_manual(values = booleanColor) +
      scale_y_discrete(breaks = c("Max. Trade\nSurplus"), drop = FALSE) +
      labs(x = NULL, y = NULL)

    surplusSummaryPlotly <- ggplotly(surplusSummary, tooltip = c("text"))
    subplots <- append(subplots, list(surplusSummaryPlotly))

    # Deviation due to price anticipation ----

    maxTolerance <- readGDX(gdx,
      name = "p80_surplusMaxTolerance",
      restore_zeros = FALSE, react = "error"
    )[, , "good"] %>%
      as.numeric()

    data <- readGDX(gdx,
      name = "p80_DevPriceAnticipGlobAllMax2100Iter",
      restore_zeros = FALSE, react = "error"
    ) %>%
      as.quitte() %>%
      select("iteration", "value") %>%
      mutate(
        "iteration" := as.numeric(as.character(.data$iteration)),
        "converged" = ifelse(.data$value > devPriceAnticipTolFactor * maxTolerance, "no", "yes"),
        "tooltip" = ifelse(.data$value > devPriceAnticipTolFactor * maxTolerance,
          paste0(
            "Iteration: ", .data$iteration, "<br>",
            "Not converged<br>Price Anticipation deviation is not low enough<br>",
            round(.data$value, 5), " > ", devPriceAnticipTolFactor * maxTolerance
          ),
          paste0(
            "Iteration: ", .data$iteration, "<br>",
            "Converged"
          )
        ),
      )

    priceAnticipationDeviation <- ggplot(data, aes_(x = ~iteration)) +
      suppressWarnings(geom_point(
        size = 2,
        aes_(y = 0.0001, fill = ~converged, text = ~tooltip),
        alpha = aestethics$alpha
      )) +
      theme_minimal() +
      scale_fill_manual(values = booleanColor) +
      scale_y_continuous(breaks = c(0.0001), labels = c("Price Anticipation\nDeviation")) +
      scale_x_continuous(breaks = c(data$iteration)) +
      labs(x = NULL, y = NULL) +
      coord_cartesian(ylim = c(-0.2, 1)) +
      theme(axis.text.y = element_text(colour = ifelse(("DevPriceAnticip" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    priceAnticipationDeviation <- ggplotly(priceAnticipationDeviation, tooltip = c("text"))
    subplots <- append(subplots, list(priceAnticipationDeviation))

    # Emission Market Deviation (optional) ----

    pmEmiMktTarget <- readGDX(gdx, name = "pm_emiMktTarget", react = "silent", restore_zeros = FALSE)

    if (!is.null(pmEmiMktTarget)) {

      pmEmiMktTargetDevIter <- suppressWarnings(
        readGDX(gdx, name = "pm_emiMktTarget_dev_iter", react = "silent", restore_zeros = FALSE)
      )

      pm_emiMktTarget_tolerance <- mip::getPlotData("pm_emiMktTarget_tolerance", gdx)
      emiMktTarget_tolerance <- setNames(pm_emiMktTarget_tolerance$pm_emiMktTarget_tolerance,pm_emiMktTarget_tolerance$ext_regi)

      pmEmiMktTargetDevIter <- pmEmiMktTargetDevIter %>%
        as.quitte() %>%
        filter(!is.na(.data$value)) %>% # remove unwanted combinations introduced by readGDX
        select("period", "iteration", "ext_regi", "emiMktExt", "value") %>%
        mutate("converged" = .data$value <= emiMktTarget_tolerance[.data$ext_regi])

      data <- pmEmiMktTargetDevIter %>%
        group_by(.data$iteration) %>%
        summarise(converged = ifelse(any(.data$converged == FALSE), "no", "yes")) %>%
        mutate("tooltip" = paste0("Iteration: ", .data$iteration, "<br>","Converged"))

      for (i in unique(pmEmiMktTargetDevIter$iteration)) {
        if (data[data$iteration == i, "converged"] == "no") {
          tmp <- filter(pmEmiMktTargetDevIter, .data$iteration == i, .data$converged == FALSE) %>%
            mutate("item" = paste0(.data$ext_regi, " ", .data$period, " ", .data$emiMktExt)) %>%
            select("ext_regi", "period", "emiMktExt", "item") %>%
            distinct()

          if (nrow(tmp) > 10) {
            data[data$iteration == i, "tooltip"] <- paste0(
              "Iteration ", i, "<br>",
              "Not converged:<br>",
              paste0(unique(tmp$ext_regi), collapse = ", "),
              "<br>",
              paste0(unique(tmp$period), collapse = ", "),
              "<br>",
              paste0(unique(tmp$emiMktExt), collapse = ", ")
            )
          } else {
            data[data$iteration == i, "tooltip"] <- paste0(
              "Iteration ", i, "<br>",
              "Not converged:<br>",
              paste0(unique(tmp$item), collapse = ", ")
            )
          }
        }
      }

      emiMktTargetDev <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "Emission Market\nTarget",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("Emission Market\nTarget"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("regiTarget" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      emiMktTargetDevPlotly <- ggplotly(emiMktTargetDev, tooltip = c("text"))

      subplots <- append(subplots, list(emiMktTargetDevPlotly))
    }

    # Implicit Quantity Target (optional) ----

    pmImplicitQttyTarget <- readGDX(gdx, name = "pm_implicitQttyTarget", restore_zeros = FALSE,
                                    react = "silent")

    if (!is.null(pmImplicitQttyTarget)) {

      cmImplicitQttyTargetTolerance <- as.vector(readGDX(gdx, name = "cm_implicitQttyTarget_tolerance",
                                                         react = "error"))

      pmImplicitQttyTarget <- readGDX(gdx, name = "pm_implicitQttyTarget", restore_zeros = FALSE, react = "error") %>%
        as.quitte() %>%
        select("period", "ext_regi", "taxType", "qttyTarget", "qttyTargetGroup")

      pmImplicitQttyTargetIsLimited <- readGDX(gdx, name = "pm_implicitQttyTarget_isLimited", restore_zeros = FALSE, react = "error")

      p80ImplicitQttyTargetDevIter <- readGDX(gdx, name = "p80_implicitQttyTarget_dev_iter",
                                              restore_zeros = FALSE, react = "error") %>%
        as.quitte() %>%
        select("period", "value", "iteration", "ext_regi", "qttyTarget", "qttyTargetGroup")

      if(all(lengths(attr(pmImplicitQttyTargetIsLimited, 'dimnames')) != 0)){
        pmImplicitQttyTargetIsLimited <- pmImplicitQttyTargetIsLimited %>%
          as.quitte() %>%
          select("iteration", "qttyTarget", "qttyTargetGroup", "isLimited" = "value")
      } else {
        pmImplicitQttyTargetIsLimited <- p80ImplicitQttyTargetDevIter %>% select(-"value") %>% mutate("isLimited" = 0)
      }

      p80ImplicitQttyTargetDevIter <- p80ImplicitQttyTargetDevIter %>%
        left_join(pmImplicitQttyTarget, by = c("period", "ext_regi", "qttyTarget", "qttyTargetGroup")) %>%
        left_join(pmImplicitQttyTargetIsLimited, by = c("iteration", "qttyTarget", "qttyTargetGroup")) %>%
        mutate(
          "failed" =
            abs(.data$value) > cmImplicitQttyTargetTolerance & (
              !(ifelse(.data$taxType == "tax", .data$value < 0, FALSE)) |
              ifelse(.data$taxType == "sub", .data$value > 0, FALSE)
            ) & .data$isLimited != 1
        )

      data <- p80ImplicitQttyTargetDevIter %>%
        group_by(.data$iteration) %>%
        summarise(converged = ifelse(any(.data$failed == TRUE), "no", "yes")) %>%
        mutate("tooltip" = ifelse(.data$converged == "yes", paste0("Iteration: ", .data$iteration, "<br>","Converged"), paste0("Iteration: ", .data$iteration, "<br>","Not converged")))

      qttyTarget <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "Implicit Quantity\nTarget",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("Implicit Quantity\nTarget"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("implicitEnergyTarget" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      qttyTargetPlotly <- ggplotly(qttyTarget, tooltip = c("text"))
      subplots <- append(subplots, list(qttyTargetPlotly))

    }

    # Implicit FE Price Target (optional) ----
    # The convergence criterion is |deviation| <= 5% (hardcoded in
    # regiCarbonPrice/postsolve.gms). We read the per-iteration deviation and flag
    # any iteration where a target exceeds the tolerance. Ignored ("ignConv") cases
    # are only stored for the final iteration, so the intermediate coloring here is a
    # close approximation of the criterion, see nashConvergence-priceTarget.html for detail.

    p47ImplicitPriceDevIter <- suppressWarnings(
      readGDX(gdx, name = "p47_implicitPrice_dev_iter", restore_zeros = FALSE, react = "silent")
    )

    if (!is.null(p47ImplicitPriceDevIter)) {

      implicitPriceTol <- suppressWarnings(as.numeric(readGDX(gdx, name = "cm_implicitPriceTarget_tolerance", react = "silent")))
      if (length(implicitPriceTol) == 0 || is.na(implicitPriceTol)) implicitPriceTol <- 0.05

      p47ImplicitPriceDevIter <- p47ImplicitPriceDevIter %>%
        as.quitte() %>%
        filter(!is.na(.data$value)) %>%
        select("iteration", "value") %>%
        mutate(
          "iteration" := as.numeric(as.character(.data$iteration)),
          "failed" = abs(.data$value) > implicitPriceTol
        )

      data <- p47ImplicitPriceDevIter %>%
        group_by(.data$iteration) %>%
        summarise(
          converged = ifelse(any(.data$failed == TRUE), "no", "yes"),
          worst = max(abs(.data$value))
        ) %>%
        mutate("tooltip" = ifelse(.data$converged == "yes",
          paste0("Iteration: ", .data$iteration, "<br>Converged<br>Max |deviation|: ", round(100 * .data$worst, 2), "%"),
          paste0("Iteration: ", .data$iteration, "<br>Not converged<br>Max |deviation|: ", round(100 * .data$worst, 2), "% > ", round(100 * implicitPriceTol, 2), "%")))

      implicitPriceTargetDev <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "FE Price\nTarget",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("FE Price\nTarget"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("cm_implicitPriceTarget" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      implicitPriceTargetDevPlotly <- ggplotly(implicitPriceTargetDev, tooltip = c("text"))
      subplots <- append(subplots, list(implicitPriceTargetDevPlotly))
    }

    # Implicit PE Price Target (optional) ----

    p47ImplicitPePriceDevIter <- suppressWarnings(
      readGDX(gdx, name = "p47_implicitPePrice_dev_iter", restore_zeros = FALSE, react = "silent")
    )

    if (!is.null(p47ImplicitPePriceDevIter)) {

      implicitPePriceTol <- suppressWarnings(as.numeric(readGDX(gdx, name = "cm_implicitPePriceTarget_tolerance", react = "silent")))
      if (length(implicitPePriceTol) == 0 || is.na(implicitPePriceTol)) implicitPePriceTol <- 0.05

      p47ImplicitPePriceDevIter <- p47ImplicitPePriceDevIter %>%
        as.quitte() %>%
        filter(!is.na(.data$value)) %>%
        select("iteration", "value") %>%
        mutate(
          "iteration" := as.numeric(as.character(.data$iteration)),
          "failed" = abs(.data$value) > implicitPePriceTol
        )

      data <- p47ImplicitPePriceDevIter %>%
        group_by(.data$iteration) %>%
        summarise(
          converged = ifelse(any(.data$failed == TRUE), "no", "yes"),
          worst = max(abs(.data$value))
        ) %>%
        mutate("tooltip" = ifelse(.data$converged == "yes",
          paste0("Iteration: ", .data$iteration, "<br>Converged<br>Max |deviation|: ", round(100 * .data$worst, 2), "%"),
          paste0("Iteration: ", .data$iteration, "<br>Not converged<br>Max |deviation|: ", round(100 * .data$worst, 2), "% > ", round(100 * implicitPePriceTol, 2), "%")))

      implicitPePriceTargetDev <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "PE Price\nTarget",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("PE Price\nTarget"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("cm_implicitPePriceTarget" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      implicitPePriceTargetDevPlotly <- ggplotly(implicitPePriceTargetDev, tooltip = c("text"))
      subplots <- append(subplots, list(implicitPePriceTargetDevPlotly))
    }

    # Global Bugdet Deviation (optional) ----
    cm_budgetCO2_absDevTol <- as.vector(readGDX(gdx, name = "cm_budgetCO2_absDevTol", react = "error"))
    p80_globalBudget_absDev_iter <- readGDX(gdx, name = "p80_globalBudget_absDev_iter",
                                      restore_zeros = FALSE, react = "error")

    if(all(lengths(attr(p80_globalBudget_absDev_iter, 'dimnames')) != 0)){

      p80_globalBudget_absDev_iter <- p80_globalBudget_absDev_iter %>%
        as.quitte() %>%
        select("value", "iteration") %>%
        mutate("failed" = .data$value > cm_budgetCO2_absDevTol | .data$value < -cm_budgetCO2_absDevTol)

      data <- p80_globalBudget_absDev_iter %>%
        mutate(
          "converged" = ifelse(.data$failed == TRUE, "no", "yes"),
          "tooltip" = ifelse(.data$failed, paste0("Iteration: ", .data$iteration, "<br>","Not converged"), paste0("Iteration: ", .data$iteration, "<br>","Converged"))
        )

      globalBuget <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "Global Budget\nDeviation",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("Global Budget\nDeviation"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("globalbudget" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      globalBugetPlotly <- ggplotly(globalBuget, tooltip = c("text"))
      subplots <- append(subplots, list(globalBugetPlotly))

    }

    # Internalized Damages (optional) ----

    module2realisation <- readGDX(gdx, name = "module2realisation", react = "error")
    if (module2realisation[module2realisation$modules == "internalizeDamages", ][, 2] != "off") {
      cmSccConvergence <- as.numeric(readGDX(gdx, name = "cm_sccConvergence",
                                             types = c("parameters"), react = "error"))
      cmTempConvergence <- as.numeric(readGDX(gdx, name = "cm_tempConvergence",
                                              types = c("parameters"), react = "error"))
      p80SccConvergenceMaxDeviationIter <- readGDX(gdx, name = "p80_sccConvergenceMaxDeviation_iter",
                                                   react = "error") %>%
        as.quitte() %>%
        select("iteration", "p80SccConvergenceMaxDeviationIter" = "value") %>%
        mutate("iteration" := as.numeric(as.character(.data$iteration))) %>%
        filter(.data$iteration <= lastIteration)

      p80GmtConvIter <- readGDX(gdx, name = "p80_gmt_conv_iter", react = "error") %>%
        as.quitte() %>%
        select("iteration", "p80GmtConvIter" = "value") %>%
        mutate("iteration" := as.numeric(as.character(.data$iteration))) %>%
        filter(.data$iteration <= lastIteration)

      data <- left_join(p80SccConvergenceMaxDeviationIter, p80GmtConvIter, by = "iteration") %>%
        mutate(
          "converged" = ifelse(.data$p80SccConvergenceMaxDeviationIter > cmSccConvergence |
                                 .data$p80GmtConvIter >  cmTempConvergence, "no", "yes"),
          "tooltip" = ifelse(.data$converged == "no", paste0("Iteration: ", .data$iteration, "<br>","Not converged"), paste0("Iteration: ", .data$iteration, "<br>","Converged"))
        )

      damageInternalization <- suppressWarnings(ggplot(data, aes_(
        x = ~iteration, y = "Damage\nInternalization",
        fill = ~converged, text = ~tooltip
      ))) +        theme_minimal() +
        geom_point(size = 2, alpha = aestethics$alpha) +
        scale_fill_manual(values = booleanColor) +
        scale_y_discrete(breaks = c("Damage\nInternalization"), drop = FALSE) +
        labs(x = NULL, y = NULL) +
        theme(axis.text.y = element_text(colour = ifelse(("damage" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

      damageInternalizationPlotly <- ggplotly(damageInternalization, tooltip = c("text"))
      subplots <- append(subplots, list(damageInternalizationPlotly))
    }

    # Tax Convergence (optional) ----

    cmTaxConvCheck <- as.vector(readGDX(gdx, name = "cm_TaxConvCheck", react = "error"))

    p80ConvNashTaxrevIter <- readGDX(gdx, name = "p80_convNashTaxrev_iter", restore_zeros = FALSE, react = "error") %>%
      as.quitte() %>%
      select("region", "period", "iteration", "value") %>%
      mutate("failed" = abs(.data$value) > taxConvTol)

    data <- p80ConvNashTaxrevIter %>%
      group_by(.data$iteration) %>%
      summarise(converged = ifelse(any(.data$failed == TRUE), "no", "yes")) %>%
      mutate("tooltip" = ifelse(.data$converged == "yes", paste0("Iteration: ", .data$iteration, "<br>","Converged"), paste0("Iteration: ", .data$iteration, "<br>","Not converged")))

    for (i in unique(p80ConvNashTaxrevIter$iteration)) {
      if (data[data$iteration == i, "converged"] == "no") {
        failedIter <- filter(p80ConvNashTaxrevIter, .data$iteration == i, .data$failed == TRUE)
        worstRow <- failedIter[which.max(abs(failedIter$value)), ]
        worstStr <- paste0(
          "<br>Worst: ", worstRow$region, " ", worstRow$period, " | ",
          round(100 * worstRow$value, 3), "% of GDP (limit +-", round(100 * taxConvTol, 2), "%)"
        )
        tmp <- failedIter %>%
          mutate("item" = paste0(.data$region, " ", .data$period)) %>%
          select("region", "period", "item") %>%
          distinct()

        if (nrow(tmp) > 10) {
          data[data$iteration == i, "tooltip"] <- paste0(
            "Iteration ", i, "<br>",
            "Not converged:", worstStr, "<br>",
            paste0(unique(tmp$region), collapse = ", "),
            "<br>",
            paste0(unique(tmp$period), collapse = ", ")
          )
        } else {
          data[data$iteration == i, "tooltip"] <- paste0(
            "Iteration ", i, "<br>",
            "Not converged:", worstStr, "<br>",
            paste0(unique(tmp$item), collapse = ", ")
          )
        }
      }
    }

    yLabel <- ifelse(cmTaxConvCheck == 0, "Tax Convergence\n(inactive)", "Tax Convergence")

    taxConvergence <- suppressWarnings(ggplot(data, aes_(
      x = ~iteration, y = yLabel,
      fill = ~converged, text = ~tooltip
    ))) +      theme_minimal() +
      geom_point(size = 2, alpha = aestethics$alpha) +
      scale_fill_manual(values = booleanColor) +
      scale_y_discrete(breaks = c(yLabel), drop = FALSE) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_text(colour = ifelse(("taxconv" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    taxConvergencePlotly <- ggplotly(taxConvergence, tooltip = c("text"))
    subplots <- append(subplots, list(taxConvergencePlotly))

    # Price anticipation (optional) ----

    cmMaxFadeoutPriceAnticip <- as.vector(readGDX(gdx, name = "cm_maxFadeoutPriceAnticip", react = "error"))
    p80FadeoutPriceAnticipIter <- readGDX(gdx, name = "p80_fadeoutPriceAnticip_iter",
                                          restore_zeros = FALSE, react = "error") %>%
      as.quitte() %>%
      select("iteration", "fadeoutPriceAnticip" = "value")

    data <- p80FadeoutPriceAnticipIter %>%
      mutate(
        "iteration" := as.numeric(as.character(.data$iteration)),
        "converged" = ifelse(.data$fadeoutPriceAnticip > cmMaxFadeoutPriceAnticip, "no", "yes"),
        "tooltip" = ifelse(
          .data$converged == "yes",
          paste0(
            "Iteration: ", .data$iteration, "<br>",
            "Converged<br>Price Anticipation fade out is low enough<br>",
            round(.data$fadeoutPriceAnticip, 5), " <= ", cmMaxFadeoutPriceAnticip
          ),
          paste0(
            "Iteration: ", .data$iteration, "<br>",
            "Not converged<br>Price Anticipation fade out is not low enough<br>",
            round(.data$fadeoutPriceAnticip, 5), " > ", cmMaxFadeoutPriceAnticip
          )
        )
      )

    iterationsXaxis <- unique(c(1,data$iteration[(data$iteration %% 5) == 0],max(data$iteration)))
    iterationsXaxis <- iterationsXaxis[iterationsXaxis != max(data$iteration)-1]

    priceAnticipation <- ggplot(data, aes_(x = ~iteration)) +
      geom_line(aes_(y = ~fadeoutPriceAnticip), alpha = 0.3, linewidth = aestethics$line$size) +
      suppressWarnings(geom_point(
        size = 2,
        aes_(y = 0.0001, fill = ~converged, text = ~tooltip),
        alpha = aestethics$alpha
      )) +
      theme_minimal() +
      scale_fill_manual(values = booleanColor) +
      scale_y_continuous(breaks = c(0.0001), labels = c("Price\nAnticipation (inactive)")) +
      scale_x_continuous(breaks = iterationsXaxis) +
      labs(x = NULL, y = NULL) +
      coord_cartesian(ylim = c(-0.2, 1)) +
      theme(axis.text.y = element_text(colour = ifelse(("anticip" %in% activeCriteria), activeCriteriaColor["true"], activeCriteriaColor["false"])))

    priceAnticipationPlotly <- ggplotly(priceAnticipation, tooltip = c("text"))
    subplots <- append(subplots, list(priceAnticipationPlotly))

    # Summary plot ----

    out <- list()

    out$tradeDetailPlot <- surplusConvergencePlotly

    n <- length(subplots)
    out$plot <- subplot(
      subplots,
      nrows = n,
      heights = c(2 / (n + 1), rep(1 / (n + 1), n - 1)),
      shareX = TRUE,
      titleX = FALSE
    ) %>%
      hide_legend() %>%
      config(displayModeBar = FALSE, displaylogo = FALSE)

    return(out)
  }

  if (!file.exists(gdx)) {
    warning("gdx file not found!")
    return(list())
  }

  modelstat <- readGDX(gdx, name = "o_modelstat", react = "error")[[1]]

  if (!(modelstat %in% c(1, 2, 3, 4, 5, 6, 7))) {
    warning("Run failed - Check code, pre-triangular infes ...")
    return(list())
  }

  tryCatch(
    expr = {
      return(.generatePlots(gdx))
    },
    error = function(e) {
      if (e$message == "No corresponding object found in the GDX!") {
        message("This function does not support runs before version 3.2.1.dev333")
      }
      warning(e)
      return(list())
    }
  )

}
