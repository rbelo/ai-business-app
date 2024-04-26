roc.plot <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0,
                     profit.constraints = c("Unconstrained",
                                            "Independence (PP)",
                                            "Separation (TPR)",
                                            "Separation (FPR)",
                                            "Sufficiency (PPV)",
                                            "Sufficiency (NPV)",
                                            "Unaware")
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
                        )
  roc.plot.from.roc(dt.roc, profit.constraints)
}

roc.plot.from.roc <- function(dt.roc, profit.constraints) {

  dt.roc.merged <- fairness.metrics(dt.roc)

  ggplot(dt.roc) +
    geom_line(aes(x=FPR, y= TPR),  linetype = "dashed", color="gray",
              data = data.table(FPR = c(0, 1), TPR = c(0, 1))) +
    geom_line(aes(FPR, TPR, color=partition)) +
    geom_point(aes(fpr_a, tpr_a, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(fpr_b, tpr_b, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(FPR, TPR, color=partition, fill=partition), size=2, shape = 23,
               data = dt.roc[Profit == max_profit, .SD[1], by=partition]) +
    theme_bw()
}

cum.resp.plot <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0,
                     profit.constraints = c("Unconstrained",
                                            "Independence (PP)",
                                            "Separation (TPR)",
                                            "Separation (FPR)",
                                            "Sufficiency (PPV)",
                                            "Sufficiency (NPV)",
                                            "Unaware")
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
                        )
  cum.resp.plot.from.roc(dt.roc, profit.constraints)
}

cum.resp.plot.from.roc <- function(dt.roc, profit.constraints) {

  dt.roc.merged <- fairness.metrics(dt.roc)

  ggplot(dt.roc) +
    geom_line(aes(x=p_targeted, y= TPR),  linetype = "dashed", color="gray",
              data = data.table(p_targeted = c(0, 1), TPR = c(0, 1))) +
    geom_line(aes(p_targeted, TPR, color=partition)) +
    geom_point(aes(p_targeted_a, tpr_a, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted_b, tpr_b, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted, TPR, color=partition, fill=partition), size=2, shape = 23,
               data = dt.roc[Profit == max_profit, .SD[1], by=partition]) +
    theme_bw()
}

lift.curve.plot <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0,
                     profit.constraints = c("Unconstrained",
                                            "Independence (PP)",
                                            "Separation (TPR)",
                                            "Separation (FPR)",
                                            "Sufficiency (PPV)",
                                            "Sufficiency (NPV)",
                                            "Unaware")
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
)
  lift.curve.plot.from.roc(dt.roc, profit.constraints)
}

lift.curve.plot.from.roc <- function(dt.roc, profit.constraints) {

  dt.roc.merged <- fairness.metrics(dt.roc)

  ggplot(dt.roc) +
    geom_hline(aes(yintercept = 1),  linetype = "dashed", color="gray") +
    geom_line(aes(p_targeted, Lift, color=partition)) +
    geom_point(aes(p_targeted_a, lift_a, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted_b, lift_b, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted, Lift, color=partition, fill=partition), size=2, shape = 23,
               data = dt.roc[Profit == max_profit, .SD[1], by=partition]) +
    theme_bw()
}

profit.plot <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0,
                     profit.constraints = c("Unconstrained",
                                            "Independence (PP)",
                                            "Separation (TPR)",
                                            "Separation (FPR)",
                                            "Sufficiency (PPV)",
                                            "Sufficiency (NPV)",
                                            "Unaware")
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
)

  profit.plot.from.roc(dt.roc, profit.constraints)
}

profit.plot.from.roc <- function(dt.roc, profit.constraints) {

  dt.roc.merged <- fairness.metrics(dt.roc)

  ggplot(dt.roc) +
    geom_line(aes(x=p_targeted, y= Profit),  linetype = "dashed", color="gray",
              data = data.table(p_targeted = c(0, 1),
                                Profit = c(dt.roc[partition == " All" & n_targeted == 1, Profit],
                                           dt.roc[partition == " All" & p_targeted == 1, Profit]))) +
    geom_line(aes(p_targeted, Profit, color=partition)) +
    geom_point(aes(p_targeted_a, profit_a, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted_b, profit_b, fill=optimal),  size=4, shape = 23,
               data = dt.roc.merged[!is.na(optimal) & optimal %in% profit.constraints, .SD[1], by=optimal]) +
    geom_point(aes(p_targeted, Profit, color=partition, fill=partition), size=2, shape = 23,
               data = dt.roc[Profit == max_profit, .SD[1], by=partition]) +
    theme_bw()
}

profit.uplift.plot <- function(dt.data,
                        susceptible.benefit = 1,
                        donotdisturb.benefit = -1,
                        surething.benefit = 0,
                        lostcause.benefit = 0,
                     profit.constraints = c("Unconstrained",
                                            "Independence (PP)",
                                            "Separation (TPR)",
                                            "Separation (FPR)",
                                            "Sufficiency (PPV)",
                                            "Sufficiency (NPV)",
                                            "Unaware")
                     ) {
  dt.roc.uplift <- uplift.metrics(dt.data,
                        susceptible.benefit = susceptible.benefit,
                        donotdisturb.benefit = donotdisturb.benefit,
                        surething.benefit = surething.benefit,
                        lostcause.benefit = lostcause.benefit
)

  profit.uplift.plot.from.roc(dt.roc.uplift, profit.constraints)
}

profit.uplift.plot.from.roc <- function(dt.roc.uplift, profit.constraints) {

  ggplot(dt.roc.uplift) +
    geom_line(aes(x=p_targeted, y= Profit),  linetype = "dashed", color="gray",
              data = data.table(p_targeted = c(0, 1),
                                Profit = c(dt.roc.uplift[partition == " All" & n_targeted == 1, Profit],
                                           dt.roc.uplift[partition == " All" & p_targeted == 1, Profit]))) +
    geom_line(aes(p_targeted, Profit, color=partition)) +
    geom_point(aes(p_targeted, Profit, color=partition, fill=partition), size=2, shape = 23,
               data = dt.roc.uplift[Profit == max_profit, .SD[1], by=partition]) +
    theme_bw()
}


fairness.stats <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
                        )
  dt.roc.merged <- fairness.metrics(dt.roc)

  dt.roc.merged[!is.na(optimal),
                .SD[1],
                by=optimal][order(-total_profit),
                            list(optimal, total_profit, n_targeted_a, n_targeted_b,
                                 p_targeted_a, p_targeted_b, tpr_a, tpr_b, fpr_a, fpr_b, ppv_a, ppv_b, npv_a, npv_b)]

}

roc.stats <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0,
                        fixed.cost = 0
                     ) {
  dt.roc <- roc.metrics(dt.data,
                        tp.benefit = tp.benefit,
                        fp.benefit = fp.benefit,
                        fn.benefit = fn.benefit,
                        tn.benefit = tn.benefit,
                        fixed.cost = fixed.cost
)
  dt.roc
}

server <- function(input, output, session) {
  observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['n']])) {
        updateTextInput(session, "n.obs", value = query[['n']])
      }
      if (!is.null(query[['ppos']])) {
        updateSliderInput(session, "p.outcome.group.a", value = query[['ppos']])
      }
      if (!is.null(query[['auc']])) {
        updateSliderInput(session, "auc.group.a", value = query[['auc']])
      }
      if (!is.null(query[['tp']])) {
        updateTextInput(session, "tp.benefit", value = query[['tp']])
      }
      if (!is.null(query[['fp']])) {
        updateTextInput(session, "fp.benefit", value = query[['fp']])
      }
      if (!is.null(query[['tn']])) {
        updateTextInput(session, "tn.benefit", value = query[['tn']])
      }
      if (!is.null(query[['fn']])) {
        updateTextInput(session, "fn.benefit", value = query[['fn']])
      }
      if (!is.null(query[['fixedcost']])) {
        updateTextInput(session, "fixed.cost", value = query[['fixedcost']])
      }
    })
 dt.data <- data.table()
 get.data <- reactive({
    inFile <- input$target_upload
    if (input$typeofdata == "load.data" & is.null(inFile)) {
    return(dt.data)
    }
    if (input$typeofdata == "generate.data" ) {
      dt.data <<- generate.data(n.obs = input$n.obs,
                        p.group.b = (input$p.group.b / 100),
                        p.treated = (input$p.treated / 100),
                        p.outcome.group.a = (input$p.outcome.group.a / 100),
                        p.outcome.group.b = (input$p.outcome.group.b / 100),
                        auc.group.a = input$auc.group.a,
                        auc.group.b = input$auc.group.b,
                        ate.group.a = input$ate,
                        ate.group.b = input$ate,
                        auc.uplift = input$auc.uplift,
                        corr.uplift.outcome = input$corr.uplift.outcome
                                           )
    } else {
      dt.data <<- fread(inFile$datapath)
    }
    return(dt.data)
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(paste(as_datetime(now()), "generated_data",
            input$n.obs, input$p.group.b,
            input$p.outcome.group.a, input$p.outcome.group.b,
            input$auc.group.a, input$auc.group.b,
            sep = "-"),
            ".csv")
    },
    content = function(file) {
      fwrite(get.data(), file, sep=";")
    }
  )
  output$data.info <- renderUI({
    dt.data <<- get.data()
    dt.roc <- roc.metrics(dt.data)
    dt.auc <- dt.roc[, list(TPR, d_FPR = FPR - shift(FPR)),
                     by=partition][, list(AUC = round(sum(TPR * d_FPR, na.rm=TRUE), 3)),
                                   by=list(Group = partition)]
    fluidPage(
      h4("Statistics:"),
      renderTable({merge(dt.roc[, list(.N,
                                  Positives = Positives[1],
                                  "% Positives" = Positives[1] / .N),
                                by=list(Group = partition)],
                         dt.auc, by="Group")[order(Group)]}),
     conditionalPanel(condition = "input.fairnessanalysis == ''",
      renderPlotly({
        ggplot(cbind(dt.data,
                     dt.data[, list(Outcome = ifelse(outcome == 1, "Positive", "Negative"))])) +
                     geom_histogram(
                                    aes(score, fill=Outcome),
                                    alpha = 0.5, position = "identity", bins = 50) +
                      scale_fill_manual(values = c("Positive" = "green",
                                                   "Negative" = "red")) +
          xlab("Score")
      }),
      ), # conditional panel
     conditionalPanel(condition = "input.fairnessanalysis != ''",
      renderPlotly({
        dt.tmp <- cbind(dt.data,
                     dt.data[, list(Outcome = ifelse(group == "A",
                                           ifelse(outcome == 1, "Positive / Group A", "Negative / Group A"),
                                           ifelse(outcome == 1, "Positive / Group B", "Negative / Group B")))])
        ggplot() +
                     geom_histogram(data = dt.tmp[outcome == 0],
                                    aes(score, fill=Outcome),
                                    alpha = 0.5, bins = 50) +
                     geom_histogram(data = dt.tmp[outcome == 1],
                                    aes(score, fill=Outcome),
                                    alpha = 0.5, bins = 50) +
                      scale_fill_manual(values = c("Positive / Group A" = "green",
                                                   "Negative / Group A" = "red",
                                                   "Positive / Group B" = "darkgreen",
                                                   "Negative / Group B" = "darkred")) +
          xlab("Score")
      }),
      ), # conditional panel
     conditionalPanel(condition = "input.upliftanalysis != ''",
      renderPlotly({
        ggplot(dt.data[order(complier_status)]) +
                     geom_histogram(data=,
                                    aes(score_uplift, fill=complier_status),
                                    alpha = 0.5, position = "identity", bins = 50) +
                      scale_fill_manual(values = c("Do Not Disturb" = "red",
                                                   "Lost Cause" = "gray",
                                                   "Sure Thing" = "lightblue",
                                                   "Susceptible" = "green")) +
          xlab("Score Uplift")
        }),
#      DT::datatable(dt.data, filter = "top", options = list(pageLength = 20)))
      renderTable({dt.data[order(complier_status), list(.N), by=list(Type = complier_status)]})))

  })
  output$roc.plot <-
        renderPlotly({roc.plot(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        ## fn.benefit = input$fn.benefit,
                        ## tn.benefit = input$tn.benefit,
                        profit.constraints = input$profit.constraints
                        )})
  output$cum.resp.plot <-
        renderPlotly({cum.resp.plot(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        ## fn.benefit = input$fn.benefit,
                        ## tn.benefit = input$tn.benefit,
                        profit.constraints = input$profit.constraints
                        )})
  output$lift.curve.plot <-
        renderPlotly({lift.curve.plot(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        ## fn.benefit = input$fn.benefit,
                        ## tn.benefit = input$tn.benefit,
                        profit.constraints = input$profit.constraints
                        )})
  output$profit.plot <-
    renderUI({
      fluidPage(
    conditionalPanel(condition = "input.upliftanalysis == ''",
        renderPlotly({ggplotly(profit.plot(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        fn.benefit = input$fn.benefit,
                        tn.benefit = input$tn.benefit,
                        fixed.cost = input$fixed.cost,
                        profit.constraints = input$profit.constraints
                        ), width=800, height=500)}),
       ),
     conditionalPanel(condition = "input.upliftanalysis != ''",
        renderPlotly({ggplotly(profit.uplift.plot(get.data(),
                        susceptible.benefit = input$susceptible.benefit,
                        donotdisturb.benefit = input$donotdisturb.benefit,
                        surething.benefit = input$surething.benefit,
                        lostcause.benefit = input$lostcause.benefit
                        ), width=800, height=500)})
        )
      )
    })
  output$fairness.stats <- renderUI({
        DT::datatable(fairness.stats(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        fn.benefit = input$fn.benefit,
                        tn.benefit = input$tn.benefit,
                        fixed.cost = input$fixed.cost
))})
  output$roc.stats <- renderUI({
    DT::datatable(roc.stats(get.data(),
                        tp.benefit = input$tp.benefit,
                        fp.benefit = input$fp.benefit,
                        fn.benefit = input$fn.benefit,
                        tn.benefit = input$tn.benefit,
                        fixed.cost = input$fixed.cost
))})

}

server
