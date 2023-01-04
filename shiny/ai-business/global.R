library(data.table)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(MASS)

min_max_norm <- function(x, range = c(0,1)) {
    (x - min(x)) / (max(x) - min(x)) * (max(range) - min(range)) + min(range)
  }

generate.data <- function(n.obs = 10000,
                          p.group.b = 0.5,
                          p.treated = 0.5,
                          p.outcome.group.a = 0.7,
                          p.outcome.group.b = 0.2,
                          disc.power.group.a = 0.5,
                          disc.power.group.b = 0.5,
                          ate.group.a = 1,
                          ate.group.b = 0,
                          disc.power.uplift = 0.5,
                          corr.uplift.outcome = 0
                         ) {

    # RB: TODO: Consider being able to control the variance of the uplift
    dt.errors <- data.table(mvrnorm(n.obs, mu=c(0,0),
                                    Sigma = matrix(c(1, corr.uplift.outcome,
                                                     corr.uplift.outcome, 1), ncol=2)))
    setnames(dt.errors, names(dt.errors), c("propensity_control", "uplift"))
    dt.data <- data.table(group_a = as.integer(runif(n.obs) >= p.group.b),
                          treated = as.integer(runif(n.obs) < p.treated))
    dt.data <- cbind(dt.data, round(dt.errors, 3))
    dt.data[, group := ifelse(group_a, "A", "B")]

    dt.data <- dt.data[order(-propensity_control)]
    dt.data[, propensity_control_quantile_by_group := (1:.N)/.N, by = group]
    dt.data[group_a == TRUE,  outcome_control := as.integer(propensity_control_quantile_by_group <=
                                                             p.outcome.group.a - ate.group.a * p.treated)]
    dt.data[group_a == FALSE, outcome_control := as.integer(propensity_control_quantile_by_group <=
                                                             p.outcome.group.b - ate.group.a * p.treated)]

    dt.data[, propensity_treated := propensity_control + uplift]
    dt.data <- dt.data[order(-propensity_treated)]
    dt.data[, propensity_treated_quantile_by_group := (1:.N)/.N, by = group]
    dt.data[group_a == TRUE,  outcome_treated := as.integer(propensity_treated_quantile_by_group <=
                                                            p.outcome.group.a + ate.group.a * (1 - p.treated))]
    dt.data[group_a == FALSE, outcome_treated := as.integer(propensity_treated_quantile_by_group <=
                                                            p.outcome.group.b + ate.group.a * (1 - p.treated))]

    dt.data[, propensity := propensity_control + treated * uplift]
    dt.data <- dt.data[order(-propensity)]
    dt.data[, propensity_quantile_by_group := (1:.N)/.N, by = group]
    dt.data[group_a == TRUE,  outcome := as.integer(propensity_quantile_by_group <= p.outcome.group.a )]
    dt.data[group_a == FALSE, outcome := as.integer(propensity_quantile_by_group <= p.outcome.group.b )]

    dt.data[outcome_control == 1 & outcome_treated == 1 , complier_status := "Sure Thing"]
    dt.data[outcome_control == 0 & outcome_treated == 1 , complier_status := "Susceptible"]
    dt.data[outcome_control == 1 & outcome_treated == 0 , complier_status := "Do Not Disturb"]
    dt.data[outcome_control == 0 & outcome_treated == 0 , complier_status := "Lost Cause"]

    dt.data[group_a == TRUE,  score := rnorm(n = .N, mean = outcome * disc.power.group.a^3 * 5)]
    dt.data[group_a == FALSE, score := rnorm(n = .N, mean = outcome * disc.power.group.b^3 * 5)]
    dt.data[, score := round(min_max_norm(score), 3)]

    dt.data[complier_status == "Sure Thing",     score_uplift := rnorm(.N, mean = 0)]
    dt.data[complier_status == "Lost Cause",     score_uplift := rnorm(.N, mean = 0)]
    dt.data[complier_status == "Susceptible",    score_uplift := rnorm(.N, mean =   disc.power.uplift^3 * 5)]
    dt.data[complier_status == "Do Not Disturb", score_uplift := rnorm(.N, mean = - disc.power.uplift^3 * 5)]
    dt.data[, score_uplift := round(min_max_norm(score_uplift, range=c(-1,1)), 3)]

    dt.data <- dt.data[order(-score)]
    dt.data[, list(score, outcome, group, treated, score_uplift, complier_status)]
    ## dt.data[, list(group, treated, score, score_uplift, outcome, unobs_propensity = propensity, unobs_uplift = uplift, unobs_score_error = score_error, unobs_complier_status = complier_status)]
}

roc.metrics <- function(dt.data,
                        tp.benefit = 1,
                        fp.benefit = -1,
                        fn.benefit = 0,
                        tn.benefit = 0
                        ) {
    ## Calculate threshold statistics
    dt.roc <- rbind(data.table(partition = " All", dt.data[, list(outcome, score)]),
                     data.table(partition = dt.data[, group], dt.data[, list(outcome, score)]))
    dt.roc <- dt.roc[order(-score)]
    dt.roc[, one := 1]
    dt.roc[, score_calibrated := cumsum(outcome)/cumsum(one), by=partition]
    dt.roc <- dt.roc[order(-score_calibrated)]
    dt.roc[, Positives := sum(outcome == 1), by = partition]
    dt.roc[, Negatives := sum(outcome == 0), by = partition]
    dt.roc[, TP := cumsum(outcome == 1), by = partition]
    dt.roc[, FP := cumsum(outcome == 0), by = partition]
    dt.roc[, FN := Positives - TP, by = partition]
    dt.roc[, TN := Negatives - FP, by = partition]
    dt.roc[, PP := TP + FP, by = partition]
    dt.roc[, PN := FN + TN, by = partition]

    dt.roc[, TPR := TP / Positives, by = partition]
    dt.roc[, FPR := FP / Negatives, by = partition]
    dt.roc[, PPV := TP / PP, by = partition]
    dt.roc[, NPV := TN / PN, by = partition]
    dt.roc[, n_targeted := (1:.N), by = partition]
    dt.roc[, p_targeted := (1:.N)/.N, by = partition]

    dt.roc[, Lift := (TP / n_targeted) / (Positives / .N), by = partition]

    dt.roc[, Profit := TP * tp.benefit + FP * fp.benefit +
                       FN * fn.benefit + TN * tn.benefit, by = partition]
    dt.roc[, max_profit := max(Profit), by = partition]
    dt.roc[, max_total_profit := max(Profit[partition == "A"]) + max(Profit[partition == "B"])]
    dt.roc[, max_total_profit_unaware := max_profit[partition == " All"][1]]
    return(dt.roc)
}

uplift.metrics <- function(dt.data,
                           susceptible.benefit = 1,
                           donotdisturb.benefit = -1,
                           surething.benefit = 0,
                           lostcause.benefit = 0
                           ) {
    ## Calculate threshold statistics
    dt.roc.uplift <- rbind(data.table(partition = " All", dt.data[, list(complier_status, score_uplift)]),
                     data.table(partition = dt.data[, group], dt.data[, list(complier_status, score_uplift)]))
    dt.roc.uplift <- dt.roc.uplift[order(-score_uplift)]

    dt.roc.uplift[, TotalSusceptibles  := sum(complier_status == "Susceptible"), by = partition]
    dt.roc.uplift[, TotalDoNotDisturbs := sum(complier_status == "Do Not Disturb"), by = partition]
    dt.roc.uplift[, TotalSureThings    := sum(complier_status == "Sure Thing"), by = partition]
    dt.roc.uplift[, TotalLostCauses    := sum(complier_status == "Lost Cause"), by = partition]

    dt.roc.uplift[, Susceptibles       := cumsum(complier_status == "Susceptible"), by = partition]
    dt.roc.uplift[, DoNotDisturbs      := cumsum(complier_status == "Do Not Disturb"), by = partition]
    dt.roc.uplift[, SureThings         := cumsum(complier_status == "Sure Thing"), by = partition]
    dt.roc.uplift[, LostCauses         := cumsum(complier_status == "Lost Cause"), by = partition]

    dt.roc.uplift[, SR := Susceptibles / TotalSusceptibles, by = partition]
    dt.roc.uplift[, DNDR := DoNotDisturbs / TotalDoNotDisturbs, by = partition]
    dt.roc.uplift[, STR := SureThings / TotalSureThings, by = partition]
    dt.roc.uplift[, LCR := LostCauses / TotalLostCauses, by = partition]
    dt.roc.uplift[, n_targeted := (1:.N), by = partition]
    dt.roc.uplift[, p_targeted := (1:.N)/.N, by = partition]

    dt.roc.uplift[, Profit :=
                      Susceptibles * susceptible.benefit +
                      DoNotDisturbs * donotdisturb.benefit +
                      SureThings * surething.benefit +
                      LostCauses * lostcause.benefit, by = partition]
    dt.roc.uplift[, max_profit := max(Profit), by = partition]
    dt.roc.uplift[, max_total_profit := max(Profit[partition == "A"]) + max(Profit[partition == "B"])]
    dt.roc.uplift[, max_total_profit_unaware := max_profit[partition == " All"][1]]
    return(dt.roc.uplift)

}

fairness.metrics <- function(dt.roc) {
    dt.roc.merged <- CJ(p_targeted_a = dt.roc[partition == "A", unique(round(p_targeted, 2))],
                        p_targeted_b = dt.roc[partition == "B", unique(round(p_targeted, 2))])
    dt.roc.merged <- merge(dt.roc.merged,
                       dt.roc[partition == "B",
                              list(n_targeted_b = n_targeted[Profit == max(Profit)][1],
                                   tpr_b = round(TPR[Profit == max(Profit)][1], 2),
                                   fpr_b = round(FPR[Profit == max(Profit)][1], 2),
                                   ppv_b = round(PPV[Profit == max(Profit)][1], 2),
                                   npv_b = round(NPV[Profit == max(Profit)][1], 2),
                                   lift_b = Lift[Profit == max(Profit)][1],
                                   profit_b = max(Profit)),
                              by=list(p_targeted_b = round(p_targeted, 2))],
                       by = "p_targeted_b")
    dt.roc.merged <- merge(dt.roc.merged,
                       dt.roc[partition == "A",
                              list(n_targeted_a = n_targeted[Profit == max(Profit)][1],
                                   tpr_a = round(TPR[Profit == max(Profit)][1], 2),
                                   fpr_a = round(FPR[Profit == max(Profit)][1], 2),
                                   ppv_a = round(PPV[Profit == max(Profit)][1], 2),
                                   npv_a = round(NPV[Profit == max(Profit)][1], 2),
                                   lift_a = Lift[Profit == max(Profit)][1],
                                   profit_a = max(Profit)),
                              by=list(p_targeted_a = round(p_targeted, 2))],
                       by = "p_targeted_a")

    ## dt.roc.merged[, p_targeted_all := round(( n_targeted_a + n_targeted_b )  / max(n_targeted_a + n_targeted_b), 2)]
    ## dt.roc.merged <- merge(dt.roc.merged,
    ##                    dt.roc[partition == " All",
    ##                           list(n_targeted_all = n_targeted[Profit == max(Profit)][1],
    ##                                tpr_all = round(TPR[Profit == max(Profit)][1], 2),
    ##                                fpr_all = round(FPR[Profit == max(Profit)][1], 2),
    ##                                ppv_all = round(PPV[Profit == max(Profit)][1], 2),
    ##                                npv_all = round(NPV[Profit == max(Profit)][1], 2),
    ##                                profit_all = max(Profit)),
    ##                           by=list(p_targeted_all = round(p_targeted, 2))],
    ##                    by = "p_targeted_all", all.x = TRUE)

    dt.roc.merged[, total_profit := profit_a + profit_b]
    dt.roc.merged[, max_total_profit := max(total_profit)]
    dt.roc.merged[, max_total_profit_unaware := dt.roc[1, max_total_profit_unaware]]
    ## dt.roc.merged[, p_targeted_unaware := dt.roc[partition == " All", p_targeted[Profit == max_total_profit_unaware][1]]]
    dt.roc.merged[, max_total_profit_indep := max(total_profit[p_targeted_b == p_targeted_a])]
    # calculate minimum differences because for some metrics it may not be possible to get exactly the same value
    dt.roc.merged[, min_diff_tpr := min(abs(tpr_b - tpr_a))]
    dt.roc.merged[, min_diff_fpr := min(abs(fpr_b - fpr_a))]
    dt.roc.merged[, min_diff_ppv := min(abs(ppv_b - ppv_a))]
    dt.roc.merged[, min_diff_npv := min(abs(npv_b - npv_a))]
    dt.roc.merged[, max_total_profit_sep_tpr := max(total_profit[abs(tpr_b - tpr_a) == min_diff_tpr])]
    dt.roc.merged[, max_total_profit_sep_fpr := max(total_profit[abs(fpr_b - fpr_a) == min_diff_fpr])]
    dt.roc.merged[, max_total_profit_suf_ppv := max(total_profit[abs(ppv_b - ppv_a) == min_diff_ppv])]
    dt.roc.merged[, max_total_profit_suf_npv := max(total_profit[abs(npv_b - npv_a) == min_diff_npv])]

    dt.roc.merged[total_profit == max_total_profit, optimal := "Unconstrained"]
    dt.roc[partition == " All" & Profit == max_total_profit_unaware]
    dt.roc.merged[total_profit == max_total_profit_unaware, optimal := "Unaware"]
    dt.roc.merged[total_profit == max_total_profit_indep & p_targeted_a == p_targeted_b,         optimal := "Independence (PP)"]
    dt.roc.merged[total_profit == max_total_profit_sep_tpr & abs(tpr_b - tpr_a) == min_diff_tpr, optimal := "Separation (TPR)"]
    dt.roc.merged[total_profit == max_total_profit_sep_fpr & abs(fpr_b - fpr_a) == min_diff_fpr, optimal := "Separation (FPR)"]
    dt.roc.merged[total_profit == max_total_profit_suf_ppv & abs(ppv_b - ppv_a) == min_diff_ppv, optimal := "Sufficiency (PPV)"]
    dt.roc.merged[total_profit == max_total_profit_suf_npv & abs(npv_b - npv_a) == min_diff_npv, optimal := "Sufficiency (NPV)"]
    dt.roc.merged
}
