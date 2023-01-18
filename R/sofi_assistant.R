library(iosoi)
require(tidyverse)


lst_of_ref_sol_crit <- c("Lower bound",
                         "Upper bound",
                         "Equal weights evaluation")

lst_of_ref_sol_crit_abbrv <- c("LB", "UB","EW")

lst_attitudes <- c("Neutral attitude",
                   "Optimistic attitude",
                   "Pessimistic attitude")

lst_element_to_be_ordered_by <- c("Lower bound",
                   "Upper bound",
                   "Equal weights evaluation",
                   "Neutral attitude",
                   "Optimistic attitude",
                   "Pessimistic attitude",
                   "Solution")

lst_data_to_export <- c("Input data (current version)",
                        "Results (all solutions)",
                        "Results (only solutions of interest)",
                        "Plots (only solutions of interest)")


# Retorna un dataframe con los indicadores del enfoque para evaluar
# a cada solución

compute_extreme_weights <- function(n_crit){
  m_extpoints <- matrix(1/c(n_crit:1), n_crit, n_crit)
  m_extpoints[lower.tri(m_extpoints)] <- 0
  m_extpoints <- t(m_extpoints)
  return(m_extpoints)
}



compute_results <- function(criteria, evaluations){

  dfr_criteria <- criteria %>%
    mutate(`Short name` = factor(`Short name`, levels = criteria$`Short name`[order(criteria$Importance)])) %>%
    rename("Criterion" = `Short name`, "LB" = `Domain min`, "UB" = `Domain max`)

  n_crit <- nrow(dfr_criteria)

  dfr_evaluations <- evaluations %>%
    gather(key = "Criterion", value = "Evaluation", -Solution) %>%
    inner_join(dfr_criteria, by="Criterion") %>%
    mutate(Norm.Eval = ifelse(
      Goal == "min",
      1-(Evaluation - LB)/(UB - LB),
      (Evaluation - LB)/(UB - LB))) %>%
    select(Solution, Criterion, Norm.Eval) %>%
    spread(key="Criterion", value="Norm.Eval")

  m_evaluations <- as.matrix(dfr_evaluations[,-1][,sort(dfr_criteria$Criterion)])

  colnames(m_evaluations) <- sort(dfr_criteria$Criterion)
  rownames(m_evaluations) <- dfr_evaluations$Solution

  m_extpoints <- iosoi::generate_polyhedron_vertices(ncrit = n_crit)

  vert_scores <- iosoi::score(eval_matrix = m_evaluations,
                              vert_matrix = m_extpoints,
                              append_output = FALSE)

  equal_weight_scores <- vert_scores[, n_crit]

  dfr_computed_results <- vert_scores %>%
    iosoi::intervals(append_output = FALSE) %>%
    iosoi::reference() %>%
    iosoi::poss_assess(
      by = "all"
    ) %>%
    as_tibble(rownames = "Solution") %>%
    mutate(
      EW = equal_weight_scores,
      REF = (REF == 1)
    ) %>%
    select(c("Solution", "REF", "LB", "UB", "EW",
             "optimistic", "neutral", "pessimistic"
             )) %>%
    rename(
      "Lower bound" = LB,
      "Upper bound" = UB,
      "Equal weights evaluation" = EW,
      "Pessimistic attitude" = pessimistic,
      "Optimistic attitude" = optimistic,
      "Neutral attitude" = neutral,
      "Ref. solution" = REF
    ) %>%
    arrange(desc(`Lower bound`))


  # if(ref_sol_goal == "max"){
  #   dfr_computed_results <- dfr_computed_results %>%
  #     arrange(desc(!!sym(ref_sol_criteria)))
  # } else{
  #   dfr_computed_results <- dfr_computed_results %>%
  #     arrange(!!sym(ref_sol_criteria))
  # }


  return(dfr_computed_results)

}



ggplot_intervals <- function(app_input, results){

  ordered_element <- app_input$sel_element_to_be_ordered_by
  descending <- app_input$cbx_descending
  flip_coords <- app_input$cbx_flip_coords
  rotate_x_axis_label <- app_input$cbx_rotate_x
  rotate_y_axis_label <- app_input$cbx_rotate_y

  # rs_goal <- app_input$sel_rs_goal
  rs_crit <- app_input$sel_rs_criteria

  ref_sol_idx <- which.max(results$`Ref. solution`)

  if(descending){
    dfr_alt <- results %>%
      arrange(desc(!!sym(ordered_element)))
  }else{
    dfr_alt <- results %>%
      arrange(!!sym(ordered_element))
  }
  dfr_alt <- dfr_alt %>% select(Solution)

  dfr_results <- results %>%
    mutate(`Type` = ifelse(`Ref. solution`, "Reference solution", "Others"),
           Solution = factor(Solution, levels = dfr_alt$Solution)
    )

  p <- dfr_results %>%
    ggplot(aes(x=Solution,
               y=`Equal weights evaluation`,
               ymin=`Lower bound`,
               ymax=`Upper bound`,
               color=`Type`)) +
    geom_pointrange() +
    geom_point(shape=21, fill="white") +
    geom_hline(yintercept = dfr_results$`Lower bound`[ref_sol_idx], linetype="dotted") +
    ylab("Aggregate evaluation") +
    xlab("Solutions") +
    theme_classic()

  if(rotate_x_axis_label){
    p <- p + theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  }

  if(rotate_y_axis_label){
    p <- p + theme(
      axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)
    )
  }

  if(flip_coords){
    p <- p + coord_flip()
  }
  p
}





