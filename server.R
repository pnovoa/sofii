#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sortable)
library(openxlsx)


function(input, output, session) {


  app_data <- reactiveValues(dfr_criteria = NULL,
                             dfr_solutions = NULL,
                             dfr_evaluations = NULL,
                             dfr_results = NULL,
                             dfr_results_all = NULL,
                             lst_weights = NULL
  )

  data_is_loaded <- function(){
    return(!is.null(app_data$dfr_criteria))
  }

  result_is_computed <- function(){
    return(!is.null(app_data$dfr_results))
  }


  compute_results_ <- function(){

    #Data required for computing results

    ref_sol_goal <- input$sel_rs_goal
    ref_sol_criteria <- input$sel_rs_criteria

    app_data$dfr_results_all <- compute_results(
      criteria=app_data$dfr_criteria,
      evaluations=app_data$dfr_evaluations #,
      # ref_sol_goal=ref_sol_goal,
      # ref_sol_criteria=ref_sol_criteria
    )

    app_data$dfr_results <- app_data$dfr_results_all #%>% select(-REF)

    print(app_data$dfr_results)
  }



  observeEvent(input$file_loaded,{

    file <- input$file_loaded
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))

    app_data$dfr_criteria <- readxl::read_excel(path = file$datapath, sheet = "criteria")
    app_data$dfr_criteria <- app_data$dfr_criteria %>% mutate(Goal = factor(Goal, levels = c("min", "max")))
    app_data$dfr_solutions <- readxl::read_excel(path = file$datapath, sheet = "solutions")
    app_data$dfr_evaluations <- readxl::read_excel(path = file$datapath, sheet = "evaluations")
    app_data$n_solutions <- nrow(app_data$dfr_solutions)
    #app_data$df_comp_orig <- app_data$df_comp
    compute_results_()


    #app_data$criteria_val_lbl <- setNames(app_data$df_criteria$`Short name`, app_data$df_criteria$`Long name`)
    #app_data$criteria_lbl_val <- setNames(app_data$df_criteria$`Long name`, app_data$df_criteria$`Short name`)

  })


  # Cajas de valor para el resumn (Data->Summary)


  output$vbx_criteria <- renderValueBox({
    n_ <- nrow(app_data$dfr_criteria)
    valueBox(n_,
             "Criteria", icon = icon("rectangle-list"), color = "blue")

  })

  output$vbx_solutions <- renderValueBox({
    n_ <- nrow(app_data$dfr_solutions)
    valueBox(n_,
             "Solutions", icon = icon("rectangle-list"), color = "yellow")
  })

  output$vbx_evaluations <- renderValueBox({
    n_ <- nrow(app_data$dfr_evaluations)*(ncol(app_data$dfr_evaluations)-1)
    valueBox(n_,
             "Evaluations", icon = icon("rectangle-list"), color = "green")
  })



  #Tablas de entrada de datos (Data)

  output$dtb_criteria <- DT::renderDT(app_data$dfr_criteria, selection='none', server = T,
                                      editable=list(target = "cell", disable = list(columns = c(1))))
  observeEvent(input$dtb_criteria_cell_edit, {
    app_data$dfr_criteria <<- editData(app_data$dfr_criteria, input$dtb_criteria_cell_edit, "dtb_criteria")

    crit_labels <- app_data$dfr_criteria %>%
      arrange(Importance) %>%
      select(`Long name`)

    # update list of criteria in Results
    app_data$lst_weights <- as.list(crit_labels$`Long name`)

    # update Results
    #app_data$df_comp <- compute_possib()
    #app_data$df_comp_orig <- app_data$df_comp
  })


  output$dtb_solutions <- DT::renderDT(app_data$dfr_solutions, selection='none', server = T,
                                          editable=list(target = "cell", disable = list(columns = c(1))))
  observeEvent(input$dtb_solutions, {
    app_data$dfr_solutions <<- editData(app_data$dfr_solutions, input$dtb_solutions_cell_edit, "dtb_solutions")

    #app_data$dfr_eval$Solution <- app_data$dfr_solutions$`Short name`

    #app_data$df_comp <- compute_possib()
    #app_data$df_comp_orig <- app_data$df_comp
  })


  output$dtb_evaluations <- DT::renderDT({
    if(data_is_loaded()){
      datatable(
          data = app_data$dfr_evaluations,
          selection='none',
          editable=list(target = "cell", disable = list(columns = c(1)))
      ) %>% formatRound(columns=colnames(app_data$dfr_evaluations)[-1], digits=3)
    }
  }
  )
  observeEvent(input$dbl_evaluations_cell_edit, {
    app_data$dfr_evaluations <<- editData(app_data$dfr_evaluations, input$dbl_evaluations_cell_edit, "dbl_evaluations")

  })


  output$rkl_criteria_preference <- renderUI({
    if(data_is_loaded()){
      dfr_crit_labels <- app_data$dfr_criteria %>%
        arrange(Importance) %>%
        select(`Long name`)

      rank_list(
        text = "Drag the elements in the desired order to establish the importance of each criterion.
      The top criterion is the most important, the bottom criterion the least.",
        labels = dfr_crit_labels$`Long name`,
        input_id = "rkl_criteria_preference"
      )
    }

  })

  observeEvent(input$btn_results, {

    if(data_is_loaded()){

      dfr_new_crit_imp <- tibble(
        `Long name` =  input$rkl_criteria_preference,
        Importance = 1:length(input$rkl_criteria_preference)
      )

      app_data$dfr_criteria <<- app_data$dfr_criteria %>% select(-Importance) %>%
        inner_join(dfr_new_crit_imp, by="Long name")

      #sorted_weights <- input$weights_rank_list

      #datafile$df_criteria$Importance <- order(sorted_weights)

      compute_results_()
      #datafile$df_comp_orig <- datafile$df_comp
    }

  })



  output$dtb_results <- DT::renderDataTable({

    if(result_is_computed()){

      lst_criteria <- app_data$dfr_results %>%
        select(-c(Solution, `Ref. solution`)) %>%
                 colnames()
      eval_range <- range(app_data$dfr_results[lst_criteria])

      # ref_sol_goal <- "max"
      # ref_sol_crit <- "Lower bound"
      # # ref_sol_goal <- paste0("which.", ref_sol_goal)
      # agg_fun <- which.max
      #
      # max_lb <- app_data$dfr_results[app_data$dfr_results_all[, "REF"] == 1, "Solution"]

      datatable(app_data$dfr_results,
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons =
                    list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    )),
                  paging = FALSE
                ),
      ) %>%
        formatRound(columns=lst_criteria, digits=3) %>%
        formatStyle(lst_criteria,
                    background = styleColorBar(eval_range, '#FFCB42'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') #%>%
        #formatStyle(
        #   1,
        #   target = "row",
        #   fontWeight = styleEqual(max_lb, "bold")
        # )
    }
  })

  output$sld_number_sois <- renderUI({

    if(!result_is_computed()) {
      return(NULL)
    }

    sliderInput("sld_number_sois_", label = "Number of solutions with the highest degree of possibility",
                min = 1,
                max = app_data$n_solutions,
                step = 1,
                value = nrow(app_data$dfr_results),
                ticks = F)
  })


  filter_results <- function(){

    if(result_is_computed()){

      the_attitude <- input$sel_attitudes
      the_n_of_sois <- input$sld_number_sois_

      app_data$dfr_results <- app_data$dfr_results_all %>%
        slice_max(order_by = !!sym(the_attitude), n = the_n_of_sois)

    }
  }


  observeEvent(input$sld_number_sois_, {

    filter_results()

  })

  observeEvent(input$sel_attitudes, {

    filter_results()

  })


  # Plots

  output$plt_intervals <- renderPlotly({
    if(result_is_computed()){

      pgg <- ggplot_intervals(input, app_data$dfr_results)

      p <- ggplotly(
        pgg
      )

      showleg <- input$cbx_show_legend
      p <- p %>% layout(showlegend = showleg)

      p

    }
  })


  # Export


  output$btn_download_excel <- downloadHandler(

    filename = function() {
      "sofi_export.xlsx"
    },

    content = function(file) {


      if(result_is_computed()){

        choics <- input$cbg_data_to_export

        if(length(choics) > 0){

          my_workbook <- createWorkbook()

          data_to_export <- list()

          if(lst_data_to_export[1] %in% choics){

            data_to_export$criteria <- app_data$dfr_criteria
            data_to_export$solutions <- app_data$dfr_solutions
            data_to_export$evaluations <- app_data$dfr_evaluations

          }

          if (lst_data_to_export[2] %in% choics){

            data_to_export$results_all <- app_data$dfr_results_all

          }

          if (lst_data_to_export[3] %in% choics){

            data_to_export$results_soi <- app_data$dfr_results

          }


          for(n in names(data_to_export)){

            sheet_name <- n
            sheet_data <- data_to_export[[n]]

            addWorksheet(
              wb = my_workbook,
              sheetName = sheet_name

            )

            writeDataTable(
              my_workbook,
              sheet = sheet_name,
              x = sheet_data
            )

          }

          if (lst_data_to_export[4] %in% choics){

            addWorksheet(my_workbook, "plots", gridLines = F)

            p1 <- ggplot_intervals(input, app_data$dfr_results)
            ggsave(filename = "p1.png", plot = p1, scale = 0.6)
            insertImage(my_workbook, "plots", "p1.png", width = 6, height = 8)

            #

          }

          saveWorkbook(my_workbook, file)

        }


      }

    }
  )


}
