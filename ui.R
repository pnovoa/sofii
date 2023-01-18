#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(sortable)
library(DT)
library(plotly)
source("R/sofi_assistant.R")



dashboardPage(
  dashboardHeader(title = "SOFFI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "tbi_home", icon = icon("house")),
      menuItem("Data", tabName = "tbi_data", icon = icon("upload")),
      menuItem("Settings", tabName = "tbi_settings", icon = icon("gear")),
      menuItem("Results", tabName = "tbi_results", icon = icon("table")),
      menuItem("Plots", tabName = "tbi_plots", icon = icon("chart-line")),
      menuItem("Export", tabName = "tbi_export", icon = icon("file-export")),
      menuItem("Help", tabName = "tbi_help", icon = icon("info"),
               menuItem("Input",  tabName = "tbi_help_input", icon = icon("info")),
               menuItem("Output", tabName = "tbi_help_output", icon = icon("info"))
               )
    )
  ),


  dashboardBody(
    tabItems(
      tabItem(
        "tbi_home",
        box(
          title = NULL,
          status = "info",
          includeHTML("www/home.html"),
          width = NULL
        )
      ),

      tabItem(
        "tbi_data",
        box(
          includeHTML("www/data.html"),
          status = "info",
          fileInput(
            inputId = "file_loaded",
            label = "Choose a .xlsx file to upload",
            accept = ".xlsx"
          ),
          width = NULL
        ),
        box(
          width = NULL,
          collapsible = T,
          title = "Summary",
          valueBoxOutput(outputId = "vbx_criteria"),
          valueBoxOutput(outputId = "vbx_solutions"),
          valueBoxOutput(outputId = "vbx_evaluations")
        ),
        box(
          title = "Criteria",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_criteria")
        ),
        box(
          title = "Solutions",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_solutions")
        ),
        box(
          title = "Evaluations",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_evaluations")
        )
      ),

      tabItem(
        tabName = "tbi_settings",
        box(
          includeHTML("www/settings.html"),
          status = "info",
          width = NULL
        ),
        box(width = 6, collapsible = F, title = "Criteria preference", collapsed = T,
            htmlOutput("rkl_criteria_preference")
        ),
        box(
          title = "Criteria for the Reference Solution", width = 6, collapsible = F,

          selectInput(inputId = "sel_rs_goal", label = "Goal", choices = c("min", "max"), selected = "max"),

          selectInput(inputId = "sel_rs_criteria", label = "Criteria", choices = lst_of_ref_sol_crit),

          actionButton("btn_results", "Compute results")
        ),
      ),

      tabItem(tabName = "tbi_results",
              box(
                width = NULL,
                status = "info",
                includeHTML("www/results.html")
              ),
              box(
                title = "Solutions",
                width = NULL,
                sidebarLayout(
                  position = "left",
                  fluid = F,
                  sidebarPanel = sidebarPanel(
                    width = 3,
                    #h4("Selection of solutions of interest (SOI)"),
                    p("To filter the solutions of interest select an attitude and a threshold."),
                    br(),
                    selectInput(inputId = "sel_attitudes", label = "Attitude", choices = lst_attitudes),
                    uiOutput("sld_number_sois")
                    #actionButton("btn_filter_results", "Filter", icon = icon("filter"))
                  ),
                  mainPanel = mainPanel(

                    DT::dataTableOutput(outputId = "dtb_results")

                  )
                )
              )
      ),

      tabItem(tabName = "tbi_plots",
              box(
                includeHTML("www/plots.html"),width = NULL,status = "info"
              ),
              box(title = "Aggregate evaluation",
                  width = NULL,
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(

                      selectInput(inputId = "sel_element_to_be_ordered_by", label = "Order by", choices = lst_element_to_be_ordered_by),
                      checkboxInput(inputId = "cbx_descending", label = "Descending", value = F),
                      checkboxInput(inputId = "cbx_flip_coords", label = "Flip the coordinates", value = T),
                      checkboxInput(inputId = "cbx_rotate_x", label = "Rotate x-axis labels", value = F),
                      checkboxInput(inputId = "cbx_rotate_y", label = "Rotate y-axis labels", value = F),
                      checkboxInput(inputId = "cbx_show_legend", label = "Show legend", value = T)

                    ),

                    mainPanel = mainPanel(

                      plotlyOutput(outputId = "plt_intervals")

                    )

                  )
              )
      ),
      tabItem(
        tabName = "tbi_export",
        box(
          includeHTML("www/export.html"),width = NULL,status = "info"
        ),
        box(
          width = NULL,
          checkboxGroupInput(inputId = "cbg_data_to_export",
                             label = "Data to export",
                             choices = lst_data_to_export,
                             selected = lst_data_to_export,
                             inline = T),
          downloadButton(outputId = "btn_download_excel", label = "Excel", icon = icon("file-excel"))
          #downloadButton(outputId = "btn_download_html", label = "HTML", icon = icon("code"))

        )
      ),

      tabItem("tbi_help_input",
               box(
                 title = NULL,  status = "info",
                 includeHTML("www/help_input.html"),
                 width = NULL
               )
       ),

      tabItem("tbi_help_output",
              box(
                title = NULL,  status = "info",
                includeHTML("www/help_output.html"),
                width = NULL
              )
      )

    ))

)
