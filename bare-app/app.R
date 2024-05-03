library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(thematic)

source("setup.R")
source("helpers.R")

thematic_shiny()   # magical function - all plots take on style features of
                   #                     your dashboard.


ui <- page_sidebar(
  version = 5,   # always hard code version number of bootstrap
                 # found using version_default()  command.
  theme = bs_theme(bootswatch = "darkly" ,
  success = "#86C7ED",   # changes success colour to #86C7ED
  "table-color" = "#86C7ED",  # bslib has variables that can be set by the user
                           # See https://rstudio.github.io/bslib/ -> themes -> variables
                           # changes to these variables have global effect.
  base_font = "comic"
#  heading_font =
#  code_font =
  ),

  title = "Effectiveness of DemoCo App Free Trial by Customer Profile",
  sidebar = sidebar(
    # sidebar pannel
    class = "bg-secondary",  # subtle change to background colour of sidebar
    HTML('<image src = "logo.png" width="50%" height = "auto"  >'),

      sidebar_content
  ),
    # main pannel
      layout_columns(
        card(card_header("Conversions over time", class = "text-success"  ), # text-success changes color of header text

             plotOutput("line")
        ),

        card(card_header("Conversion rates", class = "text-success"),
             plotOutput("bar")
        ),

        value_box(title="Recommended Trial",
                  textOutput("recommended_eval"),
                  showcase = bs_icon("stars"),
                  theme = "success"   # background color
        ),


        value_box(title="Users",
                 textOutput("number_of_users"),
                 showcase = bs_icon("people-fill"),
                 theme = "secondary"
        ),

        value_box(title="Avg Spend",
                  textOutput("average_spend"),
                  showcase = bs_icon("coin"),
                  theme = "secondary"
        ),

        card(card_header("Conversion rates by subgroup", class = "text-success"),
            tableOutput("table")
        ),




        # layout_columns always workse in units of 12. If more than 12,
        # then it will treat as a new row
        col_widths = c(8,4,    4, 4, 4,   12),  # layout_columns always divides space into 12 units
        row_heights = c(4, 1.5, 3)  # can use any numbers here, it is all proportional
              )
  )










# Define the Shiny server function
server <- function(input, output) {

  # Filter data according to inputs
  selected_industries <-
    reactive(if (is.null(input$industries)) industries else input$industries)

  selected_propensities <-
    reactive(if (is.null(input$propensities)) propensities else input$propensities)

  selected_contracts <-
    reactive(if (is.null(input$contracts)) contracts else input$contracts)

  selected_data <-
    reactive({
      filter_users(selected_industries(),
                       selected_propensities(),
                       selected_contracts())
    })

  selected_data_by_group <-
    reactive({
      filter_users_by_group(selected_industries(),
                                selected_propensities(),
                                selected_contracts())
    })


  # Make plots
  output$line <-
    renderPlot({
      plot_conversions_over_time(selected_data())
    })

  output$bar <-
    renderPlot({
      plot_conversions_by_group(selected_data_by_group())
    })


  # Compute values for value boxes
  output$recommended_eval <-
    renderText({
      choose_recommendation(selected_data())
    })

  output$number_of_users <-
    renderText({
      count_users(selected_data())
    })

  output$average_spend <-
    renderText({
      compute_average_spend(selected_data())
    })


  # Render table
  output$table <-
    renderTable(digits = 0, {
      make_table(selected_data_by_group())
    })
}


# Create the Shiny app
shinyApp(ui = ui, server = server)
