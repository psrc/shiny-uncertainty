fluidPage(tags$style(type = "text/css", ".checkbox label { font-size: 14px;}"),
          title="", windowTitle="Uncertainty",
            navbarPage(theme = shinytheme("readable"),
                       title = "UrbanSim Uncertainty", 
                       tabPanel("Confidence Intervals",
                                fluidPage(
                                  column(width = 2,
                                         uiOutput("ci_select_run_ui"), 
                                         selectInput("ci_select_geog",
                                                     label = "Geography",
                                                     choices = c("City" = "city",
                                                                 #"TAZ" = "taz",
                                                                 #"FAZ" = "faz",
                                                                 "Regional Geography" = "rgs"),
                                                     selected = "rgs"),
                                         selectInput("ci_select_year",
                                                     label = "Year",
                                                     choices = c("2017" = 2017,
                                                                 "2040" = 2040,
                                                                 "2050" = 2050),
                                                     selected = 2050),
                                         radioButtons("ci_select_ci",
                                                      label = h6("Select Confidence Interval"),
                                                      choices = list("80%" = 80, "95%" = 95)), 
                                         selectInput("ci_select_county",
                                                     label = h6("Filter by County"),
                                                     choices = cnty.choices,
                                                     selected = "All",
                                                     multiple = TRUE
                                                     ),
                                         actionButton("ci_submitButton",
                                                      label = "Enter")
                                         ),
                                  column(width = 5,
                                         # DT::dataTableOutput("ci_dt_hh")
                                         plotlyOutput("ci_plot_emp", height = "820px")
                                         ),
                                  column(width = 5,
                                         plotlyOutput("ci_plot_hh", height = "820px")
                                         )
                                ) # end fluidPage
                                ), # end tabPanel
                       tabPanel("Random Seed",
                                fluidPage(
                                  column(width = 2,
                                         helpText("Plot the largest differences at the FAZ level among multiple runs to
                                                   explore variations in runs with different random seeds."),
                                         numericInput("rs_num_group", 
                                                      label = h6("Select Number of Groups"), 
                                                      min = 1,
                                                      value = 1),
                                         actionButton("rs_enterButton", label = "Enter"),
                                         br(),
                                         br(),
                                         uiOutput("rs_group_diff_ui"),
                                         uiOutput("rs_helptext"),
                                         uiOutput("rs_group_select_ui"),
                                         uiOutput("rs_fazes_ui"),
                                         uiOutput("rs_submitButton_ui"),
                                         br(),
                                         br()
                                  ), # end column
                                  column(width = 10,
                                         fluidRow(plotlyOutput('plot_households', height = "425px")), # end fluidRow
                                         fluidRow(plotlyOutput('plot_employment', height = "425px")) # end fluidRow
                                  ) # end column
                                ) # end fluidPage
                       ), # end tabPanel
                       fluid = TRUE
                       )# end navbarPage
)# end fluidPage
  