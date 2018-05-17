fluidPage(tags$style(type = "text/css", ".checkbox label { font-size: 14px;}"),
          title="", windowTitle="Uncertainty",
            navbarPage(theme = shinytheme("readable"),
                       title = "UrbanSim Uncertainty",
                       id = "inNavbarPage",
                       tabPanel("Confidence Intervals (CI)",
                                fluidPage(
                                  column(width = 2,
                                         uiOutput("ci_select_ci_dir_ui"), 
                                         actionButton("ci_preview_submitButton",
                                                      label = "Select"),
                                         br(),
                                         br(),
                                         uiOutput("ci_select_helptext_ui"),
                                         uiOutput("ci_select_geog_ui"),
                                         # selectInput("ci_select_geog",
                                         #             label = "Geography",
                                         #             choices = c("City" = "city",
                                         #                         #"TAZ" = "taz",
                                         #                         #"FAZ" = "faz",
                                         #                         "Regional Geography" = "rgs"),
                                         #             selected = "rgs"),
                                         uiOutput("ci_select_year_ui"),
                                         # selectInput("ci_select_year",
                                         #             label = "Year",
                                         #             choices = c("2017" = 2017,
                                         #                         # "2040" = 2040,
                                         #                         "2050" = 2050),
                                         #             selected = 2050),
                                         uiOutput("ci_select_ci_ui"),
                                         # radioButtons("ci_select_ci",
                                         #              label = h6("Confidence Interval"),
                                         #              choices = list("80%" = 80, "95%" = 95)), 
                                         uiOutput("ci_select_county_ui"),
                                         # selectInput("ci_select_county",
                                         #             label = h6("Filter by County"),
                                         #             choices = cnty.choices,
                                         #             selected = "All",
                                         #             multiple = TRUE
                                         #             ),
                                         uiOutput("ci_submitButton_ui"),
                                         # actionButton("ci_submitButton",
                                         #              label = "Enter"),
                                         br(),
                                         br(),
                                         p("For more information about Confidence Intervals and its interpretation, click the link below"),
                                         actionLink("ci_link_to_About", "About::Confidence Intervals")
                                         ),
                                  column(width = 10,
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Employment",
                                                              plotlyOutput("ci_plot_emp", height = "800px")
                                                     ),
                                                     tabPanel("Households",
                                                              plotlyOutput("ci_plot_hh", height = "800px")
                                                     ),
                                                     tabPanel("Population",
                                                              plotlyOutput("ci_plot_pop", height = "800px")
                                                     )
                                         ) # end tabsetPanel
                                  ) # end column
                                ) # end fluidPage
                                ), # end tabPanel
                       tabPanel("CI AAPC",
                                fluidPage(column(width = 2,
                                                 uiOutput("aapc_select_ci_dir_ui"),
                                                 selectInput("aapc_select_geog",
                                                             label = "Geography",
                                                             choices = c("City" = "city",
                                                                         #"TAZ" = "taz",
                                                                         #"FAZ" = "faz",
                                                                         "Regional Geography" = "rgs"),
                                                             selected = "rgs"),
                                                 selectInput("aapc_select_year",
                                                             label = "Year",
                                                             choices = c("2017" = 2017,
                                                                         # "2040" = 2040,
                                                                         "2050" = 2050),
                                                             selected = 2050),
                                                 radioButtons("aapc_select_ci",
                                                              label = h6("Confidence Interval"),
                                                              choices = list("80%" = 80, "95%" = 95)),
                                                 selectInput("aapc_select_county",
                                                             label = h6("Filter by County"),
                                                             choices = cnty.choices,
                                                             selected = "All",
                                                             multiple = TRUE
                                                 ),
                                                 actionButton("aapc_submitButton",
                                                              label = "Enter"),
                                                 br(),
                                                 br(),
                                                 p("For more information about Confidence Intervals and its interpretation, click the link below"),
                                                 actionLink("ci_aapc_link_to_About", "About::Confidence Intervals")
                                                 ),# end column
                                          column(width = 10,
                                                 tabsetPanel(type = "tabs",
                                                             tabPanel("Employment",
                                                                      plotlyOutput("aapc_plot_emp", height = "800px")
                                                             ),
                                                             tabPanel("Households",
                                                                      plotlyOutput("aapc_plot_hh", height = "800px")
                                                             ),
                                                             tabPanel("Population",
                                                                      plotlyOutput("aapc_plot_pop", height = "800px")
                                                             )
                                                 ) # end tabsetPanel
                                                 )# end column
                                          )# end fluidPage
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
                       tabPanel("About",
                                fluidPage(
                                  column(width = 1),
                                  column(width = 10,
                                         br(),
                                         h6("Confidence Intervals"),
                                         p("The confidence interval is a measure of uncertainty in the forecast and there 
                                           are many possible uncertainty sources (e.g., inaccuracies in the input data, 
                                           the ways in which the model represents reality, the randomness in those processes)."),
                                         p("To calculate the confidence interval, model results are first compared with known outcomes, 
                                           which in this case are 2017 data. The uncertainty captured in those comparisons 
                                           is then extended into the future to arrive at a set of confidence intervals for a forecast year."),
                                         p("An 80% confidence interval represents a 1-in-10 chance that the actual result 
                                           will fall below the interval and a 1-in-10 chance that it will fall above. 
                                           The median value is the best forecast point.")),
                                  column(width = 1)
                                ) # end fluidPage
                                ), # end tabPanel
                       fluid = TRUE
                       )# end navbarPage
)# end fluidPage
  