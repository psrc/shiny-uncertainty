fluidPage(tags$style(type = "text/css", ".checkbox label { font-size: 14px;}"),
          title="", windowTitle="Uncertainty",
            navbarPage(theme = shinytheme("readable"),
                       title = "UrbanSim Uncertainty", 
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
                                         uiOutput("rs_helptext"),
                                         uiOutput("rs_group_diff_ui"),
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
  