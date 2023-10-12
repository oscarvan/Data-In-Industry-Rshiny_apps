# Define UI for app

navbarPage("Assignment 2 - Oscar Vanhanen",
           tabPanel("EDA", 
              fluidPage(
                tabsetPanel(
                  tabPanel("Summary", br(),
                            column(6,
                            h5("Number of Variables & Observations & Data Types"),  
                            verbatimTextOutput(outputId = "SummaryA1")),
                            
                            column(6,
                            h5("Summary of Data")    ,  
                            verbatimTextOutput(outputId = "SummaryA2"))
                            ),
                            
                  tabPanel("Categorical",                     
                           br(),
                           selectizeInput(inputId = "Variables_cat", label = "Show variables:", 
                                          choices = factVars, multiple = TRUE, 
                                          selected = factVars[1:2]),
                           plotOutput(outputId = "Mosaic")),
                  
                  tabPanel("Pairs", 
                           br(),
                           withSpinner(
                              plotOutput("pairs")),
                           selectInput(inputId="VarsToShowPairs", label = "Variables to chart", 
                                       choices = choices_pairs, multiple = TRUE, selected = choices_pairs),
                           selectInput(inputId="VarToColour", label = "Variable to colour", 
                                       choices = choices_pairs_colour, multiple = FALSE, selected = factVars[1])),
                  
                  tabPanel("Correlation", 
                           plotOutput("Corrgram"),
                           checkboxInput(inputId = "Group", label = "Ordered", value = TRUE),
                           checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                           selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                           selectInput(inputId="VarsToShowCorr", label = "Variables to chart", 
                                       choices = numVars, multiple = TRUE, selected = numVars)),
                  
                  tabPanel("Missing data", 
                           br(),
                           withSpinner(
                           plotOutput("vis_miss")),
                           checkboxInput(inputId = "cluster", label = "Cluster missingness", value = TRUE),
                           checkboxInput(inputId = "sort", label = "Sort variables", value = TRUE),
                           selectInput(inputId="VarsToShowMiss", label = "Variables to chart", 
                                       choices = allVars, multiple = TRUE, selected = allVars),
                           
                           plotOutput("r_part")
                           ),
                

                  tabPanel("Upset Chart", 
                           br(),
                           withSpinner(
                           plotOutput("gg_miss")),
                           sliderInput(inputId = "range_upset", label = "Number of sets", min = 0, max = 15, step = 1, value = 5),
                           selectInput(inputId="VarsToShowgGGMiss", label = "Variables to chart", 
                                       choices = allVars, multiple = TRUE, selected = allVars)),
                  
                  tabPanel("Missingness Correlation", 
                           br(),
                           withSpinner(
                             plotOutput("miss_correlation"))
                  ),
                  
                  tabPanel("Boxplot",         
                           plotOutput(outputId = "Boxplot_answer"),
                           
                           checkboxInput(inputId = "Label", label = "Show labels", value = FALSE),
                           checkboxInput(inputId = "BScale", label = "Scale", value = TRUE), 
                           checkboxInput(inputId = "BCentre", label = "Centre", value = FALSE),
                           
                           checkboxInput(inputId = "ShowOutliers", label = "Show outliers", value = FALSE),
                           sliderInput(inputId = "Range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                           
                  ),
                  
                  
                  tabPanel("Gaps", 
                           
                           plotOutput("rising_value"),
                           checkboxInput(inputId = "centre", label = "Show centred", value = FALSE),
                           checkboxInput(inputId = "scale", label = "Show scaled", value = TRUE),
                           selectInput(inputId="VarsToShowRis", label = "Variables to chart", 
                                       choices = numVars, multiple = TRUE, selected = numVars[1:3])
                           ),
                  
                  tabPanel("Distribution",
                           plotOutput("histogram"),
                           selectInput(inputId = "inputVarHist", label = "Select Variable", choices = numVars, selected = "Y"),
                           selectInput(inputId = "n_breaks",
                                       label = "Number of bins in histogram (approximate):",
                                       choices = c(10, 20, 35, 50),
                                       selected = 20),
                           checkboxInput(inputId = "individual_obs",
                                         label = strong("Show individual observations"),
                                         value = FALSE)
                           ),
                  
                  tabPanel("Raw Data", 
                           DT::dataTableOutput("mytable")
                           )
              
                  )
              )
            ),
           
           tabPanel("Model (single-reactive version)", 
             fluidPage(
               tabsetPanel(
                  sidebarLayout(
                    sidebarPanel(
                      sliderInput("var_miss_threshold", "Variable Missing Threshold:",
                                  min = 0, max = 100, value = 100, step = 1),
                      sliderInput("obs_miss_threshold", "Observation Missing Threshold:",
                                  min = 0, max = 100, value = 100, step = 1),
                    ),
                    
                    mainPanel(
                      h3("Processing"),
                      textOutput("dim_before"),
                      p("Variables removed:"),
                      verbatimTextOutput("vars_removed"),
                      p("Number of observations removed:"),
                      verbatimTextOutput("obs_removed"),
                      textOutput("dim_after"),
                      
                      h3("Model"),
                      withSpinner(plotOutput("model")),
                      h5("RMSE: "),verbatimTextOutput("rmse"),
                      plotOutput("residuals_boxplot"),
                      checkboxInput(inputId = "ShowOutliers_res", label = "Show outliers", value = FALSE),
                      sliderInput(inputId = "Range_res", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                    )
                  )
               )
              )
            ),
           
           tabPanel("Model (Reactive-Cascade version)", 
                    fluidPage(
                      tabsetPanel(
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("var_miss_threshold_re", "Variable Missing Threshold:",
                                        min = 0, max = 100, value = 50, step = 1),
                            sliderInput("obs_miss_threshold_re", "Observation Missing Threshold:",
                                        min = 0, max = 100, value = 50, step = 1),
                          ),
                          
                          mainPanel(
                            h3("Processing"),
                            textOutput("dim_before_re"),
                            p("Variables removed:"),
                            verbatimTextOutput("vars_removed_re"),
                            p("Number of observations removed:"),
                            verbatimTextOutput("obs_removed_re"),
                            textOutput("dim_after_re"),
                            
                            verbatimTextOutput("Model_summary_re"),
                            plotOutput("model_plot_re"),
                            h5("RMSE: "),verbatimTextOutput("rmse_re"),
                            plotOutput("residuals_boxplot_re"),
                            checkboxInput(inputId = "ShowOutliers_res_re", label = "Show outliers", value = FALSE),
                            sliderInput(inputId = "Range_res_re", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                          )
                        )
                      )
                    )
           )
           
)

