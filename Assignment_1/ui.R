# Define UI for app
fluidPage(
  titlePanel("Assignment 1 - Oscar Vanhanen"),
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
                         choices = choices_pairs, multiple = TRUE, selected = choices_pairs[1]),
             selectInput(inputId="VarToColour", label = "Variable to colour", 
                         choices = factVars, multiple = FALSE, selected = factVars[1]),),
    
    tabPanel("Correlation", 
             plotOutput("Corrgram"),
             checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
             selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
             selectInput(inputId = "Group", label = "Grouping method", choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), selected = "OLO"),
             selectInput(inputId="VarsToShowCorr", label = "Variables to chart", 
                         choices = numVars, multiple = TRUE, selected = numVars[1:3])),
    
    tabPanel("Missing data", 
             br(),
             withSpinner(
             plotOutput("vis_miss")),
             checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE),
             selectInput(inputId="VarsToShowMiss", label = "Variables to chart", 
                         choices = allVars, multiple = TRUE, selected = allVars[1:5])),
    
    tabPanel("Numeric",         
             plotOutput(outputId = "Boxplot"),
             checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
             checkboxInput(inputId = "outliers", label = "Show outliers", value = FALSE),
             sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
             selectInput(inputId="VarsToShowBox", label = "Variables to chart", 
                         choices = numVars, multiple = TRUE, selected = numVars[1:3])
             
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
                           value = FALSE),
             ),
    
    tabPanel("Distribution Ploty",
             plotOutput("histogramPlotly"),
             
             selectInput(inputId = "inputVarHist", label = "Select Variable", choices = numVars, selected = "Y"),
             selectInput(inputId = "n_breaks",
                         label = "Number of bins in histogram (approximate):",
                         choices = c(10, 20, 35, 50),
                         selected = 20),
             checkboxInput(inputId = "individual_obs",
                           label = strong("Show individual observations"),
                           value = FALSE),
             ),
    
    tabPanel("DataTable", 
             plotOutput("Datatable"),
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId = "Rownames", "Show row names", value=TRUE),
                 checkboxInput(inputId = "Order", "Column ordering", value=TRUE),
                 selectInput(inputId = "Selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                 selectInput(inputId = "Filter", "Filter type", choices=c("none","bottom"), selected = "none"),
                 selectInput(inputId = "Dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                 checkboxInput(inputId = "Responsive", "Responsive extension", value = TRUE),
                 tags$a("DOM documentation", href = "https://datatables.net/reference/option/dom", target="_blank"),
                 tags$br(),
                 tags$a("Extensions documentation", href = "https://datatables.net/extensions/index", target="_blank"),
                 width = 3
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 DT::dataTableOutput(outputId = "TableX"),
                 tableOutput(outputId = "SelRows")     # this lists any selected rows of the first table  
               )
             )
             
             
             )

    )
)

