shinyUI(fluidPage(
    titlePanel("River Watch Exploratory Data Analysis"),

sidebarLayout(sidebarPanel =  sidebarPanel(
    #       h4("Select Variable"),    
        selectInput('ycol', 'Measured Value', analytes$analyte),
        br(),
          h4("Log(Transformation)"),
           checkboxInput('logT', 'Log'),
        br(),
        h4("Fix y-axis?"),
        checkboxInput('fixY', 'fixY'),
        br(),
        h4("Quantile (Map View) "),
        sliderInput(inputId = 'quant', label = NULL, min = 0.5, max = 1, step = .05, value = 0.95)
           ),  ## close fluid row
mainPanel = mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Time Series",br(), plotOutput("plot1")),
                tabPanel("Map View", br(), plotOutput("plot2")),
                tabPanel("Boxplots", br(), plotOutput("plot3"))
    )
))
))## close shinyUI, fluidPage

