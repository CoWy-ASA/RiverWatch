shinyUI(fluidPage(
    title = "Time Series",
    
fluidRow(
    column(3,
           h4("Select Variable"),    
        selectInput('ycol', 'Measured Value', analytes$analyte),
        br()
    ),
    column(3,
           h4("Log(Transformation)"),
           checkboxInput('logT', 'Log')
           )# ,
#    column(3,
#           h4("Total Metals - Default - Dissolved"),
#           checkboxInput('totDis', 'totDis')
#    )    
    
        ),  ## close fluid row
plotOutput('plot1'),
hr()

)
)

