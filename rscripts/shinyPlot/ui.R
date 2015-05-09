shinyUI(pageWithSidebar(
    headerPanel('Initial Time Series'),
    sidebarPanel(
       # selectInput('xcol', 'X Variable', names(iris)),
        selectInput('ycol', 'Measured Value', analytes$Characteristic.Name)

    ),
    mainPanel(
        plotOutput('plot1')
    )
))