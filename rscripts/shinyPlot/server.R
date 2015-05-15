
####
shinyServer(function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
filter(small_dat, Characteristic.Name == input$ycol & Sample.Fraction != "Dissolved")
    })    
    
    output$plot1 <- renderPlot({
        unit <- unique(selectedData()$Result.Unit)
        dl <- max(selectedData()$Detection.Quantitation.Limit.Value1)
        dl2 <- max(selectedData()$Detection.Quantitation.Limit.Value1) ## check for multiple detection limits
        
        p <- ggplot( data = selectedData(), aes(x = date, y = Result.Value)) + 
                      geom_point() +  
                      facet_wrap(~Monitoring.Location.Name, scales = "free_y") + 
                        stat_smooth() + 
                        ylab(paste("Result", unit)) + 
                        labs(title = paste(input$ycol, " Dection Limit = ", dl, unit) ) + 
                        geom_hline(yintercept = c(dl, dl2) )
        print(p)
        }, height = 700)
    
})