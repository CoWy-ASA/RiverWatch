####
shinyServer(function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
 #       if(input$totDis){ frac <- "Total"}else{frac <- "Dissolved"}
#filter(small_dat, Characteristic.Name == input$ycol & Sample.Fraction != frac)
filter(small_dat,analyte == input$ycol)
})    
   
    output$plot1 <- renderPlot({
        unit <- unique(selectedData()$Result.Unit)
      
        dl <- max(selectedData()$Detection.Quantitation.Limit.Value1)
        dl2 <- min(selectedData()$Detection.Quantitation.Limit.Value1) ## check for multiple detection limits
        
        p <- ggplot( data = selectedData(), aes(x = date, y = Result.Value)) + 
                      geom_point(aes(col = DL_substitution)) +  
                      facet_wrap(~Monitoring.Location.Name, scales = "free_y",ncol = 4)  + 
                        stat_smooth(method = "lm")  + 
                        ylab(paste("Result", unit)) + 
                        labs(title = paste(input$ycol, " Dection Limit = ", dl, unit) )# + 
                    #    geom_hline(yintercept = c(dl, dl2) )
        if(input$logT){p <- p + scale_y_log10() }
        print(p)
        }, height = 1000)
    
})