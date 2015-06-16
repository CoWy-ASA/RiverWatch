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
                        stat_smooth(method = "lm")  + 
                        ylab(paste("Result", unit)) + 
                        labs(title = paste(input$ycol, " Dection Limit = ", dl, unit) )+
                         theme(legend.position="bottom") 
                    #    geom_hline(yintercept = c(dl, dl2) )
        if(input$logT){p <- p + scale_y_log10() }
        if(input$fixY){ 
             p <- p + facet_wrap(~Monitoring.Location.Name, ncol = 4)}else{
                 
                 p <- p + facet_wrap(~Monitoring.Location.Name, scales = "free_y",ncol = 4)             
        }
        print(p)
        }, height = 1000) ## end plot1

output$plot2 <- renderPlot({
    
    stats <- aggregate( Result.Value ~ Monitoring.Location.Longitude + Monitoring.Location.Latitude, selectedData(), quantile, input$quant, na.rm = TRUE)
    print(ggmap(mp) + geom_point(data = stats, aes( x = Monitoring.Location.Longitude, y = Monitoring.Location.Latitude, col = Result.Value), size = 8 ) + theme( axis.title = element_blank()) + labs( title = input$ycol) )
    
}, height = 600, width = 600) ## close plot2

output$plot3 <- renderPlot({
    dl <- max(selectedData()$Detection.Quantitation.Limit.Value1)
    
p <-     ggplot( data = selectedData(), aes( x = Monitoring.Location.Name, y = Result.Value ) ) + geom_boxplot() + coord_flip() + geom_hline(aes( yintercept=dl), col = 2)   

if(input$logT) {p <-  p  + scale_y_log10()  }
print(p)


}    
    )

})