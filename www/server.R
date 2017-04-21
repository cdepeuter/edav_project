


server <- function(input, output, session) {
    likert_data <- reactive({
        likertData(pew, input$yearRange, input$question, input$religions)
    })
    
    likert_age_data <- reactive({
        likertAgeData(pew, input$yearRange, input$question, input$religions)
    })
    
    
    filteredData <- reactive({
        getFilteredData(pew, input$yearRange, input$question, input$religions)
    })
    
    output$datatable <- renderTable(filteredData())


    output$likert <- renderPlot(plot.likert(relig~., data=likert_data(), as.percent = T, ylab="religion", main= paste("Responses to:",input$question)))
    output$age_likert <- renderPlot(plot.likert(age_bin~., data=likert_age_data(), as.percent = T, ylab="age", main= paste("Responses to:", input$question)))
    
    
    
    output$map <- renderPlot({
        print(colnames(filteredData()))
        state_choropleth(filteredData(),num_colors = 1)  +  scale_fill_gradient2(low = "red",mid="white", high="dodgerblue") + guides(fill=FALSE) +
            ggtitle(paste("Responses to:", input$question)) + theme(plot.title = element_text(size = 18))
            # viridis::scale_fill_viridis() 
    })
}
