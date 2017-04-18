


server <- function(input, output, session) {
    likert_data <- reactive({
        likertData(pew, input$yearRange, input$question, input$religions)
    })
    
    
    filteredData <- reactive({
        getFilteredData(pew, input$yearRange, input$question, input$religions)
    })
    
    output$datatable <- renderTable(filteredData())

  
    output$likert <- renderPlot(plot.likert(relig~., data=likert_data(), as.percent = T, ylab="religion", main= paste("Responses to:", question.txt)))

    output$map <- renderPlot({
        print(colnames(filteredData()))
        state_choropleth(filteredData(),num_colors = 1)  +  scale_fill_gradient2(low = "red",mid="white", high="dodgerblue")
            # viridis::scale_fill_viridis() 
    })
}
