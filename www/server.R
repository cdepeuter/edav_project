server <- function(input, output, session) {
    
    filteredData <- reactive({
        getFilteredData(pew, input$yearRange, input$question)
    })
    
    
    output$map <- renderPlot({
        print(colnames(filteredData()))
        state_choropleth(filteredData()) + ggtitle("Who is going to hell")
    })
}
