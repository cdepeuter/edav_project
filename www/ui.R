
shinyUI(fluidPage(
    
    titlePanel("Hello Shiny!"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("question", "Question:", getQuestionInputs(), "Believe in God")
        ),
        
        mainPanel(
            plotOutput("map"),
            sliderInput("yearRange", "Year", 1987, 2012, c(1987, 2012))
        )
        
    )
))