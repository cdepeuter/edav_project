
shinyUI(fluidPage(
    
    titlePanel("Exploring Social Values with Pew"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("question", "Question:", getQuestionInputs(), "School boards ought to have the right to fire teachers who are known homosexuals"),
            checkboxGroupInput("religions", "Religions", religions, religions),
            sliderInput("yearRange", "Year", 1987, 2012, c(1987, 2012), sep = "")
        ),
        
        mainPanel(
            # plotOutput("map"),
            # sliderInput("yearRange", "Year", 1987, 2012, c(1987, 2012), sep = ""),
            # tableOutput("datatable")
            # 
            tabsetPanel(
                tabPanel("Map", plotOutput("map")),
                tabPanel("Likert", plotOutput("likert")),
                tabPanel("Data", tableOutput("datatable"))
            )
        )
        
    )
))