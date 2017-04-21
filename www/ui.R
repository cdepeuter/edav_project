
shinyUI(fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    
    titlePanel("Exploring Social Values with Pew"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("question", "Question:", getQuestionInputs(), "School boards ought to have the right to fire teachers who are known homosexuals"),
            checkboxGroupInput("religions", "Religions", religions, religions),
            sliderInput("yearRange", "Year", 1987, 2012, c(1987, 2012), sep = "")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Map", plotOutput("map"), img(src = "shiny_legend.png",height = 75,  width = 400, class="img_class")),
                tabPanel("Likert", plotOutput("likert"), plotOutput("age_likert")),
                tabPanel("Data",  tableOutput("datatable")),
                tabPanel("About", tags$div(class="about", tags$p("This is a project by Daniel First, Rebecca Peyser, Karl-Loïc Kamdem, and Conrad De Peuter for STATGR5702 at Columbia. Our goal was to present the "),
             tags$a(href="http://www.people-press.org/2012/04/15/1987-2012-values-survey-combined-dataset/", "Pew Values Survey Dataset"),
            tags$p("so people could see how the general views of different religions, regions and age groups have changed throughout time.")))
            )
        )
        
    )
))