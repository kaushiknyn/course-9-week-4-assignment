library(shiny)

shinyUI(fluidPage(
    
    # App title ----
    titlePanel("Mortgage Calculator"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
            
            # Input: Simple integer interval ----
            numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
            hr(),
            numericInput("interest", "Annual interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
            hr(),
            sliderInput("length", "Duration of the loan (in years)",
                        min = 0,
                        max = 30,
                        value = 25,
                        step = 1
            ),
            hr(),
            checkboxInput("plot", "Display plot?", TRUE)
           
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Table summarizing the values entered ----
            uiOutput("text"),
            br(),
            plotOutput("distPlot"),
            br()
        )
    )
))
