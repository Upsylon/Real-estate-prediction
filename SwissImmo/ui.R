library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Pi Estimation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("method",
                  "The function to use:",
                  choices = c("estimate_pi", "estimate_pi2")
      ),
      
      numericInput("seed",
                   "The desired seed:",
                   value = 10,
                   min = 0,
                   max = Inf
      ),
      
      sliderInput("B",
                  "Number of simulated points:",
                  value = 5000,
                  min = 1,
                  max = 10^6
      ),
      actionButton("button", label = "Apply Changes", icon = NULL, width = NULL
      )
    ),
    
    mainPanel(
      
      plotOutput("plot"),
      
      textOutput("time"),
      
      textOutput("pi")
    )
  )
)
)
