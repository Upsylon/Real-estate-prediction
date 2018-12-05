library(shiny)

ui <- shinyUI(fluidPage(
  
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

server <- shinyServer(function(input, output) {
  
  
  simulate <- eventReactive(input$button, {

    # simulate pi and measure the time here

    # if (input$method == "estimate_pi") {
    #   estimated_pi <- estimate_pi(B = input$B, seed = input$seed)
    # } else {
    #   estimated_pi <- estimate_pi2(B = input$B, seed = input$seed)
    # }
    # return(estimated_pi)
    
    all_cities %>% filter(city == input$city, maxprice = input$price)
    
    
    
  })
  
  
  output$plot <- renderPlot({
    leafltet_my_df(simulate())
  })
  
  output$pi <- renderText({
    # extract the estimated value
    paste("The estimation for pi is equal to",
          round(simulate()[[1]], 6)
    )
  })
}
)

shinyApp(ui = ui, server = server)
