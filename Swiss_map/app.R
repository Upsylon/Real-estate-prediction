library(shiny)
library(leaflet)

ui <- shinyUI(fluidPage(
  leafletOutput("map"),
  absolutePanel(top = 5, right = 25,
                sliderInput("rangePrice", "Price", min(all_cities$price), max(all_cities$price),
                            value = range(all_cities$price), step = 10
                ),
                sliderInput("rangeRooms", "Rooms", min(all_cities$rooms), max(all_cities$rooms),
                            value = range(all_cities$rooms), step = 0.5
                ),
                sliderInput("rangeM2", "Surface in m\u00B2", min(all_cities$m2), max(all_cities$m2),
                            value = range(all_cities$m2), step = 1
                ),
                checkboxGroupInput("City", "City",
                            unique(all_cities$city))
                )
                #checkboxInput("legend", "Show legend", TRUE) Might be handy later if we get legends.
  # p(), #drity way of making space between map and button
  # actionButton("button", label = "Apply Changes", icon = NULL, width = NULL)
)
)

server <- function(input, output) {
  
  filteredData <- reactive({
    all_cities[all_cities$price >= input$rangePrice[1] & all_cities$price <= input$rangePrice[2],]
  })
  
  output$map <- renderLeaflet({
      leaflet(all_cities, options = leafletOptions(minZoom = 7.4)) %>%
      setMaxBounds(5.5, 48.2, 11, 45.3) %>%
      addTiles()  # Add default OpenStreetMap map tiles
        # %>%
        # for (i in 1:(length(unique(all_cities$city)))){
        #   addPopups(
        #     lng = summarize(group_by(all_cities, city)[i,], mean(longitude))[,2],
        #     lat = summarize(group_by(all_cities, city)[i,], mean(latitude))[,2],
        #     popup = unique(all_cities$city)[i],
        #     options = popupOptions(closeButton = T)
        #   )
        # }
  })
  
  observe({
    proxy <- leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(
        lng = all_cities$longitude,
        lat = all_cities$latitude,
        popup = paste(
          "<b>Price :</b>",
          all_cities$price,
          "   CHF",
          "<br/>",
          "<b>Adress :</b>",
          all_cities$address,
          "<br/>",
          "<b>Number of rooms :</b>",
          all_cities$rooms,
          "<br/>",
          "<b>Size :</b>",
          all_cities$m2,
          " m2"
        )
      )
  })
}

shinyApp(ui = ui, server = server)

#### 

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
    
    if (input$method == "estimate_pi") {
      estimated_pi <- estimate_pi(B = input$B, seed = input$seed)
    } else {
      estimated_pi <- estimate_pi2(B = input$B, seed = input$seed)
    }
    return(estimated_pi)
    
  })
  
  simulated_time <- eventReactive(input$button, {
    if (input$method == "estimate_pi") {
      system.time({ estimate_pi(B = input$B, seed = input$seed) })
    } else {
      system.time({ estimate_pi2(B = input$B, seed = input$seed) })
    }
  })
  
  output$plot <- renderPlot({
    plot(simulate())
  })
  
  output$time <- renderText({
    # extract the time of the execution
    paste("The time to run find the estimation of pi for the user is equal to",
          round(simulated_time()[3], 4), "seconds."
    )
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
