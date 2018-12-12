library(shiny)
library(leaflet)

ui <- shinyUI(fluidPage(
  leafletOutput("map"),
  wellPanel(sliderInput("rangePrice", "Price", min(all_cities$price),
                            max(all_cities$price),
                            value = range(all_cities$price), step = 10
                ),
                sliderInput("rangeRooms", "Rooms", min(all_cities$rooms),
                            max(all_cities$rooms),
                            value = range(all_cities$rooms), step = 0.5
                ),
                sliderInput("rangeM2", "Surface in m\u00B2", min(all_cities$m2),
                            max(all_cities$m2),
                            value = range(all_cities$m2), step = 1
                ),
                selectInput("City", "City",
                            unique(all_cities$city))
                )
                #checkboxInput("legend", "Show legend", TRUE) Might be handy later if we get legends.
  # p(), #drity way of making space between map and button
  # actionButton("button", label = "Apply Changes", icon = NULL, width = NULL)
))

server <- function(input, output) {
  
  filteredData <- reactive({
    all_cities[all_cities$price >= input$rangePrice[1] & all_cities$price <= input$rangePrice[2] &
               all_cities$rooms >= input$rangeRooms[1] & all_cities$rooms <= input$rangeRooms[2] &
               all_cities$m2 >= input$rangeM2[1] & all_cities$m2 <= input$rangeM2[2] &
               all_cities$city == input$City,]
  })
  
  output$map <- renderLeaflet({
      leaflet(all_cities, options = leafletOptions(minZoom = 7.4)) %>%
      setMaxBounds(5.5, 48.2, 11, 45.3) %>%
      addTiles() # Add default OpenStreetMap map tiles
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(
        radius = 20,
        lng = filteredData()$longitude,
        lat = filteredData()$latitude,
        color = ifelse(all_cities$price < all_cities$predicted_price, "green", "red"),
        popup = paste(
          "<b>Price :</b>",
          filteredData()$price,
          "   CHF",
          "<br/>",
          "<b>Predicted price :</b>",
          filteredData()$predicted_price,
          " CHF",
          "<br/>",
          "<b>Address :</b>",
          filteredData()$address,
          "<br/>",
          "<b>Number of rooms :</b>",
          filteredData()$rooms,
          "<br/>",
          "<b>Size :</b>",
          filteredData()$m2,
          " m2"
        )
      )
  })
}

shinyApp(ui = ui, server = server)
