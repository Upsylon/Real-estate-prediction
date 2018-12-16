library(shiny)
library(leaflet)

ui <- shinyUI(fluidPage(
  leafletOutput("map"),
  p(), #dirty way of making space between map and button
  column(4,
         wellPanel(
           checkboxGroupInput("City", "City",
                              unique(all_cities2$city))
         )),
  column(8,
         wellPanel(
           sliderInput(
             "rangePrice",
             "Price",
             min(all_cities2$price),
             max(all_cities2$price),
             value = range(all_cities2$price),
             step = 10
           ),
           sliderInput(
             "rangeRooms",
             "Rooms",
             min(all_cities2$rooms),
             max(all_cities2$rooms),
             value = range(all_cities2$rooms),
             step = 0.5
           ),
           sliderInput(
             "rangeM2",
             "Surface in m\u00B2",
             min(all_cities2$m2),
             max(all_cities2$m2),
             value = range(all_cities2$m2),
             step = 1
           )
         ))
))

server <- function(input, output) {
  
  filteredData <- reactive({
    all_cities2[all_cities2$price >= input$rangePrice[1] & all_cities2$price <= input$rangePrice[2] &
               all_cities2$rooms >= input$rangeRooms[1] & all_cities2$rooms <= input$rangeRooms[2] &
               all_cities2$m2 >= input$rangeM2[1] & all_cities2$m2 <= input$rangeM2[2] &
               all_cities2$city == input$City,]
  })
  
  output$map <- renderLeaflet({
      leaflet(all_cities2, options = leafletOptions(minZoom = 7.4)) %>%
      setMaxBounds(5.5, 48.2, 11, 45.3) %>%
      addTiles() # Add default OpenStreetMap map tiles
  })
  
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      #clearShapes() %>%
      addCircles(
        radius = 20,
        lng = filteredData()$longitude,
        lat = filteredData()$latitude,
        color = ifelse(all_cities2$price <= all_cities2$predicted_price, "green", "red"),
        stroke = TRUE,
        fillColor = "transparent",
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
      ) %>%
      fitBounds(
        min(filteredData()$longitude),
        min(filteredData()$latitude),
        max(filteredData()$longitude),
        max(filteredData()$latitude)
      )
  })
}

shinyApp(ui = ui, server = server)
