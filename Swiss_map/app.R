library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- shinyUI(fluidPage(
  leafletOutput("map"),
  p(), #drity way of making space between map and button
  column(4,
         wellPanel(
           checkboxGroupInput("City", "City",
                              sort(as.character(unique(all_cities$city))),
                              selected = c("Geneve", "Zurich", "Winterthur"))
         )),
  column(8,
         wellPanel(
           sliderInput(
             "rangePrice",
             "Price",
             min(all_cities$price),
             max(all_cities$price),
             value = range(all_cities$price),
             step = 10
           ),
           sliderInput(
             "rangeRooms",
             "Rooms",
             min(all_cities$rooms),
             max(all_cities$rooms),
             value = range(all_cities$rooms),
             step = 0.5
           ),
           sliderInput(
             "rangeM2",
             "Surface in m\u00B2",
             min(all_cities$m2),
             max(all_cities$m2),
             value = range(all_cities$m2),
             step = 1
           )
         ))
))

server <- function(input, output) {
  
  filteredData <- reactive({
    all_cities[all_cities$price >= input$rangePrice[1] & all_cities$price <= input$rangePrice[2] &
               all_cities$rooms >= input$rangeRooms[1] & all_cities$rooms <= input$rangeRooms[2] &
               all_cities$m2 >= input$rangeM2[1] & all_cities$m2 <= input$rangeM2[2] &
               all_cities$city == input$City,]
  })
  
  pal <- colorQuantile(c("green", "#999999", "red"), all_cities$price/all_cities$predicted_price, n = 4)
  
  output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 7.4)) %>%
      setMaxBounds(5.5, 48.2, 11, 45.3) %>%
      addTiles() # Add default OpenStreetMap map tiles
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircleMarkers(
        radius = 10,
        lng = filteredData()$longitude,
        lat = filteredData()$latitude,
        color = "Black",
        fillColor = pal(filteredData()$price/filteredData()$predicted_price),
        weight = 2,
        opacity = 1,
        fillOpacity = 1,
        fill = TRUE,
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
        ),
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11,
                                              spiderfyOnMaxZoom = FALSE,
                                              zoomToBoundsOnClick = FALSE)
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

