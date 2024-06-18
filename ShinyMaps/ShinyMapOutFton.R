#RShiny
#libraries
require(tidyverse)
require(haven)
require(dplyr)
require(ggplot2)
require(zip)
require(utils)
require(stringr)
require(sf)
require(rsconnect)
require(leaflet)
require(shiny)
#read in the shapefile
shapefile1 <- st_read("C:\\Users\\aeroa\\OneDrive - The Ohio State University\\Franklinton\\Analysis\\Axle\\Moving Families\\ShinyMaps\\ShinyMapOutFton.shp")
#Map it
server <- function(input, output, session) {
  filtered_data <- reactive({
    if (input$year == "Total Frequency") {
      shapefile1 %>%
        mutate(Freqncy = TtlFrqn)
    } else {
      shapefile1 %>%
        filter(year == input$year)
    }
  })
  
  output$map <- renderLeaflet({
    data1 <- filtered_data()
    
    # Define unique color palettes for yearly frequency and total frequency
    color_palette_yearly <- colorFactor(
      palette = c("#f7fbff", "#6baed6", "#08519c"),  # Blue shades
      domain = data1$Freqncy,
      na.color = "transparent")
    color_palette_total <- colorFactor(
      palette = c("#f7fbff", "#6baed6", "#08519c"),  # Blue shades
      domain = data1$TtlFrqn,
      na.color = "transparent")
    color_palette <- if (input$year == "Total Frequency") color_palette_total else color_palette_yearly
    
    leaflet(data1) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_palette(Freqncy),
        fillOpacity = 0.7,
        color = "#bcbddc",
        stroke = TRUE,
        weight = 1,
        popup = ~paste(
          "<strong>Tract ID:</strong>", FIPS,
          "<br>",
          "<strong>Freqncy:</strong>", Freqncy
        )
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~Freqncy,
        title = if (input$year == "Total Frequency") "Total Frequency" else "Yearly Frequency",
        opacity = 0.7,
        position = "bottomright"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
