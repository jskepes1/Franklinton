#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)
shapefile2 <- st_read("C:\\Users\\aeroa\\OneDrive\\Documents\\GitHub\\MapBeforeFranklinton\\B4Fton.shp")

# Define UI
ui <- fluidPage(
  titlePanel("Moved Families Before Fton Frequency Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year or Total:", 
                  choices = c("Total Frequency", sort(unique(shapefile2$year))))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    if (input$year == "Total Frequency") {
      shapefile2 %>%
        mutate(Freqncy = TtlFrqn)
    } else {
      shapefile2 %>%
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

