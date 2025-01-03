library(shiny)
library(spatstat)
library(sf)
library(sp)

war <- read.csv("war.csv")

conflicts_points <- data.frame(x = war$x, y = war$y, year = war$year)

# Create SF object with correct CRS
conflicts_points_sf <- st_as_sf(conflicts_points, coords = c("x", "y"), 
                                crs = 32633)

#### LETTURA SHAPEFILE ISRAELE ####
library(sf)
# Leggi lo shapefile
israel_polygon <- st_read("israel_palestine_combined.shp")

# Verifica il sistema di coordinate originale
st_crs(israel_polygon)

# Trasforma il sistema di coordinate in UTM (metri)
israel_polygon_proj <- st_transform(israel_polygon, crs = 32633)
conflict_points_proj <- st_transform(conflicts_points_sf, crs = 32633)

# Riscalare in chilometri dividendo per 1000
israel_polygon_proj_km <- israel_polygon_proj
st_geometry(israel_polygon_proj_km) <- st_geometry(israel_polygon_proj) / 1000
israel_polygon_proj <- israel_polygon_proj_km
# Aggiorna il CRS dopo la riscalatura (CRS personalizzato o rimuovi il CRS)
st_crs(israel_polygon_proj_km) <- NA
st_crs(conflict_points_proj) <- NA

# Check intersections
intersected_points <- st_intersection(conflict_points_proj, israel_polygon_proj)

# Convert to ppp
conflicts_sp <- as(intersected_points, "Spatial")

# Create window
israel_window <- as.owin(israel_polygon_proj)

# Create ppp
conflicts_ppp <- ppp(
  x = coordinates(conflicts_sp)[,1],
  y = coordinates(conflicts_sp)[,2],
  window = israel_window,
  marks = conflicts_sp$year
)

# Interfaccia utente
ui <- fluidPage(
  titlePanel("Visualizzazione della densità dei conflitti"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", 
                  "Seleziona l'anno:", 
                  min = min(conflicts_ppp$marks), 
                  max = max(conflicts_ppp$marks), 
                  value = min(conflicts_ppp$marks),
                  step = 1,
                  animate = TRUE)  # Adds animation controls
    ),
    mainPanel(
      plotOutput("densityPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$densityPlot <- renderPlot({
    year_selected <- as.numeric(input$year)
    subset_ppp <- subset(conflicts_ppp, marks == year_selected)
    density_plot <- density(subset_ppp)
    plot(density_plot, main = paste("Densità per l'anno", year_selected))
  })
}

# Esegui l'app
shinyApp(ui = ui, server = server)
