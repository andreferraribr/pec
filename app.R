#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# map_cidades@polygons[[2]]@Polygons[[1]]@coords
# https://stackoverflow.com/questions/29803253/r-extracting-coordinates-from-spatialpolygonsdataframe


library(shiny)
library(dplyr)
library(httr)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(leaflet)
library(brazilmaps)
library(shinyWidgets)
library(shiny)
library(sf)
library(vroom)

camaras <- vroom::vroom("camaras.csv")%>%
    mutate (filtro = paste0(ente_uf,"-", MicroRegion))
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Gasto das Câmaras Municipais"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("filtro", label = "Município-UF:", 
                        choices = camaras$filtro, 
                        selected = "Florianópolis-SC-42016"),
            width = 2
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("distPlot"),
           DTOutput("tabela")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    micro<- reactive({as.numeric(str_sub((input$filtro),-5))})
    
  
    
    map_cidades <- get_brmap( geo = "City",
                              class = "SpatialPolygonsDataFrame")
    
    map_cidades <- join_data(map_cidades, camaras, by = c("City" = "cod_ibge"))
    

    
    output$distPlot <- renderLeaflet({
      
        teste <- get_brmap( geo = "City",
                            geo.filter = list(MicroRegion = micro()),
                            class = "SpatialPolygonsDataFrame") 
        teste <- join_data(teste, camaras, by = c("City" = "cod_ibge"))
        teste_micro <- get_brmap( geo = "MicroRegion",
                                  geo.filter = list(MicroRegion = micro()),
                                  class = "SpatialPolygonsDataFrame")
        
        teste_state <- get_brmap( geo = "State",
                                  geo.filter = list(State = micro()%/% 1000),
                                  class = "SpatialPolygonsDataFrame")
        mapa_teste<- leaflet()%>%
            addTiles(group = "OSM (default)") %>% 
            addPolygons(
                data = teste_state, 
                fill = F, weight = 2, color = "black", group = "Estado")%>%
            addPolygons(
                data = teste,
                fill = T, weight = 0.5, color = "red", group = "Municípios",
                label = ~paste0(nome,": ", valor/1000000) )%>%    
            addLayersControl(
                overlayGroups = c("Municípios", "Microrregião"),
                options = layersControlOptions(collapsed = FALSE))
        
        (mapa_teste) 
        
   
    })
    
    output$tabela <- renderDT({
      
      
      
      teste <- get_brmap( geo = "City",
                          geo.filter = list(MicroRegion = micro()),
                          class = "SpatialPolygonsDataFrame") 
      
      teste_dt<- left_join (teste@data, map_cidades@data, by =  "City")
      datatable((teste_dt) %>% select(ente, populacao, valor)) })
}

# Run the application 
shinyApp(ui = ui, server = server)
