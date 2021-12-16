library(tidyverse)
library(leaflet)
library(sf)
library(ggmap)
library(rgdal)
library(shiny)
library(shinydashboard)
library(reactable)
library(units)
library(htmltools)
library(stringr)

census_sf <- read_sf('data/2010_CensusData/2010_CensusData.shp') %>%
  st_transform("WGS84")
school <- st_read('data/School_Boundaries/School_Boundaries.shp')
#abandon <- st_read('Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp')
#parks <- read.csv('Parks_Locations_and_Features.csv')
#business <- read.csv('Business_Licenses_geocoded.csv')
#lights <- read.csv('/data/Street_Lights.csv')

#lights.spatial <- lights %>% st_as_sf(coords = c("Lon", "Lat")) %>%
#  st_set_crs(value=4326)

schools <- readOGR('data/School_Boundaries/School_Boundaries.shp', layer = 'School_Boundaries', GDAL1_integer64_policy = TRUE)
census_layer <- readOGR('data/2010_CensusData/2010_CensusData.shp', layer = '2010_CensusData', GDAL1_integer64_policy = TRUE)


#START - RYAN
# DATASET - POINTS
street_lights_sf <- read_sf("data/Street_Lights.csv") %>% 
  st_as_sf(
    coords = c('Lon', 'Lat'),
    agr = "constant",
    crs = "WGS84",
    stringsAsFactors = FALSE,
    remove = TRUE
  )


street_lights_sf<- street_lights_sf %>% 
  mutate(Lumens = as.numeric(gsub('[[:punct:] ]+','', stringr::str_extract(Lumens, '\\d{1,2},?\\d{3}'))))

lights <- street_lights_sf %>% select(-c(geometry)) 

# DATASET - Polygons
schools_sf <- read_sf("data/School_Boundaries/School_Boundaries.shp") %>%
  st_transform("WGS84") %>% 
  mutate(area = set_units(st_area(.), mi^2))

#distance_output <- st_is_within_distance(street_lights_sf, schools_sf, dist = 10,sparse = TRUE)
#distance_output
lights_within_distance_school <- st_join(street_lights_sf, schools_sf, join=st_is_within_distance, dist=1000) #tibble::enframe(distance_output, name = 'LightID', value = 'School') %>% tidyr::unnest(School)
lights_within_distance_school <-lights_within_distance_school %>% janitor::clean_names()

schools_tibble <- as_tibble(schools_sf) %>% janitor::clean_names()
schools_tibble$objectid <- as.character(schools_tibble$objectid)
schools_tibble$School <- 1:nrow(schools_tibble)

school_count <- lights_within_distance_school %>% count(school) %>% 
  arrange(desc(n)) %>% 
  rename('count'='n') %>% 
  left_join(schools_tibble,by='school')

lights_within_distance_census<- st_join(street_lights_sf, census_sf, join = st_within)
lights_within_distance_census <- lights_within_distance_census %>% janitor::clean_names()

census_tibble <- as_tibble(census_sf) %>% janitor::clean_names()

lights_within_distance_census_viz <- lights_within_distance_census %>% count(namelsad) %>%  
  left_join(dplyr::select(census_tibble,namelsad,se_t001_00,se_t002_01),by='namelsad') %>% 
  rename('Total_Population'='se_t001_00','Population_Density'='se_t002_01','lights'='n')


school_within_census<- st_join(schools_sf, census_sf %>% select(NAMELSAD, Total_Population = SE_T001_00) , join = st_within)
school_within_census <- school_within_census %>% janitor::clean_names()
school_within_census
type <- unique(school_count$school_type)

#END - RYAN

server <- function(input, output) {
  
  #Page 1 Map
  output$map <- renderLeaflet({
    pal <- colorNumeric(
      palette = c('#2A637B', '#95F9C3'),
      domain = street_lights_sf$Lumens)
    leaflet(schools)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data = street_lights_sf, color = ~pal(Lumens), 
                       stroke = 0, fillOpacity = 1, radius = 4,
                       group = 'Lights') %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, popup = school$School,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = 'Schools') %>%
      addPolygons(data = census_layer, color = '#FF0000', weight = 2, opacity = 1.0, fillOpacity = 0.1, popup = census_sf$NAMELSAD,
                  layerId = census_sf$NAMELSAD,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = 'Census Tract') %>%
      addLayersControl(overlayGroups = c('Lights', 'Schools', 'Census Tract'),options = layersControlOptions(collapsed = FALSE),
                       position = 'bottomleft') %>%
      addLegend('bottomright', pal = pal, values = ~street_lights_sf$Lumens,
                title = "Light Brightness in Lumens", opacity = 1)
    
  })
  
  #All Pages - KPI Card 1
  output$value1a <- output$value1b <- output$value1c <- renderValueBox({
    valueBox(
      formatC(sum(census_sf$SE_T001_00), format="d", big.mark=',')
      ,subtitle = 'Total Population'
      ,icon = icon("user",lib='glyphicon')
      ,color = "green")  
  })
  
  #All Pages - KPI Card 2
  output$value2a <- output$value2b <- output$value2c <- renderValueBox({
    valueBox(
      formatC(school %>% nrow(), format="d", big.mark=',')
      ,subtitle = 'Total Schools'
      ,icon = icon("school")
      ,color = "yellow")  
  })
  
  #All Pages - KPI Card 3
  output$value3a <- output$value3b <- output$value3c <- renderValueBox({
    valueBox(
      formatC(lights %>% nrow(), format="d", big.mark=',')
      ,subtitle = 'Total Lights'
      ,icon = icon("lightbulb")
      ,color = "blue")  
  })
  
  #Page 3 - Interactive Table
  output$datatable = renderReactable({
    reactable(as.data.frame(school) %>% dplyr::select(c(-geometry, -OBJECTID)),
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 15), defaultPageSize = 10,
              details = function(index) {
                lights <- lights_within_distance_school %>% filter(., as.data.frame(school)  %>% slice(index) %>% pull(school)== school) %>%
                  select(c(object_id = objectid_x, ownership, service, wattage, lumens)) %>% st_drop_geometry()
                htmltools::div(style = "padding: 16px",
                               reactable(lights, outlined = TRUE,  defaultPageSize = 5)
                )
              }, 
              onClick = JS("function(rowInfo, colInfo) {
                //Send Event to Shiny
                if (window.Shiny) {
                  Shiny.setInputValue('show_details', { school: rowInfo.row }, { priority: 'event' })
                }
              }"))
  })
  
  #Page 2 - Bar Chart
  output$plot1 <- renderPlot({
    data <-  school_count %>%
      filter(school_type %in% input$selects)
    ggplot(data,mapping=aes(x=reorder(school,count),y=count)) +
      geom_bar(stat = "identity",fill = "#FFE900",) +
      theme_bw() +
      coord_flip() + 
      ylab('Number of Lights') + 
      xlab('Name of School')
  })
  
  #Page 2 - Scatter Plot
  output$plot2 <- renderPlot({
    data2 <-  lights_within_distance_census_viz %>% 
      filter(Total_Population > input$slider[1]) %>% 
      filter(Total_Population < input$slider[2]) 
    ggplot(data2 , aes(x=Population_Density, y=lights)) +
      geom_point(size=4,color='#E53D00') + 
      theme_bw() + 
      xlab('Population Density') + 
      ylab('Number of Lights')
  })
  
  observeEvent(input$show_details, { 
    p <- input$show_details

    #if click id isn't null render the table
    if(!is.null(p$school$`_index`)){
      selected_school <- lights_within_distance_school %>% filter(school == p$school$School)
      selected_census <- school_within_census %>% filter(school == p$school$School)
      
      output$value1c <- renderValueBox({
        valueBox(
          formatC(sum(selected_census$Total_Population), format="d", big.mark=',')
          ,subtitle = 'Total Population'
          ,icon = icon("user",lib='glyphicon')
          ,color = "green")  
      })
      output$value2c <- renderValueBox({
        valueBox(
          formatC(1, format="d", big.mark=',')
          ,subtitle = 'Total Schools'
          ,icon = icon("school")
          ,color = "yellow")  
      })
      output$value3c <- renderValueBox({
        valueBox(
          formatC(selected_school %>% nrow(), format="d", big.mark=',')
          ,subtitle = 'Total Lights'
          ,icon = icon("lightbulb")
          ,color = "blue")  
      })

    }
  })
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    
    if(!is.null(p$id)){
      selected_lights <- lights_within_distance_census %>% filter(namelsad == p$id)
      selected_census <- school_within_census %>% filter(namelsad == p$id)
      
      output$value1a <- renderValueBox({
        valueBox(
          formatC(sum(selected_census$Total_Population), format="d", big.mark=',')
          ,subtitle = 'Total Population'
          ,icon = icon("user",lib='glyphicon')
          ,color = "green")  
      })
      output$value2a <- renderValueBox({
        valueBox(
          formatC(selected_census %>% nrow(), format="d", big.mark=',')
          ,subtitle = 'Total Schools'
          ,icon = icon("school")
          ,color = "yellow")  
      })
      output$value3a <- renderValueBox({
        valueBox(
          formatC(selected_lights %>% nrow(), format="d", big.mark=',')
          ,subtitle = 'Total Lights'
          ,icon = icon("lightbulb")
          ,color = "blue")  
      })
    }
  })
  
  observeEvent(input$clear1 |input$clear3, {
    output$value1a <-output$value1c <- renderValueBox({
      valueBox(
        formatC(sum(census_sf$SE_T001_00), format="d", big.mark=',')
        ,subtitle = 'Total Population'
        ,icon = icon("user",lib='glyphicon')
        ,color = "green")  
    })
    output$value2a <-output$value2c <- renderValueBox({
      valueBox(
        formatC(school %>% nrow(), format="d", big.mark=',')
        ,subtitle = 'Total Schools'
        ,icon = icon("school")
        ,color = "yellow")  
    })
    output$value3a <-output$value3c <- renderValueBox({
      valueBox(
        formatC(lights %>% nrow(), format="d", big.mark=',')
        ,subtitle = 'Total Lights'
        ,icon = icon("lightbulb")
        ,color = "blue")  
    })
  })
}

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "South Bend School Lighting", titleWidth = 330)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "dashboard1", icon = icon("map")),
    menuItem("Stats", tabName = "dashboard2", icon = icon("stats",lib="glyphicon")),
    menuItem("Raw Data", tabName = "dashboard3", icon = icon("dashboard"))
    
  )
)

frow1a <- fluidRow(
  valueBoxOutput("value1a")
  ,valueBoxOutput("value2a")
  ,valueBoxOutput("value3a"),
  width = 12
)

frow1b <- fluidRow(
  valueBoxOutput("value1b")
  ,valueBoxOutput("value2b")
  ,valueBoxOutput("value3b"),
  width = 12
)

frow1c <- fluidRow(
  valueBoxOutput("value1c")
  ,valueBoxOutput("value2c")
  ,valueBoxOutput("value3c"),
  width = 12
)

frow2 <- fluidRow(
  box(
    title = "Map of Light Poles in South Bend"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = F 
    ,actionButton("clear1", "Clear Selection", class = "btn-action")
    ,leafletOutput('map', height= 575), 
    width = 12
  ) 
)

frow3 <- fluidRow(
  box(
    title = "Raw Data"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = F
    ,width = 12
    ,actionButton("clear3", "Clear Selection", class = "btn-action")
    ,reactableOutput('datatable')
  )
)

frow_charts <- fluidRow(
  column(width = 6,
         box(
           title = "Number of Lights Near Schools", width = NULL, solidHeader = TRUE, status = "primary",
           plotOutput("plot1", height = 400)
         ),
         box(
           title = "Explore Differences Between Public and Private School's Lighting",
           radioButtons("selects", "Choose School Type:",
                        list("Public", "Private"),
                        selected = "Public",),
           width = 250,height = 300)
  ),
  
  column(width = 6,
         box(
           title = "Number of Lights by Population Density", width = NULL, solidHeader = TRUE, status = "primary",
           plotOutput("plot2", height = 400)
         ),
         box(
           title = "Select a Range for the Total Population of Each Census Tract",
           sliderInput("slider", "Number of Residents:", min = 0, max = max(lights_within_distance_census_viz$Total_Population),value=c(0,max(lights_within_distance_census_viz$Total_Population))),
           width = 250,height = 300)
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard1", frow1a, frow2),
    tabItem(tabName = "dashboard2",frow1b, frow_charts),
    tabItem(tabName = "dashboard3",frow1c, frow3)
    
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
  
)

ui <- dashboardPage(title = 'South Bend Schools', header, sidebar, body)



shinyApp(ui, server)