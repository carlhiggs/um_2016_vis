# install.packages('shiny')
# install.packages('leaflet')
# install.packages('geojsonio')


library(shiny)
library(leaflet)
library(geojsonio)

server <- function(input, output) {
  
  # build data with 2 places
  um_2016_lga <- geojsonio::geojson_read("um_2016_pp_lga.geojson",what = "sp")
  
  # create area_popup
  area_popup <- paste0("<strong>LGA: </strong>", 
                      um_2016_lga$f2, 
                      "<br><strong>Public: </strong>", 
                      um_2016_lga$f5,"%", 
                      "<br><strong>Private: </strong>",
                      um_2016_lga$f4,"%", 
                      "<br><strong>Other: </strong>",
                      um_2016_lga$f3,"%")
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet(data = um_2016_lga) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(layerId=~f2, 
                fillColor = ~pal(f4), 
                fillOpacity = 0.7, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = area_popup) %>%
    addLegend("bottomleft", 
              pal = pal, 
              values = ~f4,
              title = "Public % tree coverage",
              opacity = 1)
  })
  # store the click
  observeEvent(input$map_shape_click,{
    data_of_click$clickedMarker  <- input$map_shape_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlot({
    area_of_interest=data_of_click$clickedMarker$f2
    area_stats=as.numeric(um_2016_lga@data[um_2016_lga@data[,'f2']==area_of_interest,c('f5','f4','f3')])
    area_labels = c(paste0('public/n',area_stats[1]),
                    paste0('private/n',area_stats[2]),
                    paste0('other/n',area_stats[3]))
    if(is.null(area_of_interest)){
        area_of_interest="no area selected"
    }else{
        pie(x = area_stats,
            labels=area_labels, 
            col=c('#7fc97f','#beaed4','#fdc086'),
            main=area_of_interest,
            border = '#ffffff')
    }
  })
}


ui <- fluidPage(
  br(),
  column(8,leafletOutput("map", height="600px")),
  column(4,br(),br(),br(),br(),plotOutput("plot", height="300px")),
  br()
)

shinyApp(ui = ui, server = server)
