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
                      um_2016_lga$lga, 
                      "<br><strong>Public: </strong>", 
                      um_2016_lga$peranytree_public,"%", 
                      "<br><strong>Private: </strong>",
                      um_2016_lga$peranytree_private,"%", 
                      "<br><strong>Other: </strong>",
                      um_2016_lga$peranytree_other,"%", 
                      "<br><strong>Other: </strong>",
                      um_2016_lga$peranytree_total,"%", 
                      "<br><strong>Other: </strong>",
                      um_2016_lga$fullarea_ha," Ha")
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet(data = um_2016_lga) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(layerId=~lga, 
                fillColor = ~pal(peranytree_total), 
                fillOpacity = 0.7, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = area_popup) %>%
    addLegend("bottomleft", 
              pal = pal, 
              values = ~peranytree_total,
              title = "Total % tree coverage",
              opacity = 1)
  })
  # store the click
  observeEvent(input$map_shape_click,{
    data_of_click$clickedMarker  <- input$map_shape_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$barplot=renderPlot({
    area_of_interest=data_of_click$clickedMarker$lga
    area_stats=as.numeric(um_2016_lga@data[um_2016_lga@data[,'lga']==area_of_interest,c('peranytree_public','peranytree_private','peranytree_other')])
    area_labels = c(paste0('public (',area_stats[1],'%)'),
                    paste0('private (',area_stats[2],'%)'),
                    paste0('other (',area_stats[3],'%)'))
    if(is.null(area_of_interest)){
        area_of_interest = 'Port Stephens'
        area_stats = c(5.84,3.07,21.90)
        area_labels = c("public (5.84%)","private (3.07%)","other (21.9%)"  )
        pie(x = area_stats,
            labels=area_labels, 
            col=c('#7fc97f','#beaed4','#fdc086'),
            main=area_of_interest,
            border = '#ffffff')
        # barplot(as.matrix(area_stats),
                # horiz=TRUE,
                # xlim = c(0,100),
                # col=c('#7fc97f','#beaed4','#fdc086'), 
                # border='#ffffff')
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
  column(4,br(),br(),br(),br(),plotOutput("barplot", height="300px")),
  br()
)

shinyApp(ui = ui, server = server)



# Stacked Bar Plot with Colors and Legend
counts <- table(x = um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')], row.names = um_2016_lga@data[, f1])
barplot(counts, main="Car Distribution by Gears and VS",
  xlab="Number of Gears", col=c("darkblue","red"),
  legend = rownames(counts))

head(um_2016_lga@data[order(-um_2016_lga@data$peranytree_total),])  

df <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
left_inches <-  max(strwidth(df[,'lga'], "inch")+0.4, na.rm = TRUE)
par(mai=c(1.02,left_inches,0.82,0.42))
barplot(as.matrix(t(df[, c('peranytree_public','peranytree_private','peranytree_other')])),horiz=TRUE,xlim = c(0,100),col=c('#7fc97f','#beaed4','#fdc086'), border='#ffffff', names.arg=df[,'lga'],las=1)