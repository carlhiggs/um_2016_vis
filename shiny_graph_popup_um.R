# install.packages('shiny')
# install.packages('leaflet')
# install.packages('geojsonio')

# MAYBE MINICHARTS? https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html
library(shiny)
library(leaflet)
library(geojsonio)

server <- function(input, output) {
  
  # build data with 2 places
  um_2016_lga <- geojsonio::geojson_read("um_2016_pp_lga.geojson",what = "sp")
  um_2016_lga@data <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
  dat = read.csv("um_2016_pp_overall.csv", header = TRUE)
  left_inches <-  max(strwidth(df[,'lga'], "inch")+0.4, na.rm = TRUE)
  labels = um_2016_lga@data[,'lga']
  bar_data <- as.matrix(t(um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')]))
  bar_legend_x=ncol(bar_data) + 3    
  bar_legend_y=max(colSums(bar_data))
  
  # create area_popup
  area_popup <- paste0("<strong>LGA: </strong>", 
                      um_2016_lga$lga, 
                      "<br><strong>Tree area percent</strong>", 
                      "<br><i>&nbsp&nbspPublic: </i>", 
                      um_2016_lga$peranytree_public,"%", 
                      "<br><i>&nbsp&nbspPrivate: </i>", 
                      um_2016_lga$peranytree_private,"%", 
                      "<br><i>&nbsp&nbspOther: </i>", 
                      um_2016_lga$peranytree_other,"%", 
                     "<br><i>&nbsp&nbspTotal: </i>", 
                      um_2016_lga$peranytree_total,"%", 
                      "<br><strong>Data coverage area: </strong>",
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
  observeEvent(input$Map_shape_click,{
    data_of_click$clickedMarker  <- input$Map_shape_click
    print(paste0(area_of_interest))
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$barplot=renderPlot({
    area_of_interest=data_of_click$clickedMarker$lga
    area_stats=as.numeric(um_2016_lga@data[um_2016_lga@data[,'lga']==area_of_interest,c('peranytree_public','peranytree_private','peranytree_other')])
    area_labels = c(paste0('public (',area_stats[1],'%)'),
                    paste0('private (',area_stats[2],'%)'),
                    paste0('other (',area_stats[3],'%)'))
    if(is.null(area_of_interest)){
        par(mai=c(1.02,left_inches,0.82,0.42))
        # par(xpd=TRUE)
        barplot(bar_data,
                horiz=TRUE,
                xlim = c(0,100),
                col=c('#7fc97f','#beaed4','#fdc086'), 
                border='#ffffff', 
                names.arg=labels,
                las=1,
                legend.text=c("Public","Private","Other"),
                args.legend=list(bty = "n", 
                                 horiz=TRUE, 
                                 x=25,
                                 xjust=0.5,
                                 yjust=0)
                )
                title(paste0("Tree area percent of ",toupper('lga'),"s, by land status."), line=2, adj=0,hjust = -0.25)
    }else{
        par(mai=c(1.02,left_inches,0.82,0.42))
        barplot(bar_data,
                horiz=TRUE,
                xlim = c(0,100),
                col=c('#7fc97f','#beaed4','#fdc086'), 
                border='#ffffff', 
                names.arg=replace(as.vector(labels),labels!=area_of_interest,''),
                las=1,
                legend.text=c("Public","Private","Other"),
                args.legend=list(bty = "n", 
                                 horiz=TRUE, 
                                 x=25,
                                 xjust=0.5,
                                 yjust=0)
                )
                title(paste0("Tree area percent of ",toupper('lga'),"s, by land status."), line=2, adj=0,hjust = -0.25)
    }
  })
}


ui <- fluidPage(
  br(),
  column(8,leafletOutput("map", height="600px")),
  column(4,plotOutput("barplot", height="600px",width="300px")),
  br()
)

shinyApp(ui = ui, server = server)



# # Pie graph
        # area_of_interest = 'Port Stephens'
        # area_stats = c(5.84,3.07,21.90)
        # area_labels = c("public (5.84%)","private (3.07%)","other (21.9%)"  
# pie(x = area_stats,
    # labels=area_labels, 
    # col=c('#7fc97f','#beaed4','#fdc086'),
    # main=area_of_interest,
    # border = '#ffffff')

# # Stacked Bar Plot with Colors and Legend
# head(um_2016_lga@data[order(-um_2016_lga@data$peranytree_total),])  

# df <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
# left_inches <-  max(strwidth(df[,'lga'], "inch")+0.4, na.rm = TRUE)
# par(mai=c(1.02,left_inches,0.82,0.42))
# barplot(as.matrix(t(df[, c('peranytree_public','peranytree_private','peranytree_other')])),horiz=TRUE,xlim = c(0,100),col=c('#7fc97f','#beaed4','#fdc086'), border='#ffffff', names.arg=df[,'lga'],las=1)
# par(mai=c(1.02,left_inches,0.82,0.42))
# barplot(as.matrix(t(um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')])),
                # horiz=TRUE,
                # xlim = c(0,100),
                # col=c('#7fc97f','#beaed4','#fdc086'), 
                # border='#ffffff', 
                # names.arg=replace(as.vector(test),test!='Cessnock',''),
                # las=1)
# text(1,10,'test')