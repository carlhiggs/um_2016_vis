# install.packages('shiny')
# install.packages('leaflet')
# install.packages('geojsonio')

# MAYBE MINICHARTS? https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html
library(shiny)
library(leaflet)
library(geojsonio)

server <- function(input, output) {
  # build data with 2 places
  um_2016_lga <- geojson_read("um_2016_pp_lga.geojson",what = "sp")
  # um_2016_ssc <- geojson_read("um_2016_pp_ssc.geojson",what = "sp")
  um_2016_lga@data <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
  um_2016_overall = read.csv("um_2016_pp_overall.csv", header = TRUE)
  left_inches <-  max(strwidth(um_2016_lga@data [,'lga'], "inch")+0.4, na.rm = TRUE)
  bar_data <- as.matrix(t(um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')]))
  bar_legend_x <- ncol(bar_data) + 3    
  bar_legend_y <- max(colSums(bar_data))
  bar_labels <- um_2016_lga@data[,'lga']

  legend_domain <- range(0,100)
  pal <- colorNumeric(palette = "RdYlGn", domain = legend_domain)

  
  groupNames <- c(paste0("<span style='color: #000000; font-size: 11pt'><strong>Total</strong></span>"),
                  paste0("<span style='color: #000000; font-size: 11pt'><strong>Public</strong></span>"),
                  paste0("<span style='color: #000000; font-size: 11pt'><strong>Private</strong></span>"),
                  paste0("<span style='color: #000000; font-size: 11pt'><strong>Other</strong></span>"))
  
  # create area_popup
  area_popup_lga <- paste0("<strong>LGA: </strong>", 
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
                      um_2016_lga$fullarea_sqkm," km<sup>2</sup>",
                      "<br><strong>Total area: </strong>",
                      um_2016_lga$area_albers_sqkm," km<sup>2</sup>")
                      
  # area_popup_ssc <- paste0("<strong>Suburb: </strong>", 
                      # um_2016_ssc$ssc_main16, 
                      # "<strong>LGA: </strong>", 
                      # um_2016_ssc$lga, 
                      # "<br><strong>Tree area percent</strong>", 
                      # "<br><i>&nbsp&nbspPublic: </i>", 
                      # um_2016_ssc$peranytree_public,"%", 
                      # "<br><i>&nbsp&nbspPrivate: </i>", 
                      # um_2016_ssc$peranytree_private,"%", 
                      # "<br><i>&nbsp&nbspOther: </i>", 
                      # um_2016_ssc$peranytree_other,"%", 
                     # "<br><i>&nbsp&nbspTotal: </i>", 
                      # um_2016_ssc$peranytree_total,"%", 
                      # "<br><strong>Data coverage area: </strong>",
                      # um_2016_ssc$fullarea_sqkm," km<sup>2</sup>",
                      # "<br><strong>Total area: </strong>",
                      # um_2016_ssc$area_albers_sqkm," km<sup>2</sup>")
  
  # load data
  # load("um2016_map_data.RData")
  
  # create a reactive value that will store the click position
  # data_of_click <- reactiveValues(clickedMarker=NULL)
  data_of_hover <- reactiveValues(hoverFeature=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>% 
    # leaflet(data = um_2016_lga) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = um_2016_lga,
                layerId = ~lga,
                fillOpacity = 0.7, 
                color = ~pal(peranytree_total), 
                weight = 1, 
                popup = area_popup_lga,
                group=groupNames[1]) %>%
    # addPolygons(data = um_2016_lga,
                # layerId = ~lga + 'Public',
                # fillOpacity = 0.7, 
                # color = ~pal(peranytree_public), 
                # weight = 1, 
                # popup = area_popup_lga,
                # group=groupNames[2]
                # # , options = pathOptions(clickable = FALSE)
                # ) %>%
    # addPolygons(data = um_2016_lga,
                # layerId = ~lga + 'Private',
                # fillOpacity = 0.7, 
                # color = ~pal(peranytree_private), 
                # weight = 1, 
                # popup = area_popup_lga,
                # group=groupNames[3]
                # # , options = pathOptions(clickable = FALSE)
                # ) %>%
    # addPolygons(data = um_2016_lga,
                # layerId = ~lga + 'Other',
                # fillOpacity = 0.7, 
                # color = ~pal(peranytree_other), 
                # weight = 1, 
                # popup = area_popup_lga,
                # group=groupNames[4]
                # # , options = pathOptions(clickable = FALSE)
                # ) %>%
    # define layers and layers tool bar
    addLayersControl(
      baseGroups = groupNames,
      options = layersControlOptions(collapsed = FALSE))%>% ## display legend by default
    addScaleBar(position = "bottomleft", options = scaleBarOptions()) %>%
    addLegend("bottomright", 
              pal = pal, 
              values = legend_domain,
              title = "Total % tree coverage",
              opacity = 0.6)
  })
  
  # observe({
    # req(input$var)
    # leafletProxy("map") %>%
      # addPolygons(data = um_2016_lga,
                  # layerId=~lga, 
                  # fillOpacity = 0.7, 
                  # color = ~pal(input$var), 
                  # weight = 1, 
                  # popup = area_popup_lga,
                  # group=groupNames[1]) 
  # })
  
  # # store the click
  # observeEvent(input$map_shape_click,{
    # data_of_click$clickedMarker  <- input$map_shape_click
    # # print(paste0(data_of_click$clickedMarker))
  # })
  # store the hover
  observeEvent(input$map_shape_mouseover,{
    data_of_hover$hoverFeature  <- input$map_shape_mouseover
    # print(paste0(data_of_hover$hoverFeature))
    # area_of_interest=data_of_hover$hoverFeature[1]
    # area_stats=as.numeric(um_2016_lga@data[um_2016_lga@data[,'lga']==area_of_interest,c('peranytree_public','peranytree_private','peranytree_other')])
    # print(paste0(area_stats))
  })
  
  # # Make a barplot or scatterplot depending of the selected point
  # output$barplot=renderPlot({
    # area_of_interest=data_of_click$clickedMarker[1]
    # area_stats=as.numeric(um_2016_overall[,c('peranytree_public','peranytree_private','peranytree_other')])
    # area_labels = c(paste0('public (',area_stats[1],'%)'),
                    # paste0('private (',area_stats[2],'%)'),
                    # paste0('other (',area_stats[3],'%)'))
    # if(is.null(area_of_interest)){
        # par(mai=c(1.02,left_inches,0.82,0.42))
        # barplot(bar_data,
                # horiz=TRUE,
                # xlim = c(0,100),
                # col=c('#7fc97f','#beaed4','#fdc086'), 
                # border='#ffffff', 
                # names.arg=bar_labels,
                # las=1,
                # legend.text=c("Public","Private","Other"),
                # args.legend=list(bty = "n", 
                                 # horiz=TRUE, 
                                 # x=50,
                                 # xjust=0.5,
                                 # yjust=0)
                # )
                # title(paste0("Tree area percent of ",toupper('lga'),"s, by land status."), line=2, adj=0)
    # }else{
        # par(mai=c(1.02,left_inches,0.82,0.42))
        # barplot(bar_data,
                # horiz=TRUE,
                # xlim = c(0,100),
                # col=c('#7fc97f','#beaed4','#fdc086'), 
                # border='#ffffff', 
                # names.arg=replace(as.vector(bar_labels),bar_labels!=area_of_interest,''),
                # las=1,
                # legend.text=c("Public","Private","Other"),
                # args.legend=list(bty = "n", 
                                 # horiz=TRUE, 
                                 # x=25,
                                 # xjust=0.5,
                                 # yjust=0)
                # )
                # title(paste0("Tree area percent of ",toupper('lga'),"s, by land status."), line=2, adj=0)
        # # pie(x = area_stats,
            # # labels=area_labels, 
            # # col=c('#7fc97f','#beaed4','#fdc086'),
            # # main=area_of_interest,
            # # border = '#ffffff')
    # }
  # })
  output$pieplot=renderPlot({
    area_of_interest=data_of_hover$hoverFeature[1]
    print(area_of_interest)
    if(is.null(area_of_interest)){
        area_stats=as.numeric(um_2016_overall[,c('peranytree_public','peranytree_private','peranytree_other')])
        area_labels = c(paste0('public (',area_stats[1],'%)'),
                    paste0('private (',area_stats[2],'%)'),
                    paste0('other (',area_stats[3],'%)'))
        pie(x = area_stats,
        labels=area_labels, 
        col=c('#7fc97f','#beaed4','#fdc086'),
        main="Tree coverage %\nSydney",
        border = '#ffffff')
    }else{
        area_stats=as.numeric(um_2016_lga@data[um_2016_lga@data[,'lga']==area_of_interest,c('peranytree_public','peranytree_private','peranytree_other')])
        area_labels = c(paste0('public (',area_stats[1],'%)'),
                    paste0('private (',area_stats[2],'%)'),
                    paste0('other (',area_stats[3],'%)'))
        pie(x = area_stats,
        labels=area_labels, 
        col=c('#7fc97f','#beaed4','#fdc086'),
        main="test",
        border = '#ffffff')
    }
  })
}


ui <- fluidPage(
  # selectInput("var", label = "Select a variable", choices = c('peranytree_public','peranytree_private','peranytree_other','peranytree_total')),
  br(),
  column(8,leafletOutput("map"), height="600px"),
  column(4,plotOutput("pieplot", width="300px")),
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