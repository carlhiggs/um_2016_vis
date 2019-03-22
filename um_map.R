# necessary libraries
library(rgdal)
library(leaflet)
library(magrittr)
library(knitr)

# url_lga = "https://www.dropbox.com/s/eu7lfhc8dexujqb/um_2016_pp_lga.geojson?dl=1"
# file <- "um_2016_pp_lga"
# download.file(url_lga, file)
# um_2016_lga <- rgdal::readOGR(file, "OGRGeoJSON")
# pal <- colorBin(palette = "BuPu", domain = um_2016_lga$f4, bins = 8)


um_2016_lga <- geojsonio::geojson_read("um_2016_pp_lga.geojson",what = "sp")
um_2016_lga@data <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
dat = read.csv("um_2016_pp_overall.csv", header = TRUE)
left_inches <-  max(strwidth(df[,'lga'], "inch")+0.4, na.rm = TRUE)
labels = um_2016_lga@data[,'lga']
bar_data <- as.matrix(t(um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')]))
bar_legend_x=ncol(bar_data) + 3    
bar_legend_y=max(colSums(bar_data))
# pop values
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
# plot the map
leaflet(data = um_2016_lga) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(f4), 
              fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = area_popup) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values = ~f4,
            title = "Public % tree coverage",
            opacity = 1)
