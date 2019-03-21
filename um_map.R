# necessary libraries
library(rgdal)
library(leaflet)
library(magrittr)
library(knitr)

url_lga = "https://www.dropbox.com/s/eu7lfhc8dexujqb/um_2016_pp_lga.geojson?dl=1"
file <- "um_2016_pp_lga"
download.file(url_lga, file)
um_2016_lga <- rgdal::readOGR(file, "OGRGeoJSON")
pal <- colorBin(palette = "BuPu", domain = um_2016_lga$f4, bins = 8)

# f2 lga
# percent any tree
# f3 other
# fr private
# f5 public

# pop values
area_popup <- paste0("<strong>LGA: </strong>", 
                      um_2016_lga$f2, 
                      "<br><strong>Public: </strong>", 
                      um_2016_lga$f5,"%", 
                      "<br><strong>Private: </strong>",
                      um_2016_lga$f4,"%", 
                      "<br><strong>Other: </strong>",
                      um_2016_lga$f3,"%")
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
