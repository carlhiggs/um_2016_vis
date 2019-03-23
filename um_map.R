library(leaflet)
library(geojsonio)

um_2016_lga <- geojson_read("um_2016_pp_lga.geojson",what = "sp")
um_2016_ssc <- geojson_read("um_2016_pp_ssc.geojson",what = "sp")
um_2016_lga@data <- um_2016_lga@data[order(um_2016_lga@data$peranytree_total),]
overall = read.csv("um_2016_pp_overall.csv", header = TRUE)
left_inches <-  max(strwidth(um_2016_lga@data [,'lga'], "inch")+0.4, na.rm = TRUE)
labels = um_2016_lga@data[,'lga']
bar_data <- as.matrix(t(um_2016_lga@data[, c('peranytree_public','peranytree_private','peranytree_other')]))
bar_legend_x=ncol(bar_data) + 3    
bar_legend_y=max(colSums(bar_data))

labels <- c(0,100)
pal <- colorNumeric(palette = "RdYlGn", domain = range(labels[1],labels[2]))


groupNames <- c(paste0("<span style='color: #000000; font-size: 11pt'><strong>Local Government Area (LGA)</strong></span>"),
                paste0("<span style='color: #000000; font-size: 11pt'><strong>Suburb</strong></span>"))

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
                    
area_popup_ssc <- paste0("<strong>Suburb: </strong>", 
                    um_2016_ssc$ssc_main16, 
                    "<strong>LGA: </strong>", 
                    um_2016_ssc$lga, 
                    "<br><strong>Tree area percent</strong>", 
                    "<br><i>&nbsp&nbspPublic: </i>", 
                    um_2016_ssc$peranytree_public,"%", 
                    "<br><i>&nbsp&nbspPrivate: </i>", 
                    um_2016_ssc$peranytree_private,"%", 
                    "<br><i>&nbsp&nbspOther: </i>", 
                    um_2016_ssc$peranytree_other,"%", 
                   "<br><i>&nbsp&nbspTotal: </i>", 
                    um_2016_ssc$peranytree_total,"%", 
                    "<br><strong>Data coverage area: </strong>",
                    um_2016_ssc$fullarea_sqkm," km<sup>2</sup>",
                    "<br><strong>Total area: </strong>",
                    um_2016_ssc$area_albers_sqkm," km<sup>2</sup>")
                    
save(um_2016_lga,um_2016_ssc,overall,left_inches,labels,bar_data,bar_legend_x,bar_legend_y,labels,pal,groupNames,area_popup_lga,area_popup_ssc, file = "um2016_map_data.RData")