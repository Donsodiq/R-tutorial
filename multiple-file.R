#making map with tmap function

setwd("C:/Users/HP/Desktop/IdeaSpaceLab/reprojection/District_Azimuthal")

data <- file.choose()
df <- st_read(data)
as.matrix(df)
View(df)

#add shape and fill the color
tm_shape(df) + 
  tm_fill(col = "orange")

tm_shape(df) +
  tm_borders(col = "black", lty = "solid", lwd = 2, alpha = 0.8)

tm_shape(df) +
  tm_polygons("DISTRICT") +
  tm_legend( show = FALSE)


library(tmap)

setwd("C:/Users/HP/Desktop/IdeaSpaceLab/Geoprocessing/Dang_Local_Level")
data <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/Geoprocessing/Dang_Local_Level/Dang_Local_Level.shp")

as.matrix(data)

#add shape and fill the color
tm_shape(data) + tm_fill()
tm_shape(data) + tm_fill(col = "yellow", alpha = 0.6)

#adding border of the layer
tm_shape(data) + tm_borders(col = "black", lwd = 2, lty = "solid", alpha = 0.7)

#adding polygons
tm_shape(data) +
  tm_polygons("PALIKA")+
  tm_legend(show = FALSE)


#plotting multiple files
setwd("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap")

library(sf)
library(raster)
library(tmap)


dang <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap/dang.shp")
points <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap/points.shp")
road <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap/road.shp")
river <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap/river.shp")
dem <- raster("C:/Users/HP/Desktop/IdeaSpaceLab/tmap/tmap/dem.tif")

tmap_mode("plot")


tm_shape(dem) + 
  tm_raster(palette = terrain.colors(10)) +
  tm_shape(dang) +
  tm_borders(col = "black", lty = "solid", lwd = 1.5, alpha = 0.5) +
  tm_text(text = "GN_TYPE", size = 0.8, col = "black")+
  tm_shape(road) +
  tm_lines(col = "brown", lwd = 1)+
  tm_shape(river) +
  tm_lines(col = "blue", lwd = 1.5)+
  tm_shape(points)+
  tm_symbols(col = "yellow", size = 1)+
  tm_compass(north = 0, type = "8star", size = 4, position = c("right", "top"))+
  tm_graticules(x = NA, y= NA, n.x =3, n.y= 3, labels.show = TRUE, labels.size = 0.5)+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_scale_bar(
    breaks = c(0, 10, 20, 30, 40, 50),
    position = c("left", "bottom")
  )


getwd()
setwd("C:/Users/HP/Desktop/IdeaSpaceLab/Raster/dang_pop")
#getwd()

#setwd("C:/Users/HP/Desktop/IdeaSpaceLab/tmap")
library(sf)
library(raster)
library(tmap)
library(RColorBrewer)

dang <- st_read("C:/Users/HP/Desktop/IdeaSpaceLab/Raster/dang_pop/dang_pop.shp")
head(dang)
tail(dang)
View(dang)

bubble_sizes <- ((c(10, 20, 30, 40)*1e4)/25e4)*0.5*2
bubble_sizes

tm_shape(dang)+
  tm_polygons()+
  tm_shape(dang)+
  tm_symbols(col="Ttl_Ppl", 
             breaks = c(0, 5, 10, 15, 25)*1e4,
             n=4,
             palette = "YlOrRd",
             size = "Ttl_Ppl",
             sizes.legend = c(10,20,30,40)*1e4,
             size.lim = c(0,25e4),
             scale = 2,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.size.is.portrait = TRUE,
             )+ tm_add_legend("symbol",
             col = RColorBrewer::brewer.pal(4, "YlOrRd"),
             border.col = "grey80",
             size = bubble_sizes,
             labels = c("0-100k", "100-200k", "200-300k", "300-400k"),
             title = "Population of Dang",
                              
              ) + tm_layout(
                legend.position = c("left", "bottom")
              )

#Bounding-Box

dang_box <- st_bbox(dang)
dang_box

#getting the range
xrange <- dang_box$xmax -dang_box$xmin
yrange <- dang_box$ymax - dang_box$ymin

xrange
yrange

dang_box[3] <- dang_box[3] + (0.35*xrange)
dang_box[2] <- dang_box[2] - (0.25*yrange)

dang_box <- dang_box%>%
  st_as_sfc()

bubble_sizes <- ((c(10, 20, 30, 40)*1e4)/25e4)*0.5*2
bubble_sizes

#tm_shape(dang, bbox = dang_box)+
 # tm_polygons()

#layout mapping

tm_shape(dang, bbox = dang_box)+
  tm_polygons()+
  tm_symbols(col="Ttl_Ppl", 
             breaks = c(0, 5, 10, 15, 25)*1e4,
             n=4,
             palette = "YlOrRd",
             size = "Ttl_Ppl",
             sizes.legend = c(10,20,30,40)*1e4,
             size.lim = c(0,25e4),
             scale = 2,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.size.is.portrait = TRUE,
  )+tm_graticules(
    x = NA, y= NA, n.x =3, n.y= 3, labels.show = TRUE, 
    labels.size = 0.5)+tm_add_legend("symbol",
                                     col = RColorBrewer::brewer.pal(4, "YlOrRd"),
                                     border.col = "grey80",
                                     size = bubble_sizes,
                                     labels = c("0-100k", "100-200k", "200-300k", "300-400k"),
                                     title = "Population of Dang",)+  
  tm_compass(north = 0, type = "8star", size = 4, position = c("right", "top"))+
  tm_scale_bar(
    breaks = c(0, 10, 20, 30, 40, 50),text.color = "red", lwd = 1,
    position = c("left", "bottom"), text.size = 1
  )+ tm_layout(
    title = "Population Distribution",
    title.position = c("center", "top"),
    title.size = 1,
    legend.position = c("right", "bottom"),
    bg.color = "skyblue"
  )
































