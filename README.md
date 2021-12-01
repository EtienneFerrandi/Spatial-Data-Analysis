# Spatial Data Analysis

This work for Laterice aims at localizing GPS coordinates of the coffee production areas on a map of Ethiopia. 

We calculate the coffee tree productivity as the total yield per household per year (in kg), divided by the number of arabica coffee trees. Then we project on a raster a scale of the results. Some areas are more productive than others.

```ruby

library(dplyr)
library(tidyverse)
library(sf)
library(mapview)
library(terra)
library(tmap)
library(rgdal)
library(raster)
library(ggplot2)
library(gstat)
table = read.csv("~/coffee_farmer_dataset.csv")
#calculation of coffee_productivity
coffee_productivity=c(table$washed_arabica_production/table$coffeetrees)
table=cbind(table, coffee_productivity)
#calculation of the average
mean_cof_prod=mean(coffee_productivity, na.rm = TRUE)
#calculation of the median
med_cof_prod=median(coffee_productivity, na.rm = TRUE)
dif_from_med=c(table$coffee_productivity-med_cof_prod)
table=cbind(table,dif_from_med)

#projection of GPS coordinates on a world map
position=data.frame(table$gpslatitude_deidentified,table$gpslongitude_deidentified)
map=as_tibble(position)
map=map%>% drop_na()
locations=st_as_sf(map, coords = c('table.gpslongitude_deidentified', 'table.gpslatitude_deidentified'), crs = 4326)
mapview(locations)

#creation of a dataset with three columns (longitude, latitude and productivity)
xyz=data.frame(table$gpslongitude_deidentified,table$gpslatitude_deidentified,table$coffee_productivity)
colnames(xyz)=c('x','y','Productivity')
xyz=xyz%>% drop_na()
#transformation of the df in a Spatial Point DataFrame
sp::coordinates(xyz)=~x+y
#calculation of the empirical variogram
vario_Productivity=variogram(Productivity~1, data = xyz)
plot(vario_Productivity)
#Gaussion model fitting
vario=fit.variogram(vario_Productivity, model = vgm(psill = 2, model = "Sph", range = 30, nugget = 0.5))
#Geostat object that describes all the characteristics of the model
geoX=gstat(formula = m~1, locations = xyz, model = vario)
spplot(geoX)

#Spatial interpolation between the coffee_productivity data and the raster of Ethiopia
grd=as.data.frame(spsample(xyz, n=100, type = "regular"))
names(grd)=c("X","Y")
coordinates(grd)=c("X","Y")
gridded(grd)=TRUE
fullgrid(grd)=TRUE

proj4string(xyz)=proj4string(xyz)
proj4string(grd)=proj4string(xyz)

xyz.idw=gstat::idw(Productivity~1,xyz,newdata=grd,idp=2.0)
r=raster(xyz.idw)
tm_shape(r) +
  tm_raster(n=10,palette = 'Reds', auto.palette.mapping = FALSE, title='coffee_productivity')+
  tm_shape(xyz) + tm_dots(size = 0.2)+
  tm_legend(legend.outside=TRUE)

```
