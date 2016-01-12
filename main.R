## Author: Maria Yi
## DAte Jan 12, 2016

library(raster)
library(rgdal)
# download data
setwd("data")
download.file(url = "https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", destfile = "MODIS.zip", method = 'wget')
unzip("MODIS.zip")

# Create a raster layer from modis information
MOD <- stack('MOD13A3.A2014001.h18v03.005.gri')

##MOD_Jan <- subset(MOD,"January", filename = 'Jan_ndvi',overwrite=TRUE)
##MOD_Aug <- subset(MOD,"August", filename = 'Aug_ndvi',overwrite=TRUE)
##MOD_mean <- stackApply(MOD, indices=c(1:12), filename ='mean_ndvi', fun = mean,overwrite=TRUE)



nlMunicipality <- getData('GADM',country='NLD', level=2)
head(nlMunicipality)
nl_mun_mean <- extract(MOD, nlMunicipality, along = TRUE, fun= mean)

cityname <- nlMunicipality$NAME_2
nl_city <- data.frame(name = cityname, nl_mun_mean)

# find the the greenest city in January over the Netherlands
MOD_Jan <- data.frame(cityname = cityname, Jan_ndvi =nl_city$January)
max_Jan <- MOD_Jan[which.max(MOD_Jan$Jan_ndvi), ]

# find the the greenest city in August over the Netherlands
MOD_Aug <- data.frame(cityname = cityname, Aug_ndvi =nl_city$August)
max_Aug <- MOD_Aug[which.max(MOD_Aug$Aug_ndvi), ]

# find the the greenest city all the year over the Netherlands
means <- rowMeans(nl_city[,2:13])
MOD_mean <-data.frame(cityname = cityname, mean_ndvi = means)
max_mean <- MOD_mean[which.max(MOD_mean$mean_ndvi), ]

# make map
opar <- par(mfrow=c(1,1))
Jan_MOD <- subset(MOD,1)
Aug_MOD <- subset(MOD,8)

Nl_UTM<- spTransform(nlMunicipality, CRS(proj4string(MOD)))
Jan_Contour <- Nl_UTM[Nl_UTM$NAME_2 == max_Jan$cityname,]

Jan_greenest <- mask(Jan_MOD, Jan_Contour)

plot(Jan_Contour)
title(main = 'Greenest city in January')
plot(Jan_greenest,add = T)
plot(Jan_Contour,border = "red", lwd = 5,add = T)
text(Jan_Contour,max_Jan$cityname,cex = 1)





