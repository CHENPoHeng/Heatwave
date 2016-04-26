len = length
library(data.table)
library(dplyr)

# load in village coordinate
load('village_coor.Rdata')
tmp <- village.coor
i <- names(tmp)[grep(names(tmp), pattern = '.code|lon|lat')]
tmp <- tmp[, i]
# remove NA
i <- which(is.na(tmp$village.code))
tmp <- tmp[-i, ]
tmp$dist.code <- substr(tmp$dist.code, 1, 7)
tmp$village.code <- sprintf('%03d', tmp$village.code)
tmp$village.code <- paste0(tmp$dist.code, '-', tmp$village.code)
# remove duplicate
tmp <- tmp[!duplicated(tmp$village.code),]
tmp <- as.data.table(tmp)
tmp$city.code <- as.character(tmp$city.code)
# format city, dist, village codes
tmp[substr(village.code, 1, 5) > 20000, 
     village.code := paste0(substr(village.code, 1, 3), substr(village.code, 6, 7), '00', substr(village.code, 8, 11))]
tmp[substr(village.code, 1, 5) == '10003', 
     village.code := paste0('680', substr(village.code, 6, 7), '00', substr(village.code, 8, 11))]
tmp[substr(village.code,1,5) < 20000, dist.code := substr(village.code,1,7)]
tmp[substr(village.code,1,5) > 20000, dist.code := substr(village.code,1,7)]
tmp[dist.code < 2000000, city.code := substr(dist.code,1,5)]
tmp[dist.code > 2000000, city.code := paste0(substr(dist.code,1,3),'00')]
village.coor <- tmp
v.coor <- village.coor

# load in temperature data
d <- fread('2008/2008/200801.csv')


# initialize unique temperature coordinate 
tmp.coor <- d[1:max(d$Locat_ID), ]

# calculate the closest temperature coordinate to each village
temp.to.village.coor <-   lapply(1:nrow(v.coor), function(x) {
  error.lon <- (v.coor[x, ]$longitude - tmp.coor$Longitude_X)
  error.lat <- (v.coor[x, ]$latitude - tmp.coor$Latitude_Y)
  rmse <- sqrt((error.lon^2 + error.lat ^2) / 2)
  tmp.coor[which.min(rmse), ]
  })

temp.to.village.coor <- do.call(rbind, temp.to.village.coor)
v.coor <- cbind(v.coor, temp.to.village.coor[, 2:3, with = F])

save(v.coor, file = 'village_temperature_location.R')