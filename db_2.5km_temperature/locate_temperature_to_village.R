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
t.lon <- unique(tmp.coor$Longitude_X)

lon.range <- lapply(1:nrow(v.coor), function(x) {
  lon.dis <- abs(v.coor[x, ]$longitude - t.lon)
  lon.range <- t.lon[which(lon.dis %in% sort(lon.dis, partial = 2)[1 : 2])]
  return(lon.range)
  })

lat.range1 <- lapply(1:nrow(v.coor), function(x) {
  t.lat <- tmp.coor[Longitude_X == lon.range[[x]][1], ]$Latitude_Y
  lat.dis <- abs(v.coor[x, ]$latitude - t.lat)
  lat.range1 <- t.lat[which(lat.dis %in% sort(lat.dis, partial = 2)[1 : 2])]
  return(lat.range1)
})

lat.range2 <- lapply(1:nrow(v.coor), function(x) {
  t.lat <- tmp.coor[Longitude_X == lon.range[[x]][2], ]$Latitude_Y
  lat.dis <- abs(v.coor[x, ]$latitude - t.lat)
  lat.range2 <- t.lat[which(lat.dis %in% sort(lat.dis, partial = 2)[1 : 2])]
  return(lat.range2)
})

v.coor <-  lapply(1:nrow(v.coor), function(x) {
  rs <- list()
  new.v.coor <- cbind(v.coor[x, ], lon.range = lon.range[[x]][1], lat.range = lat.range1[[x]][1])
  rs <- rbind(rs, new.v.coor)
  new.v.coor <- cbind(v.coor[x, ], lon.range = lon.range[[x]][1], lat.range = lat.range1[[x]][2])
  rs <- rbind(rs, new.v.coor)
  new.v.coor <- cbind(v.coor[x, ], lon.range = lon.range[[x]][2], lat.range = lat.range2[[x]][1])
  rs <- rbind(rs, new.v.coor)
  new.v.coor <- cbind(v.coor[x, ], lon.range = lon.range[[x]][2], lat.range = lat.range2[[x]][2])
  rs <- rbind(rs, new.v.coor)
  return(rs)
})
v.coor <- do.call(rbind, v.coor)
save(v.coor, file = 'village_temperature_location.R')
