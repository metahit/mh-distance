
library(dplyr)
library(xlsx)
setwd('~/overflow_dropbox/mh-distance')
## AADFs
raw_aadf <- read.csv('inputs/dft_traffic_counts_aadf.csv',stringsAsFactors = F)
dim(raw_aadf)
colnames(raw_aadf)
unique(raw_aadf$year)
raw_aadf$road_letter <- sapply(raw_aadf$road_category,function(x)strsplit(x,'')[[1]][2])
raw_aadf$link_length_km <- as.numeric(raw_aadf$link_length_km)

## city regions definitions
la_table <- read.csv('../mh-execute/inputs/mh_regions_lad_lookup.csv')
regions <- unique(la_table$cityregion)
regions <- regions[regions!='']

## la distances
la_dist <- read.xlsx('inputs/VehicleType_LALevel.xlsx',sheetIndex = 1,rowIndex = 6:1670)
la_dist$LA_Name <- as.character(la_dist$LA_Name)
la_dist$LA_Name[la_dist$LA_Name=='Bristol'] <- 'Bristol, City of'## compare to RTS
names(la_table)[2] <- 'LA_Name'
la_dist <- left_join(la_dist,la_table[,c(1:4)],by='LA_Name')
##!! LAs we've missed
print(subset(la_dist,is.na(lad11cd)))
##!! city-region LAs we've missed
print(subset(la_table,!lad14cd%in%la_dist$lad14cd&cityregion!=''))
## but we do have
print(subset(la_dist,cityregion=='nottingham'))

## names
aadf_names <- c("pedal_cycles","two_wheeled_motor_vehicles","cars_and_taxis","buses_and_coaches","lgvs","all_hgvs")
la_names <- c("Pedal.Cycles","Two.Wheeled.Motor.Vehicles", "Car","Bus","LGV","HGV")
mh_names <- c('bicycle','motorcycle','car','bus','lgv','hgv')
rts_indices <- c(3,5,6)

## get most recent RTS values
for(i in 1:length(rts_indices)){
  rts_estimates <- read.xlsx('inputs/190918_data_from_RTS.xlsx',sheetIndex=i+1,rowIndex = 3:48)
  #rownames(road_dist) <- sapply(rownames(road_dist),function(x)tolower(gsub(' ','',x)))
  #citymap <- list(bristol='bristol',
  #                nottingham='',
  #                liverpool='liverpoolcityregioncombinedauthority',
  #                northeast='northeastcombinedauthority',
  #                greatermanchester='greatermanchestercombinedauthority',
  #                sheffield='sheffieldcityregioncombinedauthority',
  #                westmidlands='westmidlandscombinedauthority',
  #                leeds='westyorkshirecombinedauthority',
  #                london='london')
  if(i==1){
    rts_estimates$NA. <- tolower(rts_estimates$NA.)
    rts_estimates$Road.Type <- as.character(rts_estimates$Road.Type)
    rts_estimates$NA.[rts_estimates$NA.=='greater manchester combined authority'] <- 'greatermanchester'
    rts_estimates$NA.[rts_estimates$NA.=='liverpool city region combined authority'] <- 'liverpool'
    rts_estimates$NA.[rts_estimates$NA.=='north east combined authority'] <- 'northeast'
    rts_estimates$NA.[rts_estimates$NA.=='sheffield city region combined authority'] <- 'sheffield'
    rts_estimates$NA.[rts_estimates$NA.=='west midlands combined authority'] <- 'westmidlands'
    rts_estimates$NA.[rts_estimates$NA.=='west yorkshire combined authority'] <- 'leeds'
    rts_estimates$Road.Type[rts_estimates$Road.Type=='Rural B,C or Unclassified'] <- 'Rural minor'
    rts_estimates$Road.Type[rts_estimates$Road.Type=='Urban B,C or Unclassified'] <- 'Urban minor'
    rts_est <- rts_estimates[,c(1,2,9)]
    colnames(rts_est) <- c('city','road',mh_names[rts_indices[i]])
  }else {
    rts_est[[mh_names[rts_indices[i]]]] <- rts_estimates[,9]
  }
}

#######################################################
## urban fraction of A roads

buff <- 0
if(file.exists(paste0('inputs/urban_road_fraction_',buff,'.Rds'))&file.exists(paste0('inputs/urban_road_points_',buff,'.Rds'))){
  road_df <- readRDS(paste0('inputs/urban_road_fraction_',buff,'.Rds'))
  point_df <- readRDS(paste0('inputs/urban_road_points_',buff,'.Rds'))
}else{
  library(rgdal)
  library(raster)
  library(rgeos)
  library(spatialEco)
  road_shape <- readOGR(dsn = "shapefiles", layer = "2018-MRDB-minimal")
  urban_shape <- readOGR(dsn = "shapefiles", layer = "Builtup_Areas_December_2011_Boundaries_V2")
  
  crs_string <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  
  urban_shp <- spTransform(urban_shape,CRS(crs_string))
  road_shp <- spTransform(road_shape,CRS(crs_string))
  urban_shape_urban <- urban_shp[urban_shp$urban_bua=='Yes',]
  urban_shp_buffered <- buffer(urban_shape_urban,buff)
  
  minor_road_coords <- raw_aadf[,c(1,16,17)]
  coordinates(minor_road_coords) <- c('longitude','latitude')
  proj4string(minor_road_coords) <- CRS("+proj=longlat")
  minor_road_coords <- spTransform(minor_road_coords,CRS(crs_string))
  point_shape <- point.in.poly(minor_road_coords,urban_shp_buffered)
  point_df <- point_shape@data
  colnames(point_df)[2] <- 'urban_point'
  point_df$urban_point[is.na(point_df$urban_point)] <- 0
  saveRDS(point_df,paste0('inputs/urban_road_points_',buff,'.Rds'))
  
  pdf('buffered_urban_area.pdf'); par(mar=c(1,1,1,1))
  plot(urban_shp_buffered,xlim=c(520000 , 550000),ylim=c( 150000,  240000))
  lines(urban_shape_urban,xlim=c(520000 , 550000),ylim=c( 150000,  240000),col='red',lty=2)
  #points(minor_road_coords[!is.na(point_shape@data$poly.ids),],cex=0.5,pch=16,col='grey')
  dev.off()
  
  urban_road <- raster::intersect(road_shp,urban_shp_buffered)
  
  urban_df <- as.data.frame(urban_road)
  urban_df$urban_length <- gLength(urban_road,byid=T)
  road_df <- as.data.frame(road_shp)
  road_df$length <- gLength(road_shp,byid=T)
  road_df <- left_join(road_df,urban_df,by=c('CP_Number','RoadNumber'))
  road_df$urban_length[is.na(road_df$urban_length)] <- 0
  #road_df <- road_df[,colnames(road_df)%in%c('length','urban_length','CP_Number')]
  road_df$rural_length <- road_df$length - road_df$urban_length
  road_df$urban_fraction <- road_df$urban_length/road_df$length
  colnames(road_df)[1] <- 'count_point_id'
  saveRDS(road_df,paste0('inputs/urban_road_fraction_',buff,'.Rds'))
}
raw_aadf <- left_join(raw_aadf,road_df,by='count_point_id')
raw_aadf <- left_join(raw_aadf,point_df,by='count_point_id')

##########################################################
## compute for modes

city_list <- list()
for(x in regions){
  tabs_list <- list()
  for(mode_number in c(rts_indices,c(1:length(mh_names))[-rts_indices])){
    mh_name <- mh_names[mode_number]
    la_name <- la_names[mode_number]
    aadf_name <- aadf_names[mode_number]
    
    raw_aadf$distance <- raw_aadf$link_length_km*raw_aadf[[aadf_name]]
    raw_aadf$urban_distance <- raw_aadf$distance * raw_aadf$urban_fraction
    raw_aadf$rural_distance <- raw_aadf$distance * (1-raw_aadf$urban_fraction)
    
    ## get sum of travel for A and M for 2010-2015
    la_codes <- unique(c(as.character(subset(la_table,cityregion==x)$lad14cd),as.character(subset(la_table,cityregion==x)$lad11cd)))
    tab <- t(sapply(la_codes,function(y) #sapply(c('A','M'),function(y)
    {
      subtab <- subset(raw_aadf,year%in%2010:2015&local_authority_code==y)
      if(mode_number==1){
        m_dist <- 0
        r_dist <- sum(subset(subtab,road_letter%in%c('A','M'))$rural_distance,na.rm=T) 
        u_dist <- sum(subset(subtab,road_letter%in%c('A','M'))$urban_distance,na.rm=T)
      }else{
        m_dist <- sum(subset(subtab,road_letter=='M')$distance,na.rm=T)
        r_dist <- sum(subset(subtab,road_letter=='A')$rural_distance,na.rm=T)
        u_dist <- sum(subset(subtab,road_letter=='A')$distance,na.rm=T) - r_dist
      }
      total <- sum(subset(la_dist,lad11cd==y&Year>2009&Year<2016)[[la_name]])
      c(m_dist,
        u_dist,
        r_dist,
        total/365*1.6)
    }
    ))*365/1000
    rownames(tab) <- la_codes
    colnames(tab) <- c('Motorway','Urban A','Rural A','total')
    
    tabs_list[[mh_name]] <- tab
  }
  city_list[[x]] <- do.call(rbind,lapply(1:length(tabs_list),function(x)cbind(names(tabs_list)[x],tabs_list[[x]])))
  write.csv(city_list[[x]],paste0('outputs/mode_road_',x,'.csv'))
}
write.csv(do.call(rbind,city_list),'outputs/mode_road_city_las.csv')

city_tab <- do.call(rbind,city_list)
la_names <-  sapply(rownames(city_tab),function(x)unique(c(as.character(la_table$LA_Name[la_table$lad11cd==x]),as.character(la_table$LA_Name[la_table$lad14cd==x]))))
city_tab <- cbind(city_tab,la_names)
colnames(city_tab)[5] <- 'LA_Name'
