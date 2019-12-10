
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

##########################################################
## compute for modes

tabs_list <- list()
for(mode_number in c(rts_indices,c(1:length(mh_names))[-rts_indices])){
  mh_name <- mh_names[mode_number]
  la_name <- la_names[mode_number]
  aadf_name <- aadf_names[mode_number]
  
  raw_aadf$distance <- raw_aadf$link_length_km*raw_aadf[[aadf_name]]
  
  years <- 2005:max(raw_aadf$year)
  tab <- sapply(regions,function(x) #sapply(c('A','M'),function(y)
  {
    sapply(years,function(yr){
      subtab <- subset(raw_aadf,year==yr&(local_authority_code%in%subset(la_table,cityregion==x)$lad14cd|
                                            local_authority_code%in%subset(la_table,cityregion==x)$lad11cd))
      if(mode_number==1){
        m_dist <- 0
        o_dist <- sum(subtab$distance,na.rm=T)
      }else{
        m_dist <- sum(subset(subtab,road_letter=='M')$distance,na.rm=T)
        o_dist <- sum(subset(subtab,road_letter!='M')$distance,na.rm=T)
      }
      c(m_dist,
        o_dist)
    })
  }
  )*365/1000
  colnames(tab) <- regions
  rownames(tab) <- rep(c('motorway','other'),length(years))
  tab <- cbind(rep(years,each=2),tab)
  
  
  #saveRDS(tab,paste0('outputs/',mh_name,'dist2010to2015.Rds'))
  tabs_list[[mh_name]] <- tab
}
write.csv(do.call(rbind,lapply(1:length(tabs_list),function(x)cbind(names(tabs_list)[x],tabs_list[[x]]))),'outputs/mode_road_city_year.csv')
write.csv(do.call(rbind,lapply(1:length(tabs_list),function(x)cbind(names(tabs_list)[x],tabs_list[[x]]))),'../mh-injury/rds_storage/mode_road_city_year.csv')

