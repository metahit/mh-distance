rm(list=ls())
library(data.table)
setwd('~/overflow_dropbox/mh-distance/')

city_regions_table <- read.csv('../mh-execute/inputs/mh_regions_lad_lookup.csv',stringsAsFactors = F)
city_regions <- unique(city_regions_table$cityregion)
city_regions <- city_regions[city_regions!='']
city_las <- city_regions_table$lad11cd[city_regions_table$cityregion%in%city_regions]
la_city_indices <- sapply(city_las,function(x) which(city_regions==city_regions_table$cityregion[city_regions_table$lad11cd==x]))

## get raw rc files
matrix_path <- '../mh-execute/inputs/travel-matrices/'
matrix_files <- list.files(matrix_path)
rc_matrix_files <- matrix_files[sapply(matrix_files,function(x)grepl('rc',x))]
raw_rc_mat_list <- list()
for(i in 1:5){ # 5 modes
  raw_rc_mat_list[[i]] <- list()
  for(j in 1:2){ # 2 urban/rural labels
    raw_rc_mat_list[[i]][[j]] <- list()
    # find out how many distance categories
    distance_categories <- sum(sapply(matrix_files,function(x)grepl(paste0('mode',i,'_u',j-1),x)))/2
    for(k in 1:distance_categories){ # up to 4 distance categories
      d_bit <- paste0('d',k) #ifelse(distance_categories==1,'',paste0('d',k))
      filename <- paste0(matrix_path,'mode',i,'_u',j-1,d_bit,'_matrc.csv')
      if(file.exists(filename)){
        mat_file <- read.csv(filename,stringsAsFactors = F)
        raw_rc_mat_list[[i]][[j]][[k]] <- mat_file
      }
    }
  }
}

## get raw la files
la_matrix_files <- matrix_files[sapply(matrix_files,function(x)grepl('la',x))]
raw_la_mat_list <- list()
for(i in 1:5){
  raw_la_mat_list[[i]] <- list()
  for(j in 1:2){
    raw_la_mat_list[[i]][[j]] <- list()
    # find out how many distance categories
    distance_categories <- sum(sapply(matrix_files,function(x)grepl(paste0('mode',i,'_u',j-1),x)))/2
    for(k in 1:distance_categories){
      d_bit <- paste0('d',k) #ifelse(distance_categories==1,'',paste0('d',k))
      filename <- paste0(matrix_path,'mode',i,'_u',j-1,d_bit,'_matla.csv')
      if(file.exists(filename)){
        mat_file <- read.csv(filename,stringsAsFactors = F)
        raw_la_mat_list[[i]][[j]][[k]] <- mat_file
      }
    }
  }
}

## make matrices uniform
# define home las
home_las <- city_regions_table$lad14cd #city_las #c('E06000022','E06000023','E06000024','E06000025') # 
# get all las in rc
rc_las <- unique(c(home_las,unique(unlist(lapply(raw_rc_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)y[,1])))))))
# get all las in la
dest_las <- unique(c(home_las,unique(unlist(lapply(raw_la_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)colnames(y)[-1])))))))
# get all road names in rc
roadnames <- unique(unlist(lapply(raw_rc_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)colnames(y)[-1])))))
# define ordered set of unique las
las <- unique(c(rc_las,dest_las))
destination_las <- c(home_las, 'none')
origin_las <- las

## make rc matrices
rc_mat_list <- list()
for(i in 1:5){
  rc_mat_list[[i]] <- list()
  for(j in 1:2){
    rc_mat_list[[i]][[j]] <- list()
    for(k in 1:length(raw_rc_mat_list[[i]][[j]])){
      rc_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=length(roadnames),nrow=length(home_las))
      row_j <- match(home_las,raw_rc_mat_list[[i]][[j]][[k]][,1])
      row_i <- home_las %in% raw_rc_mat_list[[i]][[j]][[k]][,1] # match(raw_rc_mat_list[[i]][[j]][[k]][,1],home_las)
      row_i <- row_i[!is.na(row_i)]
      row_j <- row_j[!is.na(row_j)]
      col_i <- roadnames %in% colnames(raw_rc_mat_list[[i]][[j]][[k]])[-1] # match(colnames(raw_rc_mat_list[[i]][[j]][[k]])[-1],roadnames)
      rc_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_rc_mat_list[[i]][[j]][[k]][row_j,-1])
    }
  }
}

## make la matrices
la_mat_list <- list()
for(i in 1:5){
  la_mat_list[[i]] <- list()
  for(j in 1:2){
    la_mat_list[[i]][[j]] <- list()
    for(k in 1:length(raw_la_mat_list[[i]][[j]])){
      la_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=length(destination_las),nrow=length(origin_las))
      min_dim <- min(dim(la_mat_list[[i]][[j]][[k]]))
      augment_la_mat <- raw_la_mat_list[[i]][[j]][[k]]
      augment_la_mat$none <- rowSums(augment_la_mat[,!colnames(augment_la_mat)%in%c(destination_las,'lahome')])
      row_i <- origin_las %in% augment_la_mat[,1] # match(augment_la_mat[,1],origin_las)
      #la_mat_list[[i]][[j]][[k]] <- matrix(0,nrow=length(las),ncol=length(las))
      #row_i <- match(raw_la_mat_list[[i]][[j]][[k]][,1],las)
      row_i <- row_i[!is.na(row_i)]
      col_i <- destination_las %in% colnames(augment_la_mat)[-1] # match(colnames(augment_la_mat)[-1],destination_las)
      col_i <- col_i[!is.na(col_i)]
      #la_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_la_mat_list[[i]][[j]][[k]][,-1])
      row_j <- match(origin_las,augment_la_mat[,1])
      row_j <- row_j[!is.na(row_j)]
      col_j <- match(destination_las,colnames(augment_la_mat))
      col_j <- col_j[!is.na(col_j)]
      la_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(augment_la_mat[row_j,col_j])
      diag(la_mat_list[[i]][[j]][[k]]) <- 1 - (rowSums(la_mat_list[[i]][[j]][[k]][1:min_dim,1:min_dim] ) - diag(la_mat_list[[i]][[j]][[k]]))
    }
  }
}


## get synthetic populations
synth_pops <- list()
synth_pop_path <- '../mh-execute/inputs/scenarios/'
synth_pop_files <- list.files(synth_pop_path)
synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('SPind_E[[:digit:]]+.Rds',x))]
#synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
# set to data table
for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- setDT(readRDS(paste0(synth_pop_path,synth_pop_files[i])))
# take subset of columns
for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- 
  synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                          function(x)x%in%c('census_id','urbanmatch','demogindex')|
                            ((grepl('cycle',x)|grepl('walk',x))&grepl('wkhr',x))|
                            ((grepl('base',x)|grepl('scen',x))&grepl('wkkm',x))
  ),with=F]
# rename
la_names <- sapply(synth_pop_files,function(x)gsub('SPind_','',x))
la_names <- sapply(la_names,function(x)gsub('.Rds','',x))
names(synth_pops) <- la_names
number_city_las <- length(synth_pops)

## add 'mini' to all cities, to all scenarios, for distance_for_cas, distance_for_strike, distance_for_emission, distance_for_noise
synth_pop_supp_path <- '../mh-execute/inputs/scenarios-mini/'
synth_pop_supp_files <- list.files(synth_pop_supp_path)
synth_pop_supp_files <- synth_pop_supp_files[sapply(synth_pop_supp_files,function(x)grepl('SPind_E[[:digit:]]+.Rds',x))]
#synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
# set to data table
for(i in number_city_las+1:length(synth_pop_supp_files)) synth_pops[[i]] <- setDT(readRDS(paste0(synth_pop_supp_path,synth_pop_supp_files[i-number_city_las])))
# take subset of columns
for(i in number_city_las+1:length(synth_pop_supp_files)) synth_pops[[i]] <- 
  synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                          function(x)x%in%c('census_id','urbanmatch','demogindex')|
                            ((grepl('cycle',x)|grepl('walk',x))&grepl('wkhr',x))|
                            (grepl('base',x)&grepl('wkkm',x))
  ),with=F]
# rename
la_names_supp <- sapply(synth_pop_supp_files,function(x)gsub('SPind_','',x))
la_names_supp <- sapply(la_names_supp,function(x)gsub('.Rds','',x))
names(synth_pops)[number_city_las+1:length(synth_pop_supp_files)] <- la_names_supp

modes <- c('cycle','walk','cardrive','mbikedrive','bus')
mode_list <- list('cycle','walk',c('cardrive'),'mbikedrive','bus')
driven_modes <- c(modes)
passenger_modes <- list('cycle','walk',c('carpass','taxi'),'mbikepass','bus')
add_modes <- c('cardrive','mbikedrive')
which_add_modes <- which(driven_modes%in%add_modes)

all_distances <- list()
scenarios <- c('base_','scen_')
scenario <- 'base_'
dist_cats_per_mode <- sapply(raw_rc_mat_list,function(x)sapply(x,length))[1,]
raw_la_mat_list <- raw_rc_mat_list <- NULL

# keep only present scenario
for(i in 1:number_city_las) synth_pops[[i]] <- 
  synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                          function(x)x%in%c('census_id','urbanmatch','demogindex')|
                            grepl(scenario,x)),with=F]
# rename scenario
for(i in 1:length(synth_pops)) colnames(synth_pops[[i]]) <- sapply(colnames(synth_pops[[i]]),function(x)gsub(scenario,'',x))

# noise : total distance per mode per LA
which_modes <- !driven_modes%in%c('walk')
modes <- driven_modes[which_modes]
dist_cats <- dist_cats_per_mode[which_modes]
cols <- unlist(lapply(1:length(modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(modes[x],'_wkkm_d',y))))
# expand by mode, dist_cat, home_la, including all synth pops
distance_sums <- lapply(1:length(synth_pops),function(i)synth_pops[[i]][,.(
  dist=sapply(.SD,sum),
  mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
  distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
  la_index=i,
  mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
),by=urbanmatch,.SDcols=cols])
distance_sums <- rbindlist(distance_sums)
# map to destination las
for(i in 1:length(destination_las)) distance_sums[,destination_las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]



distance_sums <- distance_sums[,lapply(.SD,sum),by=c('mode','mode_name','distcat','urbanmatch'),.SDcols=destination_las]
# allocate distance_sums to roadtypes

for(i in 1:length(roadnames)) 
  for(j in 1:length(home_las)){
    la_col <- which(colnames(distance_sums)==home_las[j])
    distance_sums[[paste0(home_las[j],roadnames[i])]] <- distance_sums[[la_col]]*apply(distance_sums[,c(1,3,4)],1,function(x)rc_mat_list[[x[1]]][[x[3]+1]][[as.numeric(x[2])]][j,i])
    
  }
# get names for la+road combinations
la_road <- sapply(home_las,function(x)paste0(x,roadnames))
# sum in la and road
temp_distance <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=la_road]
# reorganise to long form
reorganise <- list()
colnms <- colnames(temp_distance)
for(i in 1:length(home_las)) {
  rdnms <- sapply(colnms,function(x)gsub(home_las[i],'',x))
  reorganise[[i]] <- temp_distance[,rdnms%in%c(roadnames,'mode_name'),with=F]
  colnames(reorganise[[i]]) <- c('mode_name',roadnames)
  reorganise[[i]]$la <- home_las[i]
}
distance_totals <- rbindlist(reorganise)
distance_totals <- distance_totals[rowSums(distance_totals[,2:7])>0,]

england_sum <- distance_totals[,lapply(.SD,sum),by='mode_name',.SDcols=roadnames]

rts_data <- readxl::read_xlsx('inputs/five_road_data.xlsx')
rts_data <- subset(rts_data,Year>2009)
rts_data <- rts_data[,-1]
rts_data[,1] <- as.character(rts_data[[1]])
for(i in 2:ncol(rts_data)) rts_data[,i] <- as.numeric(rts_data[[i]])
rts_sums <- sapply(unique(rts_data$`Road Type`),function(x) colSums(subset(rts_data,`Road Type`==x)[,2:8]))


compare_rts <- rts_sums[c(7,4,1,5),]/6/52/1e3

compare_sp <- compare_rts
compare_sp[,1] <- unlist(england_sum[c(1,3,2,4),7])/1e6
compare_sp[,2] <- unlist(england_sum[c(1,3,2,4),4])/1e6
compare_sp[,3] <- unlist(england_sum[c(1,3,2,4),6])/1e6
compare_sp[,4] <- unlist(england_sum[c(1,3,2,4),3])/1e6
compare_sp[,5] <- unlist(england_sum[c(1,3,2,4),5])/1e6


write.csv(rbind('RTS',compare_rts,'SP',compare_sp),'outputs/compare_rts_sp.csv')
