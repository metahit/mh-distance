{
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
home_las <- city_las #c('E06000022','E06000023','E06000024','E06000025') # 
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


## duplicate car for motorbike
#la_mat_list[[5]] <- la_mat_list[[3]]
#rc_mat_list[[5]] <- rc_mat_list[[3]]

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
}
for(scenario in scenarios){
  {
    # copy synthetic population
  synth_pops_scen <- copy(synth_pops)
  # keep only present scenario
  for(i in 1:number_city_las) synth_pops_scen[[i]] <- 
      synth_pops_scen[[i]][,sapply(colnames(synth_pops_scen[[i]]),
                              function(x)x%in%c('census_id','urbanmatch','demogindex')|
                                grepl(scenario,x)),with=F]
  # rename scenario
  for(i in 1:number_city_las) colnames(synth_pops_scen[[i]]) <- sapply(colnames(synth_pops_scen[[i]]),function(x)gsub(scenario,'',x))
  # rename base for non-city la synthetic populations (they don't have scenarios)
  for(i in (number_city_las+1):length(synth_pops_scen)) colnames(synth_pops_scen[[i]]) <- sapply(colnames(synth_pops_scen[[i]]),function(x)gsub('base_','',x))
  # remove scenario from core synthetic population
  for(i in 1:number_city_las) synth_pops[[i]] <- synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),function(x)!grepl(scenario,x)),with=F]
  
  # noise : total distance per mode per LA
  which_modes_noisy <- !driven_modes%in%c('cycle','walk')
  noisy_modes <- driven_modes[which_modes_noisy]
  dist_cats <- dist_cats_per_mode[which_modes_noisy]
  cols <- unlist(lapply(1:length(noisy_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(noisy_modes[x],'_wkkm_d',y))))
  # expand by mode, dist_cat, home_la, including all synth pops
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=urbanmatch,.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  # map to destination las
  for(i in 1:length(destination_las)) distance_sums[,destination_las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  # sum in destination las
  distance_for_noise <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=destination_las]
  
  # emission : total distance per mode per road type per LA
  # collapse to target las
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
  temp_distance_for_emission <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=la_road]
  # reorganise to long form
  reorganise <- list()
  colnms <- colnames(temp_distance_for_emission)
  for(i in 1:length(home_las)) {
    rdnms <- sapply(colnms,function(x)gsub(home_las[i],'',x))
    reorganise[[i]] <- temp_distance_for_emission[,rdnms%in%c(roadnames,'mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name',roadnames)
    reorganise[[i]]$la <- home_las[i]
  }
  distance_for_emission <- rbindlist(reorganise)
  
  
  # injury rate : total distance per mode per road type per LA per demographic group
  ## rename columns
  #for(i in 1:length(synth_pops_scen)) colnames(synth_pops_scen[[i]])[colnames(synth_pops_scen[[i]])=='walk_wkkm'] <- 'walk_wkkm_d1'
  which_modes_strike <- 1:5
  strike_modes <- driven_modes[which_modes_strike]
  dist_cats <- dist_cats_per_mode[which_modes_strike]
  cols <- unlist(lapply(1:length(strike_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(strike_modes[x],'_wkkm_d',y))))
  # expand by mode, dist_cat, home_la, demographic, including all synth pops
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=c('urbanmatch','demogindex'),.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  # map to home las
  for(i in 1:length(home_las)) distance_sums[,home_las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  # map to road types, sum over cities
  for(i in 1:length(roadnames))
    for(k in 1:length(city_regions)){
      temp_dist <- rep(0,length=nrow(distance_sums))
      for(l in 1:sum(la_city_indices==k)){
        j <- which(home_las==c(city_las[la_city_indices==k])[l])
        la_col <- which(colnames(distance_sums)==home_las[j])
        temp_dist <- temp_dist + distance_sums[[la_col]]*distance_sums[,rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][j,i],by=c('mode','dist','distcat','urbanmatch','demogindex','la_index')]$V1
      }
      distance_sums[[paste0(city_regions[k],roadnames[i])]] <- temp_dist
    }

  # get city road labels
  city_road <- sapply(city_regions,function(x)paste0(x,roadnames))
  # sum over city roads
  temp_distance_for_strike <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=city_road]
  # reorganise to long form
  reorganise <- list()
  colnms <- colnames(temp_distance_for_strike)
  for(i in 1:length(city_regions)) {
    rdnms <- sapply(colnms,function(x)gsub(city_regions[i],'',x))
    reorganise[[i]] <- temp_distance_for_strike[,rdnms%in%c(roadnames,'demogindex','mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name','demogindex',roadnames)
    reorganise[[i]]$city_region <- city_regions[i]
  }
  distance_for_strike <- rbindlist(reorganise)
  
  ## casualty distances
  ## add drive to passenger
  for(i in 1:length(synth_pops_scen))
    for(j in 1:length(add_modes)){
      dist_cats <- dist_cats_per_mode[which_add_modes[j]]
      for(k in 1:dist_cats){
        add_col <- paste0(passenger_modes[[which_add_modes[j]]],'_wkkm_d',k)
        target_col <- paste0(add_modes[j],'_wkkm_d',k)
        synth_pops_scen[[i]][[target_col]] <- synth_pops_scen[[i]][[target_col]] + rowSums(synth_pops_scen[[i]][,colnames(synth_pops_scen[[i]])%in%add_col,with=F])
      }
    }
  
  dist_cats <- dist_cats_per_mode
  cols <- unlist(lapply(1:length(driven_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(driven_modes[x],'_wkkm_d',y))))
  # expand by mode, dist_cat, home_la, demographic, including all synth pops
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=c('urbanmatch','demogindex'),.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  # map to home las
  for(i in 1:length(home_las)) distance_sums[,home_las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  # map to road types, sum over cities
  for(i in 1:length(roadnames))
    for(k in 1:length(city_regions)){
      temp_dist <- rep(0,length=nrow(distance_sums))
      for(l in 1:sum(la_city_indices==k)){
        j <- which(home_las==c(city_las[la_city_indices==k])[l])
        la_col <- which(colnames(distance_sums)==home_las[j])
        temp_dist <- temp_dist + distance_sums[[la_col]]*distance_sums[,rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][j,i],by=c('mode','dist','distcat','urbanmatch','demogindex','la_index')]$V1
      }
      distance_sums[[paste0(city_regions[k],roadnames[i])]] <- temp_dist
    }
  # get city road labels
  city_road <- sapply(city_regions,function(x)paste0(x,roadnames))
  # sum over city roads
  temp_distance_for_cas <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=city_road]
  # reorganise to long form
  reorganise <- list()
  colnms <- colnames(temp_distance_for_cas)
  for(i in 1:length(city_regions)) {
    rdnms <- sapply(colnms,function(x)gsub(city_regions[i],'',x))
    reorganise[[i]] <- temp_distance_for_cas[,rdnms%in%c(roadnames,'demogindex','mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name','demogindex',roadnames)
    reorganise[[i]]$city_region <- city_regions[i]
  }
  distance_for_cas <- rbindlist(reorganise)
  temp_dist <- NULL
  
  
  # physical activity : duration per person per mode, including only city las
  pa_modes <- c('cycle','walk')
  pa_pops <- list()
  for(i in 1:number_city_las) pa_pops[[i]] <- synth_pops_scen[[i]][,.(census_id=census_id,
                                                                              walk_wkhr=walk_wkhr,
                                                                              cycle_wkhr=cycle_wkhr,
                                                                              la=i)]
  #cols <- sapply(pa_modes,function(x)paste0('base_',x,'_wkhr'))
  distance_for_pa <- rbindlist(pa_pops)
  pa_pops <- NULL
    
  
  # pollution inhalation : duration per person per mode per road type per LA (we've already added passenger to driver)
  inh_modes <- driven_modes
  cols <- unlist(lapply(1:length(inh_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(inh_modes[x],'_wkkm_d',y))))
  distance_for_inh <- list()
  }
  # go city by city
  city <- 'london'
  for(city in city_regions){
    # should include only city las
    one_city_las <- which(names(synth_pops_scen)%in%city_regions_table$lad11cd[city_regions_table$cityregion==city])
    if(length(one_city_las)>0){
      # expand by mode, dist_cat, home_la, participant_id
      distance_sums <- lapply(one_city_las,function(i){ # lapply(1:length(synth_pops_scen),function(i){
        melt2 <- melt(synth_pops_scen[[i]],id.vars=c('census_id','urbanmatch'),measure=patterns(paste0('^',inh_modes,'_wkkm')),variable.name='distcat',value.name=paste0('mode',inh_modes), variable.factor=F)
        melt3 <- melt(melt2,id.vars=c('census_id','urbanmatch','distcat'),measure=patterns('^mode'),variable.name='mode_name',value.name='dist', variable.factor=F)
        melt3$la_index <- i
        melt3$mode <- 1
        for(j in inh_modes){
          modej <- paste0('mode',j)
          melt3$mode_name[melt3$mode_name==modej] <- j
          melt3$mode[melt3$mode_name==j] <- which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,j)))))
        }
        melt3
      })
      distance_sums <- do.call(rbind,distance_sums)
      distance_sums <- distance_sums[distance_sums$dist>0,]
      distance_sums <- distance_sums[!is.na(distance_sums$dist),]
      # map to home city las
      for(i in 1:length(one_city_las)) 
        distance_sums[,home_las[one_city_las[i]]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][la_index,one_city_las[i]]),by=c('mode','dist','distcat','urbanmatch')]
      temp_distance_for_inh <- list()
      for(i in 1:length(roadnames)){
        distance_sums_temp <- copy(distance_sums)
        for(j in 1:length(one_city_las)){
          la_col <- which(colnames(distance_sums_temp)==home_las[one_city_las[j]])
          pos_indices <- distance_sums_temp[[la_col]]>0
          #distance_sums[[paste0(home_las[one_city_las[j]],roadnames[i])]] <- 0
          if(sum(pos_indices)>0)
            distance_sums_temp[[la_col]][pos_indices] <- 
            distance_sums_temp[[la_col]][pos_indices]*distance_sums_temp[pos_indices,rc_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][j,i],by=c('mode','dist','distcat','urbanmatch','census_id','la_index')]$V1
        }
        print(i)
        temp_distance_for_inh[[i]] <- distance_sums_temp[,lapply(.SD,sum),by=c('mode_name','census_id'),.SDcols=home_las[one_city_las]]
      }
      distance_sums_temp <- distance_sums <- NULL
      # get road la names
      #la_road <- sapply(home_las[one_city_las],function(x)paste0(x,roadnames))
      # sum over dist cats
      #temp_distance_for_inh <- distance_sums[,lapply(.SD,sum),by=c('mode_name','census_id'),.SDcols=la_road]
      # reorganise into long form
      reorganise <- list()
      colnms <- home_las[one_city_las]
      for(i in 1:length(one_city_las)) {
        colnm <- colnms[i]
        temp_distance_for_inh[[1]][[roadnames[1]]] <- temp_distance_for_inh[[1]][,colnames(temp_distance_for_inh[[1]])==colnm,with=F][[colnm]]
        #rdnms <- home_las[one_city_las[i]] # sapply(colnms,function(x)gsub(home_las[one_city_las[i]],'',x))
        reorganise[[i]] <- temp_distance_for_inh[[1]][,colnames(temp_distance_for_inh[[1]])%in%c(roadnames[1],'census_id','mode_name'),with=F]
        for(j in 2:length(roadnames)){
          temp_distance_for_inh[[j]][[roadnames[j]]] <- temp_distance_for_inh[[j]][,colnames(temp_distance_for_inh[[j]])==colnm,with=F][[colnm]]
          reorganise[[i]][[roadnames[j]]] <- temp_distance_for_inh[[j]][,colnames(temp_distance_for_inh[[j]])==roadnames[j],with=F][[roadnames[j]]]
        }
        #temp_distance_for_inh[[i]] <- c()
        keep_rows <- rowSums(reorganise[[i]][,2+1:length(roadnames),with=F])>0
        reorganise[[i]] <- reorganise[[i]][keep_rows,]
        colnames(reorganise[[i]]) <- c('mode_name','census_id',roadnames)
        reorganise[[i]]$la <- home_las[one_city_las[i]]
      }
      temp_distance_for_inh <- NULL
      distance_for_inh[[city]] <- rbindlist(reorganise)
    }
  }


  saveRDS(distance_for_inh$london,paste0('../mh-execute/inputs/distances/',scenario,'london_inh_distances.Rds'))
  distance_for_inh$london <- NULL
  saveRDS(distance_for_inh,paste0('../mh-execute/inputs/distances/',scenario,'inh_distances.Rds'))
  distance_for_inh <- c()
  saveRDS(distance_for_pa,paste0('../mh-execute/inputs/distances/',scenario,'pa_distances.Rds'))
  distance_for_pa <- c()
  saveRDS(list(distance_for_cas=distance_for_cas,distance_for_strike=distance_for_strike),paste0('../mh-execute/inputs/distances/',scenario,'injury_distances.Rds'))
  saveRDS(list(distance_for_emission=distance_for_emission,distance_for_noise=distance_for_noise),paste0('../mh-execute/inputs/distances/',scenario,'emissions_distances.Rds'))

}
###################################################################################
## a look at the distances

all_distances <- readRDS('outputs/all_distances.Rds')


str_dist <- all_distances$base_$distance_for_strike
distance_sums <- sapply(colnames(str_dist)[3:8],
       function(y) sapply(unique(str_dist$mode_name),
                         function(x) sapply(city_regions,function(z)
                           c(sum(subset(str_dist,mode_name==x&city_region==z)[[y]]))
                         )
       )
       )
cbind(rep(unique(str_dist$mode_name),each=length(city_regions)),rep(city_regions,times=length(unique(str_dist$mode_name))),distance_sums*52*6/1000)


path_to_injury_model_and_data <- '../mh-execute/inputs/injury/'
injury_table <- readRDS(paste0(path_to_injury_model_and_data,'processed_injuries_9.Rds'))

m_strike_car <- sum(all_distances[[1]]$distance_for_strike$motorway[all_distances[[1]]$distance_for_strike$demogindex<100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
f_strike_car <- sum(all_distances[[1]]$distance_for_strike$motorway[all_distances[[1]]$distance_for_strike$demogindex>100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
m_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='motorway'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='motorway'&cas_severity=='Fatal')$count)
f_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='motorway'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='motorway'&cas_severity=='Fatal')$count)
mway <- c(m_cas/m_strike_car,f_cas/f_strike_car)

m_strike_car <- sum(all_distances[[1]]$distance_for_strike$rural_other[all_distances[[1]]$distance_for_strike$demogindex<100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
f_strike_car <- sum(all_distances[[1]]$distance_for_strike$rural_other[all_distances[[1]]$distance_for_strike$demogindex>100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
m_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='rural_B'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='rural_B'&cas_severity=='Fatal')$count)
f_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='rural_B'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='rural_B'&cas_severity=='Fatal')$count)
rub <- c(m_cas/m_strike_car,f_cas/f_strike_car)

m_strike_car <- sum(all_distances[[1]]$distance_for_strike$urban_other[all_distances[[1]]$distance_for_strike$demogindex<100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
f_strike_car <- sum(all_distances[[1]]$distance_for_strike$urban_other[all_distances[[1]]$distance_for_strike$demogindex>100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
m_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='urban_B'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='urban_B'&cas_severity=='Fatal')$count)
f_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='urban_B'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='urban_B'&cas_severity=='Fatal')$count)
urb <- c(m_cas/m_strike_car,f_cas/f_strike_car)

m_strike_car <- sum(all_distances[[1]]$distance_for_strike$rural_primary[all_distances[[1]]$distance_for_strike$demogindex<100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
f_strike_car <- sum(all_distances[[1]]$distance_for_strike$rural_primary[all_distances[[1]]$distance_for_strike$demogindex>100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
m_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='rural_A'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='rural_A'&cas_severity=='Fatal')$count)
f_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='rural_A'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='rural_A'&cas_severity=='Fatal')$count)
rua <- c(m_cas/m_strike_car,f_cas/f_strike_car)

m_strike_car <- sum(all_distances[[1]]$distance_for_strike$urban_primary[all_distances[[1]]$distance_for_strike$demogindex<100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
f_strike_car <- sum(all_distances[[1]]$distance_for_strike$urban_primary[all_distances[[1]]$distance_for_strike$demogindex>100&all_distances[[1]]$distance_for_strike$mode_name=='cardrive'])
m_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='urban_A'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==1&road=='urban_A'&cas_severity=='Fatal')$count)
f_cas <- sum(subset(injury_table[[1]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='urban_A'&cas_severity=='Fatal')$count)+sum(subset(injury_table[[2]][[1]],strike_mode=='car/taxi'&strike_male==0&road=='urban_A'&cas_severity=='Fatal')$count)
ura <- c(m_cas/m_strike_car,f_cas/f_strike_car)

mway[1]/mway[2]
rub[1]/rub[2]
urb[1]/urb[2]
rua[1]/rua[2]
ura[1]/ura[2]

distance_for_strike <- all_distances[[1]]$distance_for_strike
distance_for_strike$total <- rowSums(distance_for_strike[,3:8])
distance_for_strike <- subset(distance_for_strike,total>0)
distance_for_strike_car <- distance_for_strike[distance_for_strike$mode_name=='cardrive',]
sum(distance_for_strike_car$motorway)/sum(distance_for_strike_car$total)

gen_m <- distance_for_strike[,.(sum(motorway)/sum(total)),by=demogindex]
cbind(gen_m[match(1:17,demogindex),],gen_m[match(101:117,demogindex),])


strike_summary <- t(sapply(la_names,function(x)sapply(c('rural_other', 'rural_primary',  'urban_other', 'urban_primary', 'motorway'),function(y)sum(subset(distance_for_strike_car,la==x)[[y]]))))
rownames(strike_summary) <- la_names
strike_summary <- rbind(strike_summary,total=colSums(strike_summary))
strike_summary <- cbind(strike_summary,total=rowSums(strike_summary))
apply(strike_summary,2,function(x)x/strike_summary[,6])
strike_summary[5,c(5,4,2)]*52/1000*6/c(15079141.1575,6539880.52283887,5734424.70466113)



distance_for_cas <- all_distances[[1]]$distance_for_inh
distance_for_cas$total <- rowSums(distance_for_cas[,3:8])
distance_for_cas <- subset(distance_for_cas,total>0)
distance_for_cas_car <- distance_for_cas[distance_for_cas$mode_name=='cardrive',]
sum(distance_for_cas_car$motorway)/sum(distance_for_cas_car$total)

cas_summary <- t(sapply(la_names,function(x)
  sapply(c('rural_other', 'rural_primary',  'urban_other', 'urban_primary', 'motorway'),function(y)sum(subset(distance_for_cas_car,la==x)[[y]]))))
rownames(cas_summary) <- la_names
cas_summary <- rbind(cas_summary,total=colSums(cas_summary))
cas_summary <- cbind(cas_summary,total=rowSums(cas_summary))
apply(cas_summary,2,function(x)x/cas_summary[,6])









#########################################

# compare to RTS

routed <- colSums(subset(all_distances$base_$distance_for_emission,mode_name=='cardrive'&la%in%c('E06000022','E06000023','E06000024','E06000025'))[,3:7])
rts <- readRDS('car_million_km_2010_to_2015.Rds')[1,]
names(rts) <- c('motorway','rural_primary','rural_other','urban_primary','urban_other')
links <- readRDS('outputs/cardist2010to2015.Rds')
links <- links[which(rownames(links)=='bristol'),]
names(links) <- c('motorway','urban_primary','rural_primary','urban_other','rural_other')

pdf('linksvsNTS.pdf')
par(mar=c(7,5,2,2))
barplot(rbind(links/6/52*1e3,c(routed)[match(names(links),names(routed))]),beside=T,las=2,col=c('navyblue','darkorange'),legend.text=c('RTS','NTS'),args.legend=list(x=10,bty='n'))
dev.off()


##########################################

#road_dist <- list()#readRDS('car_million_km_2010_to_2015.Rds')
road_dist <- lapply(c('cycle','cardrive','mbikedrive'),
                    function(x)sapply(colnames(str_dist)[3:8],
                        function(y)  sapply(city_regions,function(z)
                                             c(sum(subset(str_dist,mode_name==x&city_region==z)[[y]]))
                                           )
                        )
)
names(road_dist) <- c('cycle','cardrive','mbikedrive')

lookup_table <- readRDS('../mh-injury/rds_storage/lookup_table.Rds')
city_dist <- read.csv('outputs/mode_road_city.csv',stringsAsFactors = F)

cols <- rainbow(nrow(road_dist[[1]]))
{x11(width=10,height=10); par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in 1:3)
  for(j in 1:3){
    city_lab <- c('bicycle','car','motorcycle')[j]
    city_dist_subset <- subset(city_dist,X.1==city_lab)
    city_order <- match(city_dist_subset$X,rownames(road_dist[[j]]))
    city_road <- c('Motorway','Urban.A','Rural.A')[i]
    road_road <- c('motorway','urban_primary','rural_primary')[i]
    if(j==1&&i==1) {
      plot(0,0,col='white',frame=F,xaxt='n',yaxt='n',ylab='',xlab='')
      legend(x=-0.5,y=1,legend=city_dist_subset$X,col=cols,pch=15,cex=1.5,bty='n')
      }else{
    plot(log(city_dist_subset[[city_road]]),log(road_dist[[j]][,which(colnames(road_dist[[j]])==road_road)]),pch=15,col=cols,
         frame=F,xlab='AADF',ylab='Synthetic population',main=paste0(road_road,', ',city_lab),cex.axis=1.5,cex.lab=1.5,cex=1.5)
        lines(c(0,20),c(0,20))
      }
  }
}
