
rm(list=ls())
library(data.table)
setwd('~/overflow_dropbox/mh-distance/')

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
home_las <- c('E06000022','E06000023','E06000024','E06000025')
# get all las in rc
rc_las <- unique(c(home_las,unique(unlist(lapply(raw_rc_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)y[,1])))))))
# get all las in la
dest_las <- unique(c(home_las,unique(unlist(lapply(raw_la_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)colnames(y)[-1])))))))
# get all road names in rc
roadnames <- unique(unlist(lapply(raw_rc_mat_list,function(x)lapply(x,function(y)sapply(y,function(y)colnames(y)[-1])))))
# define ordered set of unique las
las <- unique(c(rc_las,dest_las))

## make rc matrices
rc_mat_list <- list()
for(i in 1:5){
  rc_mat_list[[i]] <- list()
  for(j in 1:2){
    rc_mat_list[[i]][[j]] <- list()
    for(k in 1:length(raw_rc_mat_list[[i]][[j]])){
      rc_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=length(roadnames),nrow=length(las))
      row_i <- match(raw_rc_mat_list[[i]][[j]][[k]][,1],las)
      col_i <- match(colnames(raw_rc_mat_list[[i]][[j]][[k]])[-1],roadnames)
      rc_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_rc_mat_list[[i]][[j]][[k]][,-1])
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
      la_mat_list[[i]][[j]][[k]] <- matrix(0,nrow=length(home_las),ncol=length(las))
      row_i <- match(raw_la_mat_list[[i]][[j]][[k]][,1],home_las)
      col_i <- match(colnames(raw_la_mat_list[[i]][[j]][[k]])[-1],las)
      la_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_la_mat_list[[i]][[j]][[k]][,-1])
      diag(la_mat_list[[i]][[j]][[k]]) <- 1 - (rowSums(la_mat_list[[i]][[j]][[k]] ) - diag(la_mat_list[[i]][[j]][[k]]))
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
synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
# set to data table
for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- setDT(readRDS(paste0(synth_pop_path,synth_pop_files[i])))
# take subset of columns
for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- 
  synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                          function(x)x%in%c('census_id','urbanmatch','demogindex')|
                            (grepl('base',x)&grepl('wkkm',x))|
                            (grepl('scen',x)&grepl('wkkm',x))
  ),with=F]
# rename
la_names <- sapply(synth_pop_files,function(x)gsub('SPind_','',x))
la_names <- sapply(la_names,function(x)gsub('_subdivide.Rds','',x))
names(synth_pops) <- la_names

# calculate total car dj
car_total <- rep(0,6)
for(j in 1:4){
total_d1 <- sapply(1:4,function(z) # apply to first four LAs
  rowSums(
    sapply(1:length(synth_pops),function(x) # sum over travel of first four LAs
      rowSums(
        sapply(1:2,function(y) # sum over rural and urban
          sum(
            subset(synth_pops[[x]],urbanmatch==y-1)[[paste0('base_cardrive_wkkm_d',j)]]
          )*la_mat_list[[3]][[y]][[j]][x,z]*rc_mat_list[[3]][[y]][[j]][z,] # sum distance times LA allocation
        )
      )
    )
  )
)
car_total <- car_total + rowSums(total_d1)
print(rowSums(total_d1))
}


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
for(scenario in scenarios){
  
  synth_pops_scen <- copy(synth_pops)
  for(i in 1:length(synth_pops_scen)) colnames(synth_pops_scen[[i]]) <- sapply(colnames(synth_pops_scen[[i]]),function(x)gsub(scenario,'',x))
  
  # noise : total distance per mode per LA
  which_modes_noisy <- !driven_modes%in%c('cycle','walk')
  noisy_modes <- driven_modes[which_modes_noisy]
  dist_cats <- dist_cats_per_mode[which_modes_noisy]
  cols <- unlist(lapply(1:length(noisy_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(noisy_modes[x],'_wkkm_d',y))))
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=urbanmatch,.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  for(i in 1:length(las)) distance_sums[,las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  distance_for_noise <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=las]
  
  # emission : total distance per mode per road type per LA
  # allocate distance_sums to roadtypes
  for(i in 1:length(roadnames)) 
    distance_sums[,sapply(las,function(x)paste0(x,roadnames[i])):=lapply(.SD,function(x)x*rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch'),.SDcols=las]
  la_road <- sapply(las,function(x)paste0(x,roadnames))
  temp_distance_for_emission <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=la_road]
  reorganise <- list()
  colnms <- colnames(temp_distance_for_emission)
  for(i in 1:length(las)) {
    rdnms <- sapply(colnms,function(x)gsub(las[i],'',x))
    reorganise[[i]] <- temp_distance_for_emission[,rdnms%in%c(roadnames,'mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name',roadnames)
    reorganise[[i]]$la <- las[i]
  }
  distance_for_emission <- rbindlist(reorganise)
  
  
  # injury rate : total distance per mode per road type per LA per demographic group
  ## rename columns
  for(i in 1:length(synth_pops_scen)) colnames(synth_pops_scen[[i]])[colnames(synth_pops_scen[[i]])=='walk_wkkm'] <- 'walk_wkkm_d1'
  which_modes_strike <- 1:5
  strike_modes <- driven_modes[which_modes_strike]
  dist_cats <- dist_cats_per_mode[which_modes_strike]
  cols <- unlist(lapply(1:length(strike_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(strike_modes[x],'_wkkm_d',y))))
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=c('urbanmatch','demogindex'),.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  for(i in 1:length(las)) distance_sums[,las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  for(i in 1:length(roadnames))
    for(j in 1:length(las)){
      la_col <- which(colnames(distance_sums)==las[j])
      distance_sums[[paste0(las[j],roadnames[i])]] <- distance_sums[[la_col]]*distance_sums[,rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][j,i],by=c('mode','dist','distcat','urbanmatch','demogindex','la_index')]$V1
    }
  la_road <- sapply(las,function(x)paste0(x,roadnames))
  temp_distance_for_strike <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=la_road]
  reorganise <- list()
  colnms <- colnames(temp_distance_for_strike)
  for(i in 1:length(las)) {
    rdnms <- sapply(colnms,function(x)gsub(las[i],'',x))
    reorganise[[i]] <- temp_distance_for_strike[,rdnms%in%c(roadnames,'demogindex','mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name','demogindex',roadnames)
    reorganise[[i]]$la <- las[i]
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
  distance_sums <- lapply(1:length(synth_pops_scen),function(i)synth_pops_scen[[i]][,.(
    dist=sapply(.SD,sum),
    mode=sapply(cols,function(x)which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(z,x)))))),
    distcat=sapply(cols,function(x)which(sapply(1:max(dist_cats),function(y)grepl(y,x)))),
    la_index=i,
    mode_name=sapply(cols,function(x)strsplit(x,'_')[[1]][1])
  ),by=c('urbanmatch','demogindex'),.SDcols=cols])
  distance_sums <- rbindlist(distance_sums)
  for(i in 1:length(las)) distance_sums[,las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  for(i in 1:length(roadnames)) 
    for(j in 1:length(las)){
      la_col <- which(colnames(distance_sums)==las[j])
      distance_sums[[paste0(las[j],roadnames[i])]] <- distance_sums[[la_col]]*distance_sums[,rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][j,i],by=c('mode','dist','distcat','urbanmatch','demogindex','la_index')]$V1
    }
  la_road <- sapply(las,function(x)paste0(x,roadnames))
  temp_distance_for_cas <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=la_road]
  reorganise <- list()
  colnms <- colnames(temp_distance_for_cas)
  for(i in 1:length(las)) {
    rdnms <- sapply(colnms,function(x)gsub(las[i],'',x))
    reorganise[[i]] <- temp_distance_for_cas[,rdnms%in%c(roadnames,'demogindex','mode_name'),with=F]
    colnames(reorganise[[i]]) <- c('mode_name','demogindex',roadnames)
    reorganise[[i]]$la <- las[i]
  }
  distance_for_cas <- rbindlist(reorganise)
  
  
  
  # physical activity : duration per person per mode
  pa_modes <- c('cycle','walk')
  pa_pops <- list()
  for(i in 1:length(synth_pops_scen)) pa_pops[[i]] <- synth_pops_scen[[i]][,.(census_id=census_id,
                                                                              walk_wkkm=walk_wkkm_d1,
                                                                              cycle_wkkm=cycle_wkkm_d1+cycle_wkkm_d2+cycle_wkkm_d3,
                                                                              la=i)]
  #cols <- sapply(pa_modes,function(x)paste0('base_',x,'_wkkm'))
  distance_for_pa <- rbindlist(pa_pops)
  
  
  # pollution inhalation : duration per person per mode per road type per LA (we've already added passenger to driver)
  inh_modes <- driven_modes
  cols <- unlist(lapply(1:length(inh_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(inh_modes[x],'_wkkm_d',y))))
  distance_sums <- lapply(1:length(synth_pops_scen),function(i){
    #melt0 <- melt(inh_pops[[i]],id.vars=c('census_id','urbanmatch'),measure=patterns('*0','*1'),variable.name='mode',value.name=c('d0','d1'))
    #melt1 <- melt(melt0,id.vars=c('census_id','urbanmatch'),measure=patterns('^d'),variable.name='distcat',value.name='dist')
    melt2 <- melt(synth_pops_scen[[i]],id.vars=c('census_id','urbanmatch'),measure=patterns(paste0('^',inh_modes)),variable.name='distcat',value.name=paste0('mode',inh_modes), variable.factor=F)
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
  distance_sums <- rbindlist(distance_sums)
  distance_sums <- distance_sums[distance_sums$dist>0,]
  distance_sums <- distance_sums[!is.na(distance_sums$dist),]
  for(i in 1:length(las)) distance_sums[,las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][la_index,i]),by=c('mode','dist','distcat','urbanmatch')]
  for(i in 1:length(roadnames)) 
    for(j in 1:length(las)){
      la_col <- which(colnames(distance_sums)==las[j])
      distance_sums[[paste0(las[j],roadnames[i])]] <- distance_sums[[la_col]]*distance_sums[,rc_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][j,i],by=c('mode','dist','distcat','urbanmatch','census_id','la_index')]$V1
    }
  la_road <- sapply(las,function(x)paste0(x,roadnames))
  temp_distance_for_inh <- distance_sums[,lapply(.SD,sum),by=c('mode_name','census_id'),.SDcols=la_road]
  reorganise <- list()
  colnms <- colnames(temp_distance_for_inh)
  for(i in 1:length(las)) {
    rdnms <- sapply(colnms,function(x)gsub(las[i],'',x))
    reorganise[[i]] <- temp_distance_for_inh[,rdnms%in%c(roadnames,'census_id','mode_name'),with=F]
    keep_rows <- rowSums(reorganise[[i]][,3:8])>0
    reorganise[[i]] <- reorganise[[i]][keep_rows,]
    colnames(reorganise[[i]]) <- c('mode_name','census_id',roadnames)
    reorganise[[i]]$la <- las[i]
  }
  distance_for_inh <- rbindlist(reorganise)
  
  all_distances[[scenario]] <- list(distance_for_inh=distance_for_inh,
                                    distance_for_pa=distance_for_pa,
                                    distance_for_cas=distance_for_cas,
                                    distance_for_strike=distance_for_strike,
                                    distance_for_emission=distance_for_emission,
                                    distance_for_noise=distance_for_noise)
}
saveRDS(all_distances,'outputs/all_distances.Rds')

###################################################################################
## a look at the distances

all_distances <- readRDS('outputs/all_distances.Rds')


str_dist <- all_distances$base_$distance_for_strike
distance_sums <- sapply(colnames(str_dist)[3:8],
       function(y) sapply(unique(str_dist$mode_name),
                         function(x) sum(subset(str_dist,la%in%home_las&mode_name==x)[[y]])/sum(subset(str_dist,mode_name==x)[[y]])
                         )
       )
distance_sums*52*6/1000


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

road_dist <- readRDS('car_million_km_2010_to_2015.Rds')
lookup_table <- readRDS('../mh-injury/rds_storage/lookup_table.Rds')
la_dist <- read.xlsx('inputs/VehicleType_LALevel.xlsx',sheetIndex = 1,rowIndex = 6:1670)
la_dist$LA_Name <- as.character(la_dist$LA_Name)
la_dist$LA_Name[la_dist$LA_Name=='Bristol'] <- 'Bristol, City of'
bristol <- subset(la_dist,Region_Name=='South West'&Year>2009&LA_Name%in%c('Bristol, City of','Bath and North East Somerset','South Gloucestershire','North Somerset'))
sapply(2010:2015,function(x)sum(subset(bristol,Year==x)$Car))/1000*1.6

sum(routed)
sum(bristol$Car)/52/6
sum(rts/6/52*1e3)
sum(links/6/52*1e3)

rownames(road_dist) <- sapply(rownames(road_dist),function(x)tolower(gsub(' ','',x)))
citymap <- list(bristol='bristol',
                nottingham='nottingham',
                liverpool='liverpoolcityregioncombinedauthority',
                northeast='northeastcombinedauthority',
                greatermanchester='greatermanchestercombinedauthority',
                sheffield='sheffieldcityregioncombinedauthority',
                westmidlands='westmidlandscombinedauthority',
                leeds='westyorkshirecombinedauthority',
                london='london')


cities <- unique(lookup_table$cityregion)
cities <- cities[cities!='']
ratios <- list()
for(city in cities){
  las <- subset(lookup_table,cityregion==city)$la
  missing <- las[!las%in%la_dist$LA_Name]
  #if(length(missing)>0){
  #  cat(paste0('Unmatched LAs, distances not extracted for ',city,':\n'))
  #  cat(paste0(missing,'\n'))
  #}else{
    la_sum <- sum(subset(la_dist,Year>2009&LA_Name%in%las)$Car)
    road_sum <- sum(road_dist[which(rownames(road_dist)==citymap[[city]]),])
    print(city)
    if(la_sum/1000*1.6-road_sum>1e3){
      print(c(la_sum/1000*1.6-road_sum)/1e6)
      print(sapply(las,function(x)sum(subset(la_dist,Year>2009&LA_Name==x)$Car))/1e9*1.6)
    }
    ratios[[city]] <- la_sum/1000*1.6/road_sum
  #}
}
pdf('RTSLAvsRTSroad.pdf')
par(mar=c(10,5,2,2)); barplot(unlist(ratios),las=2,ylab='LA sum / road sum',cex.lab=1.5,cex.axis=1.5,cex.names=1.25,col='navyblue')
dev.off()
