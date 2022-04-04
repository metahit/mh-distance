rm(list=ls())
library(data.table)
all_scens <- list.dirs(path = "../mh-execute/inputs/scenarios", full.names = FALSE, recursive = FALSE)
all_scens <- c(all_scens[1],all_scens)
scenarios <- c('base_','scen_','scen_','scen_')
scenario <- 'base_'

# things that don't depend on the scenario
{
  city_regions_table <- read.csv('../mh-execute/inputs/mh_regions_lad_lookup.csv',stringsAsFactors = F)
  city_regions <- unique(city_regions_table$cityregion)
  city_regions <- city_regions[city_regions!='']
  city_las <- city_regions_table$lad11cd[city_regions_table$cityregion%in%city_regions]
  la_city_indices <- sapply(city_las,function(x) which(city_regions==city_regions_table$cityregion[city_regions_table$lad11cd==x]))
  
  ## get raw files
  matrix_path <- '../mh-execute/inputs/travel-matrices/'
  matrix_files <- list.files(matrix_path)
  # rc=road coverage files
  # durn=duration road files
  # la= la files
  raw_matrix_names <- c('raw_rc_mat_list','raw_dur_rc_mat_list','raw_la_mat_list')
  search_terms <- c('rc[.]csv','rc_durn','la')
  file_extensions <- c('_matrc.csv','_matrc_durn.csv','_matla.csv')
  
  nUrban <- 2
  nMaxDistCat <- 4
  
  raw_matrix_list <- list()
  for(rmn in 1:length(raw_matrix_names)){
    raw_matrix_list[[raw_matrix_names[rmn]]] <- list()
    search_term <- search_terms[rmn]
    file_extension <- file_extensions[rmn]
    
    sub_matrix_files <- matrix_files[sapply(matrix_files,function(x)grepl(search_term,x))]
    for(i in 1:5){ # 5 modes
      raw_matrix_list[[raw_matrix_names[rmn]]][[i]] <- list()
      for(j in 1:nUrban){ # 2 urban/rural labels
        raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]] <- list()
        # find out how many distance categories
        distance_categories <- sum(sapply(sub_matrix_files,function(x)grepl(paste0('mode',i,'_u',j-1),x)))
        for(k in 1:distance_categories){ # up to 4 distance categories
          d_bit <- paste0('d',k) 
          filename <- paste0(matrix_path,'mode',i,'_u',j-1,d_bit,file_extension)
          if(file.exists(filename)){
            mat_file <- read.csv(filename,stringsAsFactors = F)
            raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]][[k]] <- mat_file
          }
        }
      }
    }
  }
  for(rmn in 1:length(raw_matrix_list)) assign(names(raw_matrix_list)[rmn],raw_matrix_list[[rmn]])
  
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
  nRoads <- length(roadnames)
  nDestination_las <- length(destination_las)
  nOrigin_las <- length(origin_las)
  nHome_las <- length(home_las)
  
  # define modes
  modes <- c('cycle','walk','cardrive','mbikedrive','bus','vandrive')
  nModes <- length(modes)
  mode_list <- list('cycle','walk',c('cardrive'),'mbikedrive','bus','vandrive')
  driven_modes <- c(modes)
  passenger_modes <- list('cycle','walk',c('carpass','taxi'),'mbikepass','bus','vanpass')
  inh_modes <- c('cycle','walk','car','mbike','bus','van')
  add_modes <- c('cardrive','mbikedrive','vandrive')
  which_add_modes <- which(driven_modes%in%add_modes)
  
  matrix_names <- c('rc_mat_list','dur_rc_mat_list')
  matrix_list <- list()
  for(rmn in 1:length(matrix_names)){
    matrix_list[[matrix_names[rmn]]] <- list()
    for(i in 1:5){
      matrix_list[[matrix_names[rmn]]][[i]] <- list()
      for(j in 1:2){
        matrix_list[[matrix_names[rmn]]][[i]][[j]] <- list()
        for(k in 1:length(raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]])){
          matrix_list[[matrix_names[rmn]]][[i]][[j]][[k]] <- matrix(0,ncol=nRoads,nrow=nHome_las)
          row_j <- match(home_las,raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]][[k]][,1])
          row_i <- home_las %in% raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]][[k]][,1] 
          row_i <- row_i[!is.na(row_i)]
          row_j <- row_j[!is.na(row_j)]
          col_i <- roadnames %in% colnames(raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]][[k]])[-1]
          matrix_list[[matrix_names[rmn]]][[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]][[k]][row_j,-1])
        }
      }
    }
  }
  for(rmn in 1:length(matrix_list)) assign(names(matrix_list)[rmn],matrix_list[[rmn]])
  
  ## mapping matrices ###################################################
  
  ## make rc matrices
  rc_mat_list <- list()
  for(i in 1:5){
    rc_mat_list[[i]] <- list()
    for(j in 1:nUrban){
      rc_mat_list[[i]][[j]] <- list()
      for(k in 1:length(raw_rc_mat_list[[i]][[j]])){
        rc_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=nRoads,nrow=nHome_las)
        row_j <- match(home_las,raw_rc_mat_list[[i]][[j]][[k]][,1])
        row_i <- home_las %in% raw_rc_mat_list[[i]][[j]][[k]][,1] # match(raw_rc_mat_list[[i]][[j]][[k]][,1],home_las)
        row_i <- row_i[!is.na(row_i)]
        row_j <- row_j[!is.na(row_j)]
        col_i <- roadnames %in% colnames(raw_rc_mat_list[[i]][[j]][[k]])[-1] # match(colnames(raw_rc_mat_list[[i]][[j]][[k]])[-1],roadnames)
        rc_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_rc_mat_list[[i]][[j]][[k]][row_j,-1])
      }
    }
  }
  
  ## make dur_rc matrices
  dur_rc_mat_list <- list()
  for(i in 1:5){
    dur_rc_mat_list[[i]] <- list()
    for(j in 1:nUrban){
      dur_rc_mat_list[[i]][[j]] <- list()
      for(k in 1:length(raw_dur_rc_mat_list[[i]][[j]])){
        dur_rc_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=nRoads,nrow=nHome_las)
        row_j <- match(home_las,raw_dur_rc_mat_list[[i]][[j]][[k]][,1])
        row_i <- home_las %in% raw_dur_rc_mat_list[[i]][[j]][[k]][,1] 
        row_i <- row_i[!is.na(row_i)]
        row_j <- row_j[!is.na(row_j)]
        col_i <- roadnames %in% colnames(raw_dur_rc_mat_list[[i]][[j]][[k]])[-1] 
        dur_rc_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_dur_rc_mat_list[[i]][[j]][[k]][row_j,-1])
      }
    }
  }
  
  ## make la matrices, from origin_las to destination_las
  la_mat_list <- list()
  for(i in 1:5){
    la_mat_list[[i]] <- list()
    for(j in 1:nUrban){
      la_mat_list[[i]][[j]] <- list()
      for(k in 1:length(raw_la_mat_list[[i]][[j]])){
        la_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=nDestination_las,nrow=nOrigin_las)
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
  
  
  ## duplicate car (3) for van (6)
  la_mat_list[[6]] <- la_mat_list[[3]]
  rc_mat_list[[6]] <- rc_mat_list[[3]]
  dur_rc_mat_list[[6]] <- dur_rc_mat_list[[3]]
  
  dist_cats_per_mode <- sapply(rc_mat_list,function(x)sapply(x,length))[1,]
  which_modes_noisy <- !driven_modes%in%c('cycle','walk','bus')
  noisy_modes <- driven_modes[which_modes_noisy]
  dist_cats <- dist_cats_per_mode[which_modes_noisy]
  
  # clear memory
  rm('matrix_list','raw_dur_rc_mat_list','raw_rc_mat_list','mat_file','augment_la_mat','raw_la_mat_list','raw_matrix_list')
  
}
print(sort(sapply(ls(),function(x)object.size(get(x)))))/1e6#delete

## start scenarios #######################################

scenario_index <- 1
for (scenario_index in 1:length(all_scens)){
  global_scen <- all_scens[scenario_index]
  scenario <- scenarios[scenario_index]
  
  ## get synthetic populations
  synth_pops <- list()
  synth_pop_path <- paste0('../mh-execute/inputs/scenarios/', global_scen, '/')#change the folder based on the scenario, have to convert into for loop
  synth_pop_files <- list.files(synth_pop_path)
  synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('SPind_E[[:digit:]]+.Rds',x))]
  #synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
  # read in, set to data table, take subset of columns
  for(i in 1:length(synth_pop_files)){
    synth_pops[[i]] <- setDT(readRDS(paste0(synth_pop_path,synth_pop_files[i])))
    rm_colnames <- colnames(synth_pops[[i]])[!sapply(colnames(synth_pops[[i]]),
                           function(x)x%in%c('census_id','urbanmatch','demogindex')|grepl(scenario,x)
    )]
    synth_pops[[i]][,c(rm_colnames):=NULL] 
  }
  # rename
  la_names <- sapply(synth_pop_files,function(x)gsub('SPind_','',x))
  la_names <- sapply(la_names,function(x)gsub('.Rds','',x))
  names(synth_pops) <- la_names
  number_city_las <- length(synth_pops)
  
  ## add 'mini' to all cities, to all scenarios, for distance_for_cas, distance_for_strike, distance_for_emission, distance_for_noise
  synth_pop_supp_path <- paste0('../mh-execute/inputs/scenarios-mini/', global_scen, '/') #change folder name based on scenario
  synth_pop_supp_files <- list.files(synth_pop_supp_path)
  synth_pop_supp_files <- synth_pop_supp_files[sapply(synth_pop_supp_files,function(x)grepl('SPind_E[[:digit:]]+.Rds',x))]
  synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
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
  all_sp_names <- names(synth_pops)
  
  #for(scenario in scenarios){
  scenname <- ifelse(scenario != 'base_', paste0(global_scen, '_'), 'base_')
  {
    
    # rename scenario
    for(i in 1:number_city_las) colnames(synth_pops[[i]]) <- sapply(colnames(synth_pops[[i]]),function(x)gsub(scenario,'',x))
    # rename base for non-city la synthetic populations (they don't have scenarios)
    for(i in (number_city_las+1):length(synth_pops)) colnames(synth_pops[[i]]) <- sapply(colnames(synth_pops[[i]]),function(x)gsub('base_','',x))
    
    ## noise ########################################
    # noise : total distance per mode per LA
    mode_cols <- unlist(lapply(1:length(noisy_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(noisy_modes[x],'_wkkm_d',y))))
    # expand by mode, dist_cat, home_la, including all synth pops
    distance_sums <- lapply(1:length(synth_pops),function(spi)synth_pops[[spi]][,.(
      dist = sapply(.SD,sum),
      distcat = sapply(mode_cols,function(mode_col) 
        which(sapply(1:max(dist_cats),function(dist_item)
          grepl(dist_item, mode_col)))),
      la_origin_index = which(origin_las==all_sp_names[spi]),
      mode_name = sapply(mode_cols,function(x)strsplit(x,'_')[[1]][1])
    ),by=urbanmatch,.SDcols=mode_cols])
    distance_sums <- rbindlist(distance_sums)
    distance_sums[,mode:=which(sapply(mode_list,function(x)mode_name%in%x)),by=mode_name]
    # map to destination las
    distance_sums[,(destination_las):=lapply(1:nDestination_las,function(dli)
      dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_origin_index,dli]),by=c('mode','distcat','urbanmatch')]
    # for(dli in 1:nDestination_las) distance_sums[,destination_las[dli]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_origin_index,dli]),by=c('mode','dist','distcat','urbanmatch')]
    # sum in destination las
    distance_for_noise <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=destination_las]
    
    ## emission ########################################
    # emission : total distance per mode per road type per LA
    # collapse to target las
    distance_sums <- distance_sums[,lapply(.SD,sum),by=c('mode','mode_name','distcat','urbanmatch'),.SDcols=destination_las]
    # allocate distance_sums to roadtypes
    for(ri in 1:nRoads) {
      distance_sums[,(paste0(home_las,'_',roadnames[ri])):=lapply(1:nHome_las,function(hli)
        get(home_las[hli])*rc_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][hli,ri]),by=c('mode','distcat','urbanmatch')]
    }
    # get names for la+road combinations
    la_road <- sapply(home_las,function(x)paste0(x,'_',roadnames))
    # sum in la and road
    temp_distance_for_emission <- distance_sums[,lapply(.SD,sum),by='mode_name',.SDcols=la_road]
    # reorganise to long form
    m1 <- melt(temp_distance_for_emission,id.vars=c('mode_name'))#,measure=patterns(roadnames),variable.name='la',value.name=roadnames, variable.factor=F)
    m1[,c("la", "road") := tstrsplit(variable, '_', fixed = TRUE)]
    distance_for_emission <- dcast(m1, mode_name + la ~ road, value.var = "value")
    saveRDS(list(distance_for_emission=distance_for_emission,distance_for_noise=distance_for_noise),
            paste0('../mh-execute/inputs/distances/',scenname,'emissions_distances.Rds')) #change the file names
    
    ## injury ################################################################################
    # injury : distance per mode per road type per city per demographic group
    ## rename columns
    which_modes_strike <- which(modes%in%c('cardrive','walk','cycle','mbikedrive'))
    strike_modes <- driven_modes[which_modes_strike]
    dist_cats <- dist_cats_per_mode[which_modes_strike]
    mode_cols <- unlist(lapply(1:length(strike_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(strike_modes[x],'_wkkm_d',y))))
    # expand by mode, dist_cat, home_la, demographic, including all synth pops
    distance_sums <- lapply(1:length(synth_pops),function(spi)synth_pops[[spi]][,.(
      dist = sapply(.SD,sum),
      distcat = sapply(mode_cols,function(mode_col) 
        which(sapply(1:max(dist_cats),function(dist_item)
          grepl(dist_item, mode_col)))),
      la_origin_index = which(origin_las==all_sp_names[spi]),
      mode_name = sapply(mode_cols,function(x)strsplit(x,'_')[[1]][1])
    ),by=c('urbanmatch','demogindex'),.SDcols=mode_cols])
    distance_sums <- rbindlist(distance_sums)
    distance_sums[,mode:=which(sapply(mode_list,function(x)mode_name%in%x)),by=mode_name]
    
    # map to home las
    # la_origin_index is the row of the la map, hli is the column of the la map
    distance_sums[,(home_las):=lapply(1:nHome_las,function(hli)
      dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_origin_index,hli]),by=c('mode','distcat','urbanmatch')]
    # map to road types, sum over cities
    for(ri in 1:nRoads)
      for(ci in 1:length(city_regions)){
        la_city_ci <- which(la_city_indices==ci)
        distance_sums[,paste0(city_regions[ci],'_',roadnames[ri]):=
                        Reduce(`+`, lapply(la_city_ci,function(hli)get(home_las[hli])*rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][hli,ri])),
                      by=c('mode','distcat','urbanmatch','demogindex')]
      }
    
    # get city road labels
    city_road <- sapply(city_regions,function(x)paste0(x,'_',roadnames))
    # sum over city roads
    temp_distance_for_strike <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=city_road]
    # reorganise to long form
    m1 <- melt(temp_distance_for_strike,id.vars=c('mode_name','demogindex'))
    m1[,c("city_region", "road") := tstrsplit(variable, '_', fixed = TRUE)]
    distance_for_strike <- dcast(m1, mode_name + demogindex + city_region ~ road, value.var = "value")
    
    ## casualty #####################################################################
    ## casualty distances : distance per demographic group per road per city per mode
    ## add drive to passenger
    for(spi in 1:length(synth_pops))
      for(mi in 1:length(add_modes)){
        dist_cats <- dist_cats_per_mode[which_add_modes[mi]]
        for(di in 1:dist_cats){
          add_col <- paste0(passenger_modes[[which_add_modes[mi]]],'_wkkm_d',di)
          target_col <- paste0(add_modes[mi],'_wkkm_d',di)
          synth_pops[[spi]][,paste0(add_modes[mi],'_wkkm_d',di):=Reduce(`+`,.SD),.SDcols=c(add_col,target_col)]
        }
      }
    
    dist_cats <- dist_cats_per_mode
    mode_cols <- unlist(lapply(1:length(driven_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(driven_modes[x],'_wkkm_d',y))))
    # expand by mode, dist_cat, home_la, demographic, including all synth pops
    distance_sums <- lapply(1:length(synth_pops),function(spi)synth_pops[[spi]][,.(
      dist = sapply(.SD,sum),
      distcat = sapply(mode_cols,function(mode_col) 
        which(sapply(1:max(dist_cats),function(dist_item)
          grepl(dist_item, mode_col)))),
      la_origin_index = which(origin_las==all_sp_names[spi]),
      mode_name = sapply(mode_cols,function(x)strsplit(x,'_')[[1]][1])
    ),by=c('urbanmatch','demogindex'),.SDcols=mode_cols])
    distance_sums <- rbindlist(distance_sums)
    distance_sums[,mode:=which(sapply(mode_list,function(x)mode_name%in%x)),by=mode_name]
    
    # map to home las
    distance_sums[,(home_las):=lapply(1:nHome_las,function(hli)
      dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_origin_index,hli]),by=c('mode','distcat','urbanmatch')]
    # map to road types, sum over cities
    for(ri in 1:nRoads)
      for(ci in 1:length(city_regions)){
        la_city_k <- which(la_city_indices==ci)
        distance_sums[,paste0(city_regions[ci],'_',roadnames[ri]):=
                        Reduce(`+`, lapply(la_city_k,function(hli)get(home_las[hli])*rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][hli,ri])),
                      by=c('mode','distcat','urbanmatch','demogindex')]
      }
    # get city road labels
    city_road <- sapply(city_regions,function(x)paste0(x,'_',roadnames))
    # sum over city roads
    temp_distance_for_cas <- distance_sums[,lapply(.SD,sum),by=c('mode_name','demogindex'),.SDcols=city_road]
    # reorganise to long form
    m1 <- melt(temp_distance_for_cas,id.vars=c('mode_name','demogindex'))
    m1[,c("city_region", "road") := tstrsplit(variable, '_', fixed = TRUE)]
    distance_for_cas <- dcast(m1, mode_name + demogindex + city_region ~ road, value.var = "value")
    rm('m1')
    saveRDS(list(distance_for_cas=distance_for_cas,distance_for_strike=distance_for_strike),paste0('../mh-injury/rds_storage/',scenname,'injury_distances.Rds'))
    
    ## pa #############################################
    # physical activity : duration per person per mode
    pa_pops <- list()
    cycle_cols <- sapply(colnames(synth_pops[[1]]),function(x)grepl('cycle',x)&grepl('wkhr',x))
    for(spi in 1:number_city_las) 
      pa_pops[[spi]] <- synth_pops[[spi]][,.(census_id=census_id,
                                              walking_dur_pa=walk_wkhr,
                                              cycle_dur_pa=Reduce(`+`, .SD)), .SDcols=cycle_cols]
    distance_for_pa <- rbindlist(pa_pops)
    rm('pa_pops')
    saveRDS(distance_for_pa,paste0('../mh-execute/inputs/distances/',scenname , 'pa_distances.Rds'))
    rm('distance_for_pa')
    
  }
  
  ## inhalation ##########################
  # go city by city
  city <- 'bristol'
  for(city in city_regions){
    # should include only city las
    one_city_las <- which(names(synth_pops)%in%city_regions_table$lad11cd[city_regions_table$cityregion==city])
    if(length(one_city_las)>0){
      print(city)
      # expand by mode, dist_cat, home_la, participant_id
      distance_sums <- list()
      for(spi in one_city_las){ 
        # add taxi to car
        synth_pops[[spi]][,sapply(1:4,function(x)paste0('car_wkhr_d',x)):=lapply(1:4,function(x)get(paste0('car_wkhr_d',x))+get(paste0('taxi_wkhr_d',x)))]
        pattern <- paste0('(',paste0(inh_modes,collapse='|'),')_wkhr')
        m1 <- melt(synth_pops[[spi]],id.vars=c('census_id','urbanmatch'),measure.vars=patterns(pattern),value.name='dur')
        m1[,c("mode_name",'wkhr', "distname") := tstrsplit(variable, '_', fixed = TRUE,fill='d1')]
        m1[,distcat:=as.numeric(gsub('d','',distname)),by=distname]
        m1[,mode:=which(sapply(mode_list,function(y)any(sapply(y,function(z)grepl(mode_name,z))))),by=mode_name]
        m2 <- m1[,.(census_id, urbanmatch, distcat, mode, mode_name, dur)]
        m2[,la_origin_index:=which(origin_las==all_sp_names[spi])]
        distance_sums[[spi]] <- m2[!is.na(dur)&dur>0,]
      }
      rm('m1','m2')
      distance_sums <- do.call(rbind,distance_sums)
      distance_sums <- distance_sums[distance_sums$dur>0,]
      distance_sums <- distance_sums[!is.na(distance_sums$dur),]
      print(1)
      # get tube and train durations for later
      tube_travel <- do.call(rbind, lapply(one_city_las,function(spi) synth_pops[[spi]][tube_wkhr>0,colnames(synth_pops[[spi]])%in%c('census_id','tube_wkhr'),with=F]) )
      train_travel <- do.call(rbind, lapply(one_city_las,function(spi) synth_pops[[spi]][train_wkhr>0,colnames(synth_pops[[spi]])%in%c('census_id','train_wkhr'),with=F]) )
      setkey(tube_travel, census_id)
      setkey(train_travel, census_id)
      # clear memory but keep list order
      for(spi in one_city_las) synth_pops[[spi]] <- 0
      print(2)
      # map to home city las 
      ##!! maps from home city las to home city las
      distance_sums[,(all_sp_names[one_city_las]):=lapply(match(all_sp_names[one_city_las],home_las),function(hli)dur*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_origin_index,hli]),by=c('mode','distcat','urbanmatch')]
      # map to roads within las
      temp_distance_for_inh <- list()
      for(ri in 1:nRoads){
        new_cols <- paste0(all_sp_names[one_city_las],'_',roadnames[ri])
        old_cols <- match(all_sp_names[one_city_las],home_las)
        # map to roads
        distance_sums[,(new_cols):=
                        lapply(old_cols,function(hli)get(home_las[hli])*rc_mat_list[[mode]][[urbanmatch+1]][[distcat]][hli,ri]),
                      by=c('mode','distcat','urbanmatch')]
        # sum over distance categories
        temp_distance_for_inh[[ri]] <- distance_sums[,lapply(.SD,sum),by=c('mode_name','census_id'),.SDcols=new_cols]
        setnames(temp_distance_for_inh[[ri]],old=new_cols,new=all_sp_names[one_city_las])
        temp_distance_for_inh[[ri]][,road:=roadnames[ri]]
        distance_sums[,(new_cols):=NULL]
      }
      rm('distance_sums')
      print(sort(sapply(ls(),function(x)object.size(get(x))))/1e9)
      
      # concatenate roads
      concat <- do.call(rbind,temp_distance_for_inh)
      print(object.size(concat),units='Gb')
      rm('temp_distance_for_inh')
      # cast
      temp_obj <- dcast(concat, census_id + mode_name ~ road, value.var = all_sp_names[one_city_las])
      print(object.size(temp_obj),units='Gb')
      rm('temp_obj')
      to_save <- dcast(concat, census_id ~ road + mode_name, value.var = all_sp_names[one_city_las])
      print(object.size(to_save),units='Gb')
      rm('concat')
      ## add tube and train travel 
      setkey(to_save, census_id)
      to_save <- merge(to_save, tube_travel,by='census_id', all=TRUE)
      to_save <- merge(to_save, train_travel,by='census_id', all=TRUE)
      rm('tube_travel','train_travel')
      
      for(i in 2:ncol(to_save)) set(to_save,which(is.na(to_save[[i]])),i,0)
      saveRDS(to_save,paste0('../mh-execute/inputs/distances/',scenname ,city,'_inh_distances.Rds'))
      to_save <- c()
    }
  }
}
## end #################################################################################
