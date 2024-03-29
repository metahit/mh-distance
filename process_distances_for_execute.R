rm(list=ls())
library(data.table)

all_scens <- list.dirs(path = "../mh-execute/inputs/scenarios", full.names = FALSE, recursive = FALSE)

for (global_scen in all_scens){
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
  
  raw_matrix_list <- list()
  for(rmn in 1:length(raw_matrix_names)){
    raw_matrix_list[[raw_matrix_names[rmn]]] <- list()
    search_term <- search_terms[rmn]
    file_extension <- file_extensions[rmn]
    
    sub_matrix_files <- matrix_files[sapply(matrix_files,function(x)grepl(search_term,x))]
    for(i in 1:5){ # 5 modes
      raw_matrix_list[[raw_matrix_names[rmn]]][[i]] <- list()
      for(j in 1:2){ # 2 urban/rural labels
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
  
  matrix_names <- c('rc_mat_list','dur_rc_mat_list')
  matrix_list <- list()
  for(rmn in 1:length(matrix_names)){
    matrix_list[[matrix_names[rmn]]] <- list()
    for(i in 1:5){
      matrix_list[[matrix_names[rmn]]][[i]] <- list()
      for(j in 1:2){
        matrix_list[[matrix_names[rmn]]][[i]][[j]] <- list()
        for(k in 1:length(raw_matrix_list[[raw_matrix_names[rmn]]][[i]][[j]])){
          matrix_list[[matrix_names[rmn]]][[i]][[j]][[k]] <- matrix(0,ncol=length(roadnames),nrow=length(home_las))
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
  
  ## make dur_rc matrices
  dur_rc_mat_list <- list()
  for(i in 1:5){
    dur_rc_mat_list[[i]] <- list()
    for(j in 1:2){
      dur_rc_mat_list[[i]][[j]] <- list()
      for(k in 1:length(raw_dur_rc_mat_list[[i]][[j]])){
        dur_rc_mat_list[[i]][[j]][[k]] <- matrix(0,ncol=length(roadnames),nrow=length(home_las))
        row_j <- match(home_las,raw_dur_rc_mat_list[[i]][[j]][[k]][,1])
        row_i <- home_las %in% raw_dur_rc_mat_list[[i]][[j]][[k]][,1] 
        row_i <- row_i[!is.na(row_i)]
        row_j <- row_j[!is.na(row_j)]
        col_i <- roadnames %in% colnames(raw_dur_rc_mat_list[[i]][[j]][[k]])[-1] 
        dur_rc_mat_list[[i]][[j]][[k]][row_i,col_i] <- as.matrix(raw_dur_rc_mat_list[[i]][[j]][[k]][row_j,-1])
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
  
  
  ## duplicate car (3) for van (6)
  la_mat_list[[6]] <- la_mat_list[[3]]
  rc_mat_list[[6]] <- rc_mat_list[[3]]
  dur_rc_mat_list[[6]] <- dur_rc_mat_list[[3]]
  
  ## get synthetic populations
  synth_pops <- list()
  synth_pop_path <- paste0('../mh-execute/inputs/scenarios/', global_scen, '/')#change the folder based on the scenario, have to convert into for loop
  synth_pop_files <- list.files(synth_pop_path)
  synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('SPind_E[[:digit:]]+.Rds',x))]
  #synth_pop_files <- synth_pop_files[sapply(synth_pop_files,function(x)grepl('subdivide',x))]
  # set to data table
  for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- setDT(readRDS(paste0(synth_pop_path,synth_pop_files[i])))
  # take subset of columns
  for(i in 1:length(synth_pop_files)) synth_pops[[i]] <- 
    synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                            function(x)x%in%c('census_id','urbanmatch','demogindex')|
                              #((grepl('cycle',x)|grepl('walk',x))&grepl('wkhr',x))|
                              (grepl('base',x)|grepl('scen',x))#&grepl('wkkm',x))
    ),with=F]
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
  
  modes <- c('cycle','walk','cardrive','mbikedrive','bus','vandrive')
  mode_list <- list('cycle','walk',c('cardrive'),'mbikedrive','bus','vandrive')
  driven_modes <- c(modes)
  passenger_modes <- list('cycle','walk',c('carpass','taxi'),'mbikepass','bus','vanpass')
  add_modes <- c('cardrive','mbikedrive','vandrive')
  which_add_modes <- which(driven_modes%in%add_modes)
  
  all_distances <- list()
  scenarios <- c('base_','scen_')
  scenario <- 'base_'
  dist_cats_per_mode <- sapply(rc_mat_list,function(x)sapply(x,length))[1,]
  raw_la_mat_list <- raw_rc_mat_list <- raw_dur_rc_mat_list <- matrix_list <- raw_matrix_list <- NULL

  for(scenario in scenarios){
    scenname <- ifelse(scenario != 'base_', paste0(global_scen, '_'), 'base_')
    {
      
    # copy synthetic population
    synth_pops_scen <- list()#copy(synth_pops)
    # keep only present scenario
    for(i in 1:number_city_las) synth_pops_scen[[i]] <- 
        copy(synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),
                                     function(x)x%in%c('census_id','urbanmatch','demogindex')|
                                       grepl(scenario,x)),with=F])
    # rename scenario
    for(i in 1:number_city_las) colnames(synth_pops_scen[[i]]) <- sapply(colnames(synth_pops_scen[[i]]),function(x)gsub(scenario,'',x))
    # rename base for non-city la synthetic populations (they don't have scenarios)
    for(i in (number_city_las+1):length(synth_pops)) synth_pops_scen[[i]] <- copy(synth_pops[[i]])
    for(i in (number_city_las+1):length(synth_pops_scen)) colnames(synth_pops_scen[[i]]) <- sapply(colnames(synth_pops_scen[[i]]),function(x)gsub('base_','',x))
    # remove scenario from core synthetic population
    for(i in 1:number_city_las) synth_pops[[i]] <- synth_pops[[i]][,sapply(colnames(synth_pops[[i]]),function(x)!grepl(scenario,x)),with=F]
    names(synth_pops_scen) <- names(synth_pops)
    
    # noise : total distance per mode per LA
    # AA: exclude bus from modes to be ignored
    which_modes_noisy <- !driven_modes %in% c('cycle', 'walk')

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
    distance_sums <- distance_sums[!is.na(dist) & dist > 0] 
    
    # Assume bus occupancy to be 31 people - from ITHIM-Global (ithim-r)
    # Account for bus distance by dividing it by 31, for passenger distance
    # NOTE: get updated value for the UK
    distance_sums[distance_sums$mode_name == "bus"]$dist <- distance_sums[distance_sums$mode_name == "bus"]$dist / 31
    
    # map to destination las
    for(i in 1:length(destination_las)) distance_sums[,destination_las[i]:=.(dist*la_mat_list[[mode]][[urbanmatch+1]][[distcat]][la_index,i]), by=c('mode','dist','distcat','urbanmatch')]
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
    which_modes_strike <- which(modes%in%c('cardrive','walk','cycle','mbikedrive'))
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
    distance_sums <- distance_sums[!is.na(dist) & dist > 0]
    
    # Assume bus occupancy to be 31 people - from ITHIM-Global (ithim-r)
    # Account for bus distance by dividing it by 31, for passenger distance
    # NOTE: get updated value for the UK
    distance_sums[distance_sums$mode_name == "bus"]$dist <- distance_sums[distance_sums$mode_name == "bus"]$dist / 31
    
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
    distance_sums <- distance_sums[!is.na(dist) & dist > 0] 
    
    # Assume bus occupancy to be 31 people - from ITHIM-Global (ithim-r)
    # Account for bus distance by dividing it by 31, for passenger distance
    # NOTE: get updated value for the UK
    distance_sums[distance_sums$mode_name == "bus"]$dist <- distance_sums[distance_sums$mode_name == "bus"]$dist / 31
    
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
    
    # physical activity : duration per person per mode
    pa_pops <- list()
    cycle_cols <- sapply(colnames(synth_pops_scen[[1]]),function(x)grepl('cycle',x)&grepl('wkhr',x))
    for(i in 1:number_city_las) 
      pa_pops[[i]] <- synth_pops_scen[[i]][,.(census_id=census_id,
                                              walking_dur_pa=walk_wkhr,
                                              cycle_dur_pa=Reduce(`+`, .SD)), .SDcols=cycle_cols]
    distance_for_pa <- rbindlist(pa_pops)
    pa_pops <- NULL
    
    
    # pollution inhalation : duration per person per mode per road type per LA (we've already added passenger to driver)
    inh_modes <- driven_modes
    cols <- unlist(lapply(1:length(inh_modes),function(x)sapply(1:dist_cats[x],function(y)  paste0(inh_modes[x],'_wkkm_d',y))))
    distance_for_inh <- list()
    
    time_modes <- list('cycle','walk',c('car','taxi'),'mbike','bus','van')
  }
  # go city by city
  # city <- 'bristol'
  for(city in city_regions){
    # should include only city las
    one_city_las <- which(names(synth_pops_scen)%in%city_regions_table$lad11cd[city_regions_table$cityregion==city])
    if(length(one_city_las)>0){
      print(city)
      # mode by mode, use d1 to d4 to get total distance, then divide wkhr to wkhr_d1 to d4.
      for(m in 1:length(inh_modes)){
        mode_name <- inh_modes[m]
        for(i in one_city_las){
          ##!! temp fix
          if('cycle_wkhr_d1'%in%colnames(synth_pops_scen[[i]]))
          colnames(synth_pops_scen[[i]])[colnames(synth_pops_scen[[i]])=='cycle_wkhr_d1'] <- 'cycle_wkhr'
          if(mode_name=='walk'){
            # just rename
            if('walk_wkkm_d1'%in%colnames(synth_pops_scen[[i]])){
              colnames(synth_pops_scen[[i]])[colnames(synth_pops_scen[[i]])=='walk_wkkm_d1'] <- 'old'
            }
            colnames(synth_pops_scen[[i]])[colnames(synth_pops_scen[[i]])=='walk_wkhr'] <- 'walk_wkkm_d1'
          }else{
            mode_cols <- sapply(cols,function(x)grepl(mode_name,x))
            col_names <- cols[mode_cols]
            total_distance <- rowSums(synth_pops_scen[[i]][,colnames(synth_pops_scen[[i]])%in%col_names,with=F])
            nonzero <- total_distance>0
            time_mode <- time_modes[[m]]
            total_time <- rowSums(synth_pops_scen[[i]][,colnames(synth_pops_scen[[i]])%in%paste0(time_mode,'_wkhr'),with=F])[nonzero]
            for(j in col_names) synth_pops_scen[[i]][[j]][nonzero] <- total_time  * synth_pops_scen[[i]][[j]][nonzero]/total_distance[nonzero]
          }
        }
      }
      # expand by mode, dist_cat, home_la, participant_id
      distance_sums <- lapply(one_city_las,function(i){ 
        melt2 <- melt(synth_pops_scen[[i]],id.vars=c('census_id','urbanmatch'),measure=patterns(paste0('^',inh_modes,'_wkkm')),variable.name='distcat',value.name=paste0('mode',inh_modes), variable.factor=F)
        melt3 <- melt(melt2,id.vars=c('census_id','urbanmatch','distcat'),measure=patterns('^mode'),variable.name='mode_name',value.name='dur', variable.factor=F)
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
      distance_sums <- distance_sums[distance_sums$dur>0,]
      distance_sums <- distance_sums[!is.na(distance_sums$dur),]
      print(10)
      
      # get tube duration for later
      if(city=='london') {
        print(2)
        tube_travel <- do.call(rbind, lapply(one_city_las,function(i) synth_pops_scen[[i]][tube_wkhr>0,colnames(synth_pops_scen[[i]])%in%c('census_id','tube_wkhr'),with=F]) )
        print(3)
      }
      # clear memory
      for(i in 1:length(one_city_las)) synth_pops_scen[[i]] <- c()
      
      # map to home city las
      for(i in 1:length(one_city_las)) 
        distance_sums[,home_las[one_city_las[i]]:=.(dur*la_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][la_index,one_city_las[i]]),by=c('mode','dur','distcat','urbanmatch')]
      print(20)
      # map to roads within las
      temp_distance_for_inh <- list()
      for(i in 1:length(roadnames)){
        distance_sums_temp <- copy(distance_sums)
        for(j in 1:length(one_city_las)){
          la_col <- which(colnames(distance_sums_temp)==home_las[one_city_las[j]])
          pos_indices <- distance_sums_temp[[la_col]]>0
          if(sum(pos_indices)>0)
            distance_sums_temp[[la_col]][pos_indices] <- 
            distance_sums_temp[[la_col]][pos_indices]*distance_sums_temp[pos_indices,rc_mat_list[[mode]][[urbanmatch+1]][[as.numeric(distcat)]][j,i],by=c('mode','dur','distcat','urbanmatch','census_id','la_index')]$V1
        }
        temp_distance_for_inh[[i]] <- distance_sums_temp[,lapply(.SD,sum),by=c('mode_name','census_id'),.SDcols=home_las[one_city_las]]
        colnames(temp_distance_for_inh[[i]])[colnames(temp_distance_for_inh[[i]])%in%home_las[one_city_las]] <- paste0(home_las[one_city_las],'_',roadnames[i])
      }
      distance_sums_temp <- distance_sums <- NULL
      print(30)
      
      # reorganise to have census ids as rows and all combinations of modes, roads and las as columns.
      reorganise <- list()
      concat <- list()
      for(i in 1:length(roadnames)){
        reorganise[[i]] <- list()
        colnms <- paste0(home_las[one_city_las],'_',roadnames[i])
        # split by mode
        for(j in 1:length(inh_modes)){
          reorganise[[i]][[j]] <- temp_distance_for_inh[[i]][mode_name==inh_modes[j],colnames(temp_distance_for_inh[[i]])!='mode_name',with=F]
          colnames(reorganise[[i]][[j]])[colnames(reorganise[[i]][[j]])%in%colnms] <- paste0(colnms,'_',inh_modes[j])
        }
        # concatenate modes, omitting cycle/walk and motorway
        modes_to_include <- 1:length(inh_modes)
        if(roadnames[i]=='motorway') modes_to_include <- which(!inh_modes%in%c('cycle','walk'))
        concat[[i]] <- copy(reorganise[[i]][[modes_to_include[1]]])
        for(j in 2:length(modes_to_include)) {
          newcolnns <- paste0(colnms,'_',inh_modes[modes_to_include[j]])
          concat[[i]] <- merge(concat[[i]], reorganise[[i]][[modes_to_include[j]]],on='census_id', all=TRUE,nomatch=0)
        }
        reorganise[[i]] <- c()
      }
      temp_distance_for_inh <- NULL
      print(40)
      print(sort(sapply(ls(),function(x)object.size(get(x))))/1e9)
      # concatenate roads
      to_save <- copy(concat[[2]])
      concat[[2]] <- 0
      newcolnns <- colnames(concat[[1]])[colnames(concat[[1]])!='census_id']
      
      # browser()
      
      to_save[concat[[1]],on='census_id',paste0(newcolnns):=(paste0('i.',newcolnns))]
      # to_save <- merge(concat[[1]],concat[[2]],on='census_id',all=T)
      # for(i in 2:length(roadnames)) {
      #  concat[[i-1]] <- 0
      #  print(50)
      #  newcolnns <- colnames(concat[[i]])[colnames(concat[[i]])!='census_id']
      #  print(60)
      #  print(tail(sort(sapply(ls(),function(x)object.size(get(x))))/1e9))
      #  to_save <- merge(to_save,concat[[i]],on='census_id',all=T)
      # }
      # clear memory, remove nas, and save
      concat <- NULL
      ## add tube travel for london residents
      if(city=='london') {
        print(3)
        ##!! this is a left join so someone who does no travel but tube will be lost
        to_save[tube_travel,on='census_id',subway:=i.tube_wkhr]
        tube_travel <- NULL
        print(4)
      }else{
        to_save[,subway:=0]
      }
      for(i in 2:ncol(to_save)) set(to_save,which(is.na(to_save[[i]])),i,0)
      print(5)
      # browser()
      saveRDS(to_save,paste0('../mh-execute/inputs/distances/',scenname ,city,'_inh_distances.Rds'))
      to_save <- c()
      #distance_for_inh[[city]] <- to_save
      to_save <- NULL
    }
  }
  
  
  #saveRDS(distance_for_inh$london,paste0('../mh-execute/inputs/distances/',scenario,'london_inh_distances.Rds'))
  #distance_for_inh$london <- NULL
  saveRDS(distance_for_pa,paste0('../mh-execute/inputs/distances/',scenname , 'pa_distances.Rds'))
  distance_for_pa <- c()
  saveRDS(list(distance_for_cas=distance_for_cas,distance_for_strike=distance_for_strike),paste0('../mh-injury/rds_storage/',scenname,'injury_distances.Rds'))
  saveRDS(list(distance_for_emission=distance_for_emission,distance_for_noise=distance_for_noise),paste0('../mh-execute/inputs/distances/',scenname,'emissions_distances.Rds')) #change the file names
  
  }
  
}
###################################################################################
## a look at the distances

all_distances <- readRDS('outputs/all_distances.Rds')


str_dist <- all_distances$base_$distance_for_strike
distance_sums <- sapply(colnames(str_dist)[2+1:length(roadnames)],
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
distance_for_strike$total <- rowSums(distance_for_strike[,2+1:length(roadnames)])
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
distance_for_cas$total <- rowSums(distance_for_cas[,2+1:length(roadnames)])
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
                    function(x)sapply(colnames(str_dist)[2+1:length(roadnames)],
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
