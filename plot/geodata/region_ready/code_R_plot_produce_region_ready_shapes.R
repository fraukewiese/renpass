require(sp)
require(maptools)
require(rgdal)

path                 <- "/home/frauke/Documents"
database_username    <- "root"
database_password    <- "user123"
database_host        <- "127.0.0.1"
database_port        <- 3306
database_unix.socket <- NULL

setwd(path)
require(RMySQL)


for(r in c(1)){
  
  scenario_nr <- r
  ss          <- scenario_nr
  
  functions <- dir('renpass/code_R_renpass/functions')
  for(ff in functions){
    source(paste("renpass/code_R_renpass/functions/",ff,sep = ""))
  }
  source('renpass/code_R_renpass/code_R_prepare_parameters.R')
  source('renpass/code_R_renpass/code_R_prepare_regions.R')
  
  #-----------------
  # polygons needed
  #-----------------
  
  con_renpass <- connectMysql("renpass")
  
  region_parameter <- dbGetQuery(con_renpass,
                                 "SELECT region_id, country_code
                                 FROM region_parameter")
  dbDisconnect(con_renpass)
  
  countries <- region_parameter$country_code[region_parameter$region_id %in%
                                               region_dpr_assignment$region_id]
  
  germany_split <- ifelse(sum("DEU" == countries) == 1, 
                          FALSE,
                          TRUE)
  if(germany_split){
    countries <- countries[countries !="DEU"]
  }
  
  region_list <- list()
  
  for(i in seq_along(countries)){
    load(paste(path, 
               "/renpass/plot/geodata/",
               countries[i],
               "_adm0.RData",
               sep=""))
    
    rid <- region_parameter$region_id[
      region_parameter$country_code == countries[i]]
    dpr <- region_dpr_assignment$dpr_number[
      region_dpr_assignment$region_id == rid]
    
    # I use the unionSpatialPolygons just to get rid of the @data to be able to 
    # give it the same shape like the German regions, but this workaround takes
    # a bit too long to be good....
    polys     <- unionSpatialPolygons(gadm,1)
    dpr_id    <- sapply(slot(polys, "polygons"), function(x) slot(x, "ID"))
    dpr_id_df <- data.frame(dpr_id    = dpr, 
                            row.names = dpr_id)
    
    region_list[[as.character(dpr)]] <- SpatialPolygonsDataFrame(polys, dpr_id_df)
  }
  
  if(germany_split){
    germany_reg <- readShapePoly(paste(path,
                                       "/renpass/plot/geodata/DEU3_21",
                                       sep = ""),
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    
    dpr_idx     <- region_dpr_assignment$dpr_number[
      region_dpr_assignment$region_id < 12000]
    
    germany_dpr <- unionSpatialPolygons(germany_reg,
                                        dpr_idx)
    
    # add dpr_id as data and identification for SpatialPolygonDataFrame
    # has to be as DataFrame
    dpr_id    <- sapply(slot(germany_dpr, "polygons"), function(x) slot(x, "ID"))
    dpr_id_df <- data.frame(dpr_id    = sort(unique(dpr_idx)), 
                            row.names = as.character(sort(as.numeric(dpr_id))))
    
    # Assign the id informations, now it 
    germany_dpr <- SpatialPolygonsDataFrame(germany_dpr, 
                                            dpr_id_df)
    
    region_list[c(2:(length(region_list) + 1))] <- region_list
    
    
    region_list[[1]] <- germany_dpr
  }
  
  # To rbind the different SpatialPolygonDataFrames we need unique IDs (those 
  # always start with 1, thus are not unique if we combine different)
  
  # function to extract and count the ids
  
  getID <- function(spdf){
    number_of_IDs <- length(row.names(as(spdf, "data.frame")))
    return(number_of_IDs)
  }
  
  # apply to the region_list
  number_of_id <- lapply(region_list,
                         function(x) getID(x))
  
  nofi  <- unlist(number_of_id)
  nofic <- cumsum(nofi)
  nofic_start <- c(1,nofic+1)
  nofic_end   <- c(nofic,0)
  
  newID_list <- list()
  
  for(i in seq_along(number_of_id)){
    newID_list[[i]] <- c(nofic_start[i]:nofic_end[i])
  }
  
  transformed <- list()
  
  # all regions get new ID to be able to spRbind them
  for(i in seq_along(region_list)){
    transformed[[i]] <- spChFIDs(region_list[[i]], 
                                 as.character(newID_list[[i]]))
    if(i == 1){
      all_regions <- transformed[[i]]
    }else{
      all_regions <- spRbind(all_regions,transformed[[i]])
    }
  }
  
  # dpr_id are still there, that is something else than the ID
  all_regions@data
  
  par(mar = rep(0,4),
      oma = rep(0,4))
  
  dimens <- bbox(all_regions)
  w_to_h <- diff(dimens[1,])/diff(dimens[2,])
  width  <- 800
  height <- width * w_to_h
  ##############################
  # width and height abhaengig machen von bbox, also wie gross alles ist
  
  # save it
  writePolyShape(all_regions,
                 #proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"),
                 paste("/home/frauke/Documents/renpass/plot/geodata/region_ready/",
                       region_scenario,
                       sep = ""))
}



# # I read it in again for joining it with dpr because there is one additional
# # row in @data than, that DEU3_dpr also has and it must be the same in both
# sea <- readShapePoly("/home/frauke/Documents/renpass/figures/geodata/DEU_sea",
#                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# 
# png(width  = width,
#     height = height,
#     file   = paste(folder_path,
#                    "/area_and_dpr_scenario_nr",
#                    scenario_nr,
#                    ".png",
#                    sep = ""))
# 
# plot(all_regions)
# 
# # plot dpr_id into it
# invisible(text(coordinates(all_regions), 
#                labels = as.character(all_regions$dpr_id),
#                cex = 1))
# 
# dev.off()
