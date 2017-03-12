# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, readPathway, marginalIndex
# function defined within this piece of code: hydroStorageScenario
#-----
# Name:
# hydroStorageScenario
# 
# Title:
# Generate data frame with new hydro storage plants
# 
# Description:
# For a storage scenario as defined in the database a data frame with actual or 
# generic hydro storage plants is generated.
# 
# Usage:
# hydroStorageScenario(scenario, scenario_year, region_vector)
# 
# Arguments:
# scenario       - name of the storage scenario [scalar, string]
# scenario_year  - year for the calculations [scalar, numeric]
# region_vector  - vector of regions in the scenario [vector, numeric]
# 
# Details:
# For a defined storage scenario a value for new installed hydro storage 
# capacity is loaded from the scenario database for each region. From the 
# renpass database all available new hydro storage plants are pulled. The 
# function generates a dataframe with new storage hydro plants which includes as
# much new storage capacity as is needed to fulfill the scenario requirements. 
# Turbines and pumps are separated for the following calculations.
# 
# Value:
# new_turb    - dataframe that includes all new turbines in the scenario 
#               [dataframe: id (numeric), pinst (numeric), reg (numeric), 
#               year (numeric), flo_river (numeric)] 
# new_pump    - dataframe that includes all new pumps in the scenario 
#               [dataframe: id (numeric), pinst (numeric), reg (numeric), 
#               year (numeric)]
# stor_reduce - vector that indicates for each region if hydro storage capacity
#               in the scenario is lower than the capacity of plants in the 
#               renpass database, negative values give the amount that has to be
#               reduced, length = number of regions, [vector, numeric]
#-----------------------------------------------------------------------------

hydroStorageScenario <- function(scenario,
                                 scenario_year,
                                 region_vector){
  
  con_renpass <- connectMysql("renpass")
  
  # Load storage scenario data
  st_scenario     <- readPathway(scenario      = "hydro_storage",
                                 scenario_name = scenario,
                                 order_by      = "region_id")
  
  # Only the area included in the scenario is chosen
  idx 			      <- match(region_vector, st_scenario$region_id) 
  
  stor_pot 		    <- as.numeric(st_scenario$capacity[idx])
  stor_pot[which(is.na(stor_pot) == TRUE)] 		<- 0
  
  # Already installed capacity
  hydro_status <- dbGetQuery(con_renpass, 
                            "SELECT region_id, sum(Pinst) AS capacity
                             FROM storage_register
                             WHERE type = 'PK'
                             GROUP BY region_id")
  
  idx 			      <- match(region_vector, hydro_status$region_id)
  stor_quo 		    <- as.numeric(hydro_status$capacity[idx])
  stor_quo[which(is.na(stor_quo) == TRUE)] 		<- 0
  
  # Difference of already installed and scenario value
  stor_change 		<- stor_pot - stor_quo
  stor_ext        <- ifelse(stor_change > 0, stor_change, 0)
  stor_reduce     <- ifelse(stor_change < 0, stor_change, 0)
  
  #-------------------------
  # New storage capacity
  new_pump        <- dbGetQuery(con_renpass, 
                                "SELECT stor_nr, pinst, region_id, year
                                 FROM storage_register 
                                 WHERE type = 'NP' 
                                 ORDER BY stor_nr")
  
  id        <- c(as.numeric(as.character(new_pump$stor_nr)))
  pinst     <- c(as.numeric(as.character(new_pump$pinst)))
  reg       <- c(as.numeric(as.character(new_pump$region_id)))
  year      <- c(as.numeric(as.character(new_pump$year)))
  
  new_pump <- data.frame(id, pinst, reg, year)
    
  pump_ext <- vector("list", length(region_vector))
  
  new_turb    <- dbGetQuery(con_renpass, 
                            "SELECT stor_nr, pinst, region_id, year, flo_river
                             FROM storage_register 
                             WHERE type = 'NPK' 
                             ORDER BY stor_nr")
  
  id        <- c(as.numeric(as.character(new_turb$stor_nr)))
  pinst     <- c(as.numeric(as.character(new_turb$pinst)))
  reg       <- c(as.numeric(as.character(new_turb$region_id)))
  year      <- c(as.numeric(as.character(new_turb$year)))
  flo_river <- c(as.numeric(as.character(new_turb$flo_river)))
  flo_river <- ifelse(is.na(flo_river), 0, flo_river)
  
  new_turb <- data.frame(id, pinst, reg, year, flo_river)
  
  turb_ext <- vector("list", length(region_vector))
  
  # Chose the storage plants needed to suffice the parameter stor_ext.
  # This is done within a region loop.
  
  for (rr in region_vector){
    idx_region             <- which(rr == region_vector)
    
    if (stor_ext[idx_region]> 0){
      pump_ext_reg <- subset(new_pump, new_pump$reg == rr)
      turb_ext_reg <- subset(new_turb, new_turb$reg == rr)
      
      cum_new_pump <- cumsum(pump_ext_reg$pinst)
      
      mar <- marginalIndex(stor_ext[idx_region],cum_new_pump,pump_ext_reg$id)
      
      idx_pump_new <- as.numeric(as.character(mar$m_idx))
      
      pump_ext_reg <- pump_ext_reg[1:idx_pump_new,]
      turb_ext_reg <- turb_ext_reg[1:idx_pump_new,]
      
      if(idx_pump_new > 1) {
        pump_ext_reg$pinst[idx_pump_new] <- stor_ext[idx_region] - 
                                            cum_new_pump[idx_pump_new - 1]
        turb_ext_reg$pinst[idx_pump_new] <- stor_ext[idx_region] - 
                                            cum_new_pump[idx_pump_new - 1]
      } else {
        pump_ext_reg$pinst[idx_pump_new]  <- stor_ext[idx_region] 
        turb_ext_reg$pinst[idx_pump_new]  <- stor_ext[idx_region]
      }
      
      pump_ext[[idx_region]]         <- pump_ext_reg
      turb_ext[[idx_region]]         <- turb_ext_reg 
      
    }else{
      
    }
  }
  
  new_turb                  <- do.call("rbind", turb_ext)
  new_pump                  <- do.call("rbind", pump_ext)
  
  dbDisconnect(con_renpass)
  
  return(list(new_turb = new_turb, 
              new_pump = new_pump, 
              stor_reduce = stor_reduce))
}

hydro_storage <- hydroStorageScenario(scenario      = hydro_storage_scenario,
                                      scenario_year = scenario_year,
                                      region_vector = region_vector)
new_turb    <- hydro_storage[[1]]
new_pump    <- hydro_storage[[2]]
stor_reduce <- hydro_storage[[3]]
