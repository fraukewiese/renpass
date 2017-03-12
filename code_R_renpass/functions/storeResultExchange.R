# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: StoreResultExchange
# 
# Title:
# Stores results of exchange data in the database
# 
# Usage: 
# storeResultExchange <- function(data, no_lines, scenario_nr, timesteps, 
# start_timestep, end_timestep)
#   
# Arguments:
# data            - dataframe containing the data to be stored 
#                   [dataframe: plus_region_id (numeric), 
#                   minus_region_id (numeric), capacity_used (numeric) , 
#                   grid_loss (numeric)]
# table_name        - destination database table name [scalar, string]
# no_lines        - number of grid connections [scalar, numeric]
# scenario_nr     - number of the scenario [scalar, numeric]
# timesteps       - number of timesteps in the calculation [scalar, numeric]
# start_timestep  - last timestep in the calculation [scalar, numeric]
# end_timestep    - last timestep in the calculation [scalar, numeric]
# 
# Value:
#   There is no return value.
#-------

storeResultExchange <- function(data,
                                table_name,
                                no_lines,
                                scenario_nr,
                                timesteps,
                                start_timestep,
                                end_timestep) {
  
  timesteps_vector <- c(start_timestep:end_timestep)
  timesteps_db     <- rep(timesteps_vector, each = no_lines)
      
  # exchange_data vorbereiten
  db_dataframe     <- data.frame(cbind(scenario_nr, 
                                       timesteps_db, 
                                       data,
                                       abs(data[,3] * data[,4])))
  colnames(db_dataframe)<- c("scenario_nr", 
                             "timestep", 
                             "plus_dpr_number", 
                             "minus_dpr_number", 
                             "capacity_used" , 
                             "grid_loss_rel",
                             "grid_loss_abs")
   
  con_results <- connectMysql("results")
  
  dbWriteTable(con_results, 
               table_name, 
               db_dataframe, 
               row.names = FALSE, 
               append = TRUE)
  
  dbDisconnect(con_results)
  
  rm(data)
  
}
