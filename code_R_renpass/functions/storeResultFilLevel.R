# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name:
# StoreResultFilLevel
# 
# Title:
# Stores results for reservoir levels in the database
# 
# Description:
# Reservoir levels as volume and energy content are stored in the database
#
# Usage: 
# storeResultFilLevel(data, type, scenario_nr, timesteps, start_timestep,
#                     end_timestep, dpr_number)
#   
# Arguments:
# data            - list - filling level [matrix (timesteps x dpr), numeric, 
#                                         unit of the storage medium]
#                        - filling level energy [matrix (timesteps x dpr), 
#                                         numeric, GWh]
# type            - type of storage [scalar, character]
# scenario_nr     - number of the scenario [scalar, numeric]
# timesteps       - number of timesteps in the calculation [scalar, numeric]
# start_timestep  - first timestep in the calculation [scalar, numeric]
# end_timestep    - last timestep in the calculation [scalar, numeric]
# dpr_number      - dpr in the calculation [vector, numeric]
# 
# Details:
# In this function the reservoir levels for every reservoir and timestep are 
# converted into energy content and aggregated per country. Information on 
# filling level volume and energy content is combined with the scenario nr, 
# timesteps and country ids and stored in the database.
# 
# Value:
# There is no return value.
#------

storeResultFilLevel <- function(data,
                                type,
                                scenario_nr,
                                timesteps,
                                start_timestep,
                                end_timestep,
                                dpr_number) {
  
  timesteps_vector <- c(start_timestep:(end_timestep+1))
  
  timesteps_db     <- rep(timesteps_vector, each = length(dpr_number))
  regions_db       <- rep(dpr_number, times = timesteps+1)
  
  fil_reg        <- c(t(data$filling_level))
  fil_reg_energy <- c(t(data$filling_level_energy))
  
  db_dataframe  <- data.frame(cbind(scenario_nr, 
                                    timesteps_db, 
                                    regions_db,
                                    type,
                                    fil_reg, 
                                    fil_reg_energy))
  
  colnames(db_dataframe) <- c("scenario_nr", 
                              "timestep", 
                              "dpr_number",
                              "type",
                              "filling_level", 
                              "filling_level_energy")
  
  con_results <- connectMysql("results")
  
  dbWriteTable(con_results, 
               "filling_level", 
               db_dataframe, 
               row.names = FALSE, 
               append    = TRUE)
  
  dbDisconnect(con_results)
}
