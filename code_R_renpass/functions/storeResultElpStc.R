# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name:
# StoreResultElpStc
# 
# Title:
# Stores electricity production for a specific fuel in the database
# 
# Description:
# The function binds the data on electricity production together with 
# information on time and region in a data frame and stores it in the data base.
# 
# Usage: 
# storeResultElpStc <- function(data, fuel, ...)
#   
# Arguments:
# data              - matrix containing the data to be stored 
#                     [matrix (timesteps x regions), numeric]
# table_name        - destination database table name [scalar, string]
#                      Standard: "electricity_production"
# column_name       - destination table column name [scalar, character]
#                      Standard: "electricity_production"
# type              - chosen type, must correspond to the data [scalar, string]
# scenario_nr       - number of the scenario [scalar, numeric]
# timesteps         - number of timesteps in the calculation [scalar, numeric]
# start_timestep    - start timestep in the calculation [scalar, numeric]
# end_timestep      - last timestep in the calculation [scalar, numeric]
# number_of_regions - [scalar, numeric = length(dpr_number)]
# dpr_number        - dispatch regions in the calculation [vector, numeric]
# 
# Details:
# In the function the data matrix is converted to a vector. Information on time 
# and region is made into matching vectors and all is combined together with 
# sceanrio number and fuel name in a data frame. This data frame is then written
# into the chosen database table.
# 
# Value:
#   There is no return value.
#-----

storeResultElpStc <- function(data,
                              table_name  = "electricity_production",
                              column_name = "electricity_production",
                              type,
                              scenario_nr,
                              timesteps,
                              start_timestep,
                              end_timestep,
                              number_of_regions,
                              dpr_number) {
  
  timesteps_vector <- c(start_timestep:end_timestep)
  data_vector      <- c(t(data))
  timesteps_db     <- rep(timesteps_vector, 
                          each = number_of_regions)
  regions_db       <- rep(dpr_number, 
                          times = timesteps)
   
  db_dataframe  <- data.frame(cbind(scenario_nr, 
                                    timesteps_db, 
                                    type, 
                                    regions_db, 
                                    data_vector))
  
  colnames(db_dataframe) <- c("scenario_nr", 
                              "timestep", 
                              "type", 
                              "dpr_number", 
                              column_name)

  rm(data)
  
  con_results <- connectMysql("results")
  
  dbWriteTable(con_results, 
               table_name, 
               db_dataframe, 
               row.names = FALSE, 
               append    = TRUE)
  
  dbDisconnect(con_results)
}
