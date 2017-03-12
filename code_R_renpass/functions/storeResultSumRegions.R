# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name:
# StoreResultSumRegions
# 
# Title:
# Stores results of standard matrix format summarized values for all regions
# 
# Description:
# The function binds the data together with information on time and region in 
# a data frame and stores it in the data base, one value per timestep,
# summarized for all regions
# 
# Usage: 
# storeResultSumRegions(data, table_name, column_name, scenario_nr, 
#                       start_timesteps, end_timestep)
#   
# Arguments:
# data              - matrix containing the data to be stored 
#                     [matrix (timesteps x regions), numeric]
# table_name        - destination database table name [scalar, string]
# column_name       - destination table column name [scalar, character]
# scenario_nr       - number of the scenario [scalar, numeric]
# start_timestep    - first timestep in the calculation [scalar, numeric]
# end_timestep      - last timestep in the calculation [scalar, numeric]

# Details:
# In the function the data matrix is converted to a vector and summarized for
# all regions. Combined with scenario number and timestep in a data frame. This
# data frame is then written into the chosen database table.
# 
# Value:
#   There is no return value.
#------

storeResultSumRegions <- function(data,
                                  table_name,
                                  column_name,
                                  scenario_nr,
                                  start_timestep,
                                  end_timestep){
  
  timesteps_vector <- c(start_timestep:end_timestep)
  data_vector      <- rowSums(data)
  
  db_dataframe     <- data.frame(cbind(scenario_nr, 
                                       timesteps_vector,
                                       data_vector))
  
  colnames(db_dataframe) <- c("scenario_nr", 
                              "timestep",
                              column_name)
  
  con_results <- connectMysql("results")
  
  dbWriteTable(con_results, 
               table_name, 
               db_dataframe, 
               row.names = FALSE, 
               append    = TRUE)
  
  dbDisconnect(con_results)
}
