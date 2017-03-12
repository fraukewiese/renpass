# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readWeatherScenario

# Title: Read the Pathway Parameters From the Database  

# Description: This functions reads the installed capacities of the weather 
#              dependant energy sources form the pathways database

# Usage: readWeatherScenario(table_name, scenario,Region_vector = region_vector)

# Arguments:
# table_name     table name of pathways scenario to be read from the database
#                 [scalar(character)]
# scenario       name of the chosen scenario [scalar(character)]
# Region_vector  Standard: region_vector

# Value: data.frame of 
#        region_id (numeric)
#        installed capacity (numeric)
#----------

readWeatherScenario <- function(table_name, 
                                scenario,
                                Region_vector = region_vector){
  
  con_pathways <- connectMysql("pathways")
  
  sql_command  <- paste("SELECT region_id, installed_capacity",
                        " FROM ", table_name, 
                        " WHERE scenario_name = '", scenario,
                        "' AND region_id IN (",
                        paste(Region_vector, collapse=","),
                        ") AND installed_capacity != 0", sep="")
  
  raw_data <- dbGetQuery(con_pathways, sql_command)
  
  dbDisconnect(con_pathways)
  
  return(raw_data)
}
