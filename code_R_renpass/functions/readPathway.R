# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readPathway

# Title: Read the Pathway Parameters From the Database  

# Description: This functions sets up a connection to the database and reads all
#              parameters for the pathway chosen in renpass. The output is a
#              dataframe of all parameters for this subscenario ordered by the
#              chosen column, standard should be ordered by region_id

# Usage: readPathway(scenario, scenario_name, order_by)

# Arguments:
# scenario       table name of scenario to be read from the database 
# scenario_name  name of the chosen scenario
# order_by       the data should be ordered by the column with this heading

# Details: Parameters for the chosen Pathway are read from the Database and
#          ordered by the chosen column, the normal case should be that it is
#          ordered by region_id

# Value: data.frame of the parameters, different for each pathway
#----------

readPathway <- function(scenario, 
                        scenario_name,
                        order_by){
  
  con_pathways <- connectMysql("pathways")
  
  scenario_sql <- paste("SELECT * FROM ", 
                        scenario, 
                        "_scenario WHERE ",
                        "scenario_name = '",
                        scenario_name,
                        "'", 
                        sep = "")
  
  pathway_data <- dbGetQuery(con_pathways, scenario_sql)
  
  # If the scenario is not defined in the scenario_nr table in the
  # scenario_parameters database, it stops and there is an error message
  
  if(length(pathway_data) == 0){
    stop(paste("no pathway for chosen ", 
               scenario, 
               "_scenario ", 
               scenario_name, 
               " defined", 
               sep = ""))
  }
  
  pathway_data <- pathway_data[order(eval(parse(text = paste(
                                                       "pathway_data$",
                                                       order_by,
                                                       sep = "")))),]
  
  dbDisconnect(con_pathways)
  
  return(pathway_data)
}
