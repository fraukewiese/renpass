# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass function: connectMysql
#-----
# Name: readResourcesPathway

# Title: Read the Pathway Parameters From the Database  

# Description: This functions sets up a connection to the database and reads all
#              parameters for the pathway chosen in renpass. The output is a
#              dataframe of all the parameters for this pathway ordered by the
#              chosen column, standard should be ordered by region_id

# Usage: readPathway(scenario, scenario_name, scenario_year, order_by)

# Arguments:
# scenario       which kind of scenario should be read from the database: 
#                region / demand / re / storage / wind / solar / grid /economics 
# scenario_name  name of the chosen scenario
# scenario_year  year of the chosen scenario
# order_by       the data should be ordered by the column with this heading

# Details: Parameters for the chosen Pathway are read from the Database and
#          ordered by the chosen column, the normal case should be that it is
#          ordered by region_id

# Value: data.frame of the parameters, different for each pathway
#---------

readResourcesPathway <- function(scenario, 
                                 scenario_name, 
                                 scenario_year, 
                                 order_by){
  
  con_pathways <- connectMysql("pathways")
  
  scenario_sql <- paste("SELECT * FROM ", 
                        scenario, 
                        "_scenario WHERE ",
                        "scenario_name = '",
                        scenario_name,
                        "' AND scenario_year = '",
                        scenario_year,
                        "'", 
                        sep = "")
  
  pathway_data <- dbGetQuery(con_pathways, scenario_sql)
  
  # If the resoureces_scenario is not defined in the scenario_nr table in the
  # pathways database, it stops and there is an error message
  
  if(length(pathway_data) == 0){
    stop(paste(
      "no pathway for chosen ", 
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
