# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql
# functions defined within this piece of code: prepareRegions
#-----
# Name: prepareRegions

# Title: Prepare the Dispatch Region Layout  

# Description: This functions sets up a connection to the database, reads which
#              regions take part in this chosen scenario and has as an output
#              the vector of the original region_ids and a continous vector
#              giving the dispatch regions continous numbers

# Usage: prepareRegions(region_scenario)

# Arguments:
# region_scenario   name of region scenario [scalar(character)]

# Details: Parameters for the chosen Pathway are read from the Database

# Value: 
# region_vector           region_ids of the smallest unit of regions that are in
#                         renpass [vector, numeric]
# region_dpr_assignement  the assignement of the region_ids to the dpr_numbers,
#                         the dpr_numbers are the simulation regions for this
#                         scenario. [data.frame: region_id(vector,numeric),
#                                                dpr_number(vector,numeric)]
# dpr_number              the regions from one, to number of regions, in
#                         ascending order
#                         [vector,numeric]
# number_of_regions       [scalar,numeric]
#-----

prepareRegions <- function(region_scenario){

  # Get the region scenario from the database
  con_pathways <- connectMysql("pathways")
  
  scenario_sql <- paste("SELECT * FROM region_scenario 
                        WHERE scenario_name = '",
                        region_scenario,
                        "'", 
                        sep = "")
  
  region_data <- dbGetQuery(con_pathways, scenario_sql)
  
  # If the region_scenario is not defined in the scenario_nr table in the
  # scenario_parameters database, it stops and there is an error message
  
  if(length(region_data) == 0){
    stop(paste(
      "no pathway for chosen region_scenario ",
      region_scenario,
      " defined",
      sep = ""))
  }
   
  region_data <- region_data[order(region_data$region_id),]
  
  dbDisconnect(con_pathways)

  region_vector         <- unique(as.numeric(region_data$region_id))
  region_dpr            <- as.numeric(region_data$dpr_number)
  region_dpr_assignment <- data.frame(region_id  = region_vector,
                                      dpr_number = region_dpr)
  dpr_number            <- sort(unique(region_dpr))
  number_of_regions     <- length(dpr_number)

  return(list(region_vector,
              region_dpr_assignment,
              dpr_number,
              number_of_regions))
}

a <- prepareRegions(region_scenario = region_scenario)

region_vector           <- a[[1]]
region_dpr_assignment   <- a[[2]]
dpr_number              <- a[[3]]
number_of_regions       <- a[[4]]

remove(a)
