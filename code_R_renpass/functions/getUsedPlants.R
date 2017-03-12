# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: getUsedPlants

# Title: Get all the utilised plants of a chosen fuel

# Description: The power plant id and the available capacity of all utilised
#              plants of the chosen fuel are given in a data.frame

# Usage: getUsedPlants(pp_order = used_plants, fuel)

# Arguments:
# pp_order   a data.frame of the merit order with at least the columns 
#            pp_nr (numeric),
#            available_capacity (numeric)
# fuel       name of fuel as character, e.g. "gas"

# Details: 

# Value: data.frame with the columns pp_nr and available_capacity of all
#        power plants in the given pp_order. If there is no power plant of this
#        fuel the result is NULL
#---------------

getUsedPlants <- function(fuel,
                          pp_order = used_plants){
  
  idx                    <- which(pp_order$fuel == fuel)
  
  if(length(idx) == 0){
    plants_in_action <- NULL
  }else{
    plants_in_action <- pp_order[idx,c("pp_nr","available_capacity")]
  }
  
  return(plants_in_action) 
}
