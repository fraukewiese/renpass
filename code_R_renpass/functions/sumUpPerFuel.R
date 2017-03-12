# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: sumUpPerFuel

# Title: Sum up the capacity of power plants with the same fuel

# Description: The functions looks for power plants with the chosen fuel and
#              sums up their capacity. This could be used for example to receive
#              the electricity production per fuel after dispatch and exchange
#              out of the merit order              

# Usage: sumUpPerFuel(fuel, pp_order = used_plants)

# Arguments:
# pp_order   a data.frame of the merit order with at least the columns 
#            pp_nr (numeric),
#            available_capacity (numeric)
#            Standard: used_plants
# fuel       name of fuel as character, e.g. "gas" 

# Value: a numeric value in the same unit as the available_capacity in the
#        pp_order: the sum of electricity production of power plants of the
#        chosen fuel
#--------

sumUpPerFuel <- function(fuel,
                         pp_order = used_plants){
  
  idx                    <- which(pp_order$fuel == fuel)
  
  if(length(idx) == 0){
    electricity_production <- 0
  }else{
    electricity_production <- sum(pp_order$available_capacity[idx])
  }
  
  return(electricity_production)
}
