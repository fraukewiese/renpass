# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# productionWaterPump
# 
# Title:
# Calculates change in filling level from pumping data   
# 
# Description:
# From the data on power pumping from storage plants filling level change in 
# volume for each reservoir is calulated
# 
# Usage:
# productionWaterPump(pumping, connection_data, idx_connection_reservoir, 
#                     share_res_per_plant, energy_factor)
# 
# Arguments:
# pumping                  - vector with pumping power in the current timestep 
#                            for each pump in the order of the connection table
#                            [vector, numeric]
# connection_data          - dataframe with information on the connection of 
#                            storage plants and reservoirs [dataframe: plant_id
#                            (numeric), res_id (numeric), efactor (numeric)]
# idx_connection_reservoir - index which relates the connection table to the 
#                            reservoir dataframe [vector, numeric]
# share_res_per_plant      - share that each reservoir has in the operation of a
#                            connected storage plant, between 0 and 1 [vector,
#                            numeric]
# energy_factor            - conversion factor between energy and power, 1 when 
#                            the time unit is hours, 0.25 when the time unit is
#                            quarters [scalar, numeric]
# 
# Details:
# The pumping from power plants is transformed into transfered water. The 
# transfered water is aggregated per reservoir.
# 
# Value:
# water_per_res - vector that contains filling level change in mio cbm for each
#                 reservoir [vector, numeric]
#--------

productionWaterPump <- function (pumping, 
                                 connection_data, 
                                 idx_connection_reservoir,
                                 share_res_per_plant,
                                 energy_factor) {

  water <- pumping * share_res_per_plant * 0.001 * energy_factor / 
           connection_data$efactor
  
  water <- ifelse(is.na(water), 0, water)
  
  water_sum <- aggregate(water, list(connection_data$res_id), sum)
  
  water_per_res <- water_sum[idx_connection_reservoir, 2]
  
  water_per_res <- ifelse(is.na(water_per_res), 0, water_per_res)  
  
  return(water_per_res)
}
