# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# fillingEnergy
# 
# Title:
# Calculate available energy for production and pumping from filling levels
# 
# Description:
# From the current filling levels for each reservoir the available energy and 
# the share of each reservoir in the operation of a storage plant is calculated.

# Usage:
# fillingEnergy (fil, fil_max, restriction, share_per_plant, connection_data, 
#                idx_plant, idx_sum_connection, ...)
# 
# Arguments:
# fil                - vector with current filling level in mio cmb for each 
#                      reservoir in the order of the connection table 
#                      [vector, numeric]
# fil_max            - vector with maximum filling levels in mio cbm for each
#                      reservoir in the order of the connection table 
#                      [vector, numeric]
# spill              - vector with forecast spillage in mio cbm for each
#                      reservoir in the order of the connection table 
#                      [vector, numeric]
# restriction        - either "water" or "space" indicating if available water 
#                      or available storage space is restricting the operation
#                      [scalar, string]
# share_per_plant    - vector with the share that each power plant has in a 
#                      reservoir, must be between 0 and 1, in the order of the
#                      connection table [vector, numeric]
# connection_data    - dataframe with information on the connection of storage 
#                      plants and reservoirs [dataframe: plant_id (numeric), 
#                      res_id (numeric), efactor (numeric)]
# idx_plant          - index that relates the plant to the connection data
#                      [vector, numeric]
# idx_sum_connection - index that relates the plant ids in the connection table
#                      to its unique vector which results from aggregation
#                      [vector, numeric]
# mw_to_mwh          - conversion factor between energy and power, 1 when the 
#                      time unit is hours, 0.25 when the time unit is quarters 
#                      [scalar, numeric]
#
# Details:
# The filling levels of the reservoirs or the storage space respectively are 
# transformed into available power. The are then aggregated per storage plant. 
# For each plant also the share in the reservoir is considered. For each 
# reservoir also the share in the operation of the connected storage plants is
# calculated. This depends on the current filling levels of the reservoirs.
# 
# Value:
# available_energy - boundary for production or pumping in the order of the 
#                    selected plant type, in MW [vector, numeric]
# res_share        - share that each reservoir has in the operation of a 
#                    connected storage plant, between 0 and 1 [vector, numeric]
# total_spillage   - vector with total forecast spillage in terms of energy for
#                    each connected plant, used in the price setting 
#                    [vector, numeric]
#--------- 

fillingEnergy <- function(fil, 
                          fil_max,
                          spill,
                          restriction,
                          share_per_plant,
                          connection_data, 
                          idx_plant,
                          idx_sum_connection,
                          mw_to_mwh = energy_factor) {
  
  
  if(restriction == "water"){
    
    fil_energy   <- fil * 1000 * share_per_plant * connection_data$efactor /
      mw_to_mwh
     
  } else{
    
    fil_energy <- (fil_max - fil) * 1000 * share_per_plant * 
      connection_data$efactor / mw_to_mwh
        
  }  
  
  spill_energy <- spill * 1000 * share_per_plant * connection_data$efactor /
    mw_to_mwh
  sum_spill  <- aggregate(spill_energy, list(connection_data$plant_id), sum)
  
  sum_energy <- aggregate(fil_energy, list(connection_data$plant_id), sum)
  
  available_energy <- sum_energy[idx_plant,2]
  total_spillage   <- sum_spill[idx_plant, 2]
  
  res_share <- fil_energy / sum_energy[idx_sum_connection,2]
  
  return(list(available_energy = available_energy,
              res_share = res_share,
              total_spillage = total_spillage))
}
