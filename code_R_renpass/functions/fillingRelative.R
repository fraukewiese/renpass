# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# fillingRelative
# 
# Title:
# Calculate the relative available filling level for each storage plant
# 
# Description:
# For each storahe plant current aggregated filling levels are related to the 
# maximum capacity
# 
# Usage:
# fillingRelative (fil, fil_max_sum, share_per_plant, connection_data, 
#                  idx_plant)
# 
# Arguments:
# fil             - vector with current filling level in mio cmb for each 
#                   reservoir in the order of the connection table 
#                   [vector, numeric]
# fil_max_sum     - vector with maximum filling levels in mio cbm, aggregated 
#                   per storage plant [vector, numeric]
# share_per_plant - vector with the share that each power plant has in a 
#                   reservoir, must be between 0 and 1, in the order of the
#                   connection table [vector, numeric]
# connection_data - dataframe with information on the connection of storage 
#                   plants and reservoirs [dataframe: plant_id (numeric), 
#                   res_id (numeric), efactor (numeric)]
# idx_plant       - index that relates the plant to the connection data
#                   [vector, numeric]
#
# Details:
# The current filling levels in mio cbm are aggregated per storage plant and 
# divided by the maximum aggregated filling levels. The result is brought into
# the order of the storage plants.
#
# Value:
# share_type - relative available filling level for each storage plant, must be 
#              in the range of 0 and 1 [vector, numeric]
#-------

fillingRelative <- function(fil, 
                            fil_max_sum,
                            share_per_plant,
                            connection_data, 
                            idx_plant) {
  
  sum_fil <- aggregate(fil*share_per_plant, list(connection_data$plant_id), 
                       sum)[,2]
  
  share <- sum_fil/fil_max_sum
  
  share_type <- share[idx_plant]
  
  return(share_type = share_type)
}
