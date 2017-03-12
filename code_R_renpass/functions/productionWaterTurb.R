# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# productionWaterTurb
# 
# Title:
# Calculates change in filling level from prodcution data   
# 
# Description:
# From the data on power production from storage plants filling level change in 
# volume for each reservoir is calulated
# 
# Usage:
# productionWaterTurb (hydro_production, connection_data, connection, 
#                      idx_connection_reservoir, share_res_per_plant, 
#                      energy_factor, u_flo)
# 
# Arguments:
# hydro_production         - dataframe with data on power production from hydro
#                            storage plants [dataframe: pp_nr (numeric), 
#                            available_capacity (numeric)]
# connection_data          - dataframe with information on the connection of 
#                            storage plants and reservoirs [dataframe: plant_id
#                            (numeric), res_id (numeric), efactor (numeric)]
# connection               - "upper" or "lower", indicates direction of 
#                            connection [scalar, string]
# idx_connection_reservoir - index which relates the connection table to the 
#                            reservoir dataframe [vector, numeric]
# share_res_per_plant      - share that each reservoir has in the operation of a
#                            connected storage plant, between 0 and 1 [vector,
#                            numeric]
# energy_factor            - conversion factor between energy and power, 1 when 
#                            the time unit is hours, 0.25 when the time unit is
#                            quarters [scalar, numeric]
# u_flo                    - unregulated inflow to the storage plants, in the 
#                            order of the connection table [vector, numeric]
# 
# Details:
# The power production from power plants are transformed into transfered water. 
# In addition to the production the unregulated inflow needs to be considered. 
# The filling level change in the upper reservoir is reduced by the unregulated 
# inflow that can be used for procuction. The filling level change in the lower 
# reservoir will be at least the unregulated inflow. The transfered water is 
# aggregated per reservoir. 
# 
# Value:
# water_per_res - vector that contains filling level change in mio cbm for each
#                 reservoir [vector, numeric]
#-----

productionWaterTurb <- function (hydro_production, 
                                 connection_data, 
                                 connection,
                                 idx_connection_reservoir,
                                 share_res_per_plant,
                                 energy_factor,
                                 u_flo) {
  
  # if-loop is a fast fix from Marian and Gesine
  # if hydro_production NULL elp_river initialized and filled with zero
  if(length(hydro_production) == 0){
    production_connection <- rep(0, nrow(connection_data))
  }else{
  
    idx_production <- match(connection_data$plant_id, hydro_production$pp_nr)

    production_connection <- hydro_production$available_capacity[idx_production]

    production_connection <- ifelse(is.na(production_connection), 0, 
                                  production_connection)
  }
  
  if (connection == "upper") {
    
    water <- (production_connection * share_res_per_plant * 0.001 * 
         energy_factor / connection_data$efactor) - u_flo * share_res_per_plant

   } else {
    
    water <- pmax(production_connection * share_res_per_plant * 0.001 * 
      energy_factor / connection_data$efactor, u_flo * share_res_per_plant )
      
  }
  
  water <- ifelse(water < 0, 0, water)
  water <- ifelse(is.na(water), 0, water)

  water_sum <- aggregate (water, list(connection_data$res_id), sum)

  water_per_res <- water_sum[idx_connection_reservoir, 2]
  
  water_per_res <- ifelse(is.na(water_per_res), 0, water_per_res)  

  return(water_per_res)
}
