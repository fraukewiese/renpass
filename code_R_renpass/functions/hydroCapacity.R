# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: fillingEnergy, fillingRelative
#-----
# Name:
# hydroCapacity
# 
# Title:
# Calculate the available capacity from hydro storage plants
# 
# Description:
# In this part the available production and pumping capacity from hydro 
# storage plants is calculated from the reservoir filling levels and inflow.
# 
# Usage:
# hydro_capacity(turb, pump, reservoir...)
# 
# Arguments:
# tt                 - loop index for the timestep [scalar, numeric]
# end_timestep       - last timestep to be simulated [scalar, numeric]
# reservoir          - dataframe with data on the reservoirs [dataframe: id 
#                      (numeric), fil_max (numeric), total_efactor (numeric)]
# fil                - matrix with filling levels of the reservoirs [matrix 
#                      (timesteps x reservoirs), numeric]
# fil_max_up         - maximum filling levels in the order of the upper 
#                      connection table in mio cbm [vector, numeric]
# fil_max_lo         - maximum filling levels in the order of the lower 
#                      connection table in mio cbm [vector, numeric]
# sum_fil_max_upper  - aggregated maximum filling levels per plant id in mio cbm
#                      [vector, numeric]
# sum_fil_max_lower  - aggregated maximum filling levels per plant id in mio cbm
#                      [vector, numeric]
# turb               - dataframe with data on storage turbines [dataframe: id
#                      (numeric), pinst (numeric), reg (numeric), year 
#                      (numeric), flo_river (numeric)]
# pump               - dataframe with data on storage pumps [dataframe: id
#                      (numeric), pinst (numeric), reg (numeric), year 
#                      (numeric)]
# upper              - dataframe with data on the connection of storage plants
#                      to their upper reservoirs [dataframe: plant_id (numeric),
#                      res_id (numeric), efactor (numeric)]
# lower              - dataframe with data on the connection of storage plants
#                      to their lower reservoirs [dataframe: plant_id (numeric),
#                      res_id (numeric), efactor (numeric)]
# idx_turb_upper     - index relating turbine ids to unique plant ids in the 
#                      upper connection table [vector, numeric]
# idx_turb_lower     - index relating turbine ids to unique plant ids in the 
#                      lower connection table [vector, numeric]
# idx_pump_upper     - index relating pump ids to unique plant ids in the 
#                      upper connection table [vector, numeric]
# idx_pump_lower     - index relating pump ids to unique plant ids in the 
#                      lower connection table [vector, numeric]
# idx_up_turb_rev    - index relating turbine ids from the turbine dataframe to
#                      turbine ids in the upper connection table 
#                      [vector, numeric]
# turb_per_res_upper - share that each storage turbine has in a upper reservoir, 
#                      defined as the share of capacity of the turbine to
#                      total capacity connected to the reservoir 
#                      [vector_numeric]   
# turb_per_res_lower - share that each storage turbine has in a lower reservoir, 
#                      defined as the share of capacity of the turbine to
#                      total capacity connected to the reservoir 
#                      [vector_numeric]
# pump_per_res_upper - share that each storage pump has in a upper reservoir, 
#                      defined as the share of capacity of the pump to
#                      total capacity connected to the reservoir 
#                      [vector_numeric]
# pump_per_res_lower - share that each storage pump has in a lower reservoir, 
#                      defined as the share of capacity of the pump to
#                      total capacity connected to the reservoir 
#                      [vector_numeric]
# idx_sum_upper      - index relating plant ids in the upper connection table
#                      to their unique values resulting from aggregation 
#                      [vector, numeric]
# idx_sum_lower      - index relating plant ids in the lower connection table
#                      to their unique values resulting from aggregation 
#                      [vector, numeric]
# idx_upper_res      - index relating reservoir ids in the upper connection 
#                      table to reservoir ids in the reservoir dataframe
#                      [vector, numeric]
# idx_lower_res      - index relating reservoir ids in the lower connection 
#                      table to reservoir ids in the reservoir dataframe
#                      [vector, numeric]
# u_flo              - total annual unregulated inflow directly to storage 
#                      plants in mio cbm [vector, numeric]
# flo                - normalized inflow to reservoirs all timesteps
#                      [vector, numeric]
# 
# Details:
# The production of hydro storage plants is restricted not only by the 
# installed capacity but also by the available water volume in the upper 
# reservoir for pumps or in the lower reservoir for turbines. Also there has 
# to be enough storage volume in the respective other reservoir in order not to
# spil any water. The available volumes are transformed into available MW and 
# brought into the order of turbines or pumps.

# In addition to the reservoir content, for turbines there is unregulated 
# inflow directly to the storage plants. This water cannot be stored. It is 
# passing the plant regardless of production. The unregulated inflow expands the
# restriction of upper reservoir. The restriction of the lower reservoir must 
# allow at least the unregulated inflow.

# The resulting available capacity is the minimum of the three restrictions 
# from installed capacity, upper and lower reservoir.
#
# Value:
# bid_turb           - available production capacity from turbines in MW 
#                      [vector, numeric]
# bid_pump           - available pumping capacity in MW [vector, numeric]
# spill_both         - indicator for spillage in upper and lower reservoir per 
#                      storage plant [vector, numeric]
# share_up_turb      - relative available filling level of upper reservoirs for 
#                      each turbine, must be in the range of 0 and 1 
#                      [vector, numeric]
# share_lo_turb      - relative available filling level of lower reservoirs for 
#                      each turbine, must be in the range of 0 and 1 
#                      [vector, numeric]
# share_pump         - relative filling level of pumps, product of relative 
#                      filling levels of upper and lower reservoirs 
#                      [vector, numeric]
# res_per_turb_upper - share that each reservoir has in the production of 
#                      downstream turbines, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_pump_upper - share that each reservoir has in the pumping of 
#                      downstream pumps, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_turb_lower - share that each reservoir has in the production of 
#                      upstream turbines, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_pump_lower - share that each reservoir has in the pumping of 
#                      upstream pumps, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# u_flo_turb         - unregulated inflow to storage plants in the current 
#                      timestep [vector, numeric]
#------

hydroCapacity <- function(tt,
                          end_timestep,
                          reservoir,
                          fil,
                          fil_max_up,
                          fil_max_lo,
                          sum_fil_max_upper,
                          sum_fil_max_lower,
                          turb,
                          pump,
                          upper,
                          lower,
                          idx_turb_upper,
                          idx_turb_lower,
                          idx_pump_upper,
                          idx_pump_lower,
                          idx_up_turb_rev,
                          turb_per_res_upper,
                          turb_per_res_lower,
                          pump_per_res_upper,
                          pump_per_res_lower,
                          idx_sum_upper,
                          idx_sum_lower,
                          idx_upper_res,
                          idx_lower_res,
                          u_flo,
                          flo,
                          energy_factor){
  
  # Forecasting of inflow and spillage
  
  # inflow - 12 hours
  forecast_time_flo   <- c(tt:(min(end_timestep, (tt + 12/energy_factor))))
  
  flo_forecast        <- sum(flo[forecast_time_flo]) * res_flo
  
  flo_forecast_up     <- flo_forecast[idx_upper_res] * turb_per_res_upper *
    upper$efactor * 1000  / energy_factor
  
  sum_flo_forecast_up <- aggregate(flo_forecast_up, list(upper$plant_id), sum)
  
  share_flo_up        <- sum_flo_forecast_up[,2]/sum_fil_max_upper
  share_flo_up_turb   <- share_flo_up[idx_turb_upper]
  
  # spillage - 1 week
  forecast_time_fil <- c(tt : min(end_timestep, (tt + 168/energy_factor)))
  
  spill_forecast    <- fil[tt,] + sum(flo[forecast_time_fil]) * res_flo -
    reservoir$fil_max                                    
                         
  # Get restrictions from the reservoirs, per type and connection
  
  up_turb_result <- fillingEnergy(fil = fil[tt,idx_upper_res],  
                                  fil_max = fil_max_up,
                                  spill = spill_forecast[idx_upper_res],
                                  restriction = "water",
                                  share_per_plant = turb_per_res_upper,
                                  connection_data = upper, 
                                  idx_plant = idx_turb_upper,
                                  idx_sum_connection = idx_sum_upper)
  
  up_turb            <- up_turb_result[[1]]
  res_per_turb_upper <- up_turb_result[[2]]
  up_spil            <- up_turb_result[[3]]                       
  
  share_up_turb <- fillingRelative(fil = fil[tt,idx_upper_res], 
                                   fil_max_sum = sum_fil_max_upper,
                                   share_per_plant = turb_per_res_upper,
                                   connection_data = upper, 
                                   idx_plant = idx_turb_upper)
  
  up_pump_result <- fillingEnergy(fil = fil[tt,idx_upper_res],  
                                  fil_max = fil_max_up,
                                  spill = spill_forecast[idx_upper_res], 
                                  restriction = "space",
                                  share_per_plant = pump_per_res_upper,
                                  connection_data = upper, 
                                  idx_plant = idx_pump_upper,
                                  idx_sum_connection = idx_sum_upper)
  
  up_pump            <- up_pump_result[[1]]
  res_per_pump_upper <- up_pump_result[[2]]
  
  share_up_pump <- fillingRelative(fil = fil[tt,idx_upper_res], 
                                   fil_max_sum = sum_fil_max_upper,
                                   share_per_plant = pump_per_res_upper,
                                   connection_data = upper, 
                                   idx_plant = idx_pump_upper)
  
  lo_turb_result <- fillingEnergy(fil = fil[tt,idx_lower_res],  
                                  fil_max = fil_max_lo,
                                  spill = spill_forecast[idx_lower_res],
                                  restriction = "space",
                                  share_per_plant = turb_per_res_lower,
                                  connection_data = lower, 
                                  idx_plant = idx_turb_lower,
                                  idx_sum_connection = idx_sum_lower)
  
  lo_turb            <- lo_turb_result[[1]]
  res_per_turb_lower <- lo_turb_result[[2]]
  lo_spil            <- lo_turb_result[[3]]                        
  
  share_lo_turb <- fillingRelative(fil = fil[tt,idx_lower_res], 
                                   fil_max_sum = sum_fil_max_lower,
                                   share_per_plant = turb_per_res_lower,
                                   connection_data = lower, 
                                   idx_plant = idx_turb_lower)
  
  lo_pump_result <- fillingEnergy(fil = fil[tt,idx_lower_res],  
                                  fil_max = fil_max_lo,
                                  spill = spill_forecast[idx_lower_res],
                                  restriction = "water",
                                  share_per_plant = pump_per_res_lower,
                                  connection_data = lower, 
                                  idx_plant = idx_pump_lower,
                                  idx_sum_connection = idx_sum_lower)
  
  lo_pump            <- lo_pump_result[[1]]
  res_per_pump_lower <- lo_pump_result[[2]]
  
  share_lo_pump <- fillingRelative(fil = fil[tt,idx_lower_res], 
                                   fil_max_sum = sum_fil_max_lower,
                                   share_per_plant = pump_per_res_lower,
                                   connection_data = lower, 
                                   idx_plant = idx_pump_lower)
  
  # log warning if share_up_turb or share_lo_turb have values > 1
  
  share_pump    <- share_up_pump * (1-share_lo_pump)
                         
  share_lo_turb <- ifelse(is.na(share_lo_turb), share_up_turb , share_lo_turb)               
  
  # Hydro storage plants without lower reservoir, get their installed capacity 
  # in that place instead. That way it is not restricting the production.
  lo_turb       <- ifelse(is.na(lo_turb), turb$pinst, lo_turb)
  lo_pump       <- ifelse(is.na(lo_pump), pump$pinst, lo_pump)   
  lo_spil       <- ifelse(is.na(lo_spil), 0, lo_spil)                       
  
  # Unregulated inflow
  u_flo_turb          <- u_flo * flo[tt]
  u_flo_turb_en       <- u_flo_turb* upper$efactor[idx_up_turb_rev] * 1000 /
    energy_factor
  
  # part load
  part_load_share <- (share_up_turb + share_flo_up_turb)
                         
  part_load <- ifelse(part_load_share < 0.2, 0.2, part_load_share)
  part_load <- ifelse(part_load_share > 0.7, 0.7, part_load)                         
                         
  # Calculate bids
  bid_turb            <- pmin (turb$pinst, up_turb + u_flo_turb_en, 
                               pmax(lo_turb, u_flo_turb_en)) 
  bid_pump            <- pmin (pump$pinst, up_pump, lo_pump)
  
  # log warning if any bids are < 0
                         
  # spillage indicator
  spill_both <- up_spil - 1.2 * lo_spil                       
  
  return(list(bid_turb = bid_turb, 
              bid_pump = bid_pump, 
              spill_both = spill_both,
              share_up_turb = share_up_turb, 
              share_lo_turb = share_lo_turb, 
              share_pump = share_pump,
              res_per_turb_upper = res_per_turb_upper,
              res_per_pump_upper = res_per_pump_upper,
              res_per_turb_lower = res_per_turb_lower,
              res_per_pump_lower = res_per_pump_lower,
              u_flo_turb = u_flo_turb))
}
