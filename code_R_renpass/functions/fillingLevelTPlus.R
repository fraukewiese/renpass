# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: productionWaterTurb, productionWaterPump
#-----
# Name:
# fillingLevelTPlus
# 
# Title:
# Calculate new filling level from storage plant operation and inflow   
# 
# Description:
# The power production and pumping are converted into water volume and together
# with inflow and efflux from run-of-river plants this determines the new 
# filling level
#
# Usage:
# fillingLevelTPlus(hydro_in_action, upper, lower, fil, fil_max...)
# 
# Arguments:
# hydro_in_action    - list with dataframefor every region with data on power
#                      production from hydro storage plants [list [dataframe: 
#                      pp_nr (numeric), available_capacity (numeric)]  
# elp_pump_each      - vector with pumping operation for every pump, in MW
#                      [vector, numeric]
# upper              - dataframe with data on the connection of storage plants
#                      to their upper reservoirs [dataframe: plant_id (numeric),
#                      res_id (numeric), efactor (numeric)]
# lower              - dataframe with data on the connection of storage plants
#                      to their lower reservoirs [dataframe: plant_id (numeric),
#                      res_id (numeric), efactor (numeric)]
# fil                - vector with current filling levels for each reservoir in
#                      mio cbm [vector, numeric]
# fil_max            - vector with maximum filling levels for each reservoir in
#                      mio cbm [vector, numeric]
# u_flo_turb         - unregulated inflow to storage plants in the current 
#                      timestep [vector, numeric]
# res_flo            - annual sum of regulated inflow for every reservoir in mio
#                      cbm [vector, numeric]
# flo                - normalized inflow to reservoirs for the current timestep
#                      [scalar, numeric]
# flo_de             - normalized inflow to German reservoirs for the current
#                      timestep [scalar, numeric]
# flo_river_ror      - vector with efflux from run-of-river plants for every 
#                      reservoir
# idx_up_turb        - index that relates plant ids from the upper connection
#                      table to the turbine dataframe
# idx_res_upper_un   - index that relates reservoir ids to the unique reservoir 
#                      ids from the upper connection table that result from 
#                      aggregation [vector, numeric]
# idx_res_lower_un   - index that relates reservoir ids to the unique reservoir 
#                      ids from the lower connection table that result from 
#                      aggregation [vector, numeric]
# idx_elp_pump_up    - index that relates the upper connection table to all 
#                      pumping plants, hydro and non-hydro [vector, numeric]
# idx_elp_pump_lo    - index that relates the lower connection table to all 
#                      pumping plants, hydro and non-hydro [vector, numeric] 
# idx_up_turb_rev    - index relating turbine ids from the turbine dataframe to
#                      turbine ids in the upper connection table 
#                      [vector, numeric]
# idx_flo_river      - index that relates the reservoir ids to the next 
#                      downstream reservoir from the turbine dataframe [vector,
#                      numeric]
# idx_spil           - index that relates the reservoir ids to the next 
#                      downstream reservoir for every reservoir [vector, 
#                      numeric]
# res_per_turb_upper - share that each reservoir has in the production of 
#                      downstream turbines, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_turb_lower - share that each reservoir has in the production of 
#                      upstream turbines, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_pump_upper - share that each reservoir has in the pumping of 
#                      downstream pumps, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# res_per_pump_lower - share that each reservoir has in the pumping of 
#                      upstream pumps, defined as the share of current 
#                      filling level of the reservoir to total connected current
#                      filling level [vector, numeric]
# energy_factor      - conversion factor between energy and power, 1 when the 
#                      time unit is hours, 0.25 when the time unit is quarters 
#                      [scalar, numeric]
#
# Details:
# For every reservoir the production and pumping of upstream and downstream 
# storage plants is converted into water volume. The new filling level is also
# determined by the regulated inflow, the indirect inflow from hydro storage 
# plants through rivers and the indirect inflow from run-of-river plants through
# rivers. If the calculated filling level is above the maximum capacity water
# spills to the next downstream reservoir. All filling levels that are above the 
# maximum levels after this step are set to the maximum and the difference is 
# lost to the system. 
# 
# Value:
# fil_end - vector with new filling level for every reservoir in cbm 
#           [vector, numeric]
# spil    - sum of water spillage in mio cbm in the timestep [scalar, numeric]
#------------- 

fillingLevelTPlus <- function (hydro_in_action,
                               elp_pump_each,
                               upper,
                               lower,
                               fil,
                               fil_max,
                               u_flo_turb,
                               res_flo,
                               flo,
                               flo_de,
                               flo_river_ror,
                               idx_up_turb,
                               idx_res_upper_un,
                               idx_res_lower_un,
                               idx_elp_pump_up,
                               idx_elp_pump_lo,
                               idx_up_turb_rev,
                               idx_flo_river,
                               idx_spil,
                               res_per_turb_upper,
                               res_per_turb_lower,
                               res_per_pump_upper,
                               res_per_pump_lower,
                               energy_factor){
  
  # ------------------------------------  
  # Filling level change from production  
  # ------------------------------------
  hydro_production <- do.call("rbind", hydro_in_action)
  
  u_flo_up    <- u_flo_turb[idx_up_turb]
  u_flo_up    <- ifelse(is.na(u_flo_up), 0, u_flo_up)    
  
  elp_turb_up <- productionWaterTurb(hydro_production = hydro_production, 
                                     connection_data = upper, 
                                     connection = "upper",
                                     idx_connection_reservoir = idx_res_upper_un,
                                     share_res_per_plant = res_per_turb_upper,
                                     energy_factor = energy_factor,
                                     u_flo = u_flo_up)
  
  u_flo_lo    <- u_flo_turb[idx_lo_turb]
  u_flo_lo    <- ifelse(is.na(u_flo_lo), 0, u_flo_lo)
  
  elp_turb_lo <- productionWaterTurb(hydro_production = hydro_production, 
                                     connection_data = lower, 
                                     connection = "lower",
                                     idx_connection_reservoir = idx_res_lower_un,
                                     share_res_per_plant = res_per_turb_lower,
                                     energy_factor = energy_factor,
                                     u_flo = u_flo_lo)
  
  # ---------------------------------
  # Filling level change from pumping
  # ---------------------------------
  
  elp_pump_up <- productionWaterPump(pumping = elp_pump_each[idx_elp_pump_up], 
                                    connection_data = upper, 
                                    idx_connection_reservoir = idx_res_upper_un,
                                    share_res_per_plant = res_per_pump_upper,
                                    energy_factor = energy_factor)
  
  elp_pump_lo <- productionWaterPump(pumping = elp_pump_each[idx_elp_pump_lo], 
                                    connection_data = lower, 
                                    idx_connection_reservoir = idx_res_lower_un,
                                    share_res_per_plant = res_per_pump_lower,
                                    energy_factor = energy_factor)
  # -------
  # Inflow
  # -------
  # Regulated inflow
  flo_reg[-idx_res_de] <- res_flo[-idx_res_de] * flo 
  flo_reg[idx_res_de]  <- res_flo[idx_res_de] * flo_de
  
  # Indirect inflow through rivers
  # if-loop added by Marian and Gesine as a fast fix
  # if hydro_production NULL, production_connection initialized and filled with0
  if(length(hydro_production) == 0){
    elp_river <- rep(0, nrow(upper))
  }else{
    idx_elp_river <- match(upper$plant_id, hydro_production$pp_nr)
    elp_river     <- hydro_production$available_capacity[idx_elp_river]
    elp_river     <- ifelse(is.na(elp_river), 0, elp_river)
  }
  
  flo_river <- pmax((elp_river * 0.001 * energy_factor / upper$efactor), 
                    u_flo_turb[idx_up_turb])
  
  flo_river <- flo_river[idx_up_turb_rev]
  flo_river <- flo_river[idx_flo_river]
  flo_river <- ifelse(is.na(flo_river), 0, flo_river)
  
  # -----------------
  # New filling level
  # -----------------
  fil_new <- fil + elp_turb_lo + elp_pump_up - elp_turb_up - elp_pump_lo + 
    flo_reg + flo_river + flo_river_ror
  
  fil_new <- ifelse(fil_new < 0, 0, fil_new)
  
  # --------------
  # Water spillage
  # --------------
  fil_temp <- fil_new
  for(ii in c(1:5)){ 
  fil_temp <- catchSpillage(fil = fil_temp,
                           fil_max = fil_max,
                           idx_spil = idx_spil)
  }
  
  fil_end  <- fil_temp
  
  fil_end  <- ifelse(fil_end > fil_max, fil_max, fil_end)
  
  spil <- sum(fil_new) - sum(fil_end)

  return(list(fil_end = fil_end, spil = spil))
  
}
