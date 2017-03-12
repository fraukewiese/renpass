# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: storeResultElpStc, storeResultStandard
# function defined within this piece of code: calculateResidualLoad
#-----
# Name: calculateResidualLoad

# Title: Calculate Residual Load  

# Description: This functions substracts mustrun_feedin from a timeseries,

# Usage: calculateResidualLoad(load_timeseries, mustrun_feedin)

# Arguments:
# load_timeseries  a matrix with the load timeseries with the timesteps
#                  in rows and the regions in columns, numeric [MW]
# mustrun_feedin   a matrix of the same format as the load_timeseries with the 
#                  available feedin timeseries of the regions, numeric [MW]

# Details: The mustrun_feedin is substracted from the load, the result is a 
#          matrix of the same size and shape with the residual load for each
#          region and each timestep.

# Value: Matrix with timesteps in rows and the regions in the columns. The unit
#        is MW

# Examples (optional): 
# demand_reg <- matrix(c(1:12), ncol = 2)
# mustrun_feedin <- matrix(rep(3,12), ncol = 2)
# calculateResidualLoad(load_timeseries = demand_reg,
#                       mustrun_feedin = geo_reg)
#---------------------------

calculateResidualLoad <- function(load_timeseries, 
                                  mustrun_feedin){
  
  if(length(load_timeseries) != length(mustrun_feedin)){
    stop("load_timeseries does not have the same size as mustrun_feedin")
  }
  
  rl_reg <- load_timeseries - mustrun_feedin
    
  return(rl_reg)
}

  rl_reg <- demand_reg

if("wind_onshore_reg" %in% ls()){
  
  rl_reg <- calculateResidualLoad(load_timeseries = rl_reg,
                                  mustrun_feedin  = wind_onshore_reg)
  
  storeResultElpStc(data              = wind_onshore_reg, 
                    type              = "wind_onshore", 
                    scenario_nr       = ss, 
                    timesteps         = timesteps,
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep,
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
  
  remove(wind_onshore_reg)
}

if("wind_offshore_reg" %in% ls()){
 rl_reg <- calculateResidualLoad(load_timeseries = rl_reg,
                                  mustrun_feedin = wind_offshore_reg)
  
  storeResultElpStc(data              = wind_offshore_reg, 
                    type              = "wind_offshore", 
                    scenario_nr       = ss, 
                    start_timestep    = start_timestep,
                    timesteps         = timesteps, 
                    end_timestep      = end_timestep, 
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
 
 remove(wind_offshore_reg)
}

if("solar_reg" %in% ls()){
  rl_reg <- calculateResidualLoad(load_timeseries = rl_reg,
                                  mustrun_feedin = solar_reg)
  
  storeResultElpStc(data              = solar_reg, 
                    type              = "solar", 
                    scenario_nr       = ss, 
                    timesteps         = timesteps, 
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep, 
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
  
  remove(solar_reg)
}

if("runofriver_reg" %in% ls()){
  rl_reg <- calculateResidualLoad(load_timeseries = rl_reg,
                                  mustrun_feedin  = runofriver_reg)
  
  storeResultElpStc(data              = runofriver_reg, 
                    type              = "runofriver", 
                    scenario_nr       = ss, 
                    timesteps         = timesteps, 
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep, 
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
  
  remove(runofriver_reg)
}

storeResultStandard(data              = demand_reg, 
                    table_name        = "demand",
                    column_name       = "demand",
                    scenario_nr       = ss,
                    timesteps         = timesteps, 
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep,
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)

remove(demand_reg)

# rl_reg is the residual laod before exchange, thus this can be written already
# into the results database
storeResultStandard(data              = rl_reg,
                    table_name        = "residual_load_before_exchange",
                    column_name       = "residual_load",
                    scenario_nr       = ss,
                    timesteps         = timesteps,
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep,
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
