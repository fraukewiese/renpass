# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
for(ss in scenario_nr_vector){
  
  #---------------------------
  # Preparation of Parameters
  #---------------------------
  # load all functions
  functions <- dir('renpass/code_R_renpass/functions')
  for(ff in functions){
    source(paste("renpass/code_R_renpass/functions/",ff,sep = ""))
  }
  
  logTime("start")
  
  source('renpass/code_R_renpass/code_R_prepare_parameters.R')
  writeLog(paste(scenario_parameter))
  source('renpass/code_R_renpass/code_R_prepare_regions.R')
  source('renpass/code_R_renpass/code_R_hydro_storage_scenario.R')
  source('renpass/code_R_renpass/code_R_other_storage_scenario.R')
  source('renpass/code_R_renpass/code_R_prepare_storage_plants.R')
  source('renpass/code_R_renpass/code_R_prepare_reservoirs.R')
  source('renpass/code_R_renpass/code_R_prepare_storage_connection.R')
  source('renpass/code_R_renpass/code_R_prepare_grid.R')
  
  logTime("Preparation of Parameters")
  
  #---------------
  # Residual load
  #---------------
  source('renpass/code_R_renpass/code_R_demand.R')
  writeLog(paste("Demand",
                 sum(demand_reg)*energy_factor/1000000,
                 "TWh",
                 sep = " "))
  source('renpass/code_R_renpass/code_R_wind_onshore.R')
  source('renpass/code_R_renpass/code_R_wind_offshore.R')
  source('renpass/code_R_renpass/code_R_solar.R')
  source('renpass/code_R_renpass/code_R_run_of_river.R')
  source('renpass/code_R_renpass/code_R_run_of_river_de.R')
  source('renpass/code_R_renpass/code_R_run_of_river_no.R')
  source('renpass/code_R_renpass/code_R_calculate_residual_load.R')
  writeLog(paste("Residual Load",
                 sum(rl_reg)*energy_factor/1000000,
                 "TWh",
                 sep = " "))
  
  logTime("Residual Load")
  
  #-------------------------------------
  # Availability of thermal power plants
  #-------------------------------------
  source('renpass/code_R_renpass/code_R_geothermal.R')
  source('renpass/code_R_renpass/code_R_prepare_biomass.R')
  source('renpass/code_R_renpass/code_R_merit_order.R')
  source('renpass/code_R_renpass/code_R_prepare_dispatch.R')
  
  logTime("Availability of thermal power plants")
  
  # timestep loop
  #---------------
  for(tt in 1:timesteps){
    
    writeLog(paste(tt))
    #-------------
    # Merit Order
    #-------------
    source('renpass/code_R_renpass/code_R_hydro_capacity.R', local = TRUE)
    source('renpass/code_R_renpass/code_R_storage_prices.R', local = TRUE)
    source('renpass/code_R_renpass/code_R_hydro_merit_order.R', local = TRUE)
    if(nrow(other_storage_data) > 0) {
     source('renpass/code_R_renpass/code_R_filling_level_other.R', local = TRUE)
    }
    source('renpass/code_R_renpass/code_R_biomass_merit_order.R', local = TRUE)
    source('renpass/code_R_renpass/code_R_merge_merit_orders.R', local = TRUE)
    
    logTime("Merit Order", tt)
    
    #-------------------
    # Regional Dispatch
    #-------------------
    for(rr in dpr_number){
      source('renpass/code_R_renpass/code_R_dispatch.R', local = TRUE)
    }
    
    logTime("Regional Dispatch", tt)
    
    #----------
    # Exchange
    #----------
    # iteration loop
    source('renpass/code_R_renpass/code_R_exchange.R', local = TRUE)
    
    logTime("Exchange", tt)
    
    for(rr in dpr_number){
      source('renpass/code_R_renpass/code_R_dispatch_tplus.R', local = TRUE)
    }
    
    logTime("Dispatch tplus", tt)
    
    #----------------------------
    # Excess electricity storage
    #----------------------------
    source('renpass/code_R_renpass/code_R_pump_region.R', local = TRUE)
    source('renpass/code_R_renpass/code_R_pump_merit_order.R', local = TRUE)
    
    # iteration loop
    source('renpass/code_R_renpass/code_R_pump_exchange.R', local = TRUE)
    
    source('renpass/code_R_renpass/code_R_pump_commitment.R', local = TRUE)
    
    logTime("Pump exchange", tt)
    
    #----------------------------------------
    # Filling levels of all kinds of storage
    #----------------------------------------
    source('renpass/code_R_renpass/code_R_filling_level_tplus.R', local = TRUE)
    source('renpass/code_R_renpass/code_R_filling_level_tplus_bio.R',
           local = TRUE)
    if(nrow(other_storage_data) > 0) {
      source('renpass/code_R_renpass/code_R_filling_level_tplus_other.R',
           local = TRUE)
    }
    
    logTime("Filling Level tplus", tt)
  }
  writeLog(paste("Spil sums up to ", sum(spil), sep = ""))
  
  #----------------------
  # Output: save results
  #----------------------
  source('renpass/code_R_renpass/code_R_results.R',
         local = TRUE)
  
  #----------------------
  # Output: process time
  #----------------------
  logTime("Results")
  log_time_summary <- summaryLogTime(log_time)
  writeLog(log_time_summary)
  
  #---------------
  # Output: plots
  #---------------
  if(plot != "none"){
    source('renpass/code_R_renpass/plot_code/code_R_plot_core.R')
  }
  
  #-------------
  # Cleaning up
  #-------------
  # close eventually open database connections
  open_cons <- dbListConnections(MySQL())
  for(open_con in open_cons){
    dbDisconnect(open_con)
  }
  # empty the workspace except the settings for the next scenario nr run
  keep <- c("scenario_nr_vector","path","plot","database_username",
            "database_password","database_host","database_port",
            "database_unix.socket","system_software")
  remove(list = ls()[-which(ls() %in% keep)])
  gc()
}
