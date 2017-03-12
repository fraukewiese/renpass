# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: storeResultStandard, storeResultSumRegions,
#                            storeResultExchange, storeResultElpStc,
#                            storeResultFilIndicator,storeResultPrepareFilLevel,
#                            storeResultFilLevel
#-----

# Store data in standard tables of the format: 
# scenario_nr, time step, region_id, value
standard_table_data <- c("co2",
                         "over_demand", 
                         "price_exchange", 
                         "price_start")

standard_table_data_name <- c("co2",
                              "over_demand",
                              "price_after_exchange",
                              "price_before_exchange")

standard_table_column_name <- c("co2",
                                "over_demand",
                                "price",
                                "price")

for (st in 1:length(standard_table_data)){
  
  storeResultStandard(data              = eval(parse(text = 
                                                      standard_table_data[st])), 
                      table_name        = standard_table_data_name[st],
                      column_name       = standard_table_column_name[st],
                      scenario_nr       = ss,
                      timesteps         = timesteps,
                      start_timestep    = start_timestep,
                      end_timestep      = end_timestep,
                      number_of_regions = number_of_regions,
                      dpr_number        = dpr_number)
}

#--------------------------------
# variable renewable electricity
#--------------------------------

# all values schould be positive, stored as sums of all regions
storeResultSumRegions(data           = abs(ee_reduction),
                      table_name     = "excess_vre_after_exchange",
                      column_name    = "excess_vre",
                      scenario_nr    = scenario_nr,
                      start_timestep = start_timestep,
                      end_timestep   = end_timestep)
                      
storeResultSumRegions(data           = abs(ee_reduction_pump_ex),
                      table_name     = "excess_vre_after_storage",
                      column_name    = "excess_vre",
                      scenario_nr    = scenario_nr,
                      start_timestep = start_timestep,
                      end_timestep   = end_timestep)

#---------------
# exchange data
#---------------
storeResultExchange(data           = exchange_combined,
                    table_name     = "exchange",
                    no_lines       = length(grid_exist[,1]),
                    scenario_nr    = scenario_nr,
                    timesteps      = timesteps,
                    start_timestep = start_timestep,
                    end_timestep   = end_timestep)

storeResultExchange(data           = exchange_pump_combined,
                    table_name     = "exchange_after_storage", 
                    no_lines       = length(grid_exist[,1]),
                    scenario_nr    = scenario_nr,
                    timesteps      = timesteps,
                    start_timestep = start_timestep,
                    end_timestep   = end_timestep)

#--------------------------------------------
# Electricity production for every type/fuel
#--------------------------------------------
for (ff in 1:length(fuels)){
  storeResultElpStc(data              = eval(parse(text = paste("elp_", 
                                                                fuels[ff], 
                                                                sep = ""))),
                    table_name        = "electricity_production",
                    column_name       = "electricity_production",
                    type              = fuels[ff],
                    scenario_nr       = ss,
                    timesteps         = timesteps,
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep,
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
}

#---------------------
# storage consumption
#---------------------

if(ncol(elp_other_pump) > 0){
  # elp other pump and filling level other just have the regions that do have
  # other storage, we make a matrix with all dispatch region out of them
  elp_other_pump          <- elp_other_pump[,match(dpr_number,
                                                   other_storage_data$dpr)]
  elp_other_pump[is.na(elp_other_pump)] <- 0
  
  filling_level_other_gwh <- filling_level_other[,match(dpr_number, 
                                                       other_storage_data$dpr)]/
                             1000
  filling_level_other_gwh[is.na(filling_level_other_gwh)] <- 0
  
  # in elp_pump hydro and other pump is included
  stc_hydro               <- elp_pump - elp_other_pump
  stc_other_storage       <- elp_other_pump
  storage_types           <- c("hydro","other_storage")
  
  # here, the calculation of the storage medium could be prepared for the data-
  # base, so far it is zero,
  filling_level_other_unit <- matrix(0, 
                                     nrow = timesteps + 1, 
                                     ncol = length(dpr_number))
  filling_level_other_storage <- list(filling_level        = 
                                        filling_level_other_unit,
                                      filling_level_energy = 
                                        filling_level_other_gwh) 
}else{
  storage_types     <- c("hydro")
  stc_hydro         <- elp_pump
}

for(st in 1:length(storage_types)){
  storeResultElpStc(data              = eval(parse(text = paste("stc_", 
                                                              storage_types[st], 
                                                              sep = ""))),
                    table_name        = "storage_consumption",
                    column_name       = "storage_consumption",
                    type              = storage_types[st],
                    scenario_nr       = ss,
                    timesteps         = timesteps,
                    start_timestep    = start_timestep,
                    end_timestep      = end_timestep,
                    number_of_regions = number_of_regions,
                    dpr_number        = dpr_number)
}

#----------------
# Filling levels
#----------------
storeResultFilIndicator(filling     = fil, 
                        reservoir   = reservoir,
                        timesteps   = timesteps,
                        scenario_nr = scenario_nr)

filling_level_hydro <- storeResultPrepareFilLevel(data       = fil,
                                                  reservoir  = reservoir, 
                                                  timesteps  = timesteps,
                                                  dpr_number = dpr_number)

for(st in 1:length(storage_types)){
  storeResultFilLevel(data = eval(parse(text = paste("filling_level_", 
                                                      storage_types[st], 
                                                      sep = ""))),
                      type           = storage_types[st],
                      scenario_nr    = ss,
                      timesteps      = timesteps,
                      start_timestep = start_timestep,
                      end_timestep   = end_timestep,
                      dpr_number     = dpr_number)
}
