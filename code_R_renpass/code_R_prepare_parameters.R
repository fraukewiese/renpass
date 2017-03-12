# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMySQL, readPathway
#-----
# Description  All parameters and subscenario-names for the chosen scenario are
#                read from the pathways database
# Input        ss (scenario_nr)
#                need to be defined for the RMySQL-connection:
#                database_username, database_password, database_name, 
#                databse_host, database_port, database_unix.socket
# Output       scenario_parameter, scenario_nr, scenario_year, start_hour,
#                end_hour, weather_year, time_unit, algorithm_scenario,
#                demand_scenario, grid_scenario, region_scenario,
#                renewable_scenario, resources_scenario, storage_scenario,
#                thermal_pp_scenario, wind_onshore_scenario,
#                wind_offshore_scenario, solar_scenario, runofriver_scenario,
#                geothermal_scenario, biomass_scenario, hydro_storage_scenario,
#                other_storage_scenario, iteration_maximum, timesteps,
#                start_timestep, end_timestep, energy_factor
#-----

#--------------------------------------
# Get the parameters from the database
#--------------------------------------

# connection to pathways database
con_pathways <- connectMysql("pathways")
  
sql_para     <- paste("SELECT * FROM 1_scenario_parameter WHERE scenario_nr= '",
                       ss, "'", sep = "")

# get all scenario parameter for the chosen scenario_nr 
scenario_parameter 	<- dbGetQuery(con_pathways, sql_para)

# error message if scenario_nr is not defined in the pathways database
if(length(scenario_parameter) == 0){
  stop(paste("no scenario parameters for chosen scenario_nr",
             ss, "defined", sep = " "))
}

#--------------------------------------------
# Prepare the parameters for the calculation
#--------------------------------------------
# the parameters get the same variable names they have in the database. Some are
# numeric, some characer.

parameter_names 	<- colnames(scenario_parameter)

# numeric scenario parameter
for(pp in parameter_names[c(1:4,6)]){
  eval(parse(text = paste(pp, " <- as.numeric(scenario_parameter$", pp, ")",
                          sep = "")))
}

# character scenario parameter
for(pp in parameter_names[c(5,7:length(parameter_names))]){
  eval(parse(text = paste(pp, " <- as.character(scenario_parameter$", pp, ")",
                          sep = "")))
}

# Read the renewable subscenarios' scenario names
renewable_parameter <- readPathway(scenario      = "renewable",
                                   scenario_name = renewable_scenario,
                                   order_by      = "scenario_name")

renewable_names     <- colnames(renewable_parameter)

for(pp in renewable_names[2:length(renewable_names)]){
  eval(parse(text = paste(pp, " <- as.character(renewable_parameter$", pp, ")",
                          sep = "")))
}

# Read the storage subscenarios' scenario names
storage_parameter <- readPathway(scenario      = "storage",
                                 scenario_name = storage_scenario,
                                 order_by      = "scenario_name")

storage_names     <- colnames(storage_parameter)

for(pp in storage_names[2:length(storage_names)]){
  eval(parse(text = paste(pp, " <- as.character(storage_parameter$", pp, ")",
                          sep = "")))
}

# read the parameter from the algorithm pathway
algorithm_sql <- paste("SELECT * FROM algorithm_scenario 
                       WHERE scenario_name = '",
                       algorithm_scenario,
                       "'", 
                       sep = "")

pathway_data  <- dbGetQuery(con_pathways, algorithm_sql)

# error message if algorithm scenario is not defined in the pathways database
if(length(pathway_data) == 0){
  stop(paste("no pathway for chosen algorithm_scenario ", 
             algorithm_scenario, " defined", sep = ""))
}
iteration_maximum <- pathway_data$iterations
algorithm         <- pathway_data$algorithm

# Number of timesteps that have to be calculated
timesteps      <- end_hour - start_hour + 1
start_timestep <- start_hour
end_timestep   <- end_hour

if(time_unit == "quarter"){
  timesteps      <- timesteps * 4
  start_timestep <- (start_timestep - 1) * 4 + 1
  end_timestep   <- end_timestep * 4
  }
    
# Factor from power to energy MW -> MWh
if (time_unit == "hour")    {energy_factor <- 1} 
if (time_unit == "quarter") {energy_factor <- 0.25}

# close databse connection
dbDisconnect(con_pathways)

# delete all variables not needed anymore
remove(sql_para, parameter_names, renewable_parameter, renewable_names,
       storage_parameter, storage_names, algorithm_sql, pathway_data, pp)
