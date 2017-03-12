# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, convertRegionMatrixToDpr                       
# functions defined within this piece of code: runOfRiver
#-----
# Name:
# runOfRiver
# 
# Title:
# Generate constant run-of-river feed-in curves for all regions
# 
# Description:
# From a sceanrio value for run-of-river capacity for every region, a matrix 
# with constant feed-in from run-of-river plants is generated for every region.
# 
# Usage:
# runOfRiver(runofriver_scenario = "BMU_2011", scenario_year = 2030,
# weather_year = 2003, region_vector = c(11000,12000), number_of_regions = 2, 
# timesteps = 96)
# 
# Arguments:
# runofriver_scenario   - name of the run-of-river scenario from the database 
#                         [scalar, string]
# scenario_year         - year for which the scenario is calculated 
#                         [scalar, numeric]
# weather_year          - one of three possible weather years, defined as 
#                         scenario parameter [scalar, numeric]
# region_vector         - vector with the regions included in the scenario 
#                         [vector, numeric]
# number_of_regions     - number of regions [scalar, numeric]
# timesteps             - last timestep to be calculated [scalar, numeric]
# region_dpr_assignment - the assignement of the region_ids to the dpr_numbers,
#                         the dpr_numbers are the simulation regions for this
#                         scenario. [data.frame: region_id(vector,numeric),
#                                                dpr_number(vector,numeric)]
# 
# Details:
# The default run-of-river production curve assumes a constant feed-in for every 
# region. From the value for installed capacity as matrix with feed-in for every 
# region is generated. The scenario value is given in GW and has to be converted
# to MW for the calculation. The weather year determines the utilisation of the 
# plants. It varies between 50% and 80% of the total installed capacity.
# 
# Value:
# ror_reg - matrix with run-of-river production for every dispatch region 
#           [matrix, numeric (timeteps x regions)]
# p_ror   - vector with installed run-of-river capacity per region 
#           [vector, numeric]
#--------

runOfRiver <- function(runofriver_scenario,
                       scenario_year,
                       weather_year,
                       region_vector,
                       number_of_regions,
                       timesteps,
                       region_dpr_assignment){
  
  if (weather_year == 2010){uti <- 0.45 } 
  if (weather_year == 1998){uti <- 0.65 } 
  if (weather_year == 2003){uti <- 0.55}
  
  ror_scenario_data <- readPathway(scenario      = "runofriver",
                                   scenario_name = runofriver_scenario,
                                   order_by      = "region_id")
  
  # do we have all regions from the region_vector?
  idx               <- which(region_vector %in% ror_scenario_data$region_id)
  region_id_defined <- region_vector[idx]
  missing_id        <- region_vector[-idx]
  
  # is there a region missing? Then, the parameters have been defined just for
  # subregions and we have to combine the parameters to the higher level region
  # we need for this scenario
  if(length(missing_id) > 0){
    
    # if there are more than one missing_id, a loop will help
    for(mm in missing_id){
      
      less_digits       <- round(ror_scenario_data$region_id, 
                                 digits = -3)
      
      idx_2             <- which(less_digits == mm)
        
      add_line          <- data.frame(scenario_name = runofriver_scenario,
                                      region_id     = mm,
                                      installed_capacity = sum(
                                   ror_scenario_data$installed_capacity[idx_2]))
      
      ror_scenario_data <- rbind(ror_scenario_data, add_line)
    }
  }
  
  # now we pick just the data for the regions from the region_vector for this
  # calculateion
  idx               <- which(ror_scenario_data$region_id %in% region_vector)
  ror_scenario_data <- ror_scenario_data[idx,]
  
  # order by region_id
  ror_scenario_data <- ror_scenario_data[order(ror_scenario_data$region_id),]
  
  p_ror <- ror_scenario_data$installed_capacity
  prod  <- p_ror * uti
  
  # aggregate regions
  ror_reg <- matrix(rep(prod, timesteps), 
                    ncol = length(prod), 
                    byrow = TRUE)
  
  colnames(ror_reg) <- ror_scenario_data$region_id
  
  ror_reg <- convertRegionMatrixToDpr(region_matrix = ror_reg,
                                      Region_dpr_assignment = 
                                         region_dpr_assignment)
  
  return(list(ror_reg = ror_reg, 
              p_ror   = p_ror))
}

runofriver_result <- runOfRiver(runofriver_scenario  = runofriver_scenario,
                               scenario_year         = scenario_year,
                               weather_year          = weather_year,
                               region_vector         = region_vector,
                               number_of_regions     = number_of_regions,
                               timesteps             = timesteps,
                               region_dpr_assignment = region_dpr_assignment)

runofriver_reg  <- runofriver_result[[1]]
p_ror           <- runofriver_result[[2]]
