# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, convertRegionVectorToDpr
# function defined within this piece of code: regionalGeothermal
#-----
# Name: regionalGeothermal

# Title: Calculate the Geothermal Feedin

# Description: 

# Usage: regionalGeothermal(geothermal_scenario, scenario_year, time_unit,
# Region_dpr_assignment)

# Arguments:
# geothermal_scenario  which geothermal scenario is chosen in the scenario 
#                      parameters, only scenarios that are described in the 
#                      geothermal_scenario table in the pathway database can be 
#                      chosen
# scenario_year        which scenario year is chosen
# time_unit            in which time unit the result timeseries should come out,
#                      possible: "hours" or "quarters"
# Region_dpr_assignment   data.frame which region_ids belong to which dpr -
#                         dispatch regions which are the regional unit for this
#                         scenario/simulation [data.frame: region_vector 
#                         (numeric), dpr_number(numeric)]

# Details: The function inlcudes the process of producing the matrix of 
#          geothermal feedin. Geothermal electricity is used if the residual 
#          load is positive, before the dispatch via merit order is done to
#          the rest of the demand. In the function, the installed capacity in 
#          the scenario year and the future utilisation factor are read from the
#          pathways database, from the chosen geothermal scenario. 
#          The function has one control: if there is a wrong number of regions
#          given in the geothermal scenario, it stops

# Value: Matrix with timesteps in rows and the regions in the columns. The unit
#        is MW of geothermal feedin available in every timestep and region
#        of the scenario year
#-------------------------------------

regionalGeothermal <- function(geothermal_scenario,
                               time_unit,
                               Region_dpr_assignment = region_dpr_assignment){  
  
  geothermal_scenario_data <- readPathway(scenario      = "geothermal",
                                          scenario_name = geothermal_scenario,
                                          order_by      = "region_id")
  
  # do we have all regions from the region_vector?
  idx               <- which(
                         region_vector %in% geothermal_scenario_data$region_id)
  region_id_defined <- region_vector[idx]
  missing_id        <- region_vector[-idx]
  
  # is there a region missing? Then, the parameters have been defined just for
  # subregions and we have to combine the parameters to the higher level region
  # we need for this scenario
  if(length(missing_id) > 0){
    
    # if there are more than one missing_id, a loop will help
    for(mm in missing_id){
      
      less_digits       <- round(geothermal_scenario_data$region_id, 
                                 digits = -3)
      
      idx_2             <- which(less_digits == mm)
      
      add_line          <- data.frame(scenario_name = geothermal_scenario,
                                      utilisation_factor = mean(
                            geothermal_scenario_data$utilisation_factor[idx_2]),
                                      region_id     = mm,
                                      installed_capacity = sum(
                            geothermal_scenario_data$installed_capacity[idx_2]))
      
      geothermal_scenario_data <- rbind(geothermal_scenario_data, add_line)
    }
  }
  
  # now we pick just the data for the regions from the region_vector for this
  # calculateion
  idx               <- which(
                         geothermal_scenario_data$region_id %in% region_vector)
  geothermal_scenario_data <- geothermal_scenario_data[idx,]
  
  # order by region_id
  geothermal_scenario_data <- geothermal_scenario_data[order(
                                geothermal_scenario_data$region_id),]
  
  # pick out just the regions we need
  idx <- which(geothermal_scenario_data$region_id %in% region_vector)
  geothermal_scenario_data <- geothermal_scenario_data[idx,]
  
  if(length(geothermal_scenario_data$region_id) != length(region_vector)){
    stop("wrong number of regions in the geothermal scenario")
  }
  
  geo_feedin_raw    <- geothermal_scenario_data$installed_capacity *
                       geothermal_scenario_data$utilisation_factor
  
  names(geo_feedin_raw)  <- geothermal_scenario_data$region_id
  idx                    <- which(geo_feedin_raw > 0)
  geo_available_capacity <- geo_feedin_raw[idx]
  
  names(geo_available_capacity) <- convertRegionVectorToDpr(
    vector_with_regions = 
      names(geo_available_capacity))
  
  geo_cap_sum            <- tapply(geo_available_capacity, 
                                   names(geo_available_capacity), 
                                   sum)
  
  dpr_with_geo           <- length(geo_cap_sum)
  geo_merit_order        <- vector("list", dpr_with_geo)
  names(geo_merit_order) <- names(geo_cap_sum)
  
  # MB: if-cause added, otherwise failure when geothermal = 0
  if(length(geo_cap_sum) > 0){ 
    for(i in 1:dpr_with_geo){
      geo_merit_order[[i]] <- data.frame(pp_nr              = 7777,
                                         available_capacity = geo_cap_sum[[i]],
                                         marginal_cost      = 0.1,
                                         co2                = 0,
                                         fuel               = as.character(
                                                                "geothermal"))
    }
  }

  return(geo_merit_order)
}

geo_merit_order <- regionalGeothermal(geothermal_scenario = geothermal_scenario,
                                      time_unit            = time_unit,
                                      Region_dpr_assignment = 
                                        region_dpr_assignment)
