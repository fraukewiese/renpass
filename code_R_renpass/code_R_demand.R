# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, readTimeseries, connectMysql,
#                            distributeTimeseries, varyTimeseries, 
#                            convertRegionMatrixToDpr
# function defined within this piece of code: regionalDemand
#-----
# Name: regionalDemand

# Title: Calculate the Demand Timeseries Matrix  

# Description: This functions includes getting the demand data and the demand
#              pathway parameters from the database, splitting the demand
#              if there is more than one region for one country and interpolate
#              to quarters from hours if time unit is quarter

# Usage: regionalDemand(demand_scenario, time_unit, Region_dpr_assignment)

# Arguments:
# demand_scenario  which demand scenario is chosen in the scenario parameters,
#                  only scenarios that are described in the demand_scenario
#                  table in the pathway database can be chosen
# time_unit        in which time unit the result timeseries should come out,
#                  possible: "hours" or "quarters"
# Region_dpr_assignment   data.frame which region_ids belong to which dpr -
#                         dispatch regions which are the regional unit for this
#                         scenario/simulation [data.frame: region_vector 
#                         (numeric), dpr_number(numeric)]

# Details: The function inlcudes the whole process of producing the matrix of 
#          demand for the calculation of renpass. It has two controls: when 
#          the base year for the demand is not clearly defined it stops, when
#          the variation vector does not have the same length as the region
#          vector, it stops.

# Value: Matrix with timesteps in rows and the regions (dispatch regions of this
#        scenario/simulation) in the columns. The unit is MW
#------------------------------------------

regionalDemand <- function(demand_scenario,
                           time_unit,
                           Region_dpr_assignment = region_dpr_assignment,
                           start_hour,
                           end_hour,
                           timesteps){
  
  # Get the demand-pathway from the database
  demand_scenario_data <- readPathway(scenario      = "demand", 
                                      scenario_name = demand_scenario,
                                      order_by      = "region_id")
  
  # there can only be one base year
  base_year <- unique(as.numeric(demand_scenario_data$base_year))
  
  # control
  if(length(base_year) != 1){
    stop("base year for demand not clearly defined")
  }
  
  # do we have all regions from the region_vector?
  idx               <- which(region_vector %in% demand_scenario_data$region_id)
  region_id_defined <- region_vector[idx]
  missing_id        <- region_vector[-idx]
  
  # is there a region missing? Then, the parameters have been defined just for
  # subregions and we have to combine the parameters to the higher level region
  # we need for this scenario
  if(length(missing_id) > 0){
    
    # if there are more than one missing_id, a loop will help
    for(mm in missing_id){
    
      less_digits       <- round(demand_scenario_data$region_id, 
                                 digits = -3)
      
      idx_2             <- which(less_digits == mm)
      
      add_line          <- data.frame(scenario_name = demand_scenario,
                                base_year = base_year,
                                region_id = mm,
                                demand_change = mean(
                                  demand_scenario_data$demand_change[idx_2]),
                                min_self_supply = mean(
                                  demand_scenario_data$min_self_supply[idx_2]))
      
      demand_scenario_data <- rbind(demand_scenario_data, add_line)
    }
  }
    
  # now we pick just the data for the regions from the region_vector for this
  # calculateion
  idx     <- which(demand_scenario_data$region_id %in% region_vector)
  demand_scenario_data <- demand_scenario_data[idx,]
    
  # order by region_id
  demand_scenario_data <- demand_scenario_data[
                            order(demand_scenario_data$region_id),]

  variation_vector <- as.numeric(demand_scenario_data$demand_change) 
  
  # the raw data of demand has to be read
  
  demand_raw_data <- readTimeseries(timeseries_name      = "demand", 
                                    year                 = base_year, 
                                    start_hour           = start_hour,
                                    end_hour             = end_hour)
  
  # check for which country we have more than one region, so that the demand
  # timeseries of country has to be distributed in the regions
  divide_regions       <- region_vector[(region_vector %in% 
                                seq(11000,99000,1000)) == FALSE]
  not_divide_regions   <- region_vector[(region_vector %in% 
                                     seq(11000,99000,1000)) == TRUE]
  divide_countries     <- as.numeric(unique(
                                 sub("(\\d)$","",
                                    sub("(\\d)$","",divide_regions))))*100 
  divide_demand        <- demand_raw_data$demand[
                            demand_raw_data$region_id == divide_countries]
  
  # In the renpass database the demand_distribution table gives the shares of 
  # demand for each region -until now of a country. For Germany there are four 
  # value series for four different kind of load situations(from the DENA study)
  # the mean of these four are taken as proportional indicators for the whole
  # time series
  
  # this just happens if the demand has to be divided.
  if(sum(length(divide_regions), length(divide_countries)) != 0){
    
    con_renpass <- connectMysql("renpass")
  
    distribution_raw_data  <- dbGetQuery(con_renpass, 
                                         "SELECT * FROM demand_distribution")
  
    dbDisconnect(con_renpass)
  
    distribution_raw_data  <- distribution_raw_data[
                                order(distribution_raw_data$region_id),]
  
    distribution_key       <- distribution_raw_data$share_of_country[
                              distribution_raw_data$region_id %in% divide_regions]
  
    divide_demand_matrix   <- distributeTimeseries(timeseries = divide_demand,
                              distribution_key = distribution_key)
  
    colnames(divide_demand_matrix) <- divide_regions
  }
  
  if(length(not_divide_regions) > 0){
    not_divide_demand_matrix <- matrix(demand_raw_data$demand[
                                       demand_raw_data$region_id %in% 
                                         not_divide_regions], 
                                       ncol = length(not_divide_regions),
                                       byrow = TRUE)
  
    colnames(not_divide_demand_matrix) <- not_divide_regions
  }
  
  # bring togehter the divided demand data if there is any with the demand data
  # for the countries
  
  if(sum(length(divide_regions), length(divide_countries)) == 0){
    demand_matrix <- not_divide_demand_matrix
  }else{
    if(length(not_divide_regions) > 0){
      demand_matrix <- cbind(divide_demand_matrix, not_divide_demand_matrix)
    }else{
      demand_matrix <- divide_demand_matrix
    }
  }
  
  demand_matrix <- demand_matrix[,order(colnames(demand_matrix))]
   
  varied_demand_matrix <- varyTimeseries(timeseries = demand_matrix, 
                                         variation_factor = variation_vector)
        
  if(time_unit == "quarter"){
    demand_reg <- apply(varied_demand_matrix,2,hoursToQuarters)
  }else{
    demand_reg <- varied_demand_matrix
  }
  
  demand_reg <- convertRegionMatrixToDpr(region_matrix = demand_reg)
   
  return(demand_reg)
}

demand_reg <- regionalDemand(demand_scenario       = demand_scenario,
                             time_unit             = time_unit,
                             Region_dpr_assignment = region_dpr_assignment,
                             start_hour            = start_hour,
                             end_hour              = end_hour)
