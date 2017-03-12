# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, readPathway, readResourcesPathway,
#                            marginalCosts, convertRegionMatrixToDpr
# function defined within this piece of code: regionalBiomass
#-----
# Name: regionalBiomass

# Title: Calculate the Max Possible Biomass Feedin

# Usage: regionalBiomass(biomass_scenario, scenario_year, time_unit,
#                        number_of_regions, Region_dpr_assignment)

# Arguments:
# biomass_scenario  which biomass scenario is chosen in the scenario 
#                   parameters, only scenarios that are described in the 
#                   biomass_scenario table in the pathway database can be 
#                   chosen
# scenario_year     which scenario year is chosen
# time_unit         in which time unit the result timeseries should come out,
#                   possible: "hours" or "quarters"
# number_of_regions       scalar,numeric
# Region_dpr_assignment   data.frame which region_ids belong to which dpr -
#                         dispatch regions which are the regional unit for this
#                         scenario/simulation [data.frame: region_vector 
#                         (numeric), dpr_number(numeric)]

# Details: The function inlcudes the process of producing the matrix of maximal 
#          biomass feedin for every timestep in every region. The real feedin of
#          biomass then depends on the available amount of biomass and the
#          filling level of the biogas store utility, but the real usage is then
#          definded via merit order. In the function, the installed capacity in 
#          the scenario year and the future amount of biomass is read from the 
#          biomass_scenario pathway and the maximum possible available biomass
#          feedin is calculated. normally this is the installed capacity but if 
#          the amount of biomass is zero it is zero.
#          The function has one control: if there is a wrong number of regions
#          given in the geothermal scenario, it stops

# Value: Matrix with timesteps in rows and the regions in the columns. The unit
#        is MW of possible biomass feedin available in every timestep and region
#        of the scenario year
#----------

regionalBiomass <- function(biomass_scenario,
                            scenario_year,
                            time_unit,
                            number_of_regions,
                            Region_dpr_assignment = region_dpr_assignment){  
  
  biomass_scenario_data <- readPathway(scenario      = "biomass",
                                       scenario_name = biomass_scenario,
                                       order_by      = "region_id")
  
  # do we have all regions from the region_vector?
  idx               <- which(region_vector %in% biomass_scenario_data$region_id)
  region_id_defined <- region_vector[idx]
  missing_id        <- region_vector[-idx]
  
  # is there a region missing? Then, the parameters have been defined just for
  # subregions and we have to combine the parameters to the higher level region
  # we need for this scenario
  if(length(missing_id) > 0){
    
    # if there are more than one missing_id, a loop will help
    for(mm in missing_id){
      
      less_digits       <- round(biomass_scenario_data$region_id, 
                                 digits = -3)
      
      idx_2             <- which(less_digits == mm)
      
      add_line          <- data.frame(scenario_name = biomass_scenario,
                                      amount_of_biomass = sum(
                               biomass_scenario_data$amount_of_biomass[idx_2]),
                                      region_id     = mm,
                                      installed_capacity = sum(
                               biomass_scenario_data$installed_capacity[idx_2]))
      
      biomass_scenario_data <- rbind(biomass_scenario_data, add_line)
    }
  }
  
  # now we pick just the data for the regions from the region_vector for this
  # calculation
  idx               <- which(biomass_scenario_data$region_id %in% region_vector)
  biomass_scenario_data <- biomass_scenario_data[idx,]
  
  # order by region_id
  biomass_scenario_data <- biomass_scenario_data[order(
                             biomass_scenario_data$region_id),]
  
  if(length(biomass_scenario_data$region_id) != length(region_vector)){
    stop("wrong number of regions in the biomass scenario")
  }
  
  resources_scenario_data <- readResourcesPathway(scenario = "resources",
                                             scenario_name = resources_scenario,
                                             scenario_year = scenario_year,
                                             order_by      = "region_id")
  
  # pick out just the regions we need
  idx <- which(resources_scenario_data$region_id %in% c(region_vector,0))
  resources_scenario_data <- resources_scenario_data[idx,]
  
  biomass_GJ_price <- resources_scenario_data$price[
                         resources_scenario_data$fuel == 'biomass']
  
  co2_price        <- resources_scenario_data$price[
                         resources_scenario_data$fuel == 'co2']
  
  con_renpass <- connectMysql("renpass")
  
  bio_parameter <- dbGetQuery(con_renpass,"SELECT Cvar, effbrutto, aux 
                                           FROM thermal_pp_parameter
                                           WHERE fuel LIKE 'biomass' 
                                           AND type LIKE 'ic'")
  
  dbDisconnect(con_renpass)
  
  # calculate the average price you would have to pay for biomass if the amount
  # is evenly used during the year.
  bio_mc <- marginalCosts(efficiency = bio_parameter$effbrutto - 
                                       bio_parameter$aux,
                          fuel_price = biomass_GJ_price,
                          co2_price = co2_price,
                          co2_emission_factor = 0,
                          variable_cost = bio_parameter$Cvar)
  
  bio_average_cost <- bio_mc[[1]]
  bio_co2          <- bio_mc[[2]]
  
  # If there is biomass available in a region, the installed capacity is the 
  # maximal feedin per timestep
  bio_feedin_raw    <- biomass_scenario_data$installed_capacity *
    (biomass_scenario_data$amount_of_biomass != 0)
  
  bio_capacity_reg  <- matrix(rep(bio_feedin_raw, each = timesteps), 
                             ncol = length(bio_feedin_raw))
  
  colnames(bio_capacity_reg) <- biomass_scenario_data$region_id
  
  bio_capacity_reg  <- convertRegionMatrixToDpr(region_matrix = 
                                                  bio_capacity_reg)
  
  # bio amount is made: a matrix of the amount of biomass that is left when the
  # available biomass is evenly distributed during the year. The electricity
  # outcome is calculated with the efficiency. Then the regions given are summed
  # up to the dispatch regions according to the region dpr assignment. To pump
  # up bio_amount_year to a matrix with just 0 in the second row and taking it
  # away again later is just a trick that convertRegionMatrixToDpr can be used
  # times 1000 since in the biomass_scenario it is given in GWh
  
  bio_efficiency    <- bio_parameter$effbrutto - bio_parameter$aux
  bio_amount_year   <- matrix(data = c(biomass_scenario_data$amount_of_biomass * 
                                         bio_efficiency *
                                         1000,
                                rep(0,length(biomass_scenario_data$region_id))), 
                              nrow = 2, byrow = TRUE)
  colnames(bio_amount_year) <- biomass_scenario_data$region_id
  bio_amount_year <- convertRegionMatrixToDpr(region_matrix = 
                                                bio_amount_year)
  bio_amount_year <- bio_amount_year[1,]
  
  bio_amount      <- matrix(nrow = 8760/energy_factor, 
                            ncol = number_of_regions)
  
  # distribute the available amount to the time steps of the year
  for(bb in dpr_number){
    bio_amount[,bb] <- seq(bio_amount_year[bb],
                           0,
                           length.out = 8760/energy_factor)
  }
  
  # round to no digits behind the comma
  bio_amount_reg   <- round(bio_amount[start_timestep:end_timestep,])
  
  # Column names are dpr numbers. Each region just has one biomass
  # power plant
  colnames(bio_capacity_reg) <- c(dpr_number)
  colnames(bio_amount_reg)   <- c(dpr_number)
  
  return(list(bio_capacity_reg, 
              bio_amount_reg,
              bio_average_cost))
}

bio_reg <- regionalBiomass(biomass_scenario      = biomass_scenario,
                           scenario_year         = scenario_year,
                           time_unit             = time_unit,
                           number_of_regions     = number_of_regions,
                           Region_dpr_assignment = region_dpr_assignment)

bio_capacity_reg <- bio_reg[[1]]
bio_amount_reg   <- bio_reg[[2]]
bio_average_cost <- bio_reg[[3]]

bio_filling_level           <- matrix(ncol = number_of_regions, 
                                      nrow = timesteps)
bio_filling_level[1,]       <- bio_amount_reg[1,]
colnames(bio_filling_level) <- c(dpr_number)
