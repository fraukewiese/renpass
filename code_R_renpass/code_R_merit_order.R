# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, readResourcesPathway, connectMysql,
#                            assignParameter, marginalCosts, 
#                            convertRegionVectorToDpr, addToMeritOrder
# function defined within this piece of code: fossilMeritOrder
#-----
# Name: fossilMeritOrder

# Title: Generate the Merit Order for the thermal fossil power plants 

# Description: This functions reads register of power plants, the parameters
#              table of power plants, resource prices (fuel and emissions) and 
#              the pathway for the thermal power plants. It picks the power
#              plants according to the limitation parameter. The limitation
#              parameter is either the lifetime of power plants or the total 
#              installed capacity per fuel. 
#              Then the marginal costs for each power plant are calculated via
#              the age which defines the effiency and the fuel and emission 
#              costs and the fixed cost. It then orders the power plants by 
#              their marginal costs for each region

# Usage: fossillMeritOrder(scenario_year, thermal_pp_scenario, 
#                          ressources_scenario, region_dpr_assignment)

# Arguments:
# scenario_year          the year of the scenario
# thermal_pp_scenario    the name of the thermal power plants scenario
# ressources_scenario    the name of the ressources scenario
# Region_dpr_assignment   [data.frame: region_vector(numeric), 
#                                      dpr_number(numeric)]

# Value: merit_order_fossil
#        [list(names of list = dpr_number) of 
#              data.frames with the columns 
#                id (numeric)
#                marginal_cost (numeric) [€/MWh]
#                emission (numeric) [t co2/MWh]
#-----

fossilMeritOrder <- function(scenario_year, 
                             thermal_pp_scenario,
                             ressources_scenario,
                             region_dpr_assignment){
  
  #---------------------------------------------------------
  # get the data of pathways and register from the database
  #---------------------------------------------------------

  # read pathways: thermal_pp_scenario and resources_scenario, if there is no
  # row for this region defined in thermal_pp_scenario, the function stops
  # because without installed capacity there is no merit order for this region
  thermal_pp_pathway <- readPathway(scenario      = "thermal_pp",
                                    scenario_name = thermal_pp_scenario,
                                    order_by      = "region_id")
  
  price_data <- readResourcesPathway(scenario      = "resources",
                                     scenario_name = resources_scenario,
                                     scenario_year = scenario_year,
                                     order_by      = "region_id")
  
  # read register, parameter, emission data
  con_renpass <- connectMysql("renpass")
  
  # if the power plant is already retired in the scenario year, it is not 'in'
  # at all and can just be reactivated by setting the scenario year higher, but
  # at the moment almost none of the power plants has a retire year.
  sql_pp <- paste("SELECT pp_nr, pinst, region_id, fuel, type, year",
                  " FROM thermal_pp_register", 
                  " WHERE (retire > '",scenario_year,"'",
                  " AND `year` < '",scenario_year,"')",
                  " ORDER BY fuel",
                  sep = "")
  
  thermal_pp_register  <- dbGetQuery(con_renpass, sql_pp)
 
  thermal_pp_parameter <- dbGetQuery(con_renpass, 
                         "SELECT Cvar, effbrutto, aux, fuel, type, Cfix, Cinv 
                          FROM thermal_pp_parameter")
  
  emission_data        <- dbGetQuery(con_renpass,
                          "SELECT * FROM emission_parameter")
  
  dbDisconnect(con_renpass)
  
  #-----------------------------
  # which power plants are "in"
  #-----------------------------
  # a loop around all regions appearing in the thermal_pp_pathway, the
  # in_register collects them all up in one data.frame
  
  #--------------------------------
  # Preparation thermal_pp_pathway
  #--------------------------------
  # add all regions of region_dpr_assignment if region_id = 0
  idx_zero <- which(thermal_pp_pathway$region_id == 0)
  
  if(length(idx_zero) > 0){
    region_vector      <- region_dpr_assignment$region_id
    
    for(iz in idx_zero){
    
    add_to <- as.data.frame(lapply(thermal_pp_pathway[iz,],
                                   function(x) rep(x,length(region_vector))))
    add_to$region_id   <- region_vector
    thermal_pp_pathway <- rbind(thermal_pp_pathway,
                                add_to)
    }
    thermal_pp_pathway <- thermal_pp_pathway[thermal_pp_pathway$region_id != 0,]
  }
  
  # if 11000 is given, but the region_scenario is more detailed, lines are added
  idx_11000 <- which(thermal_pp_pathway$region_id == 11000)
  
  if(length(idx_11000) > 0){
    region_vector      <- region_dpr_assignment$region_id
    region_vector_11   <- region_vector[which(region_vector < 11999)]
    
    for(iz in idx_11000){
      
      add_to <- as.data.frame(lapply(thermal_pp_pathway[iz,],
                                  function(x) rep(x,length(region_vector_11))))
      add_to$region_id   <- region_vector_11
      thermal_pp_pathway <- rbind(thermal_pp_pathway,
                                  add_to)
    }
    thermal_pp_pathway <- thermal_pp_pathway[thermal_pp_pathway$region_id != 
                                               11000,]
  }
  
  # take out all the regions that are not in the region_dpr_assignment
  idx_in <- which(thermal_pp_pathway$region_id %in% 
                    region_dpr_assignment$region_id)
  thermal_pp_pathway <- thermal_pp_pathway[idx_in,]
  
  # if fuel gas is given but no type, it is divided 50/50 to gt and cc
  idx_gas_type0 <- which(thermal_pp_pathway$fuel == "gas" && 
                           thermal_pp_pathway$type == 0)
  
  if(length(idx_gas_type0) > 0){
    
    for(iz in idx_gas_type0){
      
      add_to <- as.data.frame(lapply(thermal_pp_pathway[iz,],
                                     function(x) rep(x,2)))
      add_to$type               <- c("gt","cc")
      add_to$installed_capacity <- thermal_pp_pathway$installed_capacity[iz]*0.5
      thermal_pp_pathway        <- rbind(thermal_pp_pathway[-iz,],
                                         add_to)
    }
    thermal_pp_pathway <- thermal_pp_pathway[-c(idx_gas_type0),]
  }
  
  in_register <- numeric()
  
  for(ru in unique(thermal_pp_pathway$region_id)){
    
    region_id    <- ru
    one_pathway  <- thermal_pp_pathway[thermal_pp_pathway$region_id == ru,]
    one_register <- thermal_pp_register[thermal_pp_register$region_id == ru,]
    if(region_id == 11000){
      one_register <- thermal_pp_register[thermal_pp_register$region_id < 11999,]
    }
    
    # in the region_merit all fuels and types will be brought together
    region_merit <- numeric()

    for(opw in 1:nrow(one_pathway)){
      should <- one_pathway[opw,]
      
        # it is a difference in treatment if there are power plants of this kind
        # for this region in the register or not
        if(nrow(one_register) > 0){
  
          if(should$type == 0){
          
            # if it is the same for all fuel, we do not have to choose
            if(should$fuel == 0){
              there <- one_register
            }else{
              there <- one_register[one_register$fuel == should$fuel,]
            }
          }else{
            there <- one_register[intersect(which(one_register$fuel == 
                                                    should$fuel),
                                            which(one_register$type == 
                                                    should$type)),]
          }
  
          # if there is no start-up in the register, the mean value of this type
          # and fuel ist taken, we need this for the age later anyway
          there$year[there$year == 0] <- round(mean(there$year[there$year != 0]))
  
          # if lifetime is chosen
          if(should$limitation == "lifetime"){
    
            # we just take the ones within their lifetime
            ready <- there[which(there$year > (scenario_year-should$lifetime)),]
          }
        }else{
          there <- one_register
        }
  
        # if installed capacity is chosen
        if(should$limitation == "installed_capacity"){
    
          # first the case that power plants have to be taken away
          if(should$installed_capacity < sum(there$pinst)){
      
            # the oldest ones of each fuel_type go away
            there <- there[order(there$year, decreasing = TRUE),]
            idx   <- min(which(cumsum(there$pinst) >=should$installed_capacity))
            ready <- there[c(1:idx),]
      
            # the last power plant taken in is adapted in size to fit exactly 
            # the installed capacity chosen in the thermal_pp_pathway
            ready[idx,"pinst"] <- ready[idx,"pinst"] - 
                                  (sum(ready$pinst) - should$installed_capacity)
          }
          # second the case that power plants have to be added
          if(should$installed_capacity > sum(there$pinst)){
            # new power plants have to be build, they need a pp_nr and the age
            # has to be spread, so that they get different efficiency and thus
            # different marginal costs
            if(nrow(there) > 0){
              mean_inst <- round(mean(there$pinst),1)
              sum_there <- sum(there$pinst)
            }else{
              # if there are no plant for seize(MW) reference 300MW is the seize
              mean_inst <- 300
              sum_there <- 0
            }
            ad        <- (should$installed_capacity - sum_there) / mean_inst
            add       <- ceiling(ad)      
            
            # start year is evenly distributed between this year and the scenario year
            start_years <- round(seq(as.numeric(format(Sys.time(), "%Y")),
                                     scenario_year, 
                                     length.out = add))
            # soll ich noch den typ differenzieren wenn es nicht angegeben ist??
            add_there <- data.frame(pp_nr     = c(9999:(9999-add+1)),
                                    pinst     = c(rep(mean_inst)),
                                    region_id = region_id,
                                    fuel      = should$fuel,
                                    type      = should$type,
                                    year      = start_years)
            
            ready <- rbind(there,
                           add_there)
            
            # the last power plant taken additionally is adapted in size to fit 
            # exactly the installed capacity chosen in the thermal_pp_pathway
            ready[nrow(ready),"pinst"] <- ready[nrow(ready),"pinst"] - 
              (sum(ready$pinst) - should$installed_capacity)
          }
        }
      # get all fuels and types together, but just if something can be added
      if("ready" %in% ls() == FALSE){ready <- NULL}
      if(length(ready) >= 0){
        region_merit <- rbind(region_merit,
                              ready)
      }
    }
    # if no more power plants are added in this run of the loop, nothing happens
    if(length(region_merit) >= 0){
    in_register <- rbind(in_register,
                         region_merit)
    }
  }
  # if there are no fossils at all, it should stop here
  if(length(in_register) == 0){
    merit_order_fossil <- NULL
  }else{
    # only 85 % of the capacity is available, but rounded to 1 decimal place
    in_register$pinst <- round(0.85 * in_register$pinst, 1)
    
    #--------------------------------------------------------------------------
    # combination of in_register with all necessary parameters for calculation
    # efficiency, marginal costs, co2
    #--------------------------------------------------------------------------
    
    # age is calculated according to start-up and scenario year
    in_register$year <- scenario_year - in_register$year
    colnames(in_register)[which(colnames(in_register) == "year")] <- "age"
    
    # combine fuel and type in one parameter for later processing and matching
    in_register$fuel_type <- paste(in_register$fuel,
                                   in_register$type,
                                   sep = "_")
    
    thermal_pp_parameter$fuel_type <- paste(thermal_pp_parameter$fuel,
                                            thermal_pp_parameter$type,
                                            sep = "_")
    
    # prepare and assign all parameters needed to calculate the marginal cost 
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = thermal_pp_parameter,
                                   identifier = "fuel_type",
                                   parameter_of_interest = "Cvar")
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = thermal_pp_parameter,
                                   identifier = "fuel_type",
                                   parameter_of_interest = "Cfix")
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = thermal_pp_parameter,
                                   identifier = "fuel_type",
                                   parameter_of_interest = "aux")
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = thermal_pp_parameter,
                                   identifier = "fuel_type",
                                   parameter_of_interest = "effbrutto")
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = price_data,
                                   identifier = "fuel",
                                   parameter_of_interest = "price")
    
    in_register <- assignParameter(register   = in_register,
                                   parameter  = emission_data,
                                   identifier = "fuel",
                                   parameter_of_interest = "em_fuel")
    
    # price for co2
    in_register$co2_price <- price_data$price[price_data$fuel == "co2"]
    
    # calculate the efficiency
    in_register$efficiency <- calculateEfficiency(
      scenario_year       = scenario_year,
      measured_efficiency = in_register$effbrutto,
      efficiency_year     = 1980,
      age                 = in_register$age,
      change_factor       = 0.003,
      auxiliary_power     = in_register$aux)
    
    mc <- marginalCosts(efficiency          = in_register$efficiency,
                        fuel_price          = in_register$price,
                        co2_price           = in_register$co2_price,
                        co2_emission_factor = in_register$em_fuel,
                        variable_cost       = in_register$Cvar)
    
    in_register$marginal_cost  <- round(mc[[1]], digits = 2)
    in_register$co2            <- round(mc[[2]], digits = 2)
    
    #----------------------------------------------------------
    # make a list for each dpr_number a merit order data.frame
    #----------------------------------------------------------
    
    # add the dpr_number, we need that one instead of region_id
    in_register$dpr_number <- convertRegionVectorToDpr(vector_with_regions  = 
                                                         in_register$region_id,
                                                       Region_dpr_assignment =
                                                         region_dpr_assignment)
    
    # sort by marginal cost and get just the needed columns
    merit_order_fossil <- in_register[order(in_register$marginal_cost),
                                            c("pp_nr",
                                              "pinst",
                                              "marginal_cost",
                                              "co2",
                                              "fuel",
                                              "dpr_number")]
    
    dpr_factor <- as.factor(merit_order_fossil$dpr_number)
    
    # get the right column names and take away dpr column
    merit_order_fossil <- merit_order_fossil[,c(1:5)]
    colnames(merit_order_fossil) <- c("pp_nr",                                       
                                      "available_capacity",
                                      "marginal_cost",
                                      "co2",
                                      "fuel")
    
    # one data.frame for each dpr_number, as a list
    merit_order_fossil <- split(merit_order_fossil, 
                                dpr_factor)

  }
  return(merit_order_fossil)
}

if(thermal_pp_scenario != "no_fossils"){
  merit_order_fossil <- fossilMeritOrder(scenario_year  = scenario_year, 
                                  thermal_pp_scenario   = thermal_pp_scenario,
                                  ressources_scenario   = ressources_scenario,
                                  region_dpr_assignment = region_dpr_assignment)
}else{
  merit_order_fossil <- NULL
}

merit_order_fos_geo <- addToMeritOrder(merit_order        = merit_order_fossil,
                                       add_to_merit_order = geo_merit_order)
