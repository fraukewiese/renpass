# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# biomassMeritOrder

# Title:
# Prepare the Merit Order of Biomass Plants for 1 region in 1 timestep

# Description:
# The biomass plants offer electricity in the merit order according to the
# amount of biomass that is left, the available installed capacity in their
# region and the price of biomass and the technical parameter. This is done in 
# every timestep.

# Usage:
# biomassMeritOrder(available_capacity, average_price, amount_average,
# amount_real, scarcity_factor)

# Arguments:
# available_capacity  Available Capacity of biomass in MW [vector, numeric] 
# average_price       average price for the biomass in EUR/MWh [scalar, numeric]
# amount_average      Amount of biomass in GJ that is left in this timestep if
#                     the biomass would be evenly used during the year [vector,
#                     numeric(lenghth = length(available_capacity))]
# amount_real         the real amount of biomass left in GJ [vector, 
#                     numeric(lenghth = length(available_capacity))]
# scarcity_factor     a numeric value that determines how much the marginal cost 
#                     of the biomass vary depending on how much biomass is left.
#                     The higher the more the price rises if more biomass is
#                     already used and the more the price drops if more biomass
#                     is left. Standard value is 100. [scalar, numeric]

# Details: 
# The average amount of biomass that would be left over if the total
# year amount would be evenly distributed, is substracted from real amount of
# biomass that is left over. This is divided by the average amount and then
# multiplied witht the average price and the scarcity factor and either sub-
# tracted from the average price (if there is more biomass left over than
# usual) or added to the price (if not much biomass is left) 

# Value: 
# biomass_merit_order, a data.frame for the biomass merit order for this region
# for this timestep [dataframe: pp_nr (numeric), available_capacity (numeric), 
# marginal_cost (numeric), co2 (numeric)]
#--------------

biomassMeritOrder <- function(available_capacity,
                              average_price,
                              amount_average,
                              amount_real,
                              scarcity_factor){
  
  if(amount_average == 0 || amount_real == 0){
    
    bio_merit_order <- NULL
    
  }else{
    
    # if more biomass than average is already use, the price is higer than 
    # average
    
    if(amount_real <= amount_average){
      
      marginal_cost <- average_price * 
        (1 + (amount_average - amount_real) / 
        amount_average *
        scarcity_factor)
    }
    
    # if less biomass than average would be is used until this timestep, the
    # price is lower than average
    
    if(amount_real > amount_average){
      
      marginal_cost <- average_price *
        amount_average /
        amount_real
      
    }
    
    available_capacity <- min(available_capacity,amount_real)
    
    bio_merit_order <- data.frame(pp_nr = 9999,
                                  available_capacity = 
                                    round(available_capacity),
                                  marginal_cost = 
                                    round(marginal_cost, digits = 2),
                                  co2 = 0,
                                  fuel = "biomass")
  }
  
  rownames(bio_merit_order) <- NULL
  return(bio_merit_order)
}
