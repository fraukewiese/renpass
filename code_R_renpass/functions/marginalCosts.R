# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: marginalCosts

# Title: Calculate the marginal cost 

# Description:
# The marginal cost in Euro/MWh for a powerplant are calculated based on the
# efficiency,the fuel and co2 prices and the co2 emission factor for the
# respective fuel and the variable cost.

# Usage: 
# marginalCosts(efficiency, fuel_price, co2_price, co2_emission_factor,
# variable_cost)

# Arguments:
# efficiency             the net efficiency of the plant, numeric, between 0 and
#                        1. Two decimal places possible. e.g 0.44 would be 44%
#                        efficiency. If efficiency is 0, only the variable costs
#                        are taken into account for the marginal costs.
# fuel_price             numeric, price of the fuel in Euro/GJ fuel.
# co2_price              numeric, price of co2 in Euro/t.
# co2_emission_factor    numeric, co2 emissions in t per GJ fuel that is burned:
#                        t/GJ fuel
# variable_cost          numeric, other costs than fuel and co2 for producing
#                        one MWh electric energy with the respective source

# Value: marginal cost in Euro/MWh, of the same shape and length as the input
#        values
#-----

marginalCosts <- function(efficiency, 
                          fuel_price, 
                          co2_price, 
                          co2_emission_factor,
                          variable_cost){
  
  # fuel_cost
  fuel_cost                   <- (1 / efficiency) * 
                                 3.6 * 
                                 fuel_price

  # co2_cost
  co2                         <- (1 / efficiency) * 
                                  3.6 *
                                  co2_emission_factor

  fuel_cost[efficiency == 0]  <- 0
  co2[efficiency == 0]        <- 0

  co2_cost                    <- co2 * co2_price

  # marginal_cost
  marginal_cost              <- fuel_cost +
                                co2_cost +
                                variable_cost

  return(list(marginal_cost, co2))

}
