# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: calcuteEfficiency

# Title: Calculate the Efficiency of Power Plants  

# Description: This functions calculates the efficiency of Power Plants
#              depending on their age. An initial efficiency is given for a
#              historic year and a change rate

# Usage: calculateEffiency(scenario_year, measured_efficieny, efficiency_year,
#                          age, change_factor, auxiliary_power)

# Arguments:
# scenario_year        year of the chosen scenario
# measured_efficiency  the brutto efficiency measured in the respective 
#                      efficiency_year
# efficieny_year       the year for which the measured efficiency is given
# age                  the age of the power plant for which I want to know the
#                      efficiency
# change_factor        the change factor of the efficiency per year due to
#                      technical and other improvement
# auxiliary_power      power needed from the power plant as a share of the input
#                      power, same unit as the efficiency

# Details: There is one control included: If the efficiency is higher than 
#          0.99 it stops. If the input efficiency is zero, the result is zero
#          as well, this can be handled like no value

# Value: vector of efficieny values which are of the same length as the longest
#        input vector and each between 0 and 0.99
#--------------

calculateEfficiency <- function(scenario_year,
                                measured_efficiency,
                                efficiency_year,
                                age,
                                change_factor,
                                auxiliary_power){
  
  efficiency <- measured_efficiency - 
    auxiliary_power + 
    ((scenario_year - efficiency_year) - age) * change_factor
  
  efficiency[measured_efficiency == 0] <- 0
  efficiency[is.na(measured_efficiency) == TRUE] <- 0
  
  #if(sum(efficiency > 0.99)){
  # stop("efficieny of a power plant is above 0.99")
  #}
  
  return(efficiency)
}
