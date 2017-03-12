# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: capacityFromCircuits

# Title: Calculation of the Grid Capacity  

# Description: This function calculates the capacity of an electricyt connection
#              from the nunber of circuits and the voltage and the maximum power

# Usage: 
# capacityFromCircuits(circuits, maximal_power, voltage, utilisation_factor)

# Arguments:
# circuits            number of circuits of the connection to be calculated,
#                     single number or vector
# maximal_power       numeric, in Ampere, the maximum power that this kind of 
#                     line can handle, single number or if different of the
#                     different lines a vector
# voltage             the volatage in kiloVolt in which the line is utilized, 
#                     for highest voltage 380 or 220, single value or a vector
#                     of the length of circuits or maximal power
# utilisation_factor  a numeric decimal number between 0 and 1. The standard
#                     value is 0.7, thus just 70% of the line can be utilised
#                     due to fulfill the n-1 criteria in a simplified way
#                     usually one number but can also be a vector of the same
#                     length as the longest vector of circuits, maximal_power, 
#                     voltage

# Details: The aim is to find out how high the transport capacity of a line/
#          connection for electricity is. This is calculated from the number
#          of circuits, the voltage and the power of the line. Since the n-1
#          criteria needs to be fulfilled, an utilisation_factor can reduce the
#          maximum capacity of a line

# Value: numeric vector of the same length like the longeste input vector of the
#        four input variables, the maximum transport capacity in MW of that line

# References: renpass manual
#             DENA-Studie 2

# Examples (optional):
#------

capacityFromCircuits <- function(circuits, 
                                 maximal_power, 
                                 voltage, 
                                 utilisation_factor = 0.7){

capacity <- circuits * 
            maximal_power * 
            voltage * 
            sqrt(3) * 
            utilisation_factor /
            1000

return(capacity)

}
