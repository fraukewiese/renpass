# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: assignParameter

# Title: Assign Parameter to a Register  

# Description: This functions matches a parameter from one table to a register
#              table

# Usage: assignParameter(register, parameter, identifier, parameter_of_interest)

# Arguments:
# register               a data frame of the register that should be expanded by
#                        one column with the parameter of interest. It must
#                        include the column with the heading of the chosen
#                        identifier
# parameter              a data frame of the parameter table that includes the 
#                        identifier column and the parameter of interest
# identifier             a character, the name of the identifier column in the 
#                        register and the parameter table
# parameter_of_interest  a character, the name of the column in the parameter
#                        table which should be added in the right order to the 
#                        register

# Details: The register has a column, called the identifier column which defines
#          the new parameter. The new parameter comes from the parameter table 
#          which has identifiere and the parameter of interest column.

# Value: data frame of the input register, expanded by one column

# References: renpass manual

# Example:
# power_plants_register <- data.frame(pp_id = c(1:5), 
#                                   fuel  = c("gas","gas","coal","gas","coal"))
# power_plants_parameter <- data.frame(fuel = c("uran","coal","gas"),
#                                      efficiency = c(0.11,0.33,0.45))
# assignParameter(register              = power_plants_register,
#                 parameter             = power_plants_parameter,
#                 identifier            = "fuel",
#                 parameter_of_interest = "efficiency")
#-------------------
assignParameter <- function(register,
                            parameter,
                            identifier,
                            parameter_of_interest){
  
  column_register    <- which(colnames(register)  == identifier)
  column_parameter   <- which(colnames(parameter) == identifier)
  column_of_interest <- which(colnames(parameter) == parameter_of_interest)
  
  idx_parameter <- match(register[,column_register],
                         parameter[,column_parameter])
  
  new_column_register <- parameter[idx_parameter,column_of_interest]
  
  register_expanded <- data.frame(register, new_column_register)
  colnames(register_expanded) <- c(colnames(register), parameter_of_interest)
  
  return(register_expanded)
  
}
