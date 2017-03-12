# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# catchSpillage
# 
# Title:
# Fill spillage into downstream reservoirs
# 
# Description:
# Filling levels above the maximum filling capacity lead to spillage. 
# The spillage is added to the next downstream reservoir, if there is one.
#
# Usage:
# catchSpillage (fil, fil_max, idx_spil)
# 
# Arguments:
# fil                - vector with current filling level in mio cmb for each 
#                      reservoir in the order of reservoirs
#                      [vector, numeric]
# fil_max            - vector with maximum filling levels in mio cbm for each
#                      reservoir in the order of reservoirs
#                      [vector, numeric]
# idx_spil           - index vector in the order of reservoirs that indicates 
#                      the next downstream reservoir [vector, numeric]
#
# Details:
# Spillage is determined for each reservoir as the difference between filling
# level and maximum filling level. With the spillage index this is assigned to 
# the next downstream reservoir. The new filling level is calculated for every
# reservoir.
# 
# Value:
# fil_new - vector with the new filling level in mio cmb for each reservoir 
#           [vector, numeric]
#---------

catchSpillage <- function(fil,
                          fil_max,
                          idx_spil){

  spil <- fil - fil_max
  spil <- ifelse(spil < 0, 0, spil)
  
  fil_new  <- fil - spil
  
  spil_new <- spil[idx_spil]
  spil_new <- ifelse(is.na(spil_new), 0, spil_new) 
  
  fil_new  <- fil_new + spil_new
  
  return(fil_new)
    
}
