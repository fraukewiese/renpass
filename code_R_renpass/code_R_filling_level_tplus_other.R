# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Description:
# The filling levels of other storage plants are updated with production and 
# pumping operations
#-----

elp_other_pump[tt,]  <- elp_pump_each[idx_other_pump]

fil_other_turb       <- elp_other_storage[tt,idx_other_turb]/
                        other_storage_data$efficiency 
fil_other_pump       <- elp_other_pump[tt,] * other_storage_data$efficiency

idx_fil_other   <- which(is.na(fil_other_turb))
fil_other_turb[idx_fil_other] <- 0

filling_level_other[tt+1,] <- filling_level_other[tt,] - fil_other_turb + 
                              fil_other_pump
