# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
filling_level_other_result <- fillingLevelOther(other_storage_data = 
                                                other_storage_data,
                                                fil_other = 
                                                  filling_level_other[tt,]) 

other_storage_merit_order <- filling_level_other_result[[1]]
other_pump_data           <- filling_level_other_result[[2]]
