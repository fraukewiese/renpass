# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: initResultMatrix

# Title: Initialize a Matrix for Storing Results in it 

# Description: A matrix with regions in columns and timestep in rows with NA as
#              placeholder is made. The region_vector is the default for regions
#              and the value of timesteps of the renpass calcualation is the
#              default for timesteps

# Usage: initializeResultMatrix(regions     = region_vector, 
#                               time_length = timesteps)

# Arguments:
# regions                vector of the region names or ids. Default is the
#                        region_vector
# time_length            single numeric value, default is the timesteps value
#                        of the renpass calculateion

# Value: empty matrix

#-----

initResultMatrix <- function(regions = dpr_number,
                             time_length = timesteps){
  
  empty_matrix <- matrix(ncol = length(regions), 
                         nrow = time_length)
  
  colnames(empty_matrix) <- dpr_number
  
  return(empty_matrix)
}
