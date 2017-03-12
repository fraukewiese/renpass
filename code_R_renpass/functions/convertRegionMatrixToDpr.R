# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# convertRegionMatrixToDpr

# Description:
# converts a matrix(timesteps x number_of_regions) 
# into a matrix(timesteps x number_of_dprs)

# Arguments:
# region_matrix           matrix,numeric(timesteps * number_of_regions)
# Region_dpr_assignment   data.frame: region_vector(numeric), 
#                         dpr_number(numeric)
#                         standard value: region_dpr_assignement

# Details:
# The region_matrix has the original regions with their region_id and the 
# dpr_matrix has the dispatch regions that are the region unit for the specific
# calculation/scenario

# Value:
# dpr_matrix, numeric(timesteps * number_of_dpr)

# Examples:
# dpr_matrix <- convertRegionMatrixToDpr(wind_onshore_reg)

#------------------------------------------

convertRegionMatrixToDpr <- function(region_matrix, 
                                     Region_dpr_assignment = 
                                       region_dpr_assignment){
  
  idx <- match(Region_dpr_assignment$region_id,
               as.numeric(colnames(region_matrix)))

  region_dpr_assignment_list <- split(Region_dpr_assignment, 
                                      Region_dpr_assignment$dpr_number)

  dpr_matrix <- sapply(region_dpr_assignment_list, function(x){  
                       idx <- which(as.numeric(colnames(region_matrix)) %in% 
                                    x$region_id)
  
                       #rowSums needs two columns
                       if(length(idx) == 1){
                         as.numeric(region_matrix[,idx])
                       }else{
                         rowSums(region_matrix[,idx])
                       }
  })

  return(dpr_matrix)
}
