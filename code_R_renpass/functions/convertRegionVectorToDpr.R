# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# convertRegionVectorToDpr

# Description:
# converts a vector of regions into a vector of dispatch regions 

# Arguments:
# vector_with_regions     vector
# Region_dpr_assignment   data.frame: region_vector(numeric), 
#                         dpr_number(numeric)
#                         standard value: region_dpr_assignement

# Details:
# The vector_with_regions has the original regions with their region_id and the 
# resulting vector_with_dpr has the dispatch regions that are the region unit 
# for the specific calculation/scenario

# Value:
# dpr vector

# References: renpass manual

# Examples:
# pump$dpr <- convertRegionVectorToDpr(pump$reg)

#------------------------------------------

convertRegionVectorToDpr <- function(vector_with_regions, 
                                     Region_dpr_assignment = 
                                       region_dpr_assignment){
  
  idx <- match(as.numeric(vector_with_regions),
               Region_dpr_assignment$region_id)
  
  vector_with_dpr <- Region_dpr_assignment$dpr_number[idx]
  
  return(vector_with_dpr)
}
