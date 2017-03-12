# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: addToMeritOrder
#-----
# Description:
# fossil_merit_order, bio_merit_order and hydro_merit_order are merged and the
# merged merit order is again a list with one table for each region and each
# table ordered by the marginal cost.
#-----

merit_order <- addToMeritOrder(merit_order        = merit_order_fos_geo,
                               add_to_merit_order = bio_merit_order)

merit_order <- addToMeritOrder(merit_order        = merit_order,
                               add_to_merit_order = hydro_merit_order)

if(nrow(other_storage_data) > 0){
  merit_order <- addToMeritOrder(merit_order        = merit_order,
                                 add_to_merit_order = other_storage_merit_order)
}
# round available capacity and marginal cost to 2 digits
for(ll in 1:length(merit_order)){
  
  if(is.null(merit_order[[ll]])){break}
  
  merit_order[[ll]]$available_capacity <- 
                                  round(merit_order[[ll]]$available_capacity, 2)
  merit_order[[ll]]$marginal_cost <- round(merit_order[[ll]]$marginal_cost, 2)
}
