# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: addToMeritOrder

# Title: Add supply to an already existing Merit Order

# Description: 
# Supplying power plants can be added to an existing Merit Order if the
# additional plants have the same information: pp_nr / marginal_cost / co2 as
# the original one.

# Usage: 
# addToMeritOrder(merit_order,add_to_merit_order)

# Arguments:
# merit_order         list with one table per region with the columns pp_nr / 
#                     installed_capacity / marginal_cost / co2. The names of the
#                     tables of the list have to be the region_ids
# add_to_merit_order  list of the same shape as merit_order that should be added
#                     to it. It does not need to have the same number of tables
#                     since addititonal power plants in other regions could be
#                     added as well. The names of the tables of the list have to
#                     be the region_ids

# Details: The merit order given by region. To each regional merit order some
#          power plants, that could be biomass plants or hydro plants are added
#          Afterwards per region, it is ordered by the marginal costs.

# Value: List of tables with four columns: pp_nr / installed_capacity /
#        marginal_cost / co2
#--------

addToMeritOrder <- function(merit_order,
                            add_to_merit_order){
  
  if(is.null(nrow(merit_order)) & length(merit_order) == 0){
    merit_order_new <- add_to_merit_order
  }else{
    if(is.null(nrow(add_to_merit_order)) & length(add_to_merit_order) == 0){
      merit_order_new <- merit_order
    }else{
      region_id_mo   <- as.numeric(names(merit_order))
      region_id_add  <- as.numeric(names(add_to_merit_order))
      
      idx_both <- intersect(region_id_mo,region_id_add)
      idx_all  <- union(region_id_mo, region_id_add)
      idx_all  <- idx_all[order(idx_all)]
      
      merit_order_new         <- vector("list", length(idx_all))
      names(merit_order_new)  <- idx_all
      
      for(aa in idx_all){
        
        new_mo <- rbind(merit_order[[paste(aa)]], 
                        add_to_merit_order[[paste(aa)]])
        
        if(length(new_mo) > 0 & sum(dim(new_mo)) != 2){
          merit_order_new[[paste(aa)]] <- new_mo[order(new_mo$marginal_cost),]
        }
      }
    }
  }

  return(merit_order_new)
  
}
