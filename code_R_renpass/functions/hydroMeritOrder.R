# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# hydroMeritOrder
# 
# Title:
# Set up hydro storage merit order
# 
# Description:
# Combines data on available capacity from hydro storage plants with production
# costs to form the hydro merit order
# 
# Usage:
# hydroMeritOrder(turb_data, bid, marginal_cost, region_vector)
# 
# Arguments:
# turb_data             - dataframe with data on hydro storage turbines 
#                         [dataframe: id (numeric), ..., dpr (numeric)]
# bid                   - vector with available capacity of hydro storage 
#                         turbines in MW [vector, numeric]
# marginal_cost         - vector with marginal costs of hydro storage turbines 
#                         in EUR/MWh [vector, numeric]
# Region_dpr_assignment - the assignement of the region_ids to the dpr_numbers,
#                         the dpr_numbers are the simulation regions for this
#                         scenario. [data.frame: region_id(vector,numeric),
#                                                dpr_number(vector,numeric)]
#                         Standard: region_dpr_assignement
#
# Details:
# The information on plant id, bid, marginal costs and region is combined with
# informarion on CO2 and fuel to a dataframe. For hydro storage plants CO2 is
# set to 0 and fuel to 'hydro'. In a region loop the subset of data for every 
# region is selected and stored in the list that forms the hydro merit order.
# 
# Value:
# hydro_merit_order - list with regional hydro merit orders [list[dataframe: id
#                     (numeric), available_capacity (numeric), co2 (numeric), 
#                     fuel (string)]]
#--------------

hydroMeritOrder <- function(turb_data,
                            bid,
                            marginal_cost,
                            Region_dpr_assignment = region_dpr_assignment) {
  
  co2  <- 0
  fuel <- "hydro"
  
  hydro_data <- data.frame(turb_data$id, 
                           bid,
                           marginal_cost, 
                           turb_data$dpr, 
                           co2, 
                           fuel)
  
  colnames(hydro_data) <- c("pp_nr",
                            "available_capacity",
                            "marginal_cost",
                            "dpr", 
                            "co2", 
                            "fuel") 
  
  hydro_merit_order <- vector("list", number_of_regions)
  
  for (rr in dpr_number) {
    
    if(rr %in% hydro_data$dpr) {
      
      hydro_region <- subset(hydro_data, hydro_data$dpr == rr)
      hydro_region <- subset(hydro_region, select = - dpr)
      
      hydro_merit_order[[rr]] <- hydro_region
    } 
  }
  
  names(hydro_merit_order) <- dpr_number
  
  return(hydro_merit_order)
}
