# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# fillingLevelOther
# 
# Title:
# Calculate available capacity and costs from other storages 
# 
# Description:
# From the current filling level of the storages the available production and
# pumping capacity and the marginal costs are calculated.
# 
# Usage:
# fillingLevelOther(other_storage_data, fil_other, region_vector)  
# 
# Arguments:
# other_storage_data - dataframe that includes data on non-hydro storage 
#                      capacity [dataframe: id (numeric), capacity (numeric),
#                      energy (numeric), efficiency (numeric)]
# fil_other          - vector with current filling levels of the storage plants
#                      [vector, numeric]
# region_vector      - vector with the regions in the scenario [vector, numeric]
# 
# Details:
# From the current filling levels and the installed capacity the available 
# capacity is determined as the minimum. The relative filling levels are 
# calculated by dividing current filling levels by total storage volume. The
# relative filling level is used to determine the marginal costs. When the 
# filling level is higher marginal costs are lower. The information on id, 
# available capacity, marginal costs and region is combined with information on
# co2 and fuel. CO2 is set to 0 because other storages are pure storage plants 
# without original prodcution. The fuel is set to 'other_storage'. 
# In a region loop for every region a subset of the storage data is selected
# and stored in the list that forms the other_storage_merit_order.
# 
# Value:
# other_storage_merit_order - list with regional other storage merit orders 
#                             [list[dataframe: id (numeric), available_capacity 
#                             (numeric), co2 (numeric), fuel (string)]]
# other_pump_data           - datafame with information on available pumps
#                             [dataframe: id (numeric), bid_pump (numeric), 
#                             share (numeric), dpr (numeric)]
#--------- 

fillingLevelOther <- function(other_storage_data,
                              fil_other){
  
  bid_other_turb <- pmin(other_storage_data$power, 
                         fil_other * other_storage_data$efficiency)
  bid_other_pump <- pmin(other_storage_data$power, 
                         (other_storage_data$energy - fil_other) / 
                           other_storage_data$efficiency)
  
  share_other <- fil_other/other_storage_data$energy
  
  cmar_other <- -1756.4*(share_other)^5 + 4946.5*(share_other)^4 - 
    5499.6*(share_other)^3 + 2965.2*(share_other)^2 - 
    780.58*(share_other) + 133.43
  
  co2  <- 0
  fuel <- "other_storage"
  
  other_data <- data.frame (other_storage_data$id, 
                            bid_other_turb, 
                            cmar_other, 
                            other_storage_data$dpr,
                            co2,
                            fuel)
  
  colnames(other_data) <- c("pp_nr",
                            "available_capacity",
                            "marginal_cost",
                            "dpr", 
                            "co2", 
                            "fuel")
  
  other_storage_merit_order <- vector("list", number_of_regions)
  
  for (rr in dpr_number) {
    if(rr %in% other_data$dpr) {
      other_region <- subset(other_data, other_data$dpr == rr)
      other_region <- subset(other_region, select = - dpr)
      other_storage_merit_order[[rr]] <- other_region
    }
  }
  
  names(other_storage_merit_order) <- dpr_number
  
  other_pump_data <- data.frame(other_storage_data$id, 
                                bid_other_pump, 
                                share_other, 
                                other_storage_data$dpr)
  
  colnames(other_pump_data)   <- c("id", "bid_pump","share", "dpr")                                         
  
  return(list(other_storage_merit_order = other_storage_merit_order, 
              other_pump_data = other_pump_data))
  
}
