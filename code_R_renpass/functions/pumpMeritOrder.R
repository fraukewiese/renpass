# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# pumpMeritOrder
# 
# Title:
# Generates Merit Order of pumps
# 
# Description:
# From the available data on pumps a merit order of operation for every region
# is generated.
# 
# Usage:
# pumpMeritOrder(pump_data)
# 
# Arguments:
# pump_data    - dataframe on all pumps/other electricity storage [dataframe: 
#                id (numeric), bid_pump (numeric), share (numeric), 
#                reg (numeric)]
# reg_vector   - vector with all regions, default is region_vector
#                [vector, numeric]
#
# Details:
# From the data for all the pumps (id, available capacity, region, filling level
# of the reservoir) a pump merit order (pmo) for every region is generated. The 
# order is determined by the relative filling level of the reservoir of each 
# pump. Pumps with a higher relative filling level, i.e. more water for pumping,
# are used first.
#
# Value:
# pmo_list - list including dataframes with the pumps in merit order for every 
#            region [list [dataframe: id (numeric), bid_pump (numeric), 
#            share (numeric), pump_cum (numeric)]]
#---------

pumpMeritOrder <- function(pump_data,
                           reg_vector = region_vector) {
  
  pmo_list  <- vector("list", length(region_vector))
  idx_pump  <- numeric(length(region_vector))
  
  for(rr in 1:length(region_vector)){
    pump_region 	<- subset(pump_data, pump_data$dpr == dpr_number[rr])
    
    if (nrow(pump_region)>0) {
      
      idx_share          <- order(pump_region$share, decreasing = FALSE)  
      pump_part_sorted   <- pump_region[idx_share,] 
      
      pump_cum                <- cumsum(pump_part_sorted$bid_pump)  
      pmo                     <- cbind(pump_part_sorted[,1:3], pump_cum)
      pmo_list[[rr]]          <- pmo   
    } 
  }
  
  return(pmo_list)
}
