# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: marginalIndex
#-----
# Name:
# pumpCommitment
# 
# Title:
# Determine which pumps are used for delivering the required pumping capacity
# 
# Description:
# The pump merit order (pmo) is matched with the required amount of pumping 
# capacity to determine with pumps are used.
#
# Usage:
# pumpCommitment(pmo_list, region_vector, pump_order, pumping)
# 
# Arguments:
# pmo_list      - list including dataframes with the pumps in merit order for 
#                 every region [list [dataframe: id (numeric), 
#                 bid_pump (numeric), share (numeric), pump_cum (numeric)]] 
# dpr_number    - vector with all dispatch regions [vector, numeric]
# pump_order    - standard order of all pump ids [vector, numeric]
# pumping       - required pumping capacity in every region, in MW 
#                 [vector, numeric]
# 
# Details:
# For every region the pump merit order (pmo) is matched with the required 
# pumping capacity. For that the function marginalIndex is used.The used 
# capacity is recorded in elp_pump_each for every pump.
# 
# Value:
# elp_pump_each - vector with pumping operation for every pump, in MW
#                 [vector, numeric]
# -------------

pumpCommitment <- function (pmo_list,
                            dpr_number,
                            pump_order,
                            pumping) {
  
  elp_pump_each  <- numeric(length(pump_order))
  idx_pump       <- numeric(length(dpr_number))
  
  for(rr in dpr_number){
    
    if(pumping[rr] > 0) {
      pmo 		 <- pmo_list[[rr]]
      mar_pump <- marginalIndex(d=pumping[rr], s=pmo$pump_cum, cm=pmo$share) 
    
      idx_pump[rr] 		 <- as.numeric(as.character(mar_pump$m_idx))
      unused_part_load <- mar_pump$upl
    
      unit_pump_used          <- pmo$id[1:idx_pump[rr]] 
      Pinst_pump_used         <- pmo$bid_pump[1:idx_pump[rr]]
    
      if (unused_part_load > 0) {
        Pinst_pump_used[idx_pump[rr]]<- pmo$bid_pump[idx_pump[rr]] - 
          unused_part_load
      }
    
      idx_elp_pump  <- match(unit_pump_used, pump_order)
    
      idx_9999 <- which(unit_pump_used == 9999)
      idx_elp_pump[idx_9999]  <- which(pump_order == 9999)[which(
        other_storage_data$dpr == rr)]
    
      elp_pump_each[idx_elp_pump]  <- Pinst_pump_used
    }
  }
  
  return(elp_pump_each)
  
}
