# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# pumpRegion
# 
# Title:
# calculate used capacity for pumping excess electricity within a region
# 
# Description:
# The function matches the available pumping capacity in each region with the 
# excess electricity within the region
# 
# Usage:
# pumpRegion(pump_data, bis, share, other, ee_reduction, elp_pump)
# 
# Arguments:
# pump_data     - dataframe with data on pumps [dataframe: id (numeric), 
#                 pinst (numeric), dpr (numeric), year (numeric)]
# bid           - available pumping capacity of every pumpin MW 
#                 [vector, numeric]
# share         - relative filling level of every pump [vector, numeric]
# other         - dataframe with data on other storage plants 
#                 [dataframe: id (numeric), bid_pump (numeric), share (numeric), 
#                 reg (numeric)]
# ee_reduction  - matrix with excess electricity in every region for every 
#                 timestep [matrix (timesteps x regions), numeric]
# region_vector - vector with all regions [vector, numeric]  
#
# Details:
# The used capacity for pumping excess electricity within every region is 
# determined as the minimum of available pumping capacity and excess electricity
# to be pumped. The remaining available pumping capacity is found by subtracting
# used pumping capacity in every region from available pumping capacity in the 
# region. The remaining excess electricity is determined acccordingly.
#
# Value:
# elp_pump          - vector with pumping capacity per region, in MW 
#                     [vector, numeric]
# pump_for_ex       - remaining available pumping capacity that can be used for 
#                     pumping excess electricity from other regions, in MW
#                     [vector, numeric]
# ee_reduction_pump - remaining excess electricity in every region that cannot 
#                     be pumped within the region, in MW [vector, numeric]
# pump_data         - dataframe on all pumps/other electricity storage 
#                     [dataframe: id (numeric), bid_pump (numeric), 
#                     share (numeric), reg (numeric)]
#------

pumpRegion <- function(pump_data,
                       bid,
                       share,
                       other,
                       ee_reduction,
                       region_vector) {
  
  pump_data           <- data.frame(pump_data$id, bid, share, pump_data$dpr)
  colnames(pump_data) <- c("id", "bid_pump","share", "dpr")
  
  pump_data  <- rbind(pump_data, other)
  
  pump_reg   <- aggregate(pump_data$bid_pump, list(pump_data$dpr), sum)
  idx_pump   <- match(dpr_number, pump_reg[,1])
  pump_reg   <- pump_reg[idx_pump,2]
  pump_reg   <- ifelse(is.na(pump_reg), 0, pump_reg)
  
  elp_pump        		   <- pmin(pump_reg, abs(ee_reduction)) 
  pump_for_ex 			     <- pump_reg - elp_pump 
  ee_reduction_pump      <- ee_reduction + elp_pump 
  
  return(list(elp_pump          = elp_pump, 
              pump_for_ex       = pump_for_ex, 
              ee_reduction_pump = ee_reduction_pump,
              pump_data         = pump_data))
  
}
