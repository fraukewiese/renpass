# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: pumpRegion
#-----

if(exists("other_pump_data")){
  other <- other_pump_data
} else {
  other <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(other)   <- c("id", "bid_pump","share", "dpr")
}

pump_region_result <- pumpRegion(pump_data     = pump,
                                 bid           = bid_pump, 
                                 share         = share_pump,
                                 other         = other,
                                 ee_reduction  = ee_reduction[tt,],
                                 region_vector = region_vector)

elp_pump[tt,]           <- pump_region_result[[1]]
pump_for_ex             <- pump_region_result[[2]]
ee_reduction_pump[tt, ] <- pump_region_result[[3]]
pump_data               <- pump_region_result[[4]]
