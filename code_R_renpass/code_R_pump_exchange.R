# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: storeExcessElectricity
#-----

out <- storeExcessElectricity(grid               = grid_after_exchange,
                              grid_exist         = grid_exist,
                              dpr_number         = dpr_number,
                              storage_capacity   = pump_for_ex,
                              excess_electricity = ee_reduction_pump[tt,],
                              exchange_loops     = iteration_maximum,
                              Tt                 = tt)

pump_temp              <- out$pump_temp
over_ee_temp           <- out$over_ee_temp
grid_temp_pump         <- out$grid_temp_pump
# At the end of the pump exchange loop, the results are saved and prepared for
# the output

elp_pump[tt,] 		            <- elp_pump[tt,] + pump_for_ex - pump_temp
ee_reduction_pump_ex[tt,] 	  <- over_ee_temp

if(tt == 1){
  exchange_pump_combined 	<- grid_temp_pump
}else{
  exchange_pump_combined 	<- rbind(exchange_pump_combined, grid_temp_pump)
}
