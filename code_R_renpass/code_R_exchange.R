# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: exchangeStandard, exchangeSimulatedAnnealing
#-----
# Description:
# The operational optimization by exchange loop takes place
#-----

if(algorithm == "standard"){
  new <- exchangeStandard(RL             = rl_reg[tt,],
                          grid           = grid_exist,
                          idx            = idx_mar,
                          price          = price_start[tt,],
                          bmo_list       = merit_order,
                          neg_demand     = ee_reduction[tt,],  	
                          ov_demand      = over_demand[tt,],
                          exchange_loops = iteration_maximum)
}

if(algorithm == "simulated_annealing"){
  new <- exchangeSimulatedAnnealing(RL             = rl_reg[tt,],
                                    grid           = grid_exist,
                                    idx            = idx_mar,
                                    price          = price_start[tt,],
                                    bmo_list       = merit_order,
                                    neg_demand     = ee_reduction[tt,],    
                                    ov_demand      = over_demand[tt,],
                                    exchange_loops = iteration_maximum)
}

# Results of the exchange loop are written into the result matrixes
price_exchange[tt,]            <- new$price
ee_reduction[tt,]              <- new$neg_demand
over_demand[tt,]               <- new$ov_demand
residual_load_after_ex[tt,]    <- new$RL
merit_order_idx                <- new$index

if(tt == 1){
  exchange_combined 	<- new$grid
}else{
  exchange_combined 	<- rbind(exchange_combined, new$grid)
}
