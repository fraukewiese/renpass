# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass functions: gridLoss, marginalIndex
#-----
# Name: exchangeStandard

# Title: Exchange Electricity between Regions

# Description: 

# Usage: exchangeStandard(RL, grid, idx, price, bmo_list, neg_demand, ov_demand,
#                         exchange_loops)

# Arguments:
# RL                Residualload			
#	grid 	   		      Gridmatrix
#	idx			          Powerplantindex
#	price			        Price
#	bmo_list          Basic Merit Order
# neg_demand        Negative Demand
# ov_demand         Positive Demand/Over Demand
#	exchange_loops    Exchange Loops

# Details:

# Value: 
# out$RL            Residual load after exchange
#	out$idex,         index after exchange
#	out$price         price after ex
#	out$neg_demand    neg_demand
# out$ov_demand     ov_demand	 
#	out$grid          grid
#	out$cost_total    cost_total
#---------------------

exchangeStandard <- function(RL,
				                     grid,
                             idx,
                             price,
                             bmo_list,
                             neg_demand,
                             ov_demand,
                             exchange_loops){
  # Initialisation
  RL_new 	       <- RL 
  grid_new       <- matrix(c(unlist(grid[,1:2]),
                             rep(0,length(grid[,1])),
                             grid[,4]), 
                           ncol = 4)
  idx_new	       <- idx
  price_new      <- price 
  neg_demand_new <- as.integer(neg_demand)
  ov_demand_new  <- as.integer(ov_demand)

  cost_total     <- integer(exchange_loops)


  # exchange main loop
  cost_total[1]  <- sum(RL * price)
  ex             <- 1
  
  while(ex < exchange_loops){
    ex <- ex + 1;

    # choose one pair out of "grid"
    index               <- sample(1:length(grid[,1]),1)
    first_region        <- grid_new[index,1]
    second_region       <- grid_new[index,2]
    grid_loss           <- grid_new[index,4]

    # determine capacity
 	  capacity     	<- grid[index,3]; 
	  capacity_used	<- grid_new[index,3];

 	  cap_min	     	<- c(-capacity - capacity_used);
 	  cap_max	     	<- c(capacity - capacity_used);
	  cap_spread    <- c(seq.int(cap_min,cap_max,1),cap_max); 
	  exchange 	    <- sample(cap_spread,1);
 
    # choose "give"- and "get"-Region 
    if(exchange > 0){
	    give_region <- first_region;		
	    get_region  <- second_region;	
	  }else{
	    give_region <- second_region;
	    get_region  <- first_region; 
	  }

    # get merit order of both regions  
    bmo_give  	  <- bmo_list[[give_region]]
    bmo_get	      <- bmo_list[[get_region]]

    # costs in both regions before the exchange
    cost_give_before  <- RL_new[give_region] * price_new[give_region];
    cost_get_before   <- RL_new[get_region]  * price_new[get_region];
    cost_both_before  <- cost_get_before + cost_give_before; 

    # Residualload in both regions after exchange
    RL_grid_loss 	   <- gridLoss(RL_new        = RL_new,
                                 capacity_used = capacity_used,
                                 exchange      = exchange,
                                 give_region   = give_region,
                                 get_region    = get_region,
                                 grid_loss     = grid_loss)
  
    # determine new marginal-costs (function_marginal_idx)
    give_mar 	 <- marginalIndex(d  = RL_grid_loss$RL_give_after, 
                                 s  = cumsum(bmo_give$available_capacity), 
                                 cm = bmo_give$marginal_cost)
   
    get_mar 	 <- marginalIndex(d  = RL_grid_loss$RL_get_after, 
                                 s  = cumsum(bmo_get$available_capacity), 
                                 cm = bmo_get$marginal_cost)

    # determine new costs (after exchange with new Powerplantcosts)
    #a               <- (give_mar$m_idx != 0);
    #b               <- (get_mar$m_idx != 0);
    #cost_give_after <- 0 + a * (RL_grid_loss$RL_give_after * give_mar$price);
    #cost_get_after  <- 0 + b * (RL_grid_loss$RL_get_after * get_mar$price);
    
    cost_give_after <- RL_grid_loss$RL_give_after * give_mar$price
    cost_get_after  <- RL_grid_loss$RL_get_after * get_mar$price
    
    #rm(a,b);
	
    cost_both_after <- cost_give_after + cost_get_after; 

    # check if costs have been reduced by exchange
   if(cost_both_after <= cost_both_before){
     # new residualload / new marginal-index / new price / new negative demand
     # new positive demand / # new grid
	   RL_new[give_region]         <- RL_grid_loss$RL_give_after;
     RL_new[get_region]          <- RL_grid_loss$RL_get_after;
     idx_new[give_region]        <- as.integer(give_mar$m_idx);    
	   idx_new[get_region]         <- as.integer(get_mar$m_idx);
     price_new[give_region]      <- as.integer(give_mar$price);      
 	   price_new[get_region]	     <- as.integer(get_mar$price);
	   neg_demand_new[give_region] <- as.integer(give_mar$nd);     
  	 neg_demand_new[get_region]  <- as.integer(get_mar$nd);
	   ov_demand_new[give_region]  <- as.integer(give_mar$od);     
 	   ov_demand_new[get_region]   <- as.integer(get_mar$od);
  	 grid_new[index,3]	         <- grid_new[index,3] + exchange; 
        }
   
    # save all results per for output  
    cost_total[ex]      <- sum(RL_new * price_new);

    #BREAK 
    #if(ex > exchange_loops / 10){
    #	if(cost_total[ex] == cost_total[(ex - exchange_loops/10)]){
    #	 break
    #	}
    #}# end of while loop (BREAK)

  }

  # save results in a list for output
  out = list(RL         = RL_new,
             index      = idx_new,
             price      = price_new,
             neg_demand = neg_demand_new,
             ov_demand  = ov_demand_new,
             grid 	    = grid_new,
             cost_total = cost_total);

  return(out)
}
