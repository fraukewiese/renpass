# to do: understand...
#        change the T as variable name since T also means TRUE
#        define variables in algorithm scenario and make them really variable
#           abkuehlung 0.9999 usw. simon fragen

# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: exchangeSimulatedAnnealing

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

# References: renpass Manual

#----------
exchangeSimulatedAnnealing <- function(RL,
                                       grid,
                                       idx,
                                       price,
                                       bmo_list,
                                       neg_demand,
                                       ov_demand,
                                       exchange_loops){
  #------
  # INIT
  #------
  RL_new 	       <- RL
  grid_new       <- matrix(c(unlist(grid[,1:2]),
                             rep(0,length(grid[,1]))), 
                           ncol=3)
  idx_new	       <- idx
  price_new      <- price 
  neg_demand_new <- as.integer(neg_demand) 
  ov_demand_new  <- as.integer(ov_demand)

  cost_total     <- integer(exchange_loops)

  # for output
  RL_out 	       <- matrix(NaN,exchange_loops,length(RL))
  idx_out        <- matrix(NaN,exchange_loops,length(idx))
  price_out      <- matrix(NaN,exchange_loops,length(price))
  neg_demand_out <- matrix(NaN,exchange_loops,length(neg_demand))
  ov_demand_out  <- matrix(NaN,exchange_loops,length(ov_demand))
  grid_out       <- replicate(exchange_loops,list())
  

  cost_total[1]  <- sum(RL * price)
  T_out	         <- numeric(exchange_loops)
  p_out 	       <- numeric(exchange_loops)  

  step <- 1
  p    <- 1	
  n    <- 1
  T    <- 100 * exchange_loops 	 # ca  exchange_loops
  k    <- 12		 # 12.5 
  #--------------------
  # exchange main loop
  #--------------------
  ex <- 1
  while(ex < exchange_loops){
    j  <- 0


    # choose one pair out of "grid"
 	  index               <- sample(1:length(grid[,1]),1)
 	  first_region        <- grid_new[index,1]
    first_region        <- which(regions == first_region)
 	  second_region       <- grid_new[index,2]
    second_region       <- which(regions == second_region)
 
    while(j < n & ex < exchange_loops){
      j  <- j + 1
      ex <- ex + 1 
      # determine capacity
 	    capacity     	<- grid[index,3]
	    capacity_used	<- grid_new[index,3]
	    cap_min	      <- -capacity - capacity_used
	  cap_max 	    <-  capacity - capacity_used
	  cap_spread    <- c(seq.int(cap_min,cap_max,step),cap_max) 
	  exchange 	    <- sample(cap_spread,1)
 
    # choose "give"- and "get"-Region 
    if(exchange > 0){
	    give_region <- first_region;		
	    get_region  <- second_region;	
	  } else {
	    give_region <- second_region;
	    get_region  <- first_region; 
	  }

    # get merit order of both regions  
    bmo_give  	   <- bmo_list[[give_region]]
    bmo_get	   <- bmo_list[[get_region]]

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
    give_mar 	   <- marginalIndex(d  = RL_grid_loss$RL_give_after, 
                                  s  = cumsum(bmo_give$available_capacity), 
                                  cm = bmo_give$marginal_cost)
    get_mar 	   <- marginalIndex(d  = RL_grid_loss$RL_get_after, 
                                 s  = cumsum(bmo_get$available_capacity), 
                                 cm = bmo_get$marginal_cost)

    # determine new costs (after exchange with new Powerplantcosts)
    a               <- (give_mar[1] != 0)
    b               <- (get_mar[1] != 0)
    cost_give_after <- 0 + a * (RL_grid_loss$RL_give_after * give_mar[2])
    cost_get_after  <- 0 + b * (RL_grid_loss$RL_get_after * get_mar[2])
    rm(a,b)
	
    cost_both_after <- cost_give_after + cost_get_after

    # check if costs have been reduced by exchange
    if(cost_both_after <= cost_both_before){
	    RL_new[give_region]         <- RL_grid_loss$RL_give_after;   # new residualload 
	    RL_new[get_region]          <- RL_grid_loss$RL_get_after;
	    idx_new[give_region]        <- as.integer(give_mar[,1]);     # new marginal-index
	    idx_new[get_region]         <- as.integer(get_mar[,1]);
      price_new[give_region]      <- as.integer(give_mar[,2]);     # new price 
 	    price_new[get_region]	      <- as.integer(get_mar[,2]);
	    neg_demand_new[give_region] <- as.integer(give_mar[,3]);     # new negative demand
	    neg_demand_new[get_region]  <- as.integer(get_mar[,3]);
	    ov_demand_new[give_region]  <- as.integer(give_mar[,4]);     # new positive demand
 	    ov_demand_new[get_region]   <- as.integer(get_mar[,4]);
  	  grid_new[index,3]	          <- grid_new[index,3] + exchange; # new grid     
    
      } else { 
         
        #accept worse solution with r < p
	      r <- runif(1,0,1); 
	      p <- exp( - k * (cost_both_after - cost_both_before) / T);
	
	      if(p > r){
		      RL_new[give_region]         <- RL_grid_loss$RL_give_after;   # new residualload 
		      RL_new[get_region]          <- RL_grid_loss$RL_get_after;
		      idx_new[give_region]        <- as.integer(give_mar[,1]);     # new marginal-index
		      idx_new[get_region]         <- as.integer(get_mar[,1]);
		      price_new[give_region]      <- as.integer(give_mar[,2]);     # new price 
 		      price_new[get_region]	      <- as.integer(get_mar[,2]);
        	neg_demand_new[give_region] <- as.integer(give_mar[,3]);     # new negative demand
        	neg_demand_new[get_region]  <- as.integer(get_mar[,3]);
        	ov_demand_new[give_region]  <- as.integer(give_mar[,4]);     # new positive demand
	 	      ov_demand_new[get_region]   <- as.integer(get_mar[,4]);
 		      grid_new[index,3]	          <- grid_new[index,3] + exchange; # new grid
		    }	
		  }
      # save all results for output  
      cost_total[ex]      <- sum(RL_new * price_new);

      RL_out[ex,] 	     <- RL_new;
      idx_out[ex,]        <- idx_new;
      price_out[ex,]      <- price_new;
      neg_demand_out[ex,] <- neg_demand_new;
      ov_demand_out[ex,]  <- ov_demand_new;
      grid_out[[ex]]      <- grid_new;
  
      T_out[ex] <- T
      p_out[ex] <- p
          
    } # end inner while loop
    
    T         <-  T * 0.9994  # 0.9994 - 0.9996
    
  } # end of while-loop (exchange_loops)

  ersetze.zero <- function(x,value=NaN){  
    if(x==0)
      x <- value
    return(x)
  } 
  cost_total <- unlist(lapply(cost_total,ersetze.zero))

  opt <- which.min(cost_total[2:ex]);
  opt <- opt + 1

  RL_opt         <- RL_out[opt,];
  idx_opt        <- idx_out[opt,];
  price_opt      <- price_out[opt,];
  neg_demand_opt <- neg_demand_out[opt,];
  ov_demand_opt  <- ov_demand_out[opt,];
  grid_opt       <- grid_out[[opt]];

  # save results in a list for output
  out=list(RL         = RL_opt,
	         index      = idx_opt,
 	         price      = price_opt,
	         neg_demand = neg_demand_opt,
           ov_demand  = ov_demand_opt,	 
	         grid 	    = grid_opt,
	         cost_total = cost_total,
	         T 	        = T_out,
	         p 	        = p_out	)

  return(out)
  
} # end of function
