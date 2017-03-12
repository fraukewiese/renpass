# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: gridLoss

# Title: Repair the Grid Loss

# Description: In the iteration of the region exchange, electricty is shifted
#              from side to side, to account for that this function takes care
#              that during the iteration the grid loss is kept track of

# Usage: 
# gridLoss(RL_new, capacity_used, exchange, give_region, get_region, grid_loss)

# Arguments: 
# RL_new         residual load after exchange [vector(numeric)]
# capacity_used  capacity that was already used before exchange[scalar(numeric)]
# exchange       exchanged capacity [scalar(numeric)]
# give_region    dpr number of the export region [scalar(numeric)]
# get_region     dpr number of the import region [scalar(numeric)]
# grid_loss      share of exchange that is lost [scalar(numeric)]

# Value:
# out$RL_give_after  residual load in export region after ex [scalar(numeric)]
# out$RL_get_after   residual load in import region after ex [scalar(numeric)]
#-----

gridLoss <- function(RL_new,
                     capacity_used,
                     exchange,
                     give_region,
                     get_region,
                     grid_loss){

if((capacity_used * exchange) >= 0){ 
  RL_give_after 	<- RL_new[give_region] + abs(exchange) * ( 1 + grid_loss )
  RL_get_after 		<- RL_new[get_region] - abs(exchange)

}else{

  if(abs(exchange) < abs(capacity_used)){
    RL_give_after  <- RL_new[give_region] + abs(exchange)
    RL_get_after 	 <- RL_new[get_region] - abs(exchange) * ( 1 + grid_loss )

  }else{
    RL_give_after 	<- RL_new[give_region] +
                       abs(exchange) +
                       (abs(exchange + capacity_used) * grid_loss)
    
    RL_get_after 	  <- RL_new[get_region] - 
                        abs(exchange) - 
                        abs(capacity_used) * grid_loss
  }
}

out <- list(RL_give_after = RL_give_after,
	          RL_get_after  = RL_get_after)

return(out)
}
