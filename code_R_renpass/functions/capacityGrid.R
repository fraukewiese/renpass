# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: capacityGrid

# Title: Calculates how much electricy could go through a trasmission line

# Description: The functions calculates how much electricity can be transmitted
#              between two regions according to the grid capacity and the 
#              already utilised capacity and a given grid_loss

# Usage: capacityGrid(ge,cu)

# Arguments:
# ge           grid_exist: numeric value, always positive, grid capacity between 
#              two regions in MW
# cu           capacity_used: numeric value, negative or positive,which capacity 
#              is already used

# Details: The capacity of the grid in the simulation of renpass is stored in a
#          matrix with the columns plus region / minus region / capacity / grid
#          loss. The capacity in both directions are the same. Then there is a 
#          matrix with the same columns that stores the actual state, the 
#          capacity utilisation of each line. If the number for capacity is
#          positive in this matrix, that means it is already used to transport
#          electricity from the plus into the minus region. If it is negative
#          it is already used to transport electricty from the minus into the
#          plus region. What makes it complicated is that for each change on 
#          each line the grid loss has to be taken into account. Thus this 
#          function not only gives out the amount of electricity that can be
#          transported but also the different amounts that have to be treated
#          with regard to grid loss.

# Value:
# ep   exchange plus: electricity that can flow from the plus into the minus 
#      region -- always positive
# epa  exchange plus add: part of ep for which grid loss has to be taken into
#      account in the plus region (MW)
# epb  exchange plus back: part of ep for which grid loss has not be taken into
#      account for the plus region but for which the minus region gets the grid
#      loss back
# em   exchange minus: electricity that can flow from the plus into the minus
#      region -- always negative
# ema  exchange minus add: part of em for which grid loss has to be taken into
#      acount in the minus region
# emb  exchange minus back: part of em for which grid loss has not be taken into
#      account in the minus region but for which the plus region gets the grid
#      loss back
#--------------

capacityGrid 	<- function(ge,cu){

  if( cu == 0 ){
    epa 	<- ge
    epb 	<- 0
    ema 	<- -ge
    emb 	<- 0
  }else{
    if( cu > 0 ){
      epa 	<- ge - cu
      epb 	<- 0
      ema 	<- - ge
      emb 	<- - cu

    }else{
      epa 	<- ge
      epb 	<- - cu
      ema 	<- - ( ge + cu )
      emb 	<- 0
    }
  }
  
  # electricity that the plus region could send to the minus region
  ep <- round(epa + epb)
  
  # electricity that the minus region could send to the plus region
  em <- round(ema + emb)

  # Test if the possible exchange was calculated correctly
  # if(-1 * em + ep != 2 * ge) 
  # stop("possible exchange not calculated correctly:code_R_function_grid_loss")
  
  return(cbind(ep, epa, epb, em, ema, emb))
}
