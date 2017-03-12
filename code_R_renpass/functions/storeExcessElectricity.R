# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# applied renpass function: capacityGrid
#-----
# Name: storeExcessElectricity

# Title: Bring together Excess Electricity and Storage Capacity

# Description: 

# Usage: storeExcessElectricity(grid,
#                               grid_exist,
#                               excess_electricity, 
#                               storage_capacity,
#                               exchange_loops,
#                               tt)

# Arguments:
# grid_after_exchange  Gridmatrix with the columns region_from, region_to,
#                      capacity and grid_loss, the actual usage situation of the
#                      grid
# grid_exist           Gridmatrix with the columns region_from, region_to,
#                      capacity and grid_loss, the original state of the grid
#                      without usage
# dpr_number           dispatch region numbers as a vector in ascending order
# excess_electricity   vector of the electricity [MW] in each region that would
#                      have to be turned off if it cannot be used or brougtht to
#                      other regions. numeric. The names of the numbers of the
#                      vector indicate the region_id s
# storage_capacity     numeric vector of the capacity [MW] in each region that
#                      could be utilised for storing the excess electricity. The
#                      names of the numbers of the vector indicate the region_id
#                      s
# exchange_loops       numeric, one number that indicates the maximum number of
#                      iteration steps
# Tt                   The timestep we are in


# Value: 
# elp_pump             numeric vector of electricity [MW] pumped in each region
# pump_for_ex          numeric vector of pump power [MW] in each region
# ee_reduction_pump    numeric vector of execee electricity [MW] left after the
#                      pump / electricity distribution
#------

storeExcessElectricity <- function(grid_after_exchange,
                                   grid_exist,
                                   dpr_number,
                                   storage_capacity,
                                   excess_electricity,
                                   exchange_loops,
                                   Tt){
  
  # initialisation for vectors, written new in each iteration step
  
  over_ee_temp    <- -excess_electricity
  grid_temp_pump  <- grid_after_exchange
  pump_temp       <- storage_capacity
  
  ex <- 0
  
  while(ex < exchange_loops){
    
    # 1.Stop Criterion
    ex 		<- ex + 1
    
    idx_both 	 <- sample(length(grid_temp_pump[,1]),1)
    idx_grid_1 <- which(grid_temp_pump[idx_both,1] == dpr_number)
    idx_grid_2 <- which(grid_temp_pump[idx_both,2] == dpr_number)
    grid_loss  <- grid_temp_pump[idx_both,4]
    
    f_exchange 	<- capacityGrid(grid_exist[idx_both,3],
                                grid_temp_pump[idx_both,3])
    ep  <- f_exchange[1]
    epa <- f_exchange[2]
    epb <- f_exchange[3]
    em  <- f_exchange[4]
    ema <- f_exchange[5]
    emb <- f_exchange[6]
    
    # the amount that can be shifted to the other region cannot be bigger than
    # what excess energy is in one region
    
    ep <- min(ep, over_ee_temp[idx_grid_1])
    em <- max(em, - over_ee_temp[idx_grid_2])
    
    # sample
    exchange_2 <- ifelse(em < ep, 
                         sample(c(seq(em,ep,by = 100),ep),1), 
                         sample(c(seq(em,ep,by = -100),ep),1))
    
    # what changes in the give region and in the get region. The grid loss hast
    # to be taken into account for that.
    
    if(exchange_2 == 0){next}
    
    if(exchange_2 > 0){
      give_region <- idx_grid_1
      get_region 	<- idx_grid_2
      
      # if used by the plus direction
      if(epb == 0){ 
        change_give 	<- - exchange_2 * ( 1 + grid_loss )
        change_get 	  <- exchange_2
        
      }else{
        
        # the flow goes back, plus does not have to pay grid_loss and the minus 
        # side gets something back
        if(epb >= exchange_2){ 
          change_give   <- - exchange_2
          change_get 	  <- exchange_2 * ( 1 + grid_loss )
          
        }else{ 
          # epb is electricity production plus, 0 < epb < exchange_2, thus for 
          # the epb nothing has to be paid, but for the rest, the epa
          change_give 	<- -(epb + (exchange_2 - epb) * (1 + grid_loss))
          change_get 	  <- exchange_2 + epb * grid_loss 
        }
      }
    }else{
      give_region <- idx_grid_2
      get_region 	<- idx_grid_1
      
      # it goes further into the minus direction
      if(emb == 0){
        change_give 	<- exchange_2 * ( 1 + grid_loss )
        change_get 	<- - exchange_2
        
      }else{
        
        # only the exchange minus back emb is needed, if the grid loss is
        # reversed, give (here: plus) does not need to pay grid loss and the 
        # minus region gets something back.
        
        if(emb <= exchange_2){ 
          change_give 	<- exchange_2
          change_get 	<- - exchange_2 * ( 1 + grid_loss )
          
        }else{ 
          # exchange_2 < emb < 0, thus, first use up and dont pay grid loss for
          # it and then pay grid loss for the rest
          change_give 	<- emb + (exchange_2 - emb) * (1 + grid_loss)
          change_get 	  <- -( exchange_2 + emb * grid_loss )
        }
      }
    }
    
    # the shifting of electricity is done in every case
    # Adjusting the grid
    grid_temp_pump[idx_both,3] 	<- grid_temp_pump[idx_both,3] + exchange_2
    
    # In the give region, only the amount of excess energy (over_ee_temp) is
    # reduced. Sometimes it happens that change give gets negative, thus this
    # is only subtracted to 0. 
    over_ee_temp[give_region] 	<- over_ee_temp[give_region] + change_give
    
    if(over_ee_temp[give_region] < 0){
      over_ee_temp[give_region] <- 0
    }
    
    # for the get_region: what was stored and what is excess energy
    # if there was excess energy in the get_region already before, there are no
    # storage ootions any more, thus the imported excess electricty is added to
    # the existing excess energy.
    
    if(over_ee_temp[get_region] > 0){
      over_ee_temp[get_region] 	<- over_ee_temp[get_region] + change_get
    }else{
      
      # if there is even more storage capacity than excess electricity that 
      # comes in...
      if(pump_temp[get_region] > change_get){
        pump_temp[get_region] <- pump_temp[get_region] - change_get
        
        #...all imported is stored. the excess in this region should be 0 before
        # already, so this is not changed (change_get is always positive)
        
      }else{
        
        # if more excess electricity arrives than storage capcity is there 
        # 0 < pump_temp < over_ee_temp
        # what is left is saved as excess electricity
        over_ee_temp[get_region] <- change_get - pump_temp[get_region]
        
        # the storage capacities are filled
        pump_temp[get_region] <- 0
        
      }
    }
    
    # 2.Stop Criterion
    # If there are no storage possibilities left, the optimum is reached, since
    # as much excess electricity as possible was stored
    if(sum(pump_temp) == 0){break}
    
    # 3.Stop Criterion
    # If there is no excess electricity left, it can stop
    if(sum(over_ee_temp) == 0){break} 
    
    # 4.Stop Criterion
    # If there is excess electricity in each region, it is the same as if there
    # is no pump anywhere, but to be safe, this criterion is additionally
    # introduced
    if(0 %in% over_ee_temp == FALSE){break}
  }
  
  out <- list(pump_temp      = pump_temp,
              over_ee_temp   = over_ee_temp,
              grid_temp_pump = grid_temp_pump)
  
  return(out)
}
