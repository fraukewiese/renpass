# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name: marginalIndex

# Title: Index of the Marginal Power Plant

# Description: The index of the marginal power plant in a merit order for a 
#              given demand is calculated as well as the excess demand and the
#              excess energy.

# Usage: marginalIndex(d, s, cm)

# Arguments:
# d    demand: numeric value in MW
# s    supply: one vector of the cumulated power plant capacity which are
#              ordered by the marginal costs
# cm   vector of the marginal costs of the power plants

# Details: 

# Value: 
# marginal_idx   index of the marginal power plant in the given merit order
# price   	     marginal costs of the marginal power plant [Euro/MWh]
# nd 		         negative demand (always a negative value), which is the same as
#                excess energy or reduction of renewable feed-in
# od 		         over demand (always a positive value), demand that cannot be
#                supplied by the given merit order: there is still demand left
#                even if all power plants in the merit order are used
# upl 		       unused part load of the marginal power plant

#---------------

marginalIndex <- function(d,s,cm){ 

  # if the demand is smaller of the same as zero d <= 0, no marginal power plant
  # has to be found, then there is just negative demand nd = 0 and everything
  # else is zero
  
  if(d <= 0){
    m_idx 	<- 0
    nd 		  <- d
    price 	<- 0
    od 	    <- 0
    upl     <- 0

  # if there is demand, the negative demand is for sure 0
  }else{
    nd 	<- 0
    
    # if there are no powerplants at all
    if(length(s) == 0){
      m_idx 		<- 0
      od 		    <- d
      # if there are no power plants at all in a region and there is over demand
      # the price is 1000, this is randomly defined.
      price 		<- 1000 
      upl 		  <- 0
      
    # if there are power plants  
    }else{
      
      # and the demand is higher than the capacity
      if(d > max(s)){
        
        # the marginal power plant is the last existing power plant
        m_idx 		<- length(s)
        
        # and the over demand is what cannot be met by the existing capacity
        od 		    <- d - max(s)
        price 		<- 1000
        
        # since every plant is used fully, there is no unused part load
        upl 		  <- 0 
        
      # if the demand can be met by the capacity  
      }else{
        # index of powerplant that can meet the demand
        m_idx  <- min(which(s >= d))
        price  <- cm[m_idx]
        od 		 <- 0 
        # unused part load
        upl 	 <- s[m_idx] - d
      }
    }
  } 
  return(list(m_idx = m_idx, price = price, nd = nd, od = od, upl = upl))
}
