# This function is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# Name:
# combineGridConnections

# Title: Merge and Sum up Grid Characteristics of connections 

# Description:
# several connections between the same dpr are merged to one information line
# per two connected regions. 

# Usage: combineGridConnections(grid_input, Region_dpr_assignment)

# Arguments:
# grid_input              [data.frame: plus_region_id(numeric)
#                                      minus_region_id(numeric)
#                                      capacity(numeric)
#                                      grid_loss(numeric)]
# Region_dpr_assignment   [data.frame: region_vector(numeric), 
#                                      dpr_number(numeric)]

# Details:
# The grid_input describes connection characteristics. So for the connection
# between two regions, there can be several connection with different capacities
# and grid loss. This function has as a result just one line of connection 
# description for all connections between two regions. connections with regions 
# involved that are not in  the scenario are dropped. connections within regions
# are dropped. capacity of differnt connections between the same regions is 
# summed and the grid loss is summed, but just the grid_loss between the 
# remaining dpr, the grid_loss of connections within is dropped. 

# Value:
# grid_output              [data.frame: plus_region_id(numeric)
#                                      minus_region_id(numeric)
#                                      capacity(numeric)
#                                      grid_loss(numeric)]

# Example:
# grid_exist <- combineGridConnections(grid_exist)

#------------------------------------------

combineGridConnections <- function(grid_input, 
                                   Region_dpr_assignment = 
                                     region_dpr_assignment){
  
  # all regions not taken into account in this simulation are taken out
  idx_NA    <- union(which(is.na(grid_input[,"plus_region_id"] == TRUE)),
                     which(is.na(grid_input[,"minus_region_id"] == TRUE)))
  
  # All connections within a dispatch region are taken out
  idx_inner <- which(grid_input[,1] == grid_input[,2])
  
  idx          <- union(idx_NA, 
                        idx_inner)
  
  if(length(idx) != 0){
    grid_input   <- grid_input[-idx,]
  }
  
  # The smaller dpr number should be in the first column
  idx_change   <- which(grid_input[,"minus_region_id"] < 
                          grid_input[,"plus_region_id"])
  
  if(length(idx_change) > 0){
    grid_dummy             <- grid_input
    grid_input[idx_change,"plus_region_id"]  <- 
                                       grid_dummy[idx_change,"minus_region_id"]
    grid_input[idx_change,"minus_region_id"] <- 
                                       grid_dummy[idx_change,"plus_region_id"]
    remove("grid_dummy")
  }
  
  # make in index which connections connect the same regions
  index      <- as.factor(paste(grid_input$plus_region_id,
                                grid_input$minus_region_id,
                                sep = "_"))
  
  # and apply this index to sum up the capacity
  capacity   <- round(tapply(grid_input$capacity,
                             index,
                             sum))
  
  # and apply this index to sum up the grid_loss
  grid_loss  <- round(tapply(grid_input$grid_loss,
                             index,
                             sum),
                      6)
  
  # get the regions that were put together to identify the connections apart
  dpr_columns <- matrix(as.numeric(unlist(strsplit(names(capacity),"_"))),
                        ncol  = 2, 
                        byrow = TRUE)
  
  grid_output <- data.frame(plus_region_id  = dpr_columns[,1],
                            minus_region_id = dpr_columns[,2],
                            capacity        = capacity,
                            grid_loss       = grid_loss)
  
  rownames(grid_output) <- NULL
  
  return(grid_output)
}
