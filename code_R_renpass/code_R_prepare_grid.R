# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, capacityFromCircuits, 
#                            gridLossFromDistance, convertRegionVectorToDpr
# function defined within this piece of code: prepareGrid
#-----
# Name: prepareGrid

# Title: Prepare the Grid Matrix  

# Description: This function builds up the grid matrix for the exchange
#              iteration between the regions.

# Usage: prepareGrid(grid_scenario, region_dpr_assignment)

# Arguments:
# grid_scenario           [character value]
# Region_dpr_assignment   [data.frame: region_vector(numeric), 
#                                      dpr_number(numeric)]

# Details: grid_scenario is read from the database. If the number of circuits 
#          is given instead of the capacity of the line, the capacity is
#          calculated on basis of the number of circuits. The grid_loss is a 
#          percentage, if grid_loss is defined in the grid_scenario, this value 
#          is taken and if grid_loss is zero, the grid_loss is calculated on the
#          basis of the distance of each connection.

# Value: 
# grid_exist   [data.frame: plus_region_id(numeric)
#                           minus_region_id(numeric)
#                           capacity(numeric)
#                           grid_loss(numeric)]

# References: renpass manual

# Examples (optional):

prepareGrid <- function(grid_scenario,
                        region_dpr_assignment){
  
  grid_data <- readPathway(scenario      = "grid",
                           scenario_name = grid_scenario,
                           order_by      = "plus_region_id")
  
  #-----------------------------
  # define or read the capacity
  #-----------------------------
  # for those lines without given capacity, it is calculated
  cap_def <- grid_data[which(grid_data$capacity > 0),]
  cir_def <- grid_data[setdiff(which(grid_data$circuits > 0),
                                    which(grid_data$capacity > 0)),]

  cir_def$capacity <- capacityFromCircuits(circuits      = cir_def[,"circuits"],
                                           maximal_power = 2720,
                                           voltage       = cir_def[,"voltage"])
  
  grid_mix <- rbind(cap_def,
                    cir_def)
  
  #------------------------------
  # define or read the grid loss 
  #------------------------------
  # for those lines without given grid loss, it is calculated,
  # if both are zero, result is 0 grid_loss
  loss_def <- grid_mix[which(grid_mix$grid_loss > 0),]
  dist_def <- grid_mix[which(grid_mix$grid_loss == 0),]
  
  dist_def$grid_loss <- gridLossFromDistance(
                                distance        = dist_def[,"distance"],
                                kind_of_current = dist_def[,"kind_of_current"],
                                voltage         = dist_def[,"voltage"])
  
  grid_mix <- rbind(loss_def,
                    dist_def)
  #-----------------------
  # aggregate connections
  #-----------------------
  
  index      <- as.factor(paste(grid_mix$plus_region_id,
                                grid_mix$minus_region_id,
                                sep = "_"))
  
  capacity   <- round(tapply(grid_mix$capacity,
                             index,
                             sum))

  grid_loss  <- round(tapply(grid_mix$grid_loss,
                             index,
                             mean),
                      6)
  
  # get the regions that were put together to identify the connections apart
  region_columns <- matrix(as.numeric(unlist(strsplit(names(capacity),"_"))),
                           ncol = 2, 
                           byrow = TRUE)
  
  grid_matrix <- data.frame(plus_region_id  = region_columns[,1],
                            minus_region_id = region_columns[,2],
                            capacity        = capacity,
                            grid_loss       = grid_loss)
  
  #--------------------------------------------------
  # convert the region grid matrix to the dpr matrix
  #--------------------------------------------------

  grid_matrix[,"plus_region_id"]  <- convertRegionVectorToDpr(
                                                grid_matrix[,"plus_region_id"])
  grid_matrix[,"minus_region_id"] <- convertRegionVectorToDpr(
                                                grid_matrix[,"minus_region_id"])
  
  # grid_loss and capacity is summed up, connections within dpr taken away.
  grid_exist <- combineGridConnections(grid_input = grid_matrix)
  
  # order: first plus then minus_region_id
  grid_exist <- grid_exist[order(grid_exist$plus_region_id,
                                 grid_exist$minus_region_id),]

  return(grid_exist)
}

grid_exist <- prepareGrid(grid_scenario         = grid_scenario,
                          region_dpr_assignment = region_dpr_assignment)
