# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: readPathway, convertRegionVectorToDpr
# function defined within this piece of code: otherStorageScenario
#-----
# Name:
# otherStorageScenario
# 
# Title:
# Generate data frame with new generic storage plants
# 
# Description:
# For a storage scenario as defined as scenario parameter a data frame with 
# generic storage plants (other than hydro) is generated.
# 
# Usage:
# otherStorageScenario(other_storage_scenario, scenario_year, region_vector)
# 
# Arguments:
# scenario      - name of the other storage scenario from the database
#                 [scalar, string]
# scenario_year - year for which the scenario is to be calculated 
#                 [scalar, numeric]
# region_vector - vector with the regions in the scenario [vector, numeric]
# 
# Details:
# For a defined other storage scenario for each region new storage plants with 
# installed capacity, energy storage capacity and efficiency are loaded from the 
# database and formed to a data frame.
# 
# Value:
# other_storage_data - dataframe that includes new other storage capacity in 
#                      each region [dataframe: id (numeric), reg(numeric),
#                      power (numeric), energy (numeric), efficiency (numeric)]
#------

otherStorageScenario <- function(scenario, 
                                 scenario_year,
                                 region_vector) {

  other_storage   <- readPathway(scenario      = "other_storage",
                                 scenario_name = scenario,
                                 order_by      = "region_id")
  
  idx 			 <- match(region_vector, other_storage$region_id)
  
  power      <- as.numeric(other_storage$capacity[idx])
  energy     <- as.numeric(other_storage$energy[idx])
  efficiency <- as.numeric(other_storage$efficiency[idx])
  
  power      [which(is.na(power) == TRUE)]      <- 0
  energy     [which(is.na(energy) == TRUE)]     <- 0
  efficiency [which(is.na(efficiency) == TRUE)] <- 0
  
  id    <- rep(9999,length(region_vector))
  
  reg   <- region_vector
  
  other_storage_data           <- data.frame(cbind(id, reg, power, energy, 
                                                   efficiency))
  colnames(other_storage_data) <- c("id", "reg", "power", "energy", 
                                    "efficiency")
  
  other_storage_data <- subset(other_storage_data, 
                               other_storage_data$power > 0)

  return(other_storage_data)
}

other_storage_data <- otherStorageScenario(scenario = other_storage_scenario,
                                           scenario_year = scenario_year,
                                           region_vector = region_vector)

if(nrow(other_storage_data) > 0) {
  # change region_id to dispatch region numbers
  other_storage_data$reg <- convertRegionVectorToDpr(other_storage_data$reg)
  dummy                  <- colnames(other_storage_data)
  dummy_2                <- gsub("reg","dpr",dummy)
  colnames(other_storage_data) <- dummy_2
  for(dd in unique(other_storage_data$dpr)){
    idx <- other_storage_data$dpr == dd
    other_storage_data[idx,"power"] 
  }
  
  dummy_3 <- aggregate(other_storage_data[,c("power","energy")], 
                       list(other_storage_data$dpr), 
                       FUN = sum)
  
  dummy_4 <- aggregate(other_storage_data[,"efficiency"], 
                       list(other_storage_data$dpr), 
                       FUN = mean)
  
  other_storage_data <- cbind(rep(9999,nrow(dummy_3)),
                              dummy_3, 
                              dummy_4$x)
  
  colnames(other_storage_data) <- dummy_2
  
  remove(dummy,
         dummy_2,
         dummy_3,
         dummy_4)
  
  filling_level_other <- matrix(0, nrow = timesteps + 1, 
                                ncol = nrow(other_storage_data))
  filling_level_other[1,] <- 0 # other_storage_data$energy * 0.5
  
}
