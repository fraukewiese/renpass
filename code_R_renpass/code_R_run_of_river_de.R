# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, hoursToQuarters,
#                            convertRegionMatrixToDpr
# functions defined within this piece of code: runOfRiverDe
#-----
# Name:
# runOfRiverDe
# 
# Title:
# Generates run-of-river production curve for all regions in Germany
# 
# Description:
# From a scenario value for installed run-of-river capacityhe in the regions in 
# Germany and level data for different level meters in Germany a run-of-river
# production curve for every region in Germany is generated.
# 
# Usage:
# runOfRiverDe(p_ror_de = c(1,5,3), weather_year = 1990, time_unit = quarter,
# end_timestep = 96)
# 
# Arguments:
# p_ror_de              - Vector with installed run-of-river production for 
#                         every region in Germany [vector, numeric]
# weather_year          - one of three possible weather years, defined as 
#                         scenario parameter [scalar, numeric]
# time_unit             - hour or quarter, resolution of the resulting time
#                         series [scalar, string]
# start_timestep        - first timestep to be calculated [scalar, numeric]
# end_timestep          - last timestep to be calculated [scalar, numeric]
# dpr_number            - the regions from one to number of regions, in
#                         ascending order [vector,numeric]
# region_dpr_assignment - the assignement of the region_ids to the dpr_numbers,
#                         the dpr_numbers are the simulation regions for this
#                         scenario. [data.frame: region_id(vector,numeric),
#                                                dpr_number(vector,numeric)]
#
# Details:
# The production curve for run-of-river plants in Germany is based on level data
# from 40 level meters in Germany for the year 2006. This data is scaled 
# dependent on the inflow year. The level data is transformed into feed-in 
# curves so that the upper and lower (0) limits of production for every 
# run-of-river plant are respected and the resulting average feed-in is in 
# accordance with the assumed average utilisation based on the inflow year.
# 
# Value:
#  ror_reg - The matrix of run-of-river feed-in for the different regions is 
#            updated with more accurate data for the German regions.
#            [matrix, numeric]

#-----------------------------------------------------------------------------
runOfRiverDe <- function(ror_reg,
                         p_ror_de,
                         weather_year,
                         time_unit,
                         start_timestep,
                         end_timestep,
                         dpr_number,
                         region_dpr_assignment){

  con_renpass <- connectMysql("renpass")

  if (weather_year == 2010){uti <- 0.45 } 
  if (weather_year == 1998){uti <- 0.65 } 
  if (weather_year == 2003){uti <- 0.55 }
  
  # Load run-of-river power plant data
  ror_pp_de <- dbGetQuery(con_renpass, 
                          "SELECT ror_nr, pinst, region_id, level_meter
                          FROM runofriver_register 
                          WHERE region_id < 12000 
                          ORDER BY ror_nr")
  
  pp    <- as.numeric(as.character(unlist(ror_pp_de$ror_nr)))
  pinst <- as.numeric(as.character(unlist(ror_pp_de$pinst)))
  level <- as.numeric(as.character(unlist(ror_pp_de$level_meter)))
  reg   <- as.numeric(as.character(unlist(ror_pp_de$region_id)))
  
  ror_data  <- data.frame(cbind(pp, pinst, level, reg))
  
  # Load level data
  level_data <- dbGetQuery(con_renpass, 
                           "SELECT level_meter, flo 
                           FROM storage_flow 
                           WHERE level_meter != 'NO' 
                           ORDER BY level_meter, hour")
  
  level_data <- as.numeric(as.character(unlist(level_data$flo)))
  
  level_flo  <- matrix(c(level_data), ncol = 40, byrow = FALSE)
  
  if(time_unit == "quarter"){
    level_flo <- apply(level_flo, 2, hoursToQuarters)
  }
  
  mean_level       <- apply(level_flo, 2, mean)                              
  max_level        <- apply(level_flo, 2, max)
  
  # Transform inflow into electricity production
  
  pinst_reg   <- aggregate(ror_data$pinst, list(ror_data$reg), sum)
  pinst_reg   <- pinst_reg[match(c(11001:11021), pinst_reg[,1]),2]
  pinst_reg[which(is.na(pinst_reg))] <- 0
    
  pinst_level <- matrix(ncol = 21, nrow = 40)
  ror_dpr_de  <- matrix(0, nrow = nrow(level_flo), ncol = 21)
  colnames(ror_dpr_de) <- c(11001:11021)
  level_nr    <- c(1:40)
  
  ror_dpr_list <- vector("list", 21)
    
  for (dpr in 1:21) {
    
    ror_dpr_list[[dpr]] <- subset(ror_data, ror_data$reg == dpr + 11000)
  }
    
  for (dpr in 1:18) {
    dpr_data <- ror_dpr_list[[dpr]]
    pinst_level_dpr <- aggregate(dpr_data$pinst, list(dpr_data$level), 
                                 sum)
    idx_level         <- match(level_nr, pinst_level_dpr[,1])
    pinst_level[,dpr] <- c(pinst_level_dpr[idx_level,2])
    pinst_level[which(is.na(pinst_level[,dpr])),dpr] <- 0
    
    ror_dpr_level   <- t(t(level_flo) * (((pinst_level[,dpr] * uti) - 
      pinst_level[,dpr])/(mean_level - max_level)) + pinst_level[,dpr]  - 
      (( (pinst_level[,dpr] * uti) - pinst_level[,dpr] )/
      (mean_level- max_level))* max_level)
    
    ror_dpr_de[,dpr] <- rowSums(ror_dpr_level)
  }
  
  ror_dpr_de   <- ror_dpr_de[start_timestep:end_timestep, ]
  ror_change   <- ifelse(pinst_reg == 0, p_ror_de, p_ror_de/pinst_reg)

  ror_dpr_de   <- t(apply(t(ror_dpr_de), 2, function(x) x * ror_change))
  
  region_dpr_assignment_de <- region_dpr_assignment[
    which(region_dpr_assignment[, 1] %in% c(11000:11999)),]
  
  ror_dpr_de_sum <- convertRegionMatrixToDpr(region_matrix = ror_dpr_de,
                                             Region_dpr_assignment =
                                               region_dpr_assignment_de)
                                                
  dpr_de <- unique(region_dpr_assignment_de[, 2])

  ror_reg[, which(dpr_number %in% dpr_de)] <- ror_dpr_de_sum
            
  return(list(ror_reg = ror_reg))

  dbDisconnect(con_renpass)
}

rorDe <- runOfRiverDe(ror_reg = runofriver_reg,
                     p_ror_de = p_ror[which(region_vector %in% c(11000:11999))],
                     weather_year = weather_year,
                     time_unit = time_unit,
                     start_timestep = start_timestep,
                     end_timestep = end_timestep,
                     dpr_number = dpr_number,
                     region_dpr_assignment = region_dpr_assignment)
                              
runofriver_reg      <- rorDe[[1]]
