# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, hoursToQuarters                       
# functions defined within this piece of code: runOfRiverNo
#-----
# Name:
# runOfRiverNo
# 
# Title:
# Generates run-of-river production curve for Norway
# 
# Description:
# The function generates a run-of-river production curve for Norway from a 
# scenario value for total installed capacity. The calculation is based on data 
# on existing run-of-river plants including their capacity and annual inflow and
# inflow data for Norway.
#   
# Usage:
# runOfRiverNo <- (p_ror_no = 20, flo_year = 1969, time_unit = quarter,
# end_timestep = 35040)
# 
# Arguments:
# p_ror_no              - scenario value for installed run-of-river capacity in
#                         Norway [scalar, numeric]
# ror_reg               - matrix with run-of-river production for every region,
#                         updated in the function 
#                         [matrix, numeric (timeteps x regions)]
# weather_year          - one of three possible weather years, defined as
#                         scenario parameter [scalar, numeric]
# time_unit             - is defined as either 'hour' or 'quarter' 
#                         [scalar, string]
# start_timestep        - first timestep for which will be calculated 
#                         [scalar, numeric]
# end_timestep          - last timestep for which will be calculated 
#                         [scalar, numeric]
# energy_factor         - determines the transformation from water to energy, 
#                         depend. on the time_unit, 1 or 0.25 [scalar, numeric]
# res_vector            - vector with all reservoir ids [vector, numeric]
# dpr_number            - the regions from one to number of regions, in
#                         ascending order [vector,numeric]
# region_dpr_assignment - the assignement of the region_ids to the dpr_numbers,
#                         the dpr_numbers are the simulation regions for this
#                         scenario. [data.frame: region_id(vector,numeric),
#                                                dpr_number(vector,numeric)]
# Details:
# Load data for Norwegian run-of-river plants from the database: pinst 
# (installed capacity), inflow (annual inflow in mio cbm), eflow (annual inflow
# transformed into MW * time_unit with efactor), ror_nr (reservoir nr of next 
# downstream reservoir)
#
# Production curve is determined from inflow curve: For every timestep 
# production is restricted either by inflow or by the installed capacity. 
# A matrix of timesteps x ror plants is formed.Total production per timestep 
# is used in the following calculations. The vector of total production is 
# scaled with the scenario value for capacity.
#
# For hydro storage plants the efflux from run-of-river plants into reservoirs 
# is aggregated. A matrix is formed of timesteps x downstream reservoirs.

# Value:
# The function returns two values:
# ror_reg        - matrix with run-of-river production for every dispatch region 
#                  [matrix, numeric (timeteps x regions)]
# flow_river_ror - A matrix with inflow from run-of-river plants for every 
#                  reservoir and timestep.[matrix, numeric]
#-------

runOfRiverNo <- function(p_ror_no,
                         ror_reg,
                         weather_year, 
                         time_unit,
                         start_timestep,
                         end_timestep,
                         energy_factor,
                         res_vector,
                         dpr_number,
                         region_dpr_assignment){

  con_renpass <- connectMysql("renpass")
    
  if (weather_year == 2010){flo_year <- 1969 }
  if (weather_year == 1998){flo_year <- 1990 }
  if (weather_year == 2003){flo_year <- 1962 } 
  
  # Load power plant data  
  sql_ror_no <- dbGetQuery(con_renpass, 
                           "SELECT 
                              Pinst, total_flow, efactor * total_flow AS eflow,
                              flo_river_ror 
                           FROM runofriver_register 
                           WHERE region_id = '19000' 
                           ORDER BY ror_nr")
  
  pinst  <- as.numeric(as.character(sql_ror_no$Pinst))
  inflow <- as.numeric(as.character(sql_ror_no$total_flow))
  eflow  <- as.numeric(as.character(sql_ror_no$eflow)) * 1000 / energy_factor 
  res_nr <- as.numeric(as.character(sql_ror_no$flo_river_ror))
  
  # Load inflow curve
  sql_flow <- paste("SELECT flo 
                    FROM storage_flow 
                    WHERE year = '",flo_year,"' AND level_meter = 'NO' 
                    ORDER BY hour", sep="")
  
  flow     <- dbGetQuery (con_renpass, sql_flow)
  flow     <- as.numeric(as.character(unlist(flow)))
  
  if (time_unit == "quarter") {
    flow <- hoursToQuarters(flow)
  }
  
  # Calculate production curve
  ror_no <- apply(t(flow), 2, function(x) (pmin(pinst, eflow * x)))
  ror_no <- t(ror_no)
  
  ror_no_sum <- rowSums(ror_no)
  ror_no_sum <- ror_no_sum[start_timestep:end_timestep]
    
  ror_no_all <- ror_no_sum * p_ror_no/sum(pinst) 
  
  dpr_no <- region_dpr_assignment[which(region_dpr_assignment[, 1] == 19000), 2]
  
  ror_reg[,which(dpr_number == dpr_no)] <- ror_no_all
  
  # Calculate efflux to reservoirs
  flo_res_nr   <- aggregate(inflow, by = list(res_nr), sum)
  idx_flo_res  <- which(flo_res_nr[,1]> 0)
  
  ror_res_nr   <- flo_res_nr[idx_flo_res,1]
  
  idx_flo_river_ror   <- match(res_vector, ror_res_nr)
    
  ror_flow_no <- t(apply(t(flow), 2, function(x) flo_res_nr[idx_flo_res,2] *x))
  
  flo_river_ror       <- ror_flow_no[ , idx_flo_river_ror]
  idx_river_ror_NA    <- which(is.na(flo_river_ror) == TRUE, arr.ind=TRUE)
  flo_river_ror[idx_river_ror_NA] <- 0
  
  dbDisconnect(con_renpass)
  
  return(list(ror_reg = ror_reg, flow_river_ror = flo_river_ror))
}

rorNo_result <- runOfRiverNo(p_ror_no = p_ror[which(region_vector==19000)], 
                             ror_reg = runofriver_reg,
                             weather_year = weather_year, 
                             time_unit = time_unit,
                             start_timestep = start_timestep,
                             end_timestep = end_timestep,
                             energy_factor = energy_factor,
                             res_vector = reservoir$id,
                             dpr_number = dpr_number,
                             region_dpr_assignment = region_dpr_assignment)

runofriver_reg <- rorNo_result[[1]]
flo_river_ror  <- rorNo_result[[2]]
