# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, readWeatherScenario,
#                            readRasterWeatherTimeseries,
#                            convertRegionMatrixToDpr
# functions defined within this piece of code: feedInWindOnshore
#-----
# Name:
# feedInWindOnshore.
# 
# Title:
# feedInWindOnshore.
# 
# Description:
# Calculates the feed-in from onshore wind turbines in the selected regions.
# 
# Usage:
# feedInWindOnshore()
# 
# Details:
# Calculates the feed-in from onshore wind turbines in the selected regions.
# 
# Value:
# Wind onshore feed-in per region in MW
# [matrix(timesteps x length(region_vector)), numeric(MW),]
# 
#-------------------------------------------------------------------------------

feedInWindOnshore <- function(){
  
  # Get selected wind_onshore_scenario from database 
  installed_wind_onshore <- readWeatherScenario("wind_onshore_scenario", 
                                                wind_onshore_scenario)
  
  installed_wind_onshore <- 
    installed_wind_onshore[installed_wind_onshore$installed_capacity!=0,]
  
  # Create subvector of regions with installed onshore capacities
  region_vector_onshore <- installed_wind_onshore$region_id
  
  # Get onshore weather stations in all selected regions from database
  sql_wind_raster <- paste("SELECT region_id, raster_id, height, roughness ",
                            "FROM wind_onshore_raster_register ",
                            "WHERE region_id IN (",
                            paste(region_vector_onshore, collapse=", "), 
                            ")", 
                            " AND category LIKE '1'",
                            sep="")
  
  con_weather     <- connectMysql("weather")
  raster_register <- dbGetQuery(con_weather, sql_wind_raster)
  dbDisconnect(con_weather)
  
  # Check if every region with installed onshore capacity has at least one 
  # assigned onshore weather station
  if(sum(! installed_wind_onshore$region_id %in% raster_register$region_id) !=
       0){
    idx_missing_timeseries <- which(! installed_wind_onshore$region_id %in% 
                                      raster_register$region_id)
    stop(paste("Region", 
               installed_wind_onshore$region_id[idx_missing_timeseries], 
               "has installed wind onshore capacity",
               "but no onshore weather station. "))
  }
  
  # Get windspeed_timeseries for necessary stations from database
  wind_speed <- readRasterWeatherTimeseries(table_name = "wind_onshore_raster", 
                                            column_name = "wind_speed", 
                                            raster_id = 
                                              paste(raster_register$raster_id,
                                            collapse=", "), 
                                            Weather_year = weather_year)
  
  # Add installed capacity per region_id and station_id, equally distributed
  # over stations in each region
  
  raster_register$installed_capacity <- NA
  
  for(ww in 1:length(region_vector_onshore)){
    idx_logical_station <- installed_wind_onshore$region_id[ww] == 
                        raster_register$region_id
    raster_register$installed_capacity[idx_logical_station] <- 
     installed_wind_onshore$installed_capacity[ww] / sum(idx_logical_station)
  }
  
  # get the power curve information from the database
  con_renpass <- connectMysql("renpass")
  power_curve <- dbGetQuery(con_renpass,
                            "SELECT hub_height, wind_speed, power_output
                             FROM wind_pp_parameter
                             WHERE power_curve_name 
                             LIKE 'Enercon_Vestas_REpower_3MW'")
  dbDisconnect(con_renpass)
  
  x           <- power_curve$wind_speed
  y           <- power_curve$power_output
  hub_height  <- mean(power_curve$hub_height)
  rated_power <- max(y)
  
  #-----------------------------------------------
  # Calculate power feed in per station and region
  #-----------------------------------------------
  wind_onshore_reg_pre <- list()
  for(stst in 1:length(raster_register[,1])){
    
    # Calculate wind speed at hub height
    # roughness for each station
    wind_speed_hub_height <- wind_speed$wind_speed[wind_speed$raster_id == 
                              raster_register$raster_id[stst]] *
                              log(hub_height / raster_register$roughness[stst])/
                              log(raster_register$height[stst] /
                                    raster_register$roughness[stst])
    
    # Calculate feed in with power curve
    wind_feed_in <- approx(x, y, xout=wind_speed_hub_height, 
                           method='linear', rule = 2)$y / rated_power *
                            raster_register$installed_capacity[stst]
    wind_onshore_reg_pre[[stst]] <- wind_feed_in
    
  }
  names(wind_onshore_reg_pre) <- raster_register$region_id
  
  #-----------------------------------
  # Sum feed in of stations per region
  #-----------------------------------
  wind_onshore_reg_inst <- as.data.frame(
                             matrix(NA, nrow = end_hour - start_hour + 1, 
                                    ncol = length(region_vector_onshore)))
  z <- 1
  for(rere in region_vector_onshore){
   wind_onshore_reg_inst[,z] <- rowSums(as.data.frame(
                                  wind_onshore_reg_pre[
                                   names(wind_onshore_reg_pre) == rere]))
   z <- z+1
  }
  colnames(wind_onshore_reg_inst) <- region_vector_onshore
  
  # Create zero timeseries for regions without installed onshore
  missing <- as.data.frame(matrix(0, 
                                  nrow = end_hour - start_hour + 1, 
                                  ncol = length(region_vector)-
                                         length(region_vector_onshore)))
  colnames(missing) <- region_vector[!region_vector %in% region_vector_onshore]
  
  # Merge onshore and zero timeseries
  wind_onshore_reg <- cbind(wind_onshore_reg_inst, missing)
  
  # Order data.frame by region_vector
  wind_onshore_reg <- wind_onshore_reg[paste(region_vector)]
  
  if(time_unit == "quarter"){
    wind_onshore_reg <- sapply(wind_onshore_reg, hoursToQuarters)
  }
  
  # Change data.frame into matrix
  wind_onshore_reg <- as.matrix(wind_onshore_reg)
  
  # Combine regions into dprs (dispatch regions)
  wind_onshore_reg <- convertRegionMatrixToDpr(wind_onshore_reg)
  
  # correction factor
  wind_onshore_reg <- wind_onshore_reg * 0.80 
  
  return(wind_onshore_reg)
}

wind_onshore_reg <- feedInWindOnshore()
