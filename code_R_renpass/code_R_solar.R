# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required package: RMySQL
# applied renpass functions: connectMysql, readWeatherScenario,
#                            readRasterWeatherTimeseries,
#                            convertRegionMatrixToDpr
# functions defined within this piece of code: feedInSolar
#-----
# Name:
# feedInSolar.
# 
# Title:
# feedInSolar.
# 
# Description:
# Calculates the feed-in from solar (photovoltaik) in the selected regions.
# 
# Usage:
# feedInSolar()
# 
# Details:
# Calculates the feed-in from solar (photovoltaik) in the selected regions.
# 
# Value:
# Returns the solar feed-in per region in MW. 
# [matrix(timesteps x length(region_vector)),  numeric(MW),]
# 
#-------------------------------------------------------------------------------

feedInSolar <- function(){
  
  # Get selected solar_scenario from database 
  installed_solar <- readWeatherScenario(table_name = "solar_scenario",
                                         scenario   = solar_scenario)
  
  installed_solar <- 
    installed_solar[installed_solar$installed_capacity!=0,]
  
  # Create subvector of regions with installed solar capacities
  region_vector_solar <- installed_solar$region_id
  
  # Get solar weather rasterpoints in all selected regions from database
  sql_solar_raster <- paste("SELECT region_id, raster_id ", 
                            "FROM solar_raster_register ",
                            "WHERE region_id IN (",
                            paste(region_vector_solar, collapse=", "), 
                            ")",
                            " AND category LIKE '1'",
                            sep="")
  
  con_weather     <- connectMysql("weather")
  raster_register <- dbGetQuery(con_weather, sql_solar_raster)
  dbDisconnect(con_weather)
  
  # Check if every region with installed solar capacity has at least one 
  # assigned solar weather rasterpoint
  if(sum(! installed_solar$region_id %in% raster_register$region_id) !=
       0){
    idx_missing_timeseries <- which(! installed_solar$region_id %in% 
                                      raster_register$region_id)
    stop(paste("Region", 
               installed_solar$region_id[idx_missing_timeseries], 
               "has installed solar capacity",
               "but no solar weather raster.\n"))
  }
  
  # Get solar diffus and direct radiation for necessary raster from database
  solar_diffus <- readRasterWeatherTimeseries(table_name = "solar_diffus", 
                                              column_name = "solar_radiation", 
                                              raster_id   = 
                                                paste(raster_register$raster_id,
                                                  collapse=", "), 
                                              Weather_year= weather_year)
  
  solar_direct <- readRasterWeatherTimeseries(table_name = "solar_direct", 
                                              column_name = "solar_radiation", 
                                              raster_id   = 
                                                paste(raster_register$raster_id,
                                                      collapse=", "), 
                                              Weather_year= weather_year)
  
  # Add up diffus and direct radiation, at this point it could be treated extra
  # for now we just take the sum as global radiation and later just take 70%
  
  solar_radiation                 <- solar_diffus
  solar_radiation$solar_radiation <- solar_direct$solar_radiation +
                                     solar_diffus$solar_radiation
  
  # Add installed capacity per region_id and raster_id, equally distributed
  # over rasterpoints in each region
  raster_register$installed_capacity <- NA
  for(ww in 1:length(region_vector_solar)){
    idx_logical_raster <- installed_solar$region_id[ww] == 
      raster_register$region_id
    raster_register$installed_capacity[idx_logical_raster] <- 
      installed_solar$installed_capacity[ww] / sum(idx_logical_raster)
  }
  
  
  #-----------------------------------------------
  # Calculate power feed in per rasterpoint and region
  #-----------------------------------------------
  solar_reg_pre <- list()
  for(stst in 1:length(raster_register[,1])){
    
    # Get solar_radiation in Wh/m² for one rasterpoint
    solar_radiation_raster <- 
      solar_radiation$solar_radiation[solar_radiation$raster_id == 
                                                raster_register$raster_id[stst]]
    
    # Calculate feed in with installed capacity in MW and a correction of 0.9
    solar_feed_in <- solar_radiation_raster * 
                     raster_register$installed_capacity[stst] / 1000 * 0.9
    solar_reg_pre[[stst]] <- solar_feed_in
    
  }
  names(solar_reg_pre) <- raster_register$region_id
  
  #----------------------------------------
  # Sum feed in of rasterpoints per region
  #----------------------------------------
  solar_reg_inst <- as.data.frame(
    matrix(NA, nrow = end_hour - start_hour + 1, 
           ncol = length(region_vector_solar)))
  z <- 1
  for(rere in region_vector_solar){
    solar_reg_inst[,z] <- rowSums(as.data.frame(
      solar_reg_pre[
        names(solar_reg_pre) == rere]))
    z <- z+1
  }
  colnames(solar_reg_inst) <- region_vector_solar
  
  # Create zero timeseries for regions without installed solar
  missing <- as.data.frame(matrix(0, nrow = end_hour - start_hour + 1, 
                                     ncol = length(region_vector)-
                                            length(region_vector_solar)))
  colnames(missing) <- region_vector[!region_vector %in% region_vector_solar]
  
  # Merge solar and zero timeseries
  solar_reg <- cbind(solar_reg_inst, missing)
  
  # Order data.frame by region_vector
  solar_reg <- solar_reg[paste(region_vector)]
  
  if(time_unit == "quarter"){
    solar_reg <- sapply(solar_reg, hoursToQuarters)
  }
  
  # Change data.frame into matrix
  solar_reg <- as.matrix(solar_reg)
  
  # Combine regions into dprs (dispatch regions)
  solar_reg <- convertRegionMatrixToDpr(solar_reg)
  
  return(solar_reg)
}

solar_reg <- feedInSolar()
