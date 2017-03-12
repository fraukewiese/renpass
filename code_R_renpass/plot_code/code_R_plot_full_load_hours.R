# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# code_R_plot_full_load_hours

# erweitern um usage rate und flh der pumpen
# Die Turbinen und Pumpleistung sind in dataframes die turb und pump heißen, 
# jeweils in der Spalte pinst. Dazu muss code_R_hydro_storage_scenario.R und 
# code_R_prepare_storage_plants.R laufen.

#-----------------------------------------------
# calculate full load hours per type 
#-----------------------------------------------

source_color$Pinst <- 0

#------------------------------------------
# installed capacities variable renewables 
#------------------------------------------
for(ff in c("wind_onshore","wind_offshore","solar")){
  data <- readWeatherScenario(paste(ff,"_scenario",sep = ""),
                              eval(parse(text = paste(ff,"_scenario",sep = ""))))
  source_color$Pinst[source_color$source == ff] <- 
    sum(data$installed_capacity[data$region_id %in% region_dpr_assignment$region_id])
}

#---------------------------------------------------
# installed capacities other renewables and storage 
#---------------------------------------------------
for(ff in c("runofriver","biomass","geothermal","other_storage")){
  data <- readPathway(ff,
                      eval(parse(text = paste(ff,"_scenario",sep = ""))),
                      "region_id")
  source_color$Pinst[source_color$source == ff] <- 
    sum(data$installed_capacity[data$region_id %in% region_dpr_assignment$region_id])
}

#-------
# hydro
#-------
hy <- do.call("rbind", hydro_merit_order)
source_color$Pinst[source_color$source == "hydro"] <- sum(hy$available_capacity)

#---------------------------------
# installed capacities of fossils
#---------------------------------
source('renpass/code_R_renpass/code_R_geothermal.R')
source('renpass/code_R_renpass/code_R_merit_order.R')

if(is.null(merit_order_fossil) == FALSE){
  mo <- do.call("rbind", merit_order_fossil)
  mo_fuel <- tapply(mo$available_capacity,
                    mo$fuel,
                    sum)
  for(ff in rownames(mo_fuel)){
    source_color$Pinst[source_color$source == ff] <- mo_fuel[ff]
  }
}

source_color$MWh <- 0
electricity_production <- readResult(result_table = "electricity_production",
                                     scenario_nr  = scenario_nr,
                                     order_by     = c("timestep",
                                                      "dpr_number",
                                                      "type"))

# if a whole year is calculated, flh are on the y-axis, otherwise %
if(timesteps*energy_factor == 8760){
  
  MWh <- tapply(electricity_production$electricity_production,
                electricity_production$type,
                sum) * energy_factor
  
  source_color$MWh[match(names(MWh),
                         source_color$source,
                         nomatch = 0)] <- MWh
  source_color$flh <- source_color$MWh / source_color$Pinst
  
  idx <- which(is.na(source_color$flh) == FALSE)
  
  png(width  = 1000,
      height = 1000,
      res    = 170,
      file   = paste(folder_path,
                     "/full_load_hours_scenario_nr",
                     scenario_nr,
                     ".png",
                     sep = ""))
  
  a <- barplot(source_color$flh[idx],
               col       = as.character(source_color$color[idx]),
               names.arg = as.character(source_color$source[idx]),
               ylab      = "Full Load Hours of the Year",
               ylim      = c(0,8760),
               axisnames = FALSE,
               las       = 2)
  
  text(a,
       par("usr")[3], 
       labels = as.character(source_color$source[idx]),
       adj    = c(1,2),
       srt    = 45, 
       xpd    = TRUE)
  
  dev.off()
  
}else{ # if not a whole year is calculated flh do not make sense, thus %
  
  MWh_per_timestep <- tapply(electricity_production$electricity_production,
                             electricity_production$type,
                             sum) / timesteps
  
  source_color$MWh[match(names(MWh_per_timestep),
                         source_color$source,
                         nomatch = 0)] <- MWh_per_timestep
  
  source_color$usage <- source_color$MWh / source_color$Pinst * 100
  
  idx <- which(is.na(source_color$usage) == FALSE)
  
  png(width  = 1000,
      height = 1000,
      res    = 170,
      file   = paste(folder_path,
                     "/usage_rate_scenario_nr",
                     scenario_nr,
                     ".png",
                     sep = ""))
  
  a <- barplot(source_color$usage[idx],
               col       = as.character(source_color$color[idx]),
               names.arg = as.character(source_color$source[idx]),
               ylab      = "usage rate [%]",
               axisnames = FALSE,
               las       = 2,
               ylim      = c(0,100))
  
  text(a,
       par("usr")[3], 
       labels = as.character(source_color$source[idx]),
       adj    = c(1,2),
       srt    = 45, 
       xpd    = TRUE)
  
  dev.off()
}
