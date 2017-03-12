# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# required packags: RMySQL
# applied renapss functions: connectMysql, hoursToQuarters
#-----
# Description  
# Input        weather_year, time_unit
# Output       reservoir, flo, flo_de
#-----

# connection to database
con_renpass <- connectMysql("renpass")
# ----------------------
# Reservoirs
res   <- dbGetQuery (con_renpass, "SELECT res_nr, fil_max , total_efactor,
                                   region_id 
                                   FROM reservoirs 
                                   ORDER BY res_nr")

id                      <- c(as.numeric(as.character(res$res_nr)))
fil_max                 <- c(as.numeric(as.character(res$fil_max)))
total_efactor           <- c(as.numeric(as.character(res$total_efactor)))
region                  <- c(as.numeric(as.character(res$region_id)))

reservoir               <- data.frame(cbind(id, fil_max, total_efactor, region))
colnames(reservoir)     <- c("id", "fil_max", "total_efactor", "region")

# ----------------------
# Filling levels

# Norm filling level data 

storage_norm   <- dbGetQuery(con_renpass, "SELECT fil_norm
                                           FROM storage_fil_norm 
                                           ORDER BY hour")                                      
fil_norm       <- as.numeric(as.character(unlist(storage_norm)))

# ----------------------
# Regulated inflow to the reservoirs
res_flo             <- dbGetQuery (con_renpass, "SELECT res_nr, flo 
                                                 FROM reservoirs 
                                                 ORDER BY res_nr")

# The choice of weather year determins the year for flow data
if (weather_year == 2010){flo_year <- 1969 }
if (weather_year == 1998){flo_year <- 1990 }
if (weather_year == 2003){flo_year <- 1962 } 

# The inflow characteristic for all reservoirs in Scnadinavia
sql_flo             <- paste("SELECT flo 
                              FROM storage_flow WHERE year = '",flo_year,"' 
                              ORDER BY hour", sep="")
flo                 <- dbGetQuery (con_renpass, sql_flo)
flo                 <- as.numeric(as.character(unlist(flo)))

# The inflow characteristic for all reservoirs in Germany
inflow_de           <- dbGetQuery(con_renpass, "SELECT flo 
                                                FROM storage_flow 
                                                WHERE level_meter = 30 
                                                ORDER BY hour")

inflow_de           <- as.numeric(as.character(unlist(inflow_de)))
flo_de              <- inflow_de*sum(flo)/sum(inflow_de) 

if(time_unit == "quarter"){
  flo    <- hoursToQuarters(flo) * 0.25
  flo_de <- hoursToQuarters(flo_de) * 0.25
}

dbDisconnect(con_renpass)
