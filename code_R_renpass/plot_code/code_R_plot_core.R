# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# code_R_plot_core

# if the parameters are not set (this is the case if code_R_plot_core is run
# independently of scenario run) this is done in this function
if("path" %in% ls() == FALSE){
  scenario_nr          <- c(1)
  plot                 <- "no_spatial"
  path                 <- "/home/frauke/Documents"
  system_software      <- "linux"
  database_username    <- "root"
  database_password    <- "user123"
  database_host        <- "127.0.0.1"
  database_port        <- 3306
  database_unix.socket <- NULL
  
  # special stuff for macs
  if(system_software == "mac"){
    database_host        <- "localhost"
    database_port        <- 8888 # could also be 8889, you have to check
    database_unix.socket <- "/Applications/MAMP/tmp/mysql/mysql.sock"
  }
  
  ss        <- scenario_nr
  setwd(path)
  require(RMySQL)
  
  functions <- dir('renpass/code_R_renpass/functions')
  for(ff in functions){
    source(paste("renpass/code_R_renpass/functions/",ff,sep = ""))
  }
  source('renpass/code_R_renpass/code_R_prepare_parameters.R')
  source('renpass/code_R_renpass/code_R_prepare_regions.R')
}

#----------------------------------------------------------------
# make a folder of the scenario calculated including a timestamp
#----------------------------------------------------------------
folder_path <- paste(path, 
                     "/renpass/plot/plot_results/plot_",
                     scenario_nr, 
                     "_",
                     Sys.time(),
                     sep = "")

# special for windows, Sys.Date instead of Sys.time because in windows no : are
# allowed for the folder name
if(system_software == "windows"){
  folder_path <- paste(path, 
                       "/renpass/plot/plot_results/plot_",
                       scenario_nr, 
                       "_",
                       Sys.Date(),
                       sep = "")
}
dir.create(folder_path)

#---------------------------------
# defining colors for the sources
#---------------------------------
# define colours for the sources and shapes for the types
source_color <- data.frame(source = c("wind_onshore", 
                                      "wind_offshore",
                                      "solar", 
                                      "biomass", 
                                      "runofriver", 
                                      "geothermal",  
                                      "hydro", 
                                      "lignite",
                                      "hard_coal", 
                                      "gas", 
                                      "uran", 
                                      "refuse", 
                                      "oil", 
                                      "other_storage"),
                           color = c('lightblue2', 
                                     'steelblue2', 
                                     'yellow', 
                                     'forestgreen', 
                                     'blue', 
                                     'orange', 
                                     'darkblue', 
                                     'chocolate',
                                     'black', 
                                     'red', 
                                     'purple', 
                                     'grey', 
                                     'hotpink',
                                     'springgreen'))

#-------------------------------------------------------
# plots that need extra R-packages for spatial plotting
#-------------------------------------------------------
if(plot != "no_spatial"){
  # folders needed:
  # - renpass/plot/geodata/region_ready
  # required packages
  require(maptools)
  require(lattice)
  require(colorRamps)
  
  source("renpass/code_R_renpass/plot_code/code_R_plot_area_and_dpr.R")
  source("renpass/code_R_renpass/plot_code/code_R_plot_coverage.R")
  source("renpass/code_R_renpass/plot_code/code_R_plot_prices.R")
  
  # for plotting the grid, preparer grid is required
  # source('renpass/code_R_renpass/code_R_prepare_grid.R')
  # source("renpass/code_R_renpass/plot_code/code_R_plot_grid.R")
}

#----------------------------------------------------
# merit order plots at beginning and end of timeloop
#----------------------------------------------------
source("renpass/code_R_renpass/plot_code/code_R_plot_merit_order.R")

#------------------------------------------------
# full load hours per type per region and for whole area
#---------------------------------------------
source("renpass/code_R_renpass/plot_code/code_R_plot_full_load_hours.R")

#--------------------------------------------------------
# read results from the database for the balance picture
#--------------------------------------------------------
# demand + grid_loss + storage_consumption + excess_vre_after_storage
# = electricity_production + over_demand
demand <- readResult(result_table = "demand",
                     scenario_nr  = scenario_nr,
                     order_by     = c("timestep","dpr_number"))
demand <- tapply(demand$demand,
                 demand$timestep,
                 sum) / 1000

storage_consumption <- readResult(result_table = "storage_consumption",
                                  scenario_nr  = scenario_nr,
                                  order_by     = c("timestep",
                                                   "dpr_number",
                                                   "type"))
storage_consumption <- tapply(storage_consumption$storage_consumption,
                              storage_consumption$timestep,
                              sum) / 1000

grid_loss <- readResult(result_table = "exchange_after_storage",
                        scenario_nr  = scenario_nr,
                        order_by     = c("timestep",
                                         "plus_dpr_number",
                                         "minus_dpr_number"))
grid_loss <- tapply(grid_loss$grid_loss_abs,
                    grid_loss$timestep,
                    sum) /1000

excess_vre <- readResult(result_table = "excess_vre_after_storage",
                         scenario_nr  = scenario_nr,
                         order_by     = "timestep")
excess_vre <- tapply(excess_vre$excess_vre,
                     excess_vre$timestep,
                     sum) / 1000

electricity_production <- readResult(result_table = "electricity_production",
                                     scenario_nr  = scenario_nr,
                                     order_by     = c("timestep",
                                                      "dpr_number",
                                                      "type"))
electricity_production <- tapply(electricity_production$electricity_production,
                                 electricity_production$timestep,
                                 sum) / 1000

over_demand <- readResult(result_table = "over_demand",
                          scenario_nr  = scenario_nr,
                          order_by     = c("timestep",
                                           "dpr_number"))
over_demand <- tapply(over_demand$over_demand,
                      over_demand$timestep,
                      sum) / 1000

source("renpass/code_R_renpass/plot_code/code_R_plot_balance.R")
