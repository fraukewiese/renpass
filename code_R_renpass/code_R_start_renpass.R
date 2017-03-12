# renpass Copyleft 2014 Frauke Wiese and Gesine Bökenkamp.
# Contributions by Clemens Wingenbach, Simon Hilpert, Marian Bons and others
# Developed at the Center for Sustainable Energy Systems Flensburg

# renpass (Renewable Energy Pathways Simulation System) is a simulation energy
# model.
# renpass is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, version 3 of the License, or any later version. 
# renpass is distributed in the hope that it will be useful, but without any 
# warranty; without even the implied warranty of merchantability or fitness for
# a particular purpose.

# More details on the terms of and the complete GNU GPL 3 license can be fund on
# http://opensource.org/licenses/GPL-3.0

#----------------------------------
# START OF THE renpass CALCULATION
#----------------------------------

# clear workspace
remove(list = ls())

#-----------------------------------
# Settings for the renpass function
#-----------------------------------

# state scenario number or a vector of numbers to be calculated
scenario_nr_vector <- c(1)

# which plots should be generated "all","none" or "no_spatial"
plot               <- c("all")

# location of the folder renpass on your computer
path               <- "/home/frauke/Documents"

# system software of your computer: "linux", "windows" or "mac
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
# special for windows
if(system_software == "windows"){
  memory.limit(size = 4095)
}

# package for the R-database connection
require(RMySQL)
  
# Close all open MySQL-connections
lapply(dbListConnections(MySQL()), dbDisconnect)

# the path where all code files are located is set
setwd(path)

# all other pieces of code are sourced from the following code file:
source('renpass/code_R_renpass/code_R_renpass_core.R')
