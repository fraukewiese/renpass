# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# code_R_plot_merit_order

# aim of this code is to plot the merit order of one hour. It can be used for
# one hour of a simulation including geo fossil biomass hydro other storage
# merit order or for the set up of the thermal_pp_scenario. for each region one
# merit order and the sum of all merit orders of all regions

#---------------------------
# merit order plot function
#---------------------------


plotMeritOrder <- function(merit_order  = merit_order,
                           start_or_end){
  # set rims, so that there is space for a legend underneath the plot
  
  for(i in seq(length(merit_order))){
    
    mo     <- merit_order[[i]]
    if(is.null(merit_order[[i]])){break}
    mo_dpr <- names(merit_order[i])  
    
    png(height = 1000,
        width  = 1000,
        res    = 160,
        file   = paste(folder_path,
                       "/merit_order_",
                       start_or_end,
                       "_scenario_nr",
                       scenario_nr,
                       "_dpr",
                       mo_dpr,
                       ".png",
                       sep = ""))
    par(mar = c(c(5,4,1,2) + 0.1),
        oma = c(6,0,0,0),
        xpd = NA)
    
    # par(mar=c(9,12,8,1), mgp=c(7,2.5,0))
    barplot(height = c(mo$marginal_cost),
            width  = c(mo$available_capacity)/1000,
            space  = c(0),
            border = NA, 
            xlab   = "Capacity in GW", 
            ylab   = "Marginal Cost in €/MWh ",
            col    = c(as.character(source_color$color)
                       [c(match(mo$fuel,source_color$source))]),
            ylim   = c(-0.1, max(mo$marginal_cost)*1.1))
    
    axis(side = 1,
         at   = NULL)
    
    number_of_fuels <- length(unique(mo$fuel))
    legend_columns  <- c(1,2,3,4,5,3,4,4,5,5,5,4,5,5,5,5,5,5,5,5)
    
    legend("bottom",
           as.character(unique(mo$fuel)),
           fill = c(as.character(source_color$color))[
             match(as.character(unique(mo$fuel)),
                   as.character(source_color$source))],
           ncol = legend_columns[number_of_fuels],
           cex = 0.9,
           inset = c(0,-0.5))
    
    dev.off()
  }
  
  #------------------------------------------------
  # all dispatch regions power plant parc together
  #------------------------------------------------
  mo_all <- do.call("rbind", merit_order)
  
  mo_all <- mo_all[order(mo_all$marginal_cost),]
  
  png(height = 1000,
      width  = 1000,
      res    = 160,
      file   = paste(folder_path,
                     "/merit_order_",
                     start_or_end,
                     "_scenario_nr",
                     scenario_nr,
                     "_all.png",
                     sep = ""))
  par(mar = c(c(5,4,1,2) + 0.1),
      oma = c(6,0,0,0),
      xpd = NA)
  
  barplot(height = c(mo_all$marginal_cost),
          width  = c(mo_all$available_capacity)/1000,
          space  = c(0),
          border = NA, 
          xlab   = "Capacity in GW", 
          ylab   = "Marginal Cost in €/MWh ",
          col    = c(as.character(source_color$color)
                     [c(match(mo_all$fuel,source_color$source))]),
          ylim   = c(-0.1, max(mo_all$marginal_cost)*1.1))
  
  axis(side = 1,
       at   = NULL)
  
  number_of_fuels <- length(unique(mo_all$fuel))
  legend_columns  <- c(1,2,3,4,5,3,4,4,5,5,5,4,5,5,5,5,5,5,5,5)
  
  legend("bottom",
         as.character(unique(mo_all$fuel)),
         fill = c(as.character(source_color$color))[
           match(as.character(unique(mo_all$fuel)),
                 as.character(source_color$source))],
         ncol = legend_columns[number_of_fuels],
         cex = 0.9,
         inset = c(0,-0.5))
  
  dev.off()
  
}

# if there is already a merit order, the merit order at the end of the timeloop
# is plotted, since it is at the end of the simulation
if("merit_order" %in% ls()){
  plotMeritOrder(merit_order = merit_order,
                 start_or_end = "end")
  remove(merit_order)
}

# then the merit order at the beginning of the simulation is plotted. This is
# done no matter if before something is calculated or left in the workspace,thus
# some preparations have to be done
source('renpass/code_R_renpass/code_R_geothermal.R')
source('renpass/code_R_renpass/code_R_prepare_biomass.R')
source('renpass/code_R_renpass/code_R_merit_order.R')

# hydro preparation
source('renpass/code_R_renpass/code_R_hydro_storage_scenario.R')
source('renpass/code_R_renpass/code_R_other_storage_scenario.R')
source('renpass/code_R_renpass/code_R_prepare_storage_plants.R')
source('renpass/code_R_renpass/code_R_prepare_reservoirs.R')
source('renpass/code_R_renpass/code_R_prepare_storage_connection.R')

# hydro and biomass and other storage for the first timestep
tt <- 1
source('renpass/code_R_renpass/code_R_hydro_capacity.R', local = TRUE)
source('renpass/code_R_renpass/code_R_storage_prices.R', local = TRUE)
source('renpass/code_R_renpass/code_R_hydro_merit_order.R', local = TRUE)
if(nrow(other_storage_data) > 0) {
  source('renpass/code_R_renpass/code_R_filling_level_other.R', local = TRUE)
}

# bio merit order
source('renpass/code_R_renpass/code_R_biomass_merit_order.R', local = TRUE)
source('renpass/code_R_renpass/code_R_merge_merit_orders.R', local = TRUE)

plotMeritOrder(merit_order = merit_order,
               start_or_end = "start")
