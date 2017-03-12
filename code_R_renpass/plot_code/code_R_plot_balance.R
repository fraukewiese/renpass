# This code file is part of renpass published under the GNU GPL 3 license.
# See also: code_R_start_renpass.R and http://opensource.org/licenses/GPL-3.0
#-----
# code_R_plot_balance

# 4 pictures: 
# GW per timestep for all regions, 
# minus side: demand, storage_consumption, excess_vre and over_demand
# plus side : electricity_production, over_demand
# TWh per week for all regions,
# minus side: demand, storage_consumption, excess_vre and over_demand
# plus side : electricity_production, over_demand
# each with a red line of the respective other side

#-----------------------------------
# preparation of the six components
#-----------------------------------
d    <- demand
sd   <- demand + storage_consumption
gsd  <- grid_loss + sd
egsd <- excess_vre + gsd

p    <- electricity_production
op   <- over_demand + p

ylim <- c(0,max(egsd,op))

#y_axis <- getTimevector(time_unit = "hour",
#                       scenario_year = scenario_year)

#---------------
# EACH TIMESTEP
#---------------
xx_base      <- c(start_timestep:end_timestep)
#timevector   <- getTimevector(time_unit     = time_unit,
#                             scenario_year = scenario_year)
ylim         <- c(0, max(egsd,op))
#par_standard <- par(no.readonly = TRUE)

#------------------
## TIMESTEP MINUS ##
#------------------
# set rims, so that there is space for a legend underneath the plot
png(file   = paste(folder_path,
                   "/balance_timestep_minus_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))

par(mar = c(c(5,4,1,2) + 0.1),
    oma = c(0,0,0,6),
    xpd = NA)

# define the plotting region and the axis
plot.new()
plot.window(xlim = range(xx_base),
            ylim = ylim)
axis(1)
axis(2,las=1)
title(ylab = "GW")
title(xlab = paste(time_unit,"s of ", scenario_year, sep =""))
# box()

# plot one polygon after the other
polygon(c(xx_base[1],xx_base,xx_base[length(xx_base)]),
        c(0,d,0),
        col = "darkblue")
polygon(c(xx_base[1],xx_base,rev(xx_base)),
        c(d[1],(sd),rev(d)),
        col = "mediumblue")
polygon(c(xx_base[1],xx_base,rev(xx_base)),
        c(sd[1],(gsd),rev(sd)),
        col = "cornflowerblue")
polygon(c(xx_base[1],xx_base,rev(xx_base)),
        c(gsd[1],(egsd),rev(gsd)),
        col = "turquoise2")
lines(xx_base,
      op,
      col = "red",
      lwd = 2.5)
legend("right",
       c("Excess \nElectricity",
         "Grid Loss",
         "Storage \nConsumption",
         "Demand",
         "Balance Sum +"),
       col = c("turquoise2","cornflowerblue","mediumblue","darkblue","red"),
       lwd  = c(8,8,8,8,2.5),
       ncol = 1,
       cex = 0.9,
       inset = c(-0.35,0))

dev.off()

#-----------------
## TIMESTEP PLUS ##
#-----------------
png(file   = paste(folder_path,
                   "/balance_timestep_plus_scenario_nr",
                   scenario_nr,
                   ".png",
                   sep = ""))

par(mar = c(c(5,4,1,2) + 0.1),
    oma = c(0,0,0,6),
    xpd = NA)

# define the plotting region and the axis
plot.new()
plot.window(xlim = range(xx_base),
            ylim = ylim)
axis(1)
axis(2,las=1)
title(ylab = "GW")
title(xlab = paste(time_unit, "s of ", scenario_year, sep =""))
# box()

# plot one polygon after the other
polygon(c(xx_base[1],xx_base,xx_base[length(xx_base)]),
        c(0,p,0),
        col = "darkgreen")
polygon(c(xx_base[1],xx_base,rev(xx_base)),
        c(p[1],op,rev(p)),
        col = "limegreen")
lines(xx_base,
      egsd,
      col = "red")

legend("right",
       c("Excess \nDemand",
         "Electricty \nProduction",
         "Balance Sum -"),
       col  = c("limegreen","darkgreen","red"),
       lwd  = c(8,8,2.5),
       ncol = 1,
       cex = 0.9,
       inset = c(-0.35,0))

dev.off()

################################################################################

# only if at least two weeks are calculated:
if(timesteps*energy_factor >= 336){
  
  #--------------
  # WEEKLY MEANS
  #--------------
  idx_week    <- seq(start_timestep,end_timestep,(168/energy_factor))
  xx_base     <- round(idx_week/(168/energy_factor) +1)
  week_factor <- rep(xx_base[c(1:(length(xx_base)-1))],
                     each = 168/energy_factor)
  week_factor <- c(week_factor, 
                   rep(xx_base[length(xx_base)],
                       (timesteps - length(week_factor))))
  
  d_w <- tapply(d,week_factor,sum)/1000
  sd_w <- tapply(sd,week_factor,sum)/1000
  gsd_w <- tapply(gsd,week_factor,sum)/1000
  egsd_w <- tapply(egsd,week_factor,sum)/1000
  
  p_w <- tapply(p,week_factor,sum)/1000
  op_w <- tapply(op,week_factor,sum)/1000
  
  ylim <- c(0, max(egsd_w,op_w))
  
  #------------------
  ## WEEKLY MINUS ##
  #------------------
  # set rims, so that there is space for a legend underneath the plot
  png(file   = paste(folder_path,
                     "/balance_weekly_minus_scenario_nr",
                     scenario_nr,
                     ".png",
                     sep = ""))
  
  par(mar = c(c(5,4,1,2) + 0.1),
      oma = c(0,0,0,6),
      xpd = NA)
  
  # define the plotting region and the axis
  plot.new()
  plot.window(xlim = range(xx_base),
              ylim = ylim)
  axis(1)
  axis(2,las=1)
  title(ylab = "TWh per week")
  title(xlab = paste("weeks of ", scenario_year, sep =""))
  # box()
  
  # plot one polygon after the other
  polygon(c(xx_base[1],xx_base,xx_base[length(xx_base)]),
          c(0,d_w,0),
          col = "darkblue")
  polygon(c(xx_base[1],xx_base,rev(xx_base)),
          c(d_w[1],(sd_w),rev(d_w)),
          col = "mediumblue")
  polygon(c(xx_base[1],xx_base,rev(xx_base)),
          c(sd_w[1],(gsd_w),rev(sd_w)),
          col = "cornflowerblue")
  polygon(c(xx_base[1],xx_base,rev(xx_base)),
          c(gsd_w[1],(egsd_w),rev(gsd_w)),
          col = "turquoise2")
  lines(xx_base,
        op_w,
        col = "red",
        lwd = 2.5)
  legend("right",
         c("Excess \nElectricity",
           "Grid Loss",
           "Storage \nConsumption",
           "Demand",
           "Balance Sum +"),
         col = c("turquoise2","cornflowerblue","mediumblue","darkblue","red"),
         lwd  = c(8,8,8,8,2.5),
         ncol = 1,
         cex = 0.9,
         inset = c(-0.35,0))
  
  dev.off()
  
  #-----------------
  ## WEEKLY PLUS ##
  #-----------------
  png(width = 800,
      height = 800,
      file   = paste(folder_path,
                     "/balance_weekly_plus_scenario_nr",
                     scenario_nr,
                     ".png",
                     sep = ""))
  
  par(mar = c(c(5,4,1,2) + 0.1),
      oma = c(0,0,0,6),
      xpd = NA)
  
  # define the plotting region and the axis
  plot.new()
  plot.window(xlim = range(xx_base),
              ylim = ylim)
  axis(1)
  axis(2,las=1)
  title(ylab = "TWh per week")
  title(xlab = paste("weeks of ", scenario_year, sep =""))
  # box()
  
  # plot one polygon after the other
  polygon(c(xx_base[1],xx_base,xx_base[length(xx_base)]),
          c(0,p_w,0),
          col = "darkgreen")
  polygon(c(xx_base[1],xx_base,rev(xx_base)),
          c(p_w[1],op_w,rev(p_w)),
          col = "limegreen")
  lines(xx_base,
        egsd_w,
        col = "red")
  
  legend("right",
         c("Excess \nDemand",
           "Electricty \nProduction",
           "Balance Sum -"),
         col  = c("limegreen","darkgreen","red"),
         lwd  = c(8,8,2.5),
         ncol = 1,
         cex = 0.9,
         inset = c(-0.35,0))
  
  dev.off()
  
}
