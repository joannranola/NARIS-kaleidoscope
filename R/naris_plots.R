#-------------------------------------------------------------------------------
# Copyright © 2021 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 

#----------------------------------------------------------------------------
#' Plot a series of NARIS generation frames
#'
#' @description Plot a series of NARIS 1920x1080 frames with a generation map
#'              for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_map_series<- function(t0, tn, prefix, scenario=Scenario1, density='None',
                          types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                          ...)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]

    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(map.dir,baseFilename), width=1920, height=1080, pointsize=24)
    par(bg='black', fg='white')
    draw_naris_map(t, scenario=scenario, types=types)
    dev.off()
  }
}


#----------------------------------------------------------------------------
#' Plot NARIS generation map
#'
#' @description Draw a 1920x1080 frame of NARIS with generation map 
#'              for a single scenario
#'
#' @param t timestep of interest
draw_naris_map <- function(t, scenario=Scenario1, density='None',
                        types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                        scaling=0.002, weight=3, ...)
{
  
  par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
  par(fig=c(0, 1, 0.05, 0.95))
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, ...)
  # draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-78, ly=30, draw_legend=T)#lx=-123, ly=35#lx=-72, ly=39.9#lx = as.numeric(quantile(naris_layout[,1])[1]-1),ly = as.numeric(quantile(naris_layout[,2])[2])
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-135, ly=47, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, scenario)
  draw_shadow(t)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  # mtext(paste('NARIS', ' (', scenario, ')', sep=''), font=2, cex=1.5)
  mtext(paste('NARIS', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%Y %H:%M EST"), 3, -1)
  
}


#----------------------------------------------------------------------------
#' Plot a series of NARIS interface chord-dispatch frames
#'
#' @description Plot a series of NARIS 1920x1080 frames with a chord diagram
#'              and a dispatch stack bar chart for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_chord_dispatch_series = function(t0, tn, max_interchange=0, scenario, prefix)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(chord_dispatch.dir,baseFilename), width=1920, height=1080, pointsize=24)
    draw_naris_chord_dispatch(t, max_interchange, scenario=scenario, weight=3)
    dev.off()
  }
}               


#----------------------------------------------------------------------------
#' Plot interface chord-dispatch view of NARIS data
#'
#' @description Draw a 1920x1080 frame of NARIS with chord diagram and
#'              a dispatch stack for a single scenario
#'
#' @param t starting timestep
#' @param max_interchange maximum interchange to scale the chord diagram
draw_naris_chord_dispatch = function(t, max_interchange=0, scenario, weight=3)
{
  index = naris_netinterchange$time==t & naris_netinterchange$scenario==scenario & naris_netinterchange$value < 0
  interchange = abs(sum(naris_netinterchange$value[index]))
  
  # Sanity check
  # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
  max_interchange = max(interchange, max_interchange)
  
  # print(format(t, "%m-%d-%Y %H:%M"))
  par(bg='black', fg='white')
  #par(bg='white', fg='black')
  par(fig=c(0, 1080/1920, 0, 1), mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-interchange)/max_interchange)
  draw_naris_chord_interchange(t, naris_iso, naris_netinterchange, scenario, weight)
  mtext("Net interchange", 1)
  
  par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_naris_bars(t, scenario, weight)
  mtext("Regional dispatch", 1, 5)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  # mtext(paste('NARIS', ' (', scenario, ')', sep=''), font=2, cex=1.5)
  mtext(paste('NARIS', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%Y %H:%M EST"), 3, -1)
  
}


#----------------------------------------------------------------------------
#' Plot a series of NARIS generation map-dispatch frames
#'
#' @description Plot a series of 1920x1080 frames of NARIS generation and
#'              a dispatch stack for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_map_dispatch1_series = function(t0, tn, max_interchange=0, scenario=Scenario1, 
                                   types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                   prefix)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(map_dispatch1.dir,baseFilename), width=1920, height=1080, pointsize=24)
    draw_naris_map_dispatch1(t, scenario=scenario, types=types)
    dev.off()
  }
}               


#----------------------------------------------------------------------------
#' Plot NARIS generation map-dispatch view of NARIS data
#'
#' @description Draw a 1920x1080 frame of NARIS with generator map and
#'              a dispatch stack for a single scenario
#'
#' @param t timestep of interest
draw_naris_map_dispatch1 <- function(t, scenario=Scenario1, density='None',
                       types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                       scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
  # par(fig=c(0, 1080/1920, 0, 1))
  par(fig=c(0, 1344/1920, 0, 1))
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, ...)
  # draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-78, ly=33, draw_legend=T)#lx=-123, ly=35#lx=-72, ly=39.9#lx = as.numeric(quantile(naris_layout[,1])[1]-1),ly = as.numeric(quantile(naris_layout[,2])[2])
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-135, ly=50, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, scenario=scenario)
  draw_shadow(t)
  mtext("Generation & Flow", 1, -2, at=-92)
  
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  par(fig=c(1344/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_naris_bars(t, scenario=scenario, weight)
  mtext("Regional dispatch", 1, 5)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  # mtext(paste('NARIS', ' (', scenario, ')', sep=''), font=2, cex=1.5)
  mtext(paste('NARIS', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%Y %H:%M EST"), 3, -1)
  
}


#----------------------------------------------------------------------------
#' Stacked bar plot of NARIS generation
#'
#' @description Plot a stacked bar graph of dispatch of each NARIS region for
#'              a given scenario
#'
#' @param t timestep
draw_naris_bars <- function(t, scenario, weight=3)
{
  index = naris_dispatch_stack$time==t & naris_dispatch_stack$scenario==scenario
  
  df = data.frame(zone=naris_dispatch_stack$zone[index],
                  type=naris_dispatch_stack$Type[index],
                  value=naris_dispatch_stack$value[index])
  
  types = data.table::data.table(type=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                      color=c("#b22222","#333333", "#add8e6","#6e8b3d","orchid4","#4f94cd", "#AAAAAA", "#4444dd", "#ffc125", "khaki1", "darkorange2","mediumpurple2","#E1E1E1"))

  s = tidyr::spread(df, zone, value, fill=0)
  isonumplus1 = length(naris_iso) + 1
  m = as.matrix(s[,2:isonumplus1])
  rownames(m) = s$type
  
  missing = types$type[!types$type %in% rownames(m)]
  for (i in missing) 
  {
    m = rbind(m, 0)
    rownames(m)[dim(m)[1]] = i
  }
  
  m=m[types$type,,drop=FALSE]
  m=m[,rev(naris_iso)]
  
  # plot
  types$type[13] = "VG Curtailment"
  par(lwd=0.5)
  b=barplot(m/1000, col=types$color, horiz=T, xlab='GW', xlim=c(0, 200), col.lab=par("fg"), col.axis=par("fg"))
  # legend(x=c(50,35), y=c(4.8,2.6), legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1.5, cex=0.7)
  legend(x="topright", legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1.5, cex=0.7)
  x = as.matrix(s[s$type=='Load',2:isonumplus1])
  x=x[,rev(naris_iso)]

  for (i in 1:length(x)) lines(rep(x[i]/1000,2), rep(b[i],2)+c(-0.5,0.5), type='l', lty=2, lwd=weight, col=par('fg'))
}


#----------------------------------------------------------------------------
#' Plot a series NARIS layout for scenario comparison 
#'
#' @description Plot a series of 5760x2400 frames comparing NARIS generation,
#'              interface chord, and dispatch for all four scenarios
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_comparative_insight_series = function(t0, tn, types, prefix, max_interchange=0)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    prefix = "NARIS"
    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(comparative_insight.dir,baseFilename), width=5760, height=2400, pointsize=52)
    draw_naris_comparative_insight(t, types=types)
    dev.off()
  }
}    


#----------------------------------------------------------------------------
#' Plot NARIS layout for scenario comparison 
#'
#' @description Plot a 5760x2400 frame comparing NARIS generation, interface chord,
#'              and dispatch for all four scenarios
#'
#' @param t timestep of interest
draw_naris_comparative_insight <- function(t,
                                         density='None',
                                         max_interchange=0,
                                         types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                         scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white')
  #par(bg='white', fg='black')
  par(fig=c(0, 2400/5760, 0, 1))

  par(fig=c(0, 1200/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, scenario=Scenario1, ...)
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=Scenario1, scaling=scaling, lx=-72, ly=16.5, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack,  Scenario1, arrow.scaling=4.0)
  draw_shadow(t)
  text(101, 9, Scenario1, cex=0.7)
  
  par(fig=c(1200/5760, 2400/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, scenario=Scenario2, ...)
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=Scenario2, scaling=scaling, lx=-72, ly=16.5, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, Scenario2, arrow.scaling=4.0)
  draw_shadow(t)
  text(101, 9, Scenario2, cex=0.7)
  
  par(fig=c(0, 1200/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, scenario=Scenario3, ...)
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=Scenario3, scaling=scaling, lx=-72, ly=16.5, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, Scenario3, arrow.scaling=4.0)
  draw_shadow(t)
  text(101, 9, Scenario3, cex=0.7)
  
  par(fig=c(1200/5760, 2400/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, scenario=Scenario4, ...)
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=Scenario4, scaling=scaling, lx=-72, ly=16.5, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, Scenario4, arrow.scaling=4.0)
  draw_shadow(t)
  text(101, 9, Scenario4, cex=0.7)
  
  par(cex=1)
  par(fig=c(0, 2400/5760, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow", 1)
  
  w = 1200/5760
  h = 0.5
  
  index = naris_netinterchange$time==t & naris_netinterchange$scenario==Scenario1 & naris_netinterchange$value < 0
  Scenario1_interchange = abs(sum(naris_netinterchange$value[index]))
  index = naris_netinterchange$time==t & naris_netinterchange$scenario==Scenario2 & naris_netinterchange$value < 0
  Scenario2_interchange = abs(sum(naris_netinterchange$value[index]))
  index = naris_netinterchange$time==t & naris_netinterchange$scenario==Scenario3 & naris_netinterchange$value < 0
  Scenario3_interchange = abs(sum(naris_netinterchange$value[index]))
  index = naris_netinterchange$time==t & naris_netinterchange$scenario==Scenario4 & naris_netinterchange$value < 0
  Scenario4_interchange = abs(sum(naris_netinterchange$value[index]))

  max_interchange = max(max_interchange, max(Scenario1_interchange, Scenario2_interchange, 
                                             Scenario3_interchange, Scenario4_interchange)) # per timestep normalization
  
  x0 = 2300/5760
  x1 = 3500/5760
  y0 = 0
  y1 = 0.49
  
  par(cex=0.65)
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-Scenario1_interchange)/max_interchange)
  draw_naris_chord_interchange(t, naris_iso, naris_netinterchange, Scenario1,link.size=2)
  text(-0.90,-0.90, Scenario1, cex=1)
  
  x0 = 3500/5760
  x1 = 4700/5760
  y0 = 0
  y1 = 0.49
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-Scenario2_interchange)/max_interchange)
  draw_naris_chord_interchange(t, naris_iso, naris_netinterchange, Scenario2,link.size=2)
  text(-0.90,-0.90, Scenario2, cex=1)
  
  x0 = 2300/5760
  x1 = 3500/5760
  y0 = 0.49
  y1 = 0.98
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-Scenario3_interchange)/max_interchange)
  draw_naris_chord_interchange(t, naris_iso, naris_netinterchange, Scenario3,link.size=2)
  text(-0.90,-0.90, Scenario3, cex=1)
  
  x0 = 3500/5760 
  x1 = 4700/5760 
  y0 = 0.49
  y1 = 0.98
  par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
  circlize::circos.clear()
  circlize::circos.par(gap.degree=1 + (348/12) * (max_interchange-Scenario4_interchange)/max_interchange)
  draw_naris_chord_interchange(t, naris_iso, naris_netinterchange, Scenario4,link.size=2)
  text(-0.90,-0.90, Scenario4, cex=1)
  
  par(cex=1)
  par(fig=c(2400/5760, 2400/5760 + 2*w, 0, 2*h), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Net interchange", 1)
  
  par(fig=c(5100/5760, 1, 0, 1), mar=c(5.1,0,2.1,2.1), oma=c(2,0,0,0), las=1, new=TRUE)
  draw_naris_comparative_bars(t, x0=5100/5760, weight=weight)
  mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext('NARIS', 3, -1.25, font=2, cex=1.25, outer=TRUE)
  mtext(format(t, "%m-%d-%Y %H:%M"), 3, -2.25, cex=1.25, outer=TRUE)
}


#----------------------------------------------------------------------------
#' Stacked bar plot of NARIS generation
#'
#' @description Plot a set stacked bar graphs of dispatch of each NARIS region for
#'              the NARIS scenarios
#'
#' @param t timestep 
draw_naris_comparative_bars <- function(t, x0=1080/1920, x1=1, weight=3)
{
  types = data.table::data.table(type=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                 color=c("#b22222","#333333", "#add8e6","#6e8b3d","orchid4","#4f94cd", "#AAAAAA", "#4444dd", "#ffc125", "khaki1", "darkorange2","mediumpurple2","#E1E1E1"))
  zone = rev(naris_iso)
  
  index = naris_dispatch_stack$time==t
  
  df = data.frame(zone=naris_dispatch_stack$zone[index],
                  type=naris_dispatch_stack$Type[index],
                  value=naris_dispatch_stack$value[index],
                  scenario=naris_dispatch_stack$scenario[index])
  
  s = spread(df, scenario, value, fill=0)
  
  # convert to GW
  s[,3:6] = s[,3:6]/1000

  for(i in length(zone):1)
  {
    m=as.matrix(s[s$zone==zone[i],3:6])
    rownames(m) = s$type[s$zone==zone[i]]
    
    missing = types$type[!types$type %in% rownames(m)]
    for (j in missing) 
    {
      m = rbind(m, 0)
      rownames(m)[dim(m)[1]] = j
    }
    
    m=m[types$type,,drop=FALSE]

    par(mar=c(0.1,0.1,0.1,0.1), oma=c(6,0,1,1), fig=c(x0, x1, (i-1)/6, i/6), lwd=0.5, new=TRUE)

    if (i==1)
      b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,50), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    else
      b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,50), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    
    y = rep(b,each=2)+c(-0.5,0.5)
    x = rep(as.matrix(s[s$type=='Load' & s$zone==zone[i],3:6]), each=2)
    
    lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
    
    mtext(zone[i], 2, line=4, las=1)
  }
  mtext("GW", 1, 2.5)
  
  par(fig=c(x0,x1,0,1), mar=c(0.1,0.1,0.1,0.1), new=TRUE)
  legend(x=c(50,25), y=c(2.6,1.4), legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=0.75, cex=0.35)
}


#----------------------------------------------------------------------------
#' Plot ISO interchange data
#'
#' @param t timestep of interest
#' @param verts vector of vertex labels
#' @param layout vector of vertex positions
#' @param netinterchange net interchange data frame with
#'                       "time", "scenario", "Source2Sink", "value"
#' @param dispatch regional dispatch data frame with
#' @export draw_naris_interchange
draw_naris_interchange = function(t, verts, layout, netinterchange, dispatch, scenario=Scenario1, arrow.scaling=2.0, annotation_color='white', edge_color='white')
{
  # igraph does not support arrows of different weight/size on the same graph
  # so we're going to go through some contortions here. 
  
  # plot the vertices
  g = igraph::make_empty_graph()
  g = g + igraph::vertices(verts)
  
  plot(g, layout=layout, rescale=F, add=T, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.75)
  
  # parse the edges
  edges  = data.frame(edge=netinterchange$Source2Sink[netinterchange$time==t & netinterchange$scenario==scenario & netinterchange$value <= 0],
                      weight = abs(netinterchange$value[netinterchange$time==t & netinterchange$scenario==scenario & netinterchange$value <= 0]))

  edges$weight = ifelse(edges$weight/1000 < 0.1, 0.1, edges$weight/1000)
  
  edge_groups = split(edges, edges$weight)

  lapply(edge_groups, draw_edge_group, arrow.scaling=arrow.scaling, verts=verts, layout=layout)
  
  plot(g, layout=layout, rescale=F, add=T, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.75)
  
  # Curtailment label
  index = dispatch$time==t & dispatch$scenario==scenario 
  df = data.frame(zone=dispatch$zone[index], type=dispatch$Type[index], value=dispatch$value[index])
  c_label <- function(z,df) { ifelse(z %in% df$zone, ifelse(df$value[df$zone==z & df$type=='Curtailment'] / (sum(df$value[df$zone==z]) - df$value[df$zone==z & df$type=='Curtailment']) > 0.01, paste('\n\n(', format(100 * df$value[df$zone==z & df$type=='Curtailment'] / (sum(df$value[df$zone==z]) - df$value[df$zone==z & df$type=='Curtailment']), digits=1), '%)', sep=''), ''), '') }
  c_verts <- unlist(lapply(verts, FUN=c_label, df))
  g = igraph::make_empty_graph()
  g = g + igraph::vertices(c_verts)
  plot(g, layout=layout, rescale=F, add=T, vertex.label.font=2, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.5)
  
  # draw legend
  # x = -70.5
  # x = -77.5
  x = -134
  # alayout = as.matrix(data.frame(lon=c(x, x+.1, x, x+.1, x, x+.1), lat=c(25, 25, 26.5, 26.5, 28.5, 28.5)))
  # alayout = as.matrix(data.frame(lon=c(x, x+.1, x, x+.1, x, x+.1), lat=c(14, 14, 15.5, 15.5, 17.5, 17.5)))
  alayout = as.matrix(data.frame(lon=c(x, x+.1, x, x+.1, x, x+.1), lat=c(30, 30, 31.5, 31.5, 33.5, 33.5)))
  averts = c(1,2,3,4,5,6)
  g =igraph:: make_empty_graph()
  g = g + igraph::vertices(averts)
  g = g + igraph::edges(c(1,2))
  plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=0.5, edge.arrow.size=1.0/arrow.scaling)
  g = igraph::make_empty_graph()
  g = g + igraph::vertices(averts)
  g = g + igraph::edges(c(3,4))
  plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=1.0, edge.arrow.size=2.0/arrow.scaling)
  g = igraph::make_empty_graph()
  g = g + igraph::vertices(averts)
  g = g + igraph::edges(c(5,6))
  plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=2.0, edge.arrow.size=4.0/arrow.scaling)
}


#----------------------------------------------------------------------------
#' Draw individual generators
#'
#' @description Draws individual generators as bubbles sized by generation
#'              and colored by generation type.
#'
#' @param timestep timestep to plot
#' @param types vector of generation type strings to plot
#' @param generators generator data frame with "Generator_Name","Node_Region","Type","lat","lon"
#' @param generation generation time-series as transformed by load_generation
#' @param colors generation type color table
#' @param scaling bubble scaling factor (default=0.002)
#' @param fill fill the generator bubble (default=TRUE)
#' @param lx x position of legend
#' @param ly y position of legend
#' @param draw_legend flag for drawing legend on map
#' @export draw_naris_generators
draw_naris_generators = function(timestep, types, generators, generation, colors, scenario.name=Scenario1, scaling=0.002, fill=TRUE, lx=-72, ly=39.9, annotation_color="#FFFFFF00", legend_color="#C0C0C0", draw_legend=FALSE)#, annotation_color='white', legend_color='white'
{
  p <- generation[generation$time == timestep,]
  p <- p[p$scenario==scenario.name,]
  p <- p[,names(p)[names(p)!='Type']]
  
  g <- generators[generators$Type %in% types,]
  g <- merge(p,g, by='Generator_Name')
  g <- g[!is.na(g$power),]
  g <- g[g$power > 0,]
  g <- g[order(-g$power),]
  
  if (dim(g)[1] > 0)
  {
    if (fill)
    {
      symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, bg=sapply(g$Type, dispatch_color, colors=colors, a=0.75), lwd=0.2, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=annotation_color)
      if(draw_legend){
        # legend(lx, ly, legend=colors$type, col=legend_color, text.col=ifelse(colors$type %in% types, par("fg"), '#777777'), lwd=0.25, pt.bg=colors$color, pch=21, pt.cex=1, bty='n', cex=0.5)
        legend(lx, ly, legend=colors$type, col=legend_color, text.col=ifelse(colors$type %in% types, par("fg"), '#777777'), lwd=0.001, pt.bg=colors$color, pch=21, pt.cex=1, bty='n', cex=0.5)
      }
    }
    else
    {
      symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, lwd=0.5, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=sapply(g$Type, dispatch_color, a=0.9))
      if(draw_legend){
        legend(lx, ly, legend=types, bg='#737373', col=sapply(types, dispatch_color, colors=colors, a=0.9), pch=21, text.col=par("fg"))
      }
    }
  }

  if (draw_legend){
    # gen_legend = data.frame(power=c(1000, 2000, 4000), lat=c(25, 26.5, 28.5), lon=c(lx+1.5, lx+1.5, lx+1.5))
    # gen_legend = data.frame(power=c(1000, 2000, 4000), lat=c(14, 15.5, 17.5), lon=c(lx+0.75, lx+0.75, lx+0.75))
    gen_legend = data.frame(power=c(1000, 2000, 4000), lat=c(30, 31.5, 33.5), lon=c(lx+1.5, lx+1.5, lx+1.5))
    # symbols(gen_legend$lon, gen_legend$lat, circles=sqrt(scaling*gen_legend$power/pi), inches=FALSE, bg='#656565', fg='white', lwd=0.4, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE)
    symbols(gen_legend$lon, gen_legend$lat, circles=sqrt(scaling*gen_legend$power/pi), inches=FALSE, bg='#656565', fg="#C0C0C0", lwd=0.001, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE)
    # text(gen_legend$lon+0.5, gen_legend$lat, col=par("fg"), labels=c("1.0 GW", "2.0 GW", "4.0 GW"), cex=0.5, pos=4, offset=c(0.95))
    text(gen_legend$lon, gen_legend$lat, col=par("fg"), labels=c("1.0 GW", "2.0 GW", "4.0 GW"), cex=0.5, pos=4, offset=c(0.95))
  }
}


#----------------------------------------------------------------------------
#' Plot a chord diagram showing net interchange
#'
#' @param t  timestep 
#' @param netinterchange net interchange data frame with
#'                       "time", "scenario", "Source2Sink", "value"
#' @param scenario scenario
#' @export draw_naris_chord_interchange
draw_naris_chord_interchange = function(t, iso, netinterchange, scenario=Scenario1, link.size=1)
{
  index = netinterchange$time==t & netinterchange$scenario==scenario;
  interchange = data.frame(source2sink = netinterchange$Source2Sink[index], value=netinterchange$value[index])
  
  interchange$source = unlist(lapply(strsplit(as.character(interchange$source2sink),' '), function(l) { l[3] }))
  interchange$sink = unlist(lapply(strsplit(as.character(interchange$source2sink),' '), function(l) { l[1] }))
  
  tmp = interchange$source[interchange$value<0]
  interchange$source[interchange$value<0] = interchange$sink[interchange$value<0]
  interchange$sink[interchange$value<0] = tmp
  interchange$value = abs(interchange$value)
  
  mat = matrix(0, nrow=length(iso), ncol=length(iso))
  rownames(mat) = iso
  colnames(mat) = iso
  
  for (i in seq_along(interchange$sink))
  {
    # build adjacency matrix and scale MW to GW 
    mat[as.character(interchange$sink[i]), as.character(interchange$source[i])] = interchange$value[i]/1000
  }
  
  mat = mat[rowSums(mat)!=0,colSums(mat)!=0]
  
  col.len = length(unique(c(rownames(mat),colnames(mat))))
  col.copies = ifelse(col.len>12,ceiling(col.len/12), 1)
  
  col = adjustcolor(rep(RColorBrewer::brewer.pal(12, 'Paired'),col.copies)[1:col.len],
                    red.f=.75, green.f=.75, blue.f=.75)
  
  circlize::chordDiagram(mat, directional=1, grid.col=col, direction.type="arrows",
                         link.border=1, link.lwd=0.25, link.arr.lwd=link.size,
                         link.arr.length=link.size/4, link.arr.lty=2, reduce=-1,
                         transparency=0.4)
}

#----------------------------------------------------------------------------
#' Plot a series of NARIS generation map-dispatch frames
#'
#' @description Plot a series of 1920x1080 frames of NARIS generation and
#'              a dispatch stack for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_map_dispatch2_series = function(t0, tn, max_interchange=0, scenario=Scenario1, 
                                           types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                           prefix)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(map_dispatch2.dir,baseFilename), width=1920, height=1080, pointsize=24)
    draw_naris_map_dispatch2(t, scenario=scenario, types=types)
    dev.off()
  }
}               


#----------------------------------------------------------------------------
#' Plot NARIS generation map-dispatch view of NARIS data
#'
#' @description Draw a 1920x1080 frame of NARIS with generator map and
#'              a dispatch stack for a single scenario
#'
#' @param t timestep of interest
draw_naris_map_dispatch2 <- function(t, scenario=Scenario1, density='None',
                                     types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                     scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
  # par(fig=c(0, 1080/1920, 0, 1))
  par(fig=c(0, 1344/1920, 0, 1))
  draw_density(t, naris_regions, naris_generators, naris_generation, naris_colors, ...)
  # draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-78, ly=33, draw_legend=T)#lx=-123, ly=35#lx=-72, ly=39.9#lx = as.numeric(quantile(naris_layout[,1])[1]-1),ly = as.numeric(quantile(naris_layout[,2])[2])
  draw_naris_generators(t, types, naris_generators, naris_generation, naris_colors, scenario=scenario, scaling=0.002, lx=-135, ly=50, draw_legend=T)
  # draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, scenario=scenario)
  draw_shadow(t)
  # mtext("Generation & Flow", 1, -2, at=-92)
  mtext("Generation", 1, -2, at=-92)
  
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  par(fig=c(1344/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_naris_bars(t, scenario=scenario, weight)
  mtext("Regional dispatch", 1, 5)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  # mtext(paste('NARIS', ' (', scenario, ')', sep=''), font=2, cex=1.5)
  mtext(paste('NARIS', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%Y %H:%M EST"), 3, -1)
  
}

#----------------------------------------------------------------------------
#' Plot a series of NARIS generation map-dispatch frames
#'
#' @description Plot a series of 1920x1080 frames of NARIS generation and
#'              a dispatch stack for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_naris_map_dispatch3_series = function(t0, tn, max_interchange=0, scenario=Scenario1, 
                                           types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                           prefix)
{
  ts = unique(naris_netinterchange$time)
  ts = ts[order(ts)]
  
  i0 = which.min(abs(ts-t0))
  i1 = which.min(abs(ts-tn))
  
  for (i in i0:i1)
  {
    t = ts[i]
    
    baseFilename = sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M"))
    png(filename = file.path(map_dispatch3.dir,baseFilename), width=1920, height=1080, pointsize=24)
    draw_naris_map_dispatch3(t, scenario=scenario, types=types)
    dev.off()
  }
}               


#----------------------------------------------------------------------------
#' Plot NARIS generation map-dispatch view of NARIS data
#'
#' @description Draw a 1920x1080 frame of NARIS with generator map and
#'              a dispatch stack for a single scenario
#'
#' @param t timestep of interest
draw_naris_map_dispatch3 <- function(t, scenario=Scenario1, density='None',
                                     types=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                     scaling=0.002, weight=3, ...)
{
  par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
  # par(fig=c(0, 1080/1920, 0, 1))
  par(fig=c(0, 1344/1920, 0, 1))
  draw_density(t, naris_regions, naris_generators, naris_generation_zero, naris_colors, ...)
  # draw_naris_generators(t, types, naris_generators, naris_generation_zero, naris_colors, scenario=scenario, scaling=0.002, lx=-78, ly=33, draw_legend=T)#lx=-123, ly=35#lx=-72, ly=39.9#lx = as.numeric(quantile(naris_layout[,1])[1]-1),ly = as.numeric(quantile(naris_layout[,2])[2])
  draw_naris_generators(t, types, naris_generators, naris_generation_zero, naris_colors, scenario=scenario, scaling=0.002, lx=-135, ly=50, draw_legend=T)
  draw_naris_interchange(t, naris_verts, naris_layout, naris_netinterchange, naris_dispatch_stack, scenario=scenario)
  draw_shadow(t)
  mtext("Flow", 1, -2, at=-92)
  
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  # par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  par(fig=c(1344/1920, 1, 0, 1), mar=c(5.1,5.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  draw_naris_bars(t, scenario=scenario, weight)
  mtext("Regional dispatch", 1, 5)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
  # mtext(paste('NARIS', ' (', scenario, ')', sep=''), font=2, cex=1.5)
  mtext(paste('NARIS', sep=''), font=2, cex=1.5)
  mtext(format(t, "%m-%d-%Y %H:%M EST"), 3, -1)
  
}

