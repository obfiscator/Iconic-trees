##################################################################
# Things below this line should not be changed unless debugging. #
##################################################################

# This routine scans the aggregated data file and uses the latitude and longitude
# information therein to create a map showing where all the data comes from.
create_spatial_distribution_graph <- function(){
  
  # move to the data directory
  setwd(data.dir)
  
  # load in the aggregate data file
  combined.df <- read.table(file = aggregate.data.file.name,sep = ",", header = TRUE)
  
  # Create a big grid, 720 square by 360.  This represents a half-degree grid across the whole world.
  global.grid <- matrix(data = 0, nrow = 720, ncol = 360)
  # It appears that lat=-90,90 and long=-180,180
  # Convert latitude into points from 1 to 360, longitude into points from 1 to 720.
  # This is half-degree resolution, what is used on global land surface model simulations.
  combined.df$latitude <- ceiling((combined.df$latitude+90.0)*2)
  combined.df$longitude <- ceiling((combined.df$longitude+180.0)*2)
  
  # add the points
  # This, while pretty, doesn't properly add points if multiple sites fall in the same gridpoint.
  #global.grid[combined.df$longitude,combined.df$latitude] <- global.grid[combined.df$longitude,combined.df$latitude] + 1
  for(isite in seq(1,length(combined.df$longitude))){
    global.grid[combined.df$longitude[isite],combined.df$latitude[isite]] <- global.grid[combined.df$longitude[isite],combined.df$latitude[isite]] + 1
  }
  
  # create a polygon for each of these gridpoints.  Skip a gridsquare if it has no datapoints.
  ids <- vector("numeric",length=0)
  ids_counter <- 1.0
  lon <- vector("numeric",length=0)
  lat <- vector("numeric",length=0)
  nsites <- vector("integer",length=0)
  
  # This is probably bad.  Appending vectors should not be done.  I am only doing it here because this is a very sparse matrix.
  for(ix in seq(1,720)){
    for(iy in seq(1,360)){
      if(global.grid[ix,iy] > 0){
        nsites <- append(nsites,global.grid[ix,iy])
        ids <- append(ids,ids_counter)

        ids_counter <- ids_counter + 0.1
        # Need to create a square around half a degree.  Based on the calculation we did above, grid point 1,1 should cover the square 90S,180W to 89.5S,179.5W
        x1 <- ix/2.0-180.0-0.5
        x2 <- x1+0.5
        y1 <- iy/2.0-90.0-0.5
        y2 <- y1+0.5
        lon_poly <- c(x1, x1, x2, x2)
        lat_poly <- c(y1, y2, y2, y1)
        lon <- append(lon,lon_poly)
        lat <- append(lat,lat_poly)
        
        #browser()
      }
    }
  }
  # create the overlay polygons
  positions <- data.frame(id = rep(ids, each = 4), lon = lon, lat = lat)
  # I want to break the values up into five discrete sections to assign the fill colors
  fill.colors <- ceiling(nsites/5)
  colfunc <- colorRampPalette(c("red1", "green1")) # create a transition between two colors
  values <- data.frame( id = ids, value = nsites, fill.colors = fill.colors)
  datapoly <- merge(values, positions, by=c("id"))

  # For each lat/lon in the data.frame, I need to add a 1 to the global grid.  The indices depend on the map orientation I want to use.
  # I am using this global map.
  global.map <- map_data("world",bg="white")
  # This will plot it
  #gg <- NULL
  #mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  #gg <- ggplot() +   mapWorld
  
  gg <- ggplot() + geom_polygon(data = global.map, aes(x=long, y = lat, alpha=0.1, group = group))+coord_fixed(1.3)
  #gg <- ggplot() + geom_map(data=global.map, map=global.map, aes(long, lat, map_id=region),color="#2b2b2b", fill="white", size=0.05) +coord_fixed(1.3)
  
  
  gg <- gg + geom_polygon(data = datapoly,mapping = aes(group=id, alpha=255, x=lon, y=lat, fill=factor(fill.colors),size=0.1)) + 
    scale_x_continuous(breaks=c(seq(-180.0,180.0,60.0)), minor_breaks = c(seq(-180.0,180.0,20.0)), limits = c(-179.0, 179.0)) +
    scale_y_continuous(breaks=c(seq(-90.0,90.0,30.0)), minor_breaks = c(seq(-90.0,90.0,10.0)), limits = c(-90.0, 90.0)) +
    labs(x="Longitude", y="Latitude")
  
  ## And some special things for the legend.
  #legend.key.size <- unit(x=(0.01), units=("npc"))
  legend.key.height <- unit(x=(0.02), units=("npc"))
  legend.key.width <- unit(x=(0.02), units=("npc"))
  #legend.justification <- c(0,1)
  legend.position <- c(0.65,0.20)
  
  gg <- gg + theme(legend.position=legend.position, legend.key.height=legend.key.height, legend.key.width=legend.key.width) # remove the legend for the background.  Sadly, this removes the whole legend.
  
  scale.labels <- c(seq(0,max(datapoly$value),ceiling(max(datapoly$value)/5)))
  scale.labels <- paste(scale.labels,"-",as.integer(scale.labels-1+ceiling(max(datapoly$value)/5),sep=""))
  # change the lowest label to be "1--" instead of "0--", since we don't color boxes with 0 sites.
  scale.labels[1] <- sub("^\\s*0(\\D)", "1\\1", scale.labels[1], perl=TRUE)
  
  gg <- gg + scale_fill_manual(values=c(colfunc(5)),
                               name="Number of studies",
                               labels=scale.labels,
                               guide=guide_legend(reverse=TRUE)) + scale_colour_continuous(guide = FALSE)
  
  # There are guides (legends) for many things which get activated and clutter up the information I actually want.
  gg <- gg + guides(colour=FALSE, size=FALSE, shape=FALSE, alpha=FALSE)
  
  ggsave("spatial_distribution_highres.png", dpi=1200, dev='png', height=4, width=6, units="in")
  ggsave("spatial_distribution_medres.png", dpi=300, dev='png', height=4, width=6, units="in")
  
  #browser()
  gg




}