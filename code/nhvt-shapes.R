library(raster)
# Get the spatial extent (4 lon/lat values) for NH+VT
# maps::map('state', region=c('new hampshire', 'vermont', 'new york', 'massachusetts'))
# click twice (upper corner/lower corner) on the map to select the region of interest
# NHVT <- raster::drawExtent()
NHVT <- raster::extent(-73.61056, -70.60205, 42.48873, 45.37969)

# Get the NHVT border as a shapefile
usashp <- raster::getData('GADM', country='USA', level=1)
nhvtshp <- subset(usashp, NAME_1 %in% c("New Hampshire", "Vermont"))
crs(nhvtshp)

# Get the Hubbard Brook boundary
hbshp <- raster::shapefile("data/hbef_boundary/hbef_boundary.shp")
# wrong crs
crs(hbshp)

# transform crs
newcrs <- crs(nhvtshp)
hbshp <- sp::spTransform(hbshp, newcrs)

# plot
plot(nhvtshp)
plot(hbshp, add=TRUE)
