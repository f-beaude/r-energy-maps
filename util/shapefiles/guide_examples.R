# Guide to make shapefiles using R

library(rnaturalearth)
library(raster)
# library(rgeos)
library(sp)
library(geojsonio)

rm(list = ls())

destination_DIR <- tempdir()

#------------------------------------------------------------------------------#
# Getting data
raster::plot(rnaturalearth::ne_countries(continent = "Europe"))

raster::plot(rnaturalearth::ne_countries(country = "Italy"))
raster::plot(rnaturalearth::ne_countries(country = "Italy", scale = "large"))

raster::plot(rnaturalearth::ne_states(country = "Germany"))


#------------------------------------------------------------------------------#
# Filtering

my_geometry <- rnaturalearth::ne_states(country = "Germany")
my_geometry$name

my_france_geometry <- rnaturalearth::ne_states(country = "France")
raster::plot(my_france_geometry)

regions_of_france <- my_france_geometry[my_france_geometry$geonunit == "France", ]
raster::plot(regions_of_france)


list_of_v4_countries <- c("Czech Republic", "Hungary", "Poland", "Slovakia")
europe_countries <- rnaturalearth::ne_countries(continent = "Europe")
v4_countries <- europe_countries[europe_countries$name_long %in% list_of_v4_countries, ]
raster::plot(v4_countries)

#------------------------------------------------------------------------------#
# Dissolving boundaries

sweden_states <- rnaturalearth::ne_states(country = "Sweden")
north_sweden_states <- sweden_states[sweden_states$name == "Norrbotten" |
                                       sweden_states $name == "V?sterbotten" |
                                       sweden_states$name == "V?sternorrland" |
                                       sweden_states$name == "J?mtland", ]
# these are the names of the four north most provinces
grouped_north_sweden <- sf::st_make_valid(sf::st_union(north_sweden_states))
# now compare
raster::plot(sweden_states)
raster::plot(north_sweden_states)
raster::plot(grouped_north_sweden)


mainland_france <- sf::st_union(regions_of_france)
raster::plot(mainland_france)

#------------------------------------------------------------------------------#
# Binding geometries together

be_country <- rnaturalearth::ne_countries(country = "Belgium")
nl_country <- rnaturalearth::ne_countries(country = "Netherlands")
nl_mainland_country <- nl_country[nl_country$geounit == "Netherlands"] # filter out Caribbean part
lux_country <- rnaturalearth::ne_countries(country = "Luxembourg")
benelux <- rbind(be_country, nl_mainland_country, lux_country)
raster::plot(benelux)


#------------------------------------------------------------------------------#
# Adding information
north_data <- data.frame(
  administrative_name = c("North Sweden"),
  direction = c("North"),
  is_EU = c(TRUE)
)

north_sweden_spdf <- sp::SpatialPolygonsDataFrame(grouped_north_sweden, data = north_data)

v4_countries$eic_2letter_code <- c("CZ", "HU", "PL", "SK")


#------------------------------------------------------------------------------#
# Exporting

Europe_geojson <- geojsonio::geojson_json(benelux)
geojsonio::geojson_write(Europe_geojson, file = file.path(destination_DIR, "benelux.geojson"))


#------------------------------------------------------------------------------#
# Complete example

# Importing rnaturalearth data
europe_countries <- rnaturalearth::ne_countries(continent = "Europe", scale = "medium")

# Making a list of which country should be included in which region
list_north = c("Norway", "Sweden", "Denmark", "Finland", "Iceland")
list_centre_east = c("Estonia", "Latvia", "Lithuania", "Poland", "Germany", "Luxembourg", "Switzerland", "Austria", "Slovenia", "Croatia", "Hungary", "Slovakia", "Czech Republic")
list_south_east = c("Romania", "Bulgaria", "Greece", "Albania", "Montenegro", "Bosnia and Herzegovina", "Serbia", "Kosovo", "Macedonia", "Moldova")
list_south = c("Italy", "Spain", "Portugal")
list_west = c("France", "Belgium", "Netherlands", "United Kingdom", "Ireland")

# Subsetting the regions based on the previously defined lists
countries_north <- europe_countries[europe_countries$name_long %in% list_north, ]
countries_centre_east <- europe_countries[europe_countries$name_long %in% list_centre_east, ]
countries_south_east <- europe_countries[europe_countries$name_long %in% list_south_east, ]
countries_south <- europe_countries[europe_countries$name_long %in% list_south, ]
countries_west <- europe_countries[europe_countries$name_long %in% list_west, ]

# Dissolving internal boundaries in the regions
region_north <- sf::st_union(countries_north)
region_centre_east <- sf::st_union(countries_centre_east)
region_south_east <- sf::st_union(countries_south_east)
region_south <- sf::st_union(countries_south)
region_west <- sf::st_union(countries_west)

# Combining the regions
regions_combined <- rbind(region_north, region_centre_east, region_south_east, region_south, region_west)


regions_data <- data.frame(
  region_name = c("Northern Europe", "Central and Eastern Europe", "South-eastern Europe", "Southern Europe", "Western Europe"),
  region_letter = c("N", "CE", "SE", "S", "W")
)
regions_europa <- sp::SpatialPolygonsDataFrame(regions_combined, data = regions_data)

# Plot the result
raster::plot(europe_countries,
     border = "gray70",
     xlim = c(-10, 30),
     ylim = c(35, 70)
)
raster::plot(regions_europa,
     col = c("green", "cyan", "orange", "red", "purple"),
     add = TRUE
)
