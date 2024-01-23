library(dplyr)

rm(list = ls())

# input directory, in which to find the local administrative units
LAU_DIR <- file.path(getwd(), "util", "shapefiles", "LAU")

# destination directory, where to save the generated GEOJSON files
destination_DIR <- file.path(getwd(), "util", "shapefiles", "geojson")

#==============================================================================================#

stopifnot(dir.exists(LAU_DIR))
stopifnot(dir.exists(destination_DIR))

# Loading Local Administrative Data (LAU), if desired
# info about LAU: https://ec.europa.eu/eurostat/web/nuts/local-administrative-units
# download LAU files (geoJSON): https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau
filepath_to_LAU <- file.path(LAU_DIR, "LAU_1.geojson")
# Load LAU data as sp (SpatialPolygonsDataFrame)
LAU_raw_data <- geojsonio::geojson_read(filepath_to_LAU, what = "sp")

#------------------------------------------------------------------------------#
europe_countries <- rnaturalearth::ne_countries(type = "countries", scale = "large", continent = "Europe")
raster::plot(europe_countries)

# List of all "regular" countries where the country border and the bidding-zone coincides
# All exceptions are taken care of later
list_of_countries <- data.frame(
  countries_list = c("BE", "BG", "CZ", "CH", "EE", "FI", "GR", "HR", "HU", "LT", "LV", "PL", "RO", "SI", "SK", "MT"),
  eic_code = c("10YBE----------2", "10YCA-BULGARIA-R", "10YCZ-CEPS-----N", "10YCH-SWISSGRIDZ", "10Y1001A1001A39I", "10YFI-1--------U", "10YGR-HTSO-----Y", "10YHR-HEP------M", "10YHU-MAVIR----U", "10YLT-1001A0008Q", "10YLV-1001A00074", "10YPL-AREA-----S", "10YRO-TEL------P", "10YSI-ELES-----O", "10YSK-SEPS-----K", "10Y1001A1001A93C")
)

# List of microstates and contested areas
# Kosovo, Andorra, San Marino, Lichtenstein, Vatican city, Monaco
list_of_microstates_and_contested <- data.frame(
  micro_contest_countries_list = c("XK", "AD", "SM", "LI", "VA", "MC"),
  eic_code = c("10Y1001C--00100H", "10YAD-AND------X", "EIC_SAN_MARINO", "EIC_LIECHTENSTEIN", "EIC_HOLY_SEE", "EIC_MONACO")
)


# List of all third countries, which show up on the map but which don't partake in any analysis
list_of_third_countries <- data.frame(
  third_countries_list = c("AL", "BA", "BY", "MD", "ME", "MK", "RS", "UA"),
  eic_code = c("10YAL-KESH-----5", "10YBA-JPCC-----D", "10Y1001A1001A51S", "10Y1001A1001A990", "10YCS-CG-TSO---S", "10YMK-MEPSO----8", "10YCS-SERBIATSOV", "10Y1001C--00003F")
)


# Filter to only include the countries in our list
europe_country <- europe_countries[europe_countries$iso_a2 %in% list_of_countries$countries_list, ]
# Add our EIC codes and names to this data frame
europe_country@data <- left_join(europe_country@data, list_of_countries, by = c("iso_a2" = "countries_list"))
# Now only keep the relevant data, and discard the rest
europe_country@data <- data.frame(europe_country$iso_a2, europe_country$eic_code, europe_country$name_long, europe_country$iso_a2, europe_country$iso_a2)
# Rename the columns to be consistent, also removing the pre-appended table name
europe_country@data <- europe_country@data %>% rename(eic_display_name = europe_country.iso_a2,
                                                      eic_code = europe_country.eic_code,
                                                      long_name = europe_country.name_long,
                                                      TSO_name = europe_country.iso_a2.1,
                                                      belongs_to_country = europe_country.iso_a2.2)
raster::plot(europe_country)


europe_microstate_and_contested <- europe_countries[europe_countries$iso_a2 %in% list_of_microstates_and_contested$micro_contest_countries_list, ]
europe_microstate_and_contested@data <- left_join(europe_microstate_and_contested@data, list_of_microstates_and_contested, by = c("iso_a2" = "micro_contest_countries_list"))
europe_microstate_and_contested@data <- data.frame(europe_microstate_and_contested$iso_a2, europe_microstate_and_contested$eic_code, europe_microstate_and_contested$name_long, europe_microstate_and_contested$iso_a2, europe_microstate_and_contested$iso_a2)
europe_microstate_and_contested@data <- europe_microstate_and_contested@data %>% rename(eic_display_name = europe_microstate_and_contested.iso_a2,
                                                                                        eic_code = europe_microstate_and_contested.eic_code,
                                                                                        long_name = europe_microstate_and_contested.name_long,
                                                                                        TSO_name = europe_microstate_and_contested.iso_a2.1,
                                                                                        belongs_to_country = europe_microstate_and_contested.iso_a2.2)
raster::plot(europe_microstate_and_contested)


third_country <- europe_countries[europe_countries$iso_a2 %in% list_of_third_countries$third_countries_list, ]
third_country@data <- left_join(third_country@data, list_of_third_countries, by = c("iso_a2" = "third_countries_list"))
third_country@data <- data.frame(third_country$iso_a2, third_country$eic_code, third_country$name_long, third_country$iso_a2, third_country$iso_a2)
third_country@data <- third_country@data %>% rename(eic_display_name = third_country.iso_a2,
                                                    eic_code = third_country.eic_code,
                                                    long_name = third_country.name_long,
                                                    TSO_name = third_country.iso_a2.1,
                                                    belongs_to_country = third_country.iso_a2.2)
raster::plot(third_country)





#------------------------------------------------------------------------------#
# Special countries where some areas are shared: GB and SEM

# GB is three countries from the UK: Scotland, England, and Wales
united_kingdom <- rnaturalearth::ne_countries(type = "map_units", scale = "large", country = "United Kingdom")
island_great_britain <- united_kingdom[united_kingdom$geounit %in% c("Scotland", "England", "Wales"), ]
GB <- rgeos::gUnaryUnion(island_great_britain)
GB_spdataframe <- sp::SpatialPolygonsDataFrame(GB, data = data.frame(
  eic_display_name = "GB",
  eic_code = "10YGB----------A",
  long_name = "Great Britain",
  TSO_name = "GB",
  belongs_to_country = "UK"
))

# SEM is the single energy market, consisting of the Republic of Ireland and the UK country of Northern Ireland
north_ireland <- united_kingdom[united_kingdom$geounit == "Northern Ireland", ]
republic_of_ireland <- rnaturalearth::ne_countries(type = "countries", scale = "large", country = "Ireland")
island_of_ireland <- raster::bind(north_ireland, republic_of_ireland)
SEM <- rgeos::gUnaryUnion(island_of_ireland)
SEM_spdataframe <- sp::SpatialPolygonsDataFrame(SEM, data = data.frame(
  eic_display_name = "SEM",
  eic_code = "10Y1001A1001A59C",
  long_name = "Single Energy Market of Ireland and Northern Ireland",
  TSO_name = "Single Energy Market",
  belongs_to_country = "IE"
))


#------------------------------------------------------------------------------#
# Special countries
#------------------------------------------------------------------------------#
# Special countries with overseas territory: Spain, France, Portugal, Netherlands
spain_whole <- rnaturalearth::ne_states(country = "Spain")
# The map_units of Spain are not divided in the country data, so we have to load using ne_states instead
# Exclude Canary island, and the two African cities Ceuta and Melilla
spain_provinces <- spain_whole[spain_whole$region != "Canary Is." & spain_whole$region != "Ceuta" & spain_whole$region != "Melilla", ]
spain_zone <- rgeos::gUnaryUnion(spain_provinces)
spain_information <- data.frame(
  eic_display_name = c("ES"),
  eic_code = "10YES-REE------0",
  long_name = c("Spain"),
  TSO_name = c("ES"),
  belongs_to_country = c("ES")
)
spain <- sp::SpatialPolygonsDataFrame(spain_zone, spain_information)

france_whole <- rnaturalearth::ne_countries(type = "map_units", scale = "large", country = "France")
france <- france_whole[france_whole$formal_en == "Metropolitan France", ] # Excludes all overseas departments
france$eic_display_name <- "FR"
france$eic_code <- "10YFR-RTE------C"
france$long_name <- "France"
france$TSO_name <- "FR"
france$belongs_to_country <- "FR"
france@data <- data.frame(france$eic_display_name, france$eic_code, france$long_name, france$TSO_name, france$belongs_to_country)
france@data <- france@data %>% rename(eic_display_name = france.eic_display_name,
                                      eic_code = france.eic_code,
                                      long_name = france.long_name,
                                      TSO_name = france.TSO_name,
                                      belongs_to_country = france.belongs_to_country)


portugal_whole <- rnaturalearth::ne_countries(type = "map_units", scale = "large", country = "Portugal")
portugal <- portugal_whole[portugal_whole$geounit == "Portugal", ] # This excludes the other two geounits, Madeira and Azores
portugal$eic_display_name <- "PT"
portugal$eic_code <- "10YPT-REN------W"
portugal$long_name <- "Portugal"
portugal$TSO_name <- "PT"
portugal$belongs_to_country <- "PT"
portugal@data <- data.frame(portugal$eic_display_name, portugal$eic_code, portugal$long_name, portugal$TSO_name, portugal$belongs_to_country)
portugal@data <- portugal@data %>% rename(eic_display_name = portugal.eic_display_name,
                                      eic_code = portugal.eic_code,
                                      long_name = portugal.long_name,
                                      TSO_name = portugal.TSO_name,
                                      belongs_to_country = portugal.belongs_to_country)

netherlands_whole <- rnaturalearth::ne_countries(type = "map_units", scale = "large", country = "Netherlands")
netherlands <- netherlands_whole[netherlands_whole$geounit == "Netherlands", ] # This excludes the other geounit, Carribean Netherlands
netherlands$eic_display_name <- "NL"
netherlands$eic_code <- "10YNL----------L"
netherlands$long_name <- "The Netherlands"
netherlands$TSO_name <- "NL"
netherlands$belongs_to_country <- "NL"
netherlands@data <- data.frame(netherlands$eic_display_name, netherlands$eic_code, netherlands$long_name, netherlands$TSO_name, netherlands$belongs_to_country)
netherlands@data <- netherlands@data %>% rename(eic_display_name = netherlands.eic_display_name,
                                      eic_code = netherlands.eic_code,
                                      long_name = netherlands.long_name,
                                      TSO_name = netherlands.TSO_name,
                                      belongs_to_country = netherlands.belongs_to_country)

# Special countries: Turkey and Cyprus
# Constructing Cyprus from the two separate parts plus UK airbases
republic_of_cyprus <- rnaturalearth::ne_countries(country = "Cyprus", scale = "Large")
northern_cyprus <- rnaturalearth::ne_countries(country = "Northern Cyprus", scale = "Large")
asia_continent <- rnaturalearth::ne_countries(continent = "Asia", scale = "Large")
UK_airbase_and_buffer <- asia_continent[asia_continent$name %in% c("Dhekelia","Akrotiri", "Cyprus U.N. Buffer Zone"), ]

cyprus_complete <- rgeos::gUnaryUnion(raster::bind(republic_of_cyprus, northern_cyprus, UK_airbase_and_buffer))
cyprus_information <- data.frame(
  eic_display_name = "CY",
  eic_code = "10YCY-1001A0003J",
  long_name = "Cyprus",
  TSO_name = "CY",
  belongs_to_country = "CY"
)
cyprus <- sp::SpatialPolygonsDataFrame(cyprus_complete, cyprus_information)

# Constructing Turkey
turkey <- rnaturalearth::ne_countries(country = "Turkey", scale = "Large")
turkey$eic_display_name <- "TR"
turkey$eic_code <- "10YTR-TEIAS----W"
turkey$long_name <- "Turkey"
turkey$TSO_name <- "TR"
turkey$belongs_to_country <- "TR"
turkey@data <- data.frame(turkey$eic_display_name, turkey$eic_code, turkey$long_name, turkey$TSO_name, turkey$belongs_to_country)
turkey@data <- turkey@data %>% rename(eic_display_name = turkey.eic_display_name,
                                      eic_code = turkey.eic_code,
                                      long_name = turkey.long_name,
                                      TSO_name = turkey.TSO_name,
                                      belongs_to_country = turkey.belongs_to_country)

# Collecting all special countries
europe_special_countries <- raster::bind(spain, france, portugal, netherlands, cyprus, turkey)
raster::plot(europe_special_countries)

#----------ITALY---------------------------------------------------------------#
italy_states <- rnaturalearth::ne_states(country = 'Italy')

# create all the bidding zones as a collection of provinces

# 2020 shape, region division
#23, 42, 21, 25, 32, 34, 36, 45 --> IT_NORTH
#52, 55, 57 --> IT_CENTRE_NORTH
#65, 62, 72 --> IT_CENTRE_SOUTH
#67, 75, 77, 78 --> IT_SOUTH
#82 --> IT_SARDINIA
#88 --> IT_SICILY


italy_north_provinces <- italy_states[italy_states$region_cod <= "IT-45",]
italy_centre_north_provinces <- italy_states[italy_states$region_cod >= "IT-52" & italy_states$region_cod <= "IT-57",]
italy_centre_south_provinces <- italy_states[italy_states$region_cod == "IT-62" | italy_states$region_cod == "IT-65" | italy_states$region_cod == "IT-72",]
italy_south_provinces <- italy_states[italy_states$region_cod == "IT-67" | italy_states$region_cod == "IT-75" | italy_states$region_cod == "IT-77" | italy_states$region_cod == "IT-78",]
italy_sardinia_provinces <- italy_states[italy_states$region_cod == "IT-88",]
italy_sicily_provinces <- italy_states[italy_states$region_cod == "IT-82",]

# make a gUnaryUnion on all bidding zones, we lose in that case the data_frame property
italy_north_region <- rgeos::gUnaryUnion(italy_north_provinces, id = italy_north_provinces$geonunit)
italy_centre_north_region <- rgeos::gUnaryUnion(italy_centre_north_provinces, id = italy_centre_north_provinces$geonunit)
italy_centre_south_region <- rgeos::gUnaryUnion(italy_centre_south_provinces, id = italy_centre_south_provinces$geonunit)
italy_south_region <- rgeos::gUnaryUnion(italy_south_provinces, id = italy_south_provinces$geonunit)
italy_sardinia_region <- rgeos::gUnaryUnion(italy_sardinia_provinces, id = italy_sardinia_provinces$geonunit)
italy_sicily_region <- rgeos::gUnaryUnion(italy_sicily_provinces, id = italy_sicily_provinces$geonunit)


# bind all the bidding zones together, and give them some information
italy_electricity_zones <- raster::bind(italy_north_region, italy_centre_north_region, italy_centre_south_region, italy_south_region, italy_sardinia_region, italy_sicily_region)

italy_information = data.frame(
  eic_display_name = c("IT_NORTH", "IT_CENTRE_NORTH", "IT_CENTRE_SOUTH", "IT_SOUTH", "IT_SARDINIA", "IT_SICILY"),
  eic_code = c("10Y1001A1001A73I", "10Y1001A1001A70O", "10Y1001A1001A71M", "10Y1001A1001A788", "10Y1001A1001A74G", "10Y1001A1001A75E"),
  long_name = c("Italy North", "Italy Centre North", "Italy Centre South", "Italy South", "Italy Sardinia", "Italy Sicily"),
  TSO_name = c("NORD", "CNOR", "CSUD", "SUD", "SARD", "SICI"),
  belongs_to_country = c("IT")
  )

italy_spdataframe <- sp::SpatialPolygonsDataFrame(italy_electricity_zones, data = italy_information)
raster::plot(italy_spdataframe)



#----------ITALY 2021----------------------------------------------------------#
# 2021 shape, region division
#23, 42, 21, 25, 32, 34, 36, 45 --> IT_NORTH
#52, 57 --> IT_CENTRE_NORTH
#55, 65, 62, 72 --> IT_CENTRE_SOUTH
#67, 75, 77 --> IT_SOUTH
#78 --> Calabria
#82 --> IT_SARDINIA
#88 --> IT_SICILY

it_north_2021_list <- c("IT-23", "IT-42", "IT-21", "IT-25", "IT-32", "IT-34", "IT-36", "IT-45")
it_centre_north_2021_list <- c("IT-52", "IT-57")
it_centre_south_2021_list <- c("IT-55", "IT-65", "IT-62", "IT-72")
it_south_2021_list <- c("IT-67", "IT-75", "IT-77")
it_calabria_2021_list <- "IT-78"
it_sardinia_2021_list <- "IT-82"
it_sicily_2021_list <- "IT-88"

it_north_2021_provinces <- italy_states[italy_states$region_cod %in% it_north_2021_list,]
it_centre_north_2021_provinces <- italy_states[italy_states$region_cod %in% it_centre_north_2021_list,]
it_centre_south_2021_provinces <- italy_states[italy_states$region_cod %in% it_centre_south_2021_list,]
it_south_2021_provinces <- italy_states[italy_states$region_cod %in% it_south_2021_list,]
it_calabria_2021_provinces <- italy_states[italy_states$region_cod %in% it_calabria_2021_list,]
it_sardinia_2021_provinces <- italy_states[italy_states$region_cod %in% it_sardinia_2021_list,]
it_sicily_2021_provinces <- italy_states[italy_states$region_cod %in% it_sicily_2021_list,]

it_north_2021_region <- rgeos::gUnaryUnion(it_north_2021_provinces, id = it_north_2021_provinces$geonunit)
it_centre_north_2021_region <- rgeos::gUnaryUnion(it_centre_north_2021_provinces, id = it_centre_north_2021_provinces$geonunit)
it_centre_south_2021_region <- rgeos::gUnaryUnion(it_centre_south_2021_provinces, id = it_centre_south_2021_provinces$geonunit)
it_south_2021_region <- rgeos::gUnaryUnion(it_south_2021_provinces, id = it_south_2021_provinces$geonunit)
it_calabria_2021_region <- rgeos::gUnaryUnion(it_calabria_2021_provinces, id = it_calabria_2021_provinces$geonunit)
it_sardinia_2021_region <- rgeos::gUnaryUnion(it_sardinia_2021_provinces, id = it_sardinia_2021_provinces$geonunit)
it_sicily_2021_region <- rgeos::gUnaryUnion(it_sicily_2021_provinces, id = it_sicily_2021_provinces$geonunit)


it_2021_electricity_zones <- raster::bind(it_north_2021_region, it_centre_north_2021_region, it_centre_south_2021_region, it_south_2021_region, it_calabria_2021_region, it_sardinia_2021_region, it_sicily_2021_region)

it_2021_information = data.frame(
  eic_display_name = c("IT_NORTH", "IT_CENTRE_NORTH", "IT_CENTRE_SOUTH", "IT_SOUTH", "IT_CALABRIA", "IT_SARDINIA", "IT_SICILY"),
  eic_code = c("10Y1001A1001A73I", "10Y1001A1001A70O", "10Y1001A1001A71M", "10Y1001A1001A788","10Y1001C--00096J" , "10Y1001A1001A74G", "10Y1001A1001A75E"),
  long_name = c("Italy North", "Italy Centre North", "Italy Centre South", "Italy South","Italy Calabria", "Italy Sardinia", "Italy Sicily"),
  TSO_name = c("NORD", "CNOR", "CSUD", "SUD","CALA", "SARD", "SICI"),
  belongs_to_country = c("IT")
)

it_2021_spdataframe <- sp::SpatialPolygonsDataFrame(it_2021_electricity_zones, data = it_2021_information)
raster::plot(it_2021_spdataframe)


# Clear all variables except "italy_spdataframe"
#to_remove <- ls()
#to_remove <- c(to_remove[!grepl])







#--------- DE/LU and AT separately---------------------------------------------#
germany_luxembourg <- europe_countries[europe_countries$name == "Germany" | europe_countries$name == "Luxembourg",]
raster::plot(germany_luxembourg)

DE_LU_BZN <- rgeos::gUnaryUnion(germany_luxembourg, id = germany_luxembourg$continent)
#DE_LU_BZN <- raster::bind(DE_LU_BZN, DE_LU_BZN)
DE_LU_BZN@polygons[[1]]@ID <- '1'

DE_LU_information <- data.frame(
  eic_display_name = "DE_LU",
  eic_code = eneRgymaps::eic.code.DELU(),
  long_name = "Germany-Luxembourg",
  TSO_name = "DE/LU",
  belongs_to_country = "DE"
)

DE_LU_spdataframe <- sp::SpatialPolygonsDataFrame(DE_LU_BZN, data = DE_LU_information)
raster::plot(DE_LU_spdataframe)

# Austria

austria <- europe_countries[europe_countries$name == "Austria",]
raster::plot(austria)
austria@polygons[[1]]@ID <- '1'

AT_information <- data.frame(
  eic_display_name = "AT",
  eic_code = eneRgymaps::eic.code.AT(),
  long_name = "Austria",
  TSO_name = "AT",
  belongs_to_country = "AT"
)

AT_spdataframe <- sp::SpatialPolygonsDataFrame(austria, data = AT_information)
raster::plot(AT_spdataframe)

#--------- DE/AT/LU -----------------------------------------------------------#
germany_austria_luxembourg <- europe_countries[europe_countries$name == "Germany" | europe_countries$name == "Austria" | europe_countries$name == "Luxembourg",]
raster::plot(germany_austria_luxembourg)

DE_AT_LU_BZN <- rgeos::gUnaryUnion(germany_austria_luxembourg, id = germany_austria_luxembourg$continent)
#DE_AT_LU_BZN <- raster::bind(DE_AT_LU_BZN, DE_AT_LU_BZN)
DE_AT_LU_BZN@polygons[[1]]@ID <- '1'

DE_AT_LU_information <- data.frame(
  eic_display_name = "DE_AT_LU",
  eic_code = eneRgymaps::eic.code.merged.AT.DE(),
  long_name = "Germany-Austria-Luxembourg",
  TSO_name = "DE/AT/LU",
  belongs_to_country = "DE"
)

DE_AT_LU_spdataframe <- sp::SpatialPolygonsDataFrame(DE_AT_LU_BZN, data = DE_AT_LU_information)
raster::plot(DE_AT_LU_spdataframe)

#--------- Denmark -------------------------------------------------------------#
denmark_states <- rnaturalearth::ne_states(country = 'Denmark')

# Division of DK1 and DK2 in iso_3166_2
# DK1 = nordjylland, midtjylland, and syddanmark --> DK-81, DK-82, DK-83
# DK2 = hovedstaden and sjaelland --> DK-84 and DK-85

# Filter out the correct regions
DK1_provinces <- denmark_states[denmark_states$iso_3166_2 >= "DK-81" & denmark_states$iso_3166_2 <= "DK-83", ]
DK2_provinces <- denmark_states[denmark_states$iso_3166_2 == "DK-84" | denmark_states$iso_3166_2 == "DK-85", ]

raster::plot(DK1_provinces)
raster::plot(DK2_provinces)

# Dissolve the borders in the regions
DK1_region <- rgeos::gUnaryUnion(DK1_provinces)
DK2_region <- rgeos::gUnaryUnion(DK2_provinces)

# Join the two regions
DK_BZN <- raster::bind(DK1_region, DK2_region)

# Provide information on those regions manually
DK_information <- data.frame(
  eic_display_name = c("DK1", "DK2"),
  eic_code = c("10YDK-1--------W", "10YDK-2--------M"),
  long_name = c("Western Denmark", "Eastern Denmark"),
  TSO_name = c("DK1", "DK2"),
  belongs_to_country = c("DK")
)

# Combine outline and information to for a SpatialPolygonsDataFrame
denmark_spdataframe <- sp::SpatialPolygonsDataFrame(DK_BZN, data = DK_information)
raster::plot(denmark_spdataframe)

#----------Sweden approximate BZN----------------------------------------------#
sweden_states <- rnaturalearth::ne_states(country = "Sweden")

# approximate division (which is wrong, the Swedish bidding zones don't go along province lines)
# SE1 --> BD
# SE2 --> AC, Z, Y, X
# SE3 --> F, O, E, D, AB, C, T, U, S, W
# SE4 --> M, K N, G, H

SE1_list <- c("SE-BD")
SE2_list <- c("SE-AC", "SE-X", "SE-Y", "SE-Z")
SE3_list <- c("SE-AB", "SE-C", "SE-D", "SE-E", "SE-F", "SE-O", "SE-S", "SE-T", "SE-U", "SE-W")
SE4_list <- c("SE-G", "SE-H", "SE-K", "SE-M", "SE-N")

SE1_provinces <- sweden_states[sweden_states$iso_3166_2 %in% SE1_list, ]
SE2_provinces <- sweden_states[sweden_states$iso_3166_2 %in% SE2_list, ]
SE3_provinces <- sweden_states[sweden_states$iso_3166_2 %in% SE3_list, ]
SE4_provinces <- sweden_states[sweden_states$iso_3166_2 %in% SE4_list, ]

SE1_region <- rgeos::gUnaryUnion(SE1_provinces)
SE2_region <- rgeos::gUnaryUnion(SE2_provinces)
SE3_region <- rgeos::gUnaryUnion(SE3_provinces)
SE4_region <- rgeos::gUnaryUnion(SE4_provinces)

SE_BZN <- raster::bind(SE1_region, SE2_region, SE3_region, SE4_region)

SE_information <- data.frame(
  eic_display_name = c("SE1", "SE2", "SE3", "SE4"),
  eic_code = c("10Y1001A1001A44P","10Y1001A1001A45N","10Y1001A1001A46L","10Y1001A1001A47J"),
  long_name = c("SE1", "SE2", "SE3", "SE4"),
  TSO_name = c("SE1", "SE2", "SE3", "SE4"),
  belongs_to_country = c("SE")
)

sweden_spdataframe <- sp::SpatialPolygonsDataFrame(SE_BZN, data = SE_information)
raster::plot(sweden_spdataframe)


#----------Sweden using LAU----------------------------------------------------#
LAU_sweden_data <- LAU_raw_data[LAU_raw_data$CNTR_CODE == "SE", ]

# "se*_list" are all four-letter codes that are required to extract the relevant
# geometries from the LAU data
# "se*_lan" matches all LAU regions of a certain province (called l?n), when a
# few geometries need to be excluded, which appens in the "-c(match())" statement

se1_list <- c(grep("^25+", LAU_sweden_data$LAU_CODE, value = TRUE),
              "2417", "2418", "2482")

se2_lan <- c(grep("^22+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^23+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^21+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^24+", LAU_sweden_data$LAU_CODE, value = TRUE))
se2_list <- se2_lan[-c(match(c("2104", "2180", "2181", "2417", "2418", "2482"), se2_lan))]

se3_lan <- c(grep("^01+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^03+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^04+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^05+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^09+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^14+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^17+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^18+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^19+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^20+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^06+", LAU_sweden_data$LAU_CODE, value = TRUE),
             "0860", "0883", "0884", "1383", "1384", "2104", "2180", "2181")
se3_list <- se3_lan[-c(match("0683", se3_lan))]

se4_lan <- c(grep("^07+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^10+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^12+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^08+", LAU_sweden_data$LAU_CODE, value = TRUE),
             grep("^13+", LAU_sweden_data$LAU_CODE, value = TRUE),
             "0683")
se4_list <- se4_lan[-c(match(c("0860", "0883", "0884", "1383", "1384"), se4_lan))]

se1_LAU_data <- LAU_sweden_data[LAU_sweden_data$LAU_CODE %in% se1_list, ]
se2_LAU_data <- LAU_sweden_data[LAU_sweden_data$LAU_CODE %in% se2_list, ]
se3_LAU_data <- LAU_sweden_data[LAU_sweden_data$LAU_CODE %in% se3_list, ]
se4_LAU_data <- LAU_sweden_data[LAU_sweden_data$LAU_CODE %in% se4_list, ]

raster::plot(se1_LAU_data)
raster::plot(se2_LAU_data)
raster::plot(se3_LAU_data)
raster::plot(se4_LAU_data)

se_all_shapes <- raster::bind(rgeos::gUnaryUnion(se1_LAU_data), rgeos::gUnaryUnion(se2_LAU_data), rgeos::gUnaryUnion(se3_LAU_data), rgeos::gUnaryUnion(se4_LAU_data))

se_all_information <- data.frame(
  eic_display_name = c("SE1", "SE2", "SE3", "SE4"),
  eic_code = c("10Y1001A1001A44P","10Y1001A1001A45N","10Y1001A1001A46L","10Y1001A1001A47J"),
  long_name = c("SE1", "SE2", "SE3", "SE4"),
  TSO_name = c("SE1", "SE2", "SE3", "SE4"),
  belongs_to_country = c("SE")
)

se_all_bzn <- sp::SpatialPolygonsDataFrame(se_all_shapes, data = se_all_information)

# We have to convert our map from LAU into the same Coordinate Reference System
# as was used in rnaturalearth
CRS_string <- "+proj=longlat +datum=WGS84 +no_defs"
sweden_LAU_spdataframe <- sp::spTransform(se_all_bzn, CRS_string)

raster::plot(sweden_LAU_spdataframe)


#--------- Norway approximate BZN----------------------------------------------#
norway_all_territory <- rnaturalearth::ne_states(country = "Norway")
norway_states <- norway_all_territory[norway_all_territory$geonunit == "Norway", ]

# Approximate division using iso 3166_2 (also wrong, Norway's bidding zones do not follow region boundaries)
#NO1 --> NO-03, NO-30, NO-34
#NO2 --> NO-11, NO-42, NO-38
#NO3 --> NO-50, NO-15
#NO4 --> NO-18, NO-54
#NO5 --> NO-46

# Old ISO standard is used in the lists below
NO1_list <- c("NO-01", "NO-02", "NO-03", "NO-04", "NO-05", "NO-06", "NO-07")
NO2_list <- c("NO-08", "NO-09", "NO-10", "NO-11")
NO3_list <- c("NO-15", "NO-16", "NO-17")
NO4_list <- c("NO-18", "NO-19", "NO-20")
NO5_list <- c("NO-12", "NO-14")

NO1_provinces <- norway_states[norway_states$iso_3166_2 %in% NO1_list, ]
NO2_provinces <- norway_states[norway_states$iso_3166_2 %in% NO2_list, ]
NO3_provinces <- norway_states[norway_states$iso_3166_2 %in% NO3_list, ]
NO4_provinces <- norway_states[norway_states$iso_3166_2 %in% NO4_list, ]
NO5_provinces <- norway_states[norway_states$iso_3166_2 %in% NO5_list, ]

NO1_region <- rgeos::gUnaryUnion(NO1_provinces)
NO2_region <- rgeos::gUnaryUnion(NO2_provinces)
NO3_region <- rgeos::gUnaryUnion(NO3_provinces)
NO4_region <- rgeos::gUnaryUnion(NO4_provinces)
NO5_region <- rgeos::gUnaryUnion(NO5_provinces)

NO_BZN <- raster::bind(NO1_region, NO2_region, NO3_region, NO4_region, NO5_region)

NO_information <- data.frame(
  eic_display_name = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  eic_code = c("10YNO-1--------2","10YNO-2--------T","10YNO-3--------J","10YNO-4--------9","10Y1001A1001A48H"),
  long_name = c("Southeast Norway", "Southwest Norway", "Middle Norway", "North Norway", "West Norway"),
  TSO_name = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  belongs_to_country = c("NO")
)

norway_spdataframe <- sp::SpatialPolygonsDataFrame(NO_BZN, data = NO_information)
raster::plot(norway_spdataframe)


#--------- Norway using LAU----------------------------------------------------#
LAU_norway_data <- LAU_raw_data[LAU_raw_data$CNTR_CODE == "NO", ]

# Same logic as used for selection of regions in Swedish LAU, see that for reference
no1_lan <- c(grep("^03+", LAU_norway_data$LAU_CODE, value = TRUE), #03 Oslo
             grep("^01+", LAU_norway_data$LAU_CODE, value = TRUE), #01, 02, 06 Viken
             grep("^02+", LAU_norway_data$LAU_CODE, value = TRUE),
             grep("^06+", LAU_norway_data$LAU_CODE, value = TRUE),
             grep("^04+", LAU_norway_data$LAU_CODE, value = TRUE), #04, 05 Innlandet
             grep("^05+", LAU_norway_data$LAU_CODE, value = TRUE))
no1_list <- no1_lan[-c(match(c("0615", "0616", "0617", "0618", "0619", "0620", "0633", "0511", "0512", "0513", "0514"), no1_lan))]

no2_list <- c(grep("^07+", LAU_norway_data$LAU_CODE, value = TRUE), #07, 08 Vestfold og Telemark
              grep("^08+", LAU_norway_data$LAU_CODE, value = TRUE),
              grep("^09+", LAU_norway_data$LAU_CODE, value = TRUE), #09, 10 Agder
              grep("^10+", LAU_norway_data$LAU_CODE, value = TRUE),
              grep("^11+", LAU_norway_data$LAU_CODE, value = TRUE), #11 Rogaland
              "1211", "1216", "1219", "1221", "1222", "1223", "1224", "1228", "1231", "1241", "1243", "1244")

no3_lan <- c(grep("^15+", LAU_norway_data$LAU_CODE, value = TRUE), #15 M?re og Romsdal
             grep("^50+", LAU_norway_data$LAU_CODE, value = TRUE), #50 Tr?ndelag
             "0511", "0512", "0513", "0514", "1401", "1412", "1413", "1428", "1429", "1430", "1431", "1432", "1433", "1438", "1439", "1441", "1443", "1444", "1445", "1449")
no3_list <- no3_lan[-c(match(c("5043", "5044", "5052"), no3_lan))]

no4_list <- c(grep("^18+", LAU_norway_data$LAU_CODE, value = TRUE), #18 Nordland
              grep("^19+", LAU_norway_data$LAU_CODE, value = TRUE), #19, 20 Troms og Finnmark
              grep("^20+", LAU_norway_data$LAU_CODE, value = TRUE),
              "5043", "5044", "5052")

no5_lan <- c(grep("^12+", LAU_norway_data$LAU_CODE, value = TRUE), #12, 14 Vestland
             grep("^14+", LAU_norway_data$LAU_CODE, value = TRUE),
             "0615", "0616", "0617", "0618", "0619", "0620", "0633")
no5_list <- no5_lan[-c(match(c("1211", "1216", "1219","1221", "1222", "1223", "1224","1228", "1231", "1241", "1243", "1244", "1401", "1412", "1413", "1428", "1429", "1430", "1431", "1432", "1433", "1438", "1439", "1441", "1443", "1444", "1445", "1449"), no5_lan))]


no1_LAU_data <- LAU_norway_data[LAU_norway_data$LAU_CODE %in% no1_list, ]
no2_LAU_data <- LAU_norway_data[LAU_norway_data$LAU_CODE %in% no2_list, ]
no3_LAU_data <- LAU_norway_data[LAU_norway_data$LAU_CODE %in% no3_list, ]
no4_LAU_data <- LAU_norway_data[LAU_norway_data$LAU_CODE %in% no4_list, ]
no5_LAU_data <- LAU_norway_data[LAU_norway_data$LAU_CODE %in% no5_list, ]


no_all_shapes <- raster::bind(rgeos::gUnaryUnion(no1_LAU_data),
                              rgeos::gUnaryUnion(no2_LAU_data),
                              rgeos::gUnaryUnion(no3_LAU_data),
                              rgeos::gUnaryUnion(no4_LAU_data),
                              rgeos::gUnaryUnion(no5_LAU_data))

no_all_information <- data.frame(
  eic_display_name = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  eic_code = c("10YNO-1--------2","10YNO-2--------T","10YNO-3--------J","10YNO-4--------9","10Y1001A1001A48H"),
  long_name = c("Southeast Norway", "Southwest Norway", "Middle Norway", "North Norway", "West Norway"),
  TSO_name = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  belongs_to_country = c("NO")
)

no_all_bzn <- sp::SpatialPolygonsDataFrame(no_all_shapes, data = no_all_information)

# We have to convert our map from LAU into the same Coordinate Reference System
# as was used in rnaturalearth
CRS_string <- "+proj=longlat +datum=WGS84 +no_defs"
norway_LAU_spdataframe <- sp::spTransform(no_all_bzn, CRS_string)

raster::plot(norway_LAU_spdataframe)



#------------------------------------------------------------------------------#
# Putting it all together
# Europe 2017 and before

europe_2010_map <- raster::bind(DE_AT_LU_spdataframe,
                           italy_spdataframe,
                           denmark_spdataframe,
                           sweden_LAU_spdataframe,
                           norway_LAU_spdataframe,
                           GB_spdataframe, SEM_spdataframe,
                           europe_country,
                           europe_special_countries,
                           europe_microstate_and_contested,
                           third_country)
raster::plot(europe_2010_map)


# Europe 2019

europe_2019_map <- raster::bind(DE_LU_spdataframe,
                                AT_spdataframe,
                                italy_spdataframe,
                                denmark_spdataframe,
                                sweden_LAU_spdataframe,
                                norway_LAU_spdataframe,
                                GB_spdataframe, SEM_spdataframe,
                                europe_country,
                                europe_special_countries,
                                europe_microstate_and_contested,
                                third_country)
raster::plot(europe_2019_map)




# Europe 2021

europe_2021_map <- raster::bind(DE_LU_spdataframe,
                                AT_spdataframe,
                                it_2021_spdataframe,
                                denmark_spdataframe,
                                sweden_LAU_spdataframe,
                                norway_LAU_spdataframe,
                                GB_spdataframe, SEM_spdataframe,
                                europe_country,
                                europe_special_countries,
                                europe_microstate_and_contested,
                                third_country)
raster::plot(europe_2021_map)




#------------------------------------------------------------------------------#
# EXPORT #

# Convert to GeoJSON then export
europe_map_geojson_converted <- geojsonio::geojson_json(europe_2010_map)
geojsonio::geojson_write(europe_map_geojson_converted, file = file.path(destination_DIR,"europe_2010_map.geojson"))

europe_map_geojson_converted <- geojsonio::geojson_json(europe_2019_map)
geojsonio::geojson_write(europe_map_geojson_converted, file = file.path(destination_DIR,"europe_2019_map.geojson"))

europe_map_geojson_converted <- geojsonio::geojson_json(europe_2021_map)
geojsonio::geojson_write(europe_map_geojson_converted, file = file.path(destination_DIR,"europe_2021_map.geojson"))


