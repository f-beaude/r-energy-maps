# Utilities related to geographic maps

# Default scale used to retrieve shapes from rnaturalearth (110 -> small, 50 -> medium, 10 -> large (most detailed))
defaultRNaturalEarthScale <- 10

# rely on name and ISO2 code to identify the countries in the rnaturalearth data set
rnaturalearthCountryNameIdentifier <- "name_sort"
rnaturalearthCountryIsoIdentifier <- "iso_a2"

#' Compute a raw shapes of countries
#' @param scale the scale to use (10 - most detailed -, 50 or 110)
#' @importFrom dplyr bind_rows filter if_else select
#' @importFrom rnaturalearth ne_countries
#' @return shapes described through a large spatial polygons data frame
#' @examples
#' countries.shapes.raw ()
#' countries.shapes.raw (scale = 110)
countries.shapes.raw <- function (scale = defaultRNaturalEarthScale) {
  stopifnot ((! is.null(scale)) && (scale %in% c (10, 50, 110)))

  # countries shapefile
  WorldRaw <- rnaturalearth::ne_countries (scale = scale, returnclass = "sf") %>% # retrieve the most geographic shapes
    dplyr::select(`name_sort`, `iso_a2`, `continent`)

  # Set iso2 code of Kosovo to "XK"
  WorldRaw[[rnaturalearthCountryIsoIdentifier]] <- dplyr::if_else(WorldRaw[[rnaturalearthCountryNameIdentifier]] == "Kosovo",
                                                                  "XK", WorldRaw[[rnaturalearthCountryIsoIdentifier]])

  # Iso2 code for France and Norway is missing for some scales
  WorldRaw[[rnaturalearthCountryIsoIdentifier]] <- dplyr::if_else(WorldRaw[[rnaturalearthCountryNameIdentifier]] == "France",
                                                                  "FR", WorldRaw[[rnaturalearthCountryIsoIdentifier]])
  WorldRaw[[rnaturalearthCountryIsoIdentifier]] <- dplyr::if_else(WorldRaw[[rnaturalearthCountryNameIdentifier]] == "Norway",
                                                                  "NO", WorldRaw[[rnaturalearthCountryIsoIdentifier]])

  # Cyprus is officially one country according to the UN
  countryCy <- WorldRaw[(! is.na (WorldRaw$name_sort)) & (grepl(pattern = "^Cyprus", x = WorldRaw$name_sort)), ]
  countryCy[[rnaturalearthCountryNameIdentifier]] <- "Cyprus"
  countryCy[[rnaturalearthCountryIsoIdentifier]] <- "CY"
  countryCy <- countryCy %>%
    merge.all.countries.shapes()

  # remove old CY shapes without country name and old CY object before merging with the updated objects
  WorldRaw <- WorldRaw [(is.na (WorldRaw$name_sort)) | (! grepl(pattern = "^Cyprus", x = WorldRaw$name_sort)), ]

  return (dplyr::bind_rows (list(WorldRaw, countryCy)) %>%
            dplyr::filter(! is.na(`iso_a2`)))
}

#' Compute the shapes of Great Britain and Northern Ireland
#' @importFrom dplyr mutate select
#' @return the shape as a data.frame
#' @inheritParams countries.shapes.raw scale
#' @example GB.NI.shapes()
GB.NI.shapes <- function (scale) {
  GBNI <- rnaturalearth::ne_countries(country = 'united kingdom', scale = scale, type = 'map_units', returnclass = "sf") %>%
    dplyr::select(`name_sort`, `continent`, `geometry`) %>%
    dplyr::mutate(`iso_a2` = dplyr::if_else (`name_sort` == "Northern Ireland", "NI", "GB"),
                  `name_sort` = dplyr::if_else (`name_sort` == "Northern Ireland", "Northern Ireland", "Great Britain")) %>%
    merge.all.countries.shapes()

  return(GBNI)
}

#' Compute the shape of the single electricity market (SEM) of Ireland
#' @importFrom dplyr filter mutate select
#' @return the shape as a data.frame
#' @inheritParams countries.shapes.raw scale
#' @example IE.SEM.shape()
IE.SEM.shape <- function (scale) {
  SEM <- rnaturalearth::ne_countries(country = c('united kingdom', 'ireland'), scale = scale, type = 'map_units', returnclass = "sf") %>%
    dplyr::select(`name_sort`, `continent`, `geometry`) %>%
    dplyr::filter(`name_sort` %in% c("Ireland", "Northern Ireland")) %>%
    dplyr::mutate (`iso_a2` = "IE (SEM)",
                   `name_sort` = "Ireland (SEM)") %>%
    merge.all.countries.shapes()

  return(SEM)
}

#' Compute the shape of the bidding-zone of Germany and Luxembourg
#' @importFrom dplyr filter mutate select
#' @return the shape as a data.frame
#' @inheritParams countries.shapes.raw scale
#' @example DE.LU.shape()
DE.LU.shape <- function (scale) {
  DELU <- rnaturalearth::ne_countries(country = c('germany', 'luxembourg'), scale = scale, type = 'map_units', returnclass = "sf") %>%
    dplyr::select(`name_sort`, `continent`, `geometry`) %>%
    dplyr::mutate (`iso_a2` = "DELU",
                   `name_sort` = "Germany - Luxembourg") %>%
    merge.all.countries.shapes()

  return(DELU)
}

#' Compute geographic shapes of countries
#'
#' @param only.keep.Europe whether to remove shapes beyond Europe
#' @importFrom dplyr if_else mutate rename
#' @importFrom eneRgycache cache.initialised
#' @importFrom sf sf_use_s2 st_crop st_make_valid
#' @return country shapes as a data.frame
#' @inheritParams countries.shapes.raw scale
#' @examples
#' countries.shapes(only.keep.Europe = TRUE)
#' countries.shapes(scale = 110, only.keep.Europe = FALSE)
countries.shapes <- function (scale = defaultRNaturalEarthScale, only.keep.Europe) {
  stopifnot (eneRgycache::cache.initialised())

  # retrieve the raw World shape
  World <- countries.shapes.raw (scale = scale)
  World[[rnaturalearthCountryIsoIdentifier]] <- dplyr::if_else(World[[rnaturalearthCountryNameIdentifier]] == "United Kingdom", "UK", World[[rnaturalearthCountryIsoIdentifier]])

  # only keep Europe (when relevant)
  if (only.keep.Europe) {
    # also keep neighboring countries
    additionalCountriesIso2 <- c ("CY", "DZ", "EG", "IL", "LB", "LY", "MA", "SY", "TN", "TR")

    # disable s2, otherwise sf::st_crop doesn't work
    suppressMessages(sf::sf_use_s2(FALSE))
    World <- subset(World, (`continent` == "Europe") | (`iso_a2` %in% additionalCountriesIso2)) %>%
      dplyr::mutate(`geometry` = suppressMessages(sf::st_make_valid(sf::st_crop(`geometry`, xmin = -14, xmax = 50, ymin = 30, ymax = 73))))
    suppressMessages(sf::sf_use_s2(TRUE))
  }

  # Create add-hoc shapes
  DELU <- DE.LU.shape(scale = scale)
  GBNI <- GB.NI.shapes(scale = scale)
  SEM <- IE.SEM.shape(scale = scale)

  worldUpdated <- dplyr::bind_rows (list(World, DELU, GBNI, SEM)) %>%
    dplyr::rename (`name` = `name_sort`, `iso2` = `iso_a2`)

  return (worldUpdated)
}

#' Cached countries shape function
#' @inherit countries.shapes
#' @inheritDotParams countries.shapes
#' @examples
#' eneRgymaps::countries.shapes.cached(scale = 110, only.keep.Europe = TRUE)
#' eneRgymaps::countries.shapes.cached(only.keep.Europe = FALSE)
#' @export
countries.shapes.cached <- eneRgycache::cache.function (fun = countries.shapes)


#' Merge all shapes of countries into one shape (all shapes with the same ISO2 and name-sort will become part of a single shape)
#' @param shapes.to.merge the shapes to merge, as a data.frame
#' @importFrom dplyr group_by mutate summarise
#' @importFrom sf sf_use_s2 st_cast st_make_valid
#' @return the merged shapes, as a data.frame
#' @examples
#' merge.all.countries.shapes(my.shapes)
merge.all.countries.shapes <- function (shapes.to.merge) {
  updatedData <- tryCatch({
    shapes.to.merge %>%
      dplyr::group_by(`iso_a2`, `name_sort`, `continent`) %>%
      dplyr::summarise(`geometry` = sf::st_make_valid(sf::st_union(`geometry`, by_feature = FALSE))) %>%
      dplyr::mutate (`geometry` = sf::st_cast(x = `geometry`, to = "MULTIPOLYGON"))
  }, error=function(cond) {
    suppressMessages(sf::sf_use_s2(FALSE))
    shapes.to.merge %>%
      dplyr::group_by(`iso_a2`, `name_sort`, `continent`) %>%
      dplyr::summarise(`geometry` = sf::st_make_valid(sf::st_union(`geometry`, by_feature = FALSE))) %>%
      dplyr::mutate (`geometry` = sf::st_cast(x = `geometry`, to = "MULTIPOLYGON"))
  }, finally = {
    suppressMessages(sf::sf_use_s2(TRUE))
  })

  return (updatedData)
}

#' Define the path in which to find the bidding-zones shapes
#' (depending on file format, the path may be a file or directory)
#' @param date the date for which to retrieve the shapes
#' @return the full path towards the directory including the shapes
#' (the most recent shape file before the date is selected)
#' @examples
#' bidding.zones.shapes.path(date = Sys.Date())
#' bidding.zones.shapes.path(date = as.Date("2017-10-25"))
bidding.zones.shapes.path <- function (date) {
  stopifnot (! is.null(date))

  # retrieve the dates associated with all directories
  genericOGRBZNDir <- file.path (eneRgymaps::maps.coordinates.dir(), "BZN")
  stopifnot(file.exists(genericOGRBZNDir))

  bznOGRDates <- base::list.dirs(genericOGRBZNDir, full.names = FALSE)
  bznOGRDates <- as.Date(bznOGRDates[nchar(bznOGRDates) > 0])
  bznOGRDates <- bznOGRDates [! is.na(bznOGRDates)]

  # select the latest date which is smaller than date
  stopifnot (any(bznOGRDates <= date))
  shapeDate <- max(bznOGRDates[bznOGRDates <= date])

  bznShapesDir <- file.path(genericOGRBZNDir, shapeDate)
  stopifnot(dir.exists(bznShapesDir))

  bznShapesPath <- file.path(bznShapesDir, "BZN.geojson")
  stopifnot(base::file.exists(bznShapesPath))

  return (bznShapesPath)
}

#' Compute detailed geographic shapes of bidding-zones
#'
#' @return the bidding-zone shapes
#' @importFrom geojsonsf geojson_sf
#' @inheritParams bidding.zones.shape.path date
#' @examples
#' bidding.zones.shapes(date = Sys.Date())
#' bidding.zones.shapes(date = as.Date("2017-10-25"))
bidding.zones.shapes <- function (date) {
  # bidding-zones' shapefile
  bzn <- geojsonsf::geojson_sf (geojson = bidding.zones.shapes.path(date = date)) # Shapefile of bidding-zones

  return (bzn)
}

#' Cached bidding-zone shapes function
#' @inherit bidding.zones.shapes
#' @examples
#' bidding.zones.shapes.cached(date = Sys.Date())
#' bidding.zones.shapes.cached(date = as.Date("2017-10-25"))
#' @export
bidding.zones.shapes.cached <- eneRgycache::cache.function (fun = bidding.zones.shapes)


#' Compute a detailed shape of German TSO areas
#' (assuming that the TSO areas approximately match the German landers)
#' @return the shapes of the German TSO areas
#' @importFrom dplyr filter group_by mutate select summarise
#' @importFrom rnaturalearth ne_states
#' @importFrom sf st_cast st_union
#' @importFrom utils read.csv
#' @return shapes of German TSOs (identified with EIC codes) as a data.frame
#' @examples
#' DE.TSOs.shapes()
DE.TSOs.shapes <- function() {
  OGRCADir <- file.path (eneRgymaps::maps.coordinates.dir(), "LFCA")
  stopifnot (file.exists(OGRCADir))

  DELandsTSOs <- utils::read.csv (file = file.path(OGRCADir, "German_TSOs.csv"),
                                  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  DELandsTSOsDict <- eneRgymaps::create.dictionary(keys = DELandsTSOs$Bundesland, values = DELandsTSOs$TSO.EIC.code)
  DELandsShapes <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf") %>%
    dplyr::select(`name`, `geometry`)

  # rename all land names to become TSO EIC codes
  DELandsShapes[["eic_code"]] <- eneRgymaps::apply.dictionary(keys = DELandsShapes[["name"]],
                                                              dictionary = DELandsTSOsDict)
  DETSOsShapes <- DELandsShapes %>%
    dplyr::select (`eic_code`, `geometry`)

  # also retrieve Germany/Luxembourg as a whole (load frequency control area)
  DELU <- DE.LU.shape(scale = defaultRNaturalEarthScale) %>%
    dplyr::select(`geometry`) %>%
    dplyr::mutate(`eic_code` = eneRgymaps::eic.code.DELU())

  FullDETSOsShapes <- dplyr::bind_rows (list(DELandsShapes, DELU)) %>%
    dplyr::group_by(`eic_code`) %>%
    dplyr::summarise(`geometry` = sf::st_union(`geometry`, by_feature = FALSE)) %>%
    dplyr::mutate (`geometry` = sf::st_make_valid(sf::st_cast(x = `geometry`, to = "MULTIPOLYGON")))

  return (FullDETSOsShapes)
}

#' Compute raw control areas' geographic shapes
#'
#' @return the control areas shape as a data.frame
#' @importFrom dplyr mutate rename select
#' @importFrom sf read_sf st_make_valid
#' @examples
#' control.areas.raw.shapes()
control.areas.raw.shapes <- function() {
  geographicDataDir <- eneRgymaps::maps.coordinates.dir()
  OGRCADir <- file.path (geographicDataDir, "LFCA")
  stopifnot(file.exists(OGRCADir))

  # control areas' shapefile
  # (ignore a warning related to OGR_GEOMETRY_ACCEPT_UNCLOSED_RING)
  ca <- suppressWarnings(sf::read_sf(dsn = OGRCADir, layer = "CA")) %>%
    dplyr::select(`FID`, `geometry`) %>%
    dplyr::rename(`eic_code` = `FID`) %>%
    dplyr::mutate(`geometry` = sf::st_make_valid(`geometry`))

  return (ca)
}

#' Compute the geographic shapes of control areas
#'
#' @return the control areas shape as data.frame
#' @importFrom dplyr bind_rows
#' @examples
#' control.areas.shapes()
control.areas.shapes <- function() {
  return (dplyr::bind_rows (list(control.areas.raw.shapes(), DE.TSOs.shapes())))
}

#' Cached control areas shapes function
#' @inherit control.areas.shapes
#' @examples
#' eneRgymaps::control.areas.shapes.cached()
#' @export
control.areas.shapes.cached <- eneRgycache::cache.function (fun = control.areas.shapes)


#' Check variable names (to ensure that there is no forbidden character)
#' @param variable.names the variable names as a vector
#' @importFrom stats na.omit
#' @return whether the variable are coherent
#' @examples
#' eneRgymaps::coherent.map.variable.names.formatting(c("XB-disc", NULL))
#' eneRgymaps::coherent.map.variable.names.formatting(variable.names = c("XB-disc", "A ", NULL))
#' @export
coherent.map.variable.names.formatting <- function (variable.names) {
  filteredVariableNames <- variable.names %>% stats::na.omit() %>% unique()

  # ensure that no forbidden character is included in any variable name
  forbiddenCharacters <- c("`", " ", "@")

  coherentVarNames <- TRUE
  for (char in forbiddenCharacters) {
    charIn <- sapply(X = filteredVariableNames, FUN = function (x) {return (grepl(pattern = char, x = x))})
    coherentVarNames <- coherentVarNames && (! (TRUE %in% charIn))

    if (! coherentVarNames) { # one forbidden character found -> no need to go further
      break
    }
  }

  return (coherentVarNames)
}

#' Retrieve the geographic shapes relevant for a given geographic granularity
#' @param geographic.entity.name the name of the geographic entity ("Country", "Bidding zone" or "Control area")
#' @param date the date for which to retrieve the geographic shapes (only applies to bidding-zones)
#' @param only.keep.Europe whether to only keep shapes very close to Europe (only applies to countries)
#' @param with.centroids whether to add the centroids to the data (to e.g. add labels in the center of shapes)
#' @return the shapes definition as a data.frame
#' @importFrom eneRgycache cache.initialised
#' @examples
#' eneRgymaps::geographic.shapes ("Country", only.keep.Europe = FALSE)
#' eneRgymaps::geographic.shapes (geographic.entity.name = "Control area")
#' @export
geographic.shapes <- function (geographic.entity.name, date = base::Sys.Date(),
                               only.keep.Europe = NULL, with.centroids = FALSE) {
  stopifnot (eneRgycache::cache.initialised())

  # Only country, control area and bidding zone maps are currently supported
  stopifnot(eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name))

  data <- NULL
  if (geographic.entity.name == "Country") {
    stopifnot (! is.null (only.keep.Europe))
    data <- eneRgymaps::countries.shapes.cached(only.keep.Europe = only.keep.Europe)
  } else if (geographic.entity.name == "Bidding zone") {
    data <- eneRgymaps::bidding.zones.shapes.cached(date = date)
  } else if (geographic.entity.name == "Control area") {
    data <- eneRgymaps::control.areas.shapes.cached()
  } else {
    # bug : should not get there
    stopifnot(FALSE)
  }

  if (with.centroids) {
    data <- eneRgymaps::add.centroids(data)
  }

  return (data)
}

#' Add centroids to geographic shapes (to e.g. enable adding labels in the centre of each shape)
#' @param geographic.shapes the shapes to format, as a data.frame
#' @return an updated data.frame, with 3 more columns: "centroid" (as POINT), "centroid_longitude", "centroid_latitude"
#' @importFrom dplyr mutate
#' @importFrom sf sf_use_s2 st_centroid st_coordinates
#' @examples
#' eneRgymaps::add.centroids (myShapes)
#' @export
add.centroids <- function (geographic.shapes) {
  stopifnot("geometry" %in% colnames(geographic.shapes))
  stopifnot(! any(c("centroid", "centroid_longitude", "centroid_latitude") %in% colnames(geographic.shapes)))

  updatedData <- tryCatch({
    geographic.shapes %>%
      dplyr::mutate(`centroid` = suppressWarnings(sf::st_centroid(`geometry`)),
                    `centroid_longitude` = sf::st_coordinates(`centroid`)[,1],
                    `centroid_latitude` = sf::st_coordinates(`centroid`)[,2])
  }, error=function(cond) {
    suppressMessages(sf::sf_use_s2(FALSE))
    geographic.shapes %>%
      dplyr::mutate(`centroid` = suppressWarnings(sf::st_centroid(`geometry`)),
                    `centroid_longitude` = sf::st_coordinates(`centroid`)[,1],
                    `centroid_latitude` = sf::st_coordinates(`centroid`)[,2])
  }, finally = {
    suppressMessages(sf::sf_use_s2(TRUE))
  })
  return (updatedData)
}

#' Clean the geographic shapes associated with maps, by removing duplicate shapes which are not going to be displayed
#' @param geographic.data the geographic data as a data.frame
#' @param geographic.entity.name the name of the geographic entity depicted (e.g. "Country")
#' @param id.column.name the name of the column including the identifiers of the geographic items
#' @param GB.NI.in.map whether GB and NI are defined (instead of UK)
#' @param IE.SEM.in.map whether IE (SEM) is defined, instead of IE and NI
#' @param separate.DE.LU.TSOs.in.map whether German and Luxembourgian TSOs are separately displayed
#' @return the updated geographic shapes as a data.frame
#' @examples
#' eneRgymaps::clean.map.shapes.duplicates (geographic.data = geographicData, geographic.entity.name = "Country", GB.NI.in.map = TRUE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)
#' @export
clean.map.shapes.duplicates <- function (geographic.data, geographic.entity.name,
                                         id.column.name,
                                         GB.NI.in.map, IE.SEM.in.map,
                                         separate.DE.LU.TSOs.in.map) {
  # Only country, control area and bidding zone maps are currently supported
  stopifnot (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name))

  # the data frame should include the identifiers column
  stopifnot (id.column.name %in% colnames (geographic.data))

  # compute the list of ids to ignore
  IdsToIgnore <- c()
  if (geographic.entity.name == "Country") {
    # remove individual IE/NI country shapes when IE (SEM) is defined, otherwise remove IE (SEM)
    if (IE.SEM.in.map) {
      IdsToIgnore <- c (IdsToIgnore, "IE", "NI")
    } else {
      IdsToIgnore <- c (IdsToIgnore, "IE (SEM)")
    }

    # remove UK when relying on GB/NI, (otherwise remove GB/NI)
    if (GB.NI.in.map) {
      IdsToIgnore <- c (IdsToIgnore, "UK")
    } else {
      IdsToIgnore <- c (IdsToIgnore, "GB", "NI")
    }

  } else if (geographic.entity.name == "Control area") {
    if (separate.DE.LU.TSOs.in.map) {
      IdsToIgnore <- c (IdsToIgnore, eneRgymaps::eic.code.DELU())
    } else {
      IdsToIgnore <- c (IdsToIgnore, eneRgymaps::eic.codes.DE.TSOs(), eneRgymaps::eic.code.LU.TSO())
    }
  }

  geographicDataFiltered <- geographic.data [! geographic.data [[id.column.name]] %in% IdsToIgnore,]

  return (geographicDataFiltered)
}

#' Change negative oriented values into positive values (and flip the arrow) when displaying oriented values with an arrow
#'
#' @param plotData the data for which to fill the map as a data.frame
#' @param id.field.name the name of the column to use when mapping borders
#' @param in.id.field.name the name of the column to use when mapping the area EIC code at the beginning of the oriented border
#' @param out.id.field.name the name of the column to use when mapping the area EIC code at the end of the oriented border
#' @param width.value.field.name the field name to use to adapt the symbol width
#' @param label.value.field.name the field name to use to set the symbol label
#' @return the plot data set as a data.frame
#' @importFrom dplyr bind_rows if_else select
#' @export
flip.negative.oriented.border.values <- function (plotData,
                                                  id.field.name, in.id.field.name, out.id.field.name,
                                                  width.value.field.name = NULL, label.value.field.name = NULL) {

  # all arguments with no default value must not be NULL
  stopifnot (length(plotData) > 0)
  stopifnot (! is.null(id.field.name))
  stopifnot (! is.null(in.id.field.name))
  stopifnot (! is.null(out.id.field.name))

  # check which variable is going to set the arrow orientation
  variables.setting.orientation.raw <- unique(c(width.value.field.name, label.value.field.name))
  stopifnot (length(variables.setting.orientation.raw) >= 1)
  stopifnot(all(variables.setting.orientation.raw %in% colnames (plotData)))
  variables.setting.orientation <- c()

  # check if there is at least one negative value related to the variable
  for (variableName in variables.setting.orientation.raw) {
    if (any(plotData [[variableName]] < 0)) {
      variables.setting.orientation <- c (variables.setting.orientation, variableName)
    }
  }

  # no negative data -> nothing to do
  if (length(variables.setting.orientation) == 0) {
    return (plotData)
  }

  stopifnot (length(variables.setting.orientation) == 1)
  variable.setting.orientation <- variables.setting.orientation [[1]]

  # check whether the fields are inside the data
  stopifnot (all (c (id.field.name, in.id.field.name, out.id.field.name, variable.setting.orientation) %in% colnames (plotData)))

  # check which separator was applied when describing borders
  # by looking at the first border name
  separators <- c(" > ", " - ", ">", "-")
  testValue <- plotData [[id.field.name]] [1]

  for (separator in separators) {
    # update the table based on the first separator found
    if (grepl(separator, testValue, fixed = TRUE)) {
      # ensure that creating temporary variables will not damage the data structure
      stopifnot (! any(c("toReverse", "initialInIdFieldName") %in% colnames(plotData)))

      # keep rows with NA away from filtering
      plotDataNA <- plotData [is.na(variable.setting.orientation),]

      # when a value is negative
      # - flip the border name
      # - flip the value setting the orientation
      # - flip the arrow beginning and end
      interimPlotData <- plotData[! is.na(variable.setting.orientation),]

      interimPlotData [["toReverse"]] <- interimPlotData [[variable.setting.orientation]] < 0
      interimPlotData [[variable.setting.orientation]] <- dplyr::if_else (interimPlotData [["toReverse"]], - interimPlotData [[variable.setting.orientation]], interimPlotData [[variable.setting.orientation]])
      interimPlotData [[id.field.name]] <- dplyr::if_else (interimPlotData [["toReverse"]], eneRgymaps::reverse.border.name (border.name = interimPlotData [[id.field.name]], separator = separator), interimPlotData [[id.field.name]])
      interimPlotData [["initialInIdFieldName"]] <- interimPlotData [[in.id.field.name]]
      interimPlotData [[in.id.field.name]] <- dplyr::if_else (interimPlotData [["toReverse"]], interimPlotData [[out.id.field.name]], interimPlotData [[in.id.field.name]])
      interimPlotData [[out.id.field.name]] <- dplyr::if_else (interimPlotData [["toReverse"]], interimPlotData [["initialInIdFieldName"]], interimPlotData [[out.id.field.name]])

      # remove intermediate columns
      interimPlotData <- interimPlotData %>%
        dplyr::select (- c(`toReverse`, `initialInIdFieldName`))

      # add rows with NA in the end and sort rows by id
      updatedPlotData <- dplyr::bind_rows (list(interimPlotData, plotDataNA))

      # sanity checks
      # the updated data set must have the same number of rows and columns as the original data set
      # it must also have the same column names
      stopifnot (nrow (updatedPlotData) == nrow (plotData))
      stopifnot (ncol (updatedPlotData) == ncol (plotData))
      stopifnot (colnames (updatedPlotData) == colnames (plotData))

      return (updatedPlotData)
    }
  }
}

#' Retrieve the names of geographic entities
#'
#' @param geographic.entity.name name of the geographic entities for which to display labels ("Bidding zone", "Control area"/"LFC area", "Country"...)
#' @param political.initiatives a vector of political initiatives to filter (either NULL or "EU" - European Union, "EC" - Energy Commnuity)
#' @return a data.frame with identifiers under column "id", labels under column "label". Double spaces in labels are converted into \n
#' @importFrom dplyr all_vars filter filter_at select
#' @importFrom eneRgycache cache.initialised
#' @importFrom readxl read_xlsx
#' @examples
#' eneRgymaps::geographic.entity.labels ("Bidding zone")
#' eneRgymaps::geographic.entity.labels (geographic.entity.name = "Control area", political.initiatives = c ("EC", "EU"))
#' @export
geographic.entity.labels <- function (geographic.entity.name, political.initiatives = NULL) {

  # Only country, control area and bidding zone maps are currently supported
  stopifnot(eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name))

  stopifnot ((is.null (political.initiatives)) || (all (political.initiatives %in% c ("EC", "EFTA", "EU"))))

  # retrieve centre points' coordinates
  entityLabelsPath <- file.path (eneRgymaps::maps.coordinates.dir(), "Points", "Points.xlsx")
  entityLabels <- readxl::read_xlsx (path = entityLabelsPath, sheet = "Centre points", trim_ws = TRUE, col_names = TRUE)

  # set the names of variables to be used to generate the labels
  myFieldnamesDict <- eneRgymaps::create.dictionary (keys = c ("Bidding zone", "Country", "Control area"), values = c ("Bidding zone", "Country ISO2", "Control area"))
  geographicEntityLabelFieldName <- eneRgymaps::get.dictionary.value (key = geographic.entity.name, dictionary = myFieldnamesDict)

  geographicEntityIdFieldName <- "EIC code"
  if (geographic.entity.name == "Country") {
    geographicEntityIdFieldName <- "Country ISO2"
  }

  variablesToKeep <- unique(c(geographicEntityIdFieldName, geographicEntityLabelFieldName, "Political initiative"))

  # only keep the relevant columns from the dataset
  # and ignore entity labels, which are NAs
  entityLabels <- entityLabels %>%
    dplyr::select(dplyr::all_of(variablesToKeep)) %>%
    dplyr::filter_at (geographicEntityIdFieldName, dplyr::all_vars(! is.na (.)))

  # filter by political initiative(s), when applicable
  if (! is.null (political.initiatives)) {
    entityLabels <- entityLabels %>%
      dplyr::filter(`Political initiative` %in% political.initiatives)
  }

  # replace double spaces with line breaks
  entityLabels [[geographicEntityLabelFieldName]] <- gsub (pattern = "  ", replacement = "\n", x = entityLabels [[geographicEntityLabelFieldName]])# replace double spaces with line breaks

  # there should not be any duplicate id
  stopifnot (length (unique (entityLabels [[geographicEntityIdFieldName]])) == length (entityLabels [[geographicEntityIdFieldName]]))

  # "id" and "label should not be in the entityLabels columns
  stopifnot (! "id" %in% colnames (entityLabels))
  stopifnot (! "label" %in% colnames (entityLabels))

  entityLabels [["id"]] <- entityLabels [[geographicEntityIdFieldName]]
  entityLabels [["label"]] <- entityLabels [[geographicEntityLabelFieldName]]

  entityLabels <- entityLabels %>%
    dplyr::select (`id`, `label`)

  return (entityLabels)
}


#' Retrieve the coordinates related to the border points of geographic entities
#'
#' @return a data.frame with identifiers under columns "out_area_code" and "in_area_code", and coordinates under "coor_out_lat", "coor_out_long", "coor_in_lat", "coor_in_long", "coor_cent_lat", "coor_cent_long"
#' @importFrom readxl read_xlsx
#' @importFrom stats na.omit
#' @examples
#' eneRgymaps::geographic.border.points ()
#' @export
geographic.border.points <- function () {
  crossBorderDFFields <- c("out_area_code", "in_area_code", "coor_out_long", "coor_out_lat",
                           "coor_in_long", "coor_in_lat", "coor_cent_lat", "coor_cent_long")

  geographicDataDir <- eneRgymaps::maps.coordinates.dir()
  BZNCrossBorderPoints <- file.path(geographicDataDir, "Points", "Points.xlsx")
  stopifnot(file.exists(BZNCrossBorderPoints))

  borderPoints <- readxl::read_xlsx (path = BZNCrossBorderPoints, sheet = "Border points",
                                     trim_ws = TRUE, col_names = TRUE) %>% stats::na.omit() %>%
    # use pre-defined names within the code, and avoid changing naming conventions => rename columns when importing
    dplyr::rename (`out_area_code` = `from_area_code`, `in_area_code` = `to_area_code`,
                   `coor_out_lat` = `from_latitude`, `coor_out_long` = `from_longitude`,
                   `coor_in_lat` = `to_latitude`, `coor_in_long` = `to_longitude`,
                   `coor_cent_lat` = `centre_latitude`, `coor_cent_long` = `centre_longitude`) %>%
    dplyr::select(dplyr::all_of(crossBorderDFFields))

  # flip the borders in order to get both directions per border
  borderPointsReverse <- borderPoints %>%
    dplyr::rename(`out_area_code` = `in_area_code`, `in_area_code` = `out_area_code`,
                  `coor_in_lat` = `coor_out_lat`, `coor_in_long` = `coor_out_long`,
                  `coor_out_lat` = `coor_in_lat`, `coor_out_long` = `coor_in_long`)

  return (dplyr::bind_rows (list(borderPoints, borderPointsReverse)))
}


#' Add geographic coordinates to a data frame
#'
#' @param data the data set to enrich (as a data.frame)
#' @param data.id.field.name the id field name(s) to merge the data (will be mapped with geographic.coordinates.data.id.field.name)
#' @param geographic.coordinates.data the geographic coordinates data set to use (as a data.frame)
#' @param geographic.coordinates.data.id.field.name the geographic coordinates data id field name(s) to merge the data (will be mapped with data.id.field.name)
#' @param geographic.coordinates.data.field.names the field name to retain from the geographic coordinates data set
#' @return the merged data set including geographic coordinates as a data.frame
#' @importFrom dplyr all_of filter rename_with select
#' @examples
#' eneRgymaps::enrich.with.geographic.coordinates (myData, "country_ISO", geogData, "id", c ("longitude", "latitude"))
#' eneRgymaps::enrich.with.geographic.coordinates (data = myData, data.id.field.name = "country_ISO", geographic.coordinates.data = geogData,
#'                                     geographic.coordinates.data.id.field.name = "id", geographic.coordinates.data.field.names = c ("longitude", "latitude"))
#' @export
enrich.with.geographic.coordinates <- function (data, data.id.field.name,
                                                geographic.coordinates.data, geographic.coordinates.data.id.field.name, geographic.coordinates.data.field.names) {
  # empty data => nothing to do
  if ((length (data) == 0) || (nrow (data) == 0)) {
    return (data)
  }

  # the data id field name should be in the data
  stopifnot (data.id.field.name %in% colnames (data))

  # all geographic coordinates' fields should be in the geographic data set
  geographicCoordinatesDataFields <- unique (c (geographic.coordinates.data.id.field.name, geographic.coordinates.data.field.names))

  geographicCoordinatesData <- geographic.coordinates.data %>%
    dplyr::select (dplyr::all_of(geographicCoordinatesDataFields))

  # the coordinates field should not lead to duplicate columns
  # (the identifier columns should not be taken into account as they will be used for merging)
  idsToIgnoreData <- c (data.id.field.name)
  idsToTestData <- colnames (data)[! colnames (data) %in% idsToIgnoreData]
  idsToIgnoreGeogCoordData <- c (geographic.coordinates.data.id.field.name)
  idsToTestGeogCoordData <- colnames (geographicCoordinatesData)[! colnames (geographicCoordinatesData) %in% idsToIgnoreGeogCoordData]
  stopifnot (! any (idsToTestGeogCoordData %in% colnames (data)))
  stopifnot (! any (idsToTestData %in% colnames (geographicCoordinatesData)))

  # unique field names should be defined (in case list of ids are defined)
  stopifnot(identical (data.id.field.name, unique(data.id.field.name)))
  stopifnot(identical (geographic.coordinates.data.id.field.name, unique(geographic.coordinates.data.id.field.name)))
  stopifnot(identical (geographic.coordinates.data.field.names, unique(geographic.coordinates.data.field.names)))

  # define the geographic data as "x" data frame, to maintain the sf class
  # rename the id column name(s) to keep the initial id.field.name column name(s)
  dataWithCoordinates <- base::merge (x = geographicCoordinatesData, by.x = geographic.coordinates.data.id.field.name,
                                      y = data, by.y = data.id.field.name,
                                      all = FALSE) %>%
    dplyr::filter (! anyNA (geographic.coordinates.data.field.names)) %>%
    dplyr::rename_with(.cols = dplyr::all_of(geographic.coordinates.data.id.field.name), .fn = ~data.id.field.name)

  return (dataWithCoordinates)
}

#' Check whether a given legend position argument is valid
#'
#' @param legend.position the legend.position to check
#' @return whether the legend position is relevant
#' @importFrom dplyr between
#' @examples
#' eneRgymaps::is.relevant.legend.position ("top")
#' eneRgymaps::is.relevant.legend.position (legend.position =  c(0.5, 0.5))
#' @export
is.relevant.legend.position <- function (legend.position) {
  if (length (legend.position) == 1) {
    # either the position is a string, then it should be one of the values below
    return (legend.position %in% c ("none", "left", "top", "right", "bottom"))
  } else if (length (legend.position) == 2) {
    # or it is a vector of two numbers between zero and 1
    allNumbers <- all (is.numeric (legend.position) & (! is.na (legend.position)))
    if (allNumbers) { # need for two separate conditions because R does not shortcut condition evaluations
      return (all (dplyr::between(x = legend.position, left = 0, right = 1)))
    } else {
      return (FALSE)
    }
  } else {
    return (FALSE)
  }
}


#' Generate a given list of ordered legend names relating to a data set (in order to ease legend readability)
#'
#' @param keys.used a list of keys used in the data set (usually the list of raw values used to depict the field linked with the legend)
#' @param legend.keys a list of legend keys
#' @param legend.values a list of legend values
#' @return ordered legend values to be used to ease readability of the legend (or NULL if no legend is used)
#' @importFrom stats na.omit
#' @examples
#' ordered.legend.names (keys.used = c (1, 2, 3), legend.keys = c (1, 2, 3, 4), legend.values = c ("one", "two", "three", "four"))
ordered.legend.names <- function (keys.used, legend.keys, legend.values) {
  # legend keys should only be set along with legend values (and both should have the same size)
  stopifnot (length (legend.keys) == length (legend.values))

  orderedValuesNames <- NULL
  if (length (keys.used) > 0) {
    uniqueKeysUsedSorted <- sort (unique (stats::na.omit(keys.used)))

    if (! is.null (legend.keys)) {
      # all fill values should be inside the legend keys (when legend keys are set)
      stopifnot (all (uniqueKeysUsedSorted %in% legend.keys))
      keysIndexes <- which (legend.keys %in% uniqueKeysUsedSorted, arr.ind = TRUE)
      legendKeysFiltered <- legend.keys [keysIndexes]
      legendValuesFiltered <- legend.values [keysIndexes]
      legendDictionary <- eneRgymaps::create.dictionary (keys = legendKeysFiltered, values = legendValuesFiltered)
      orderedValuesNames <- eneRgymaps::apply.dictionary (keys = sort(legendKeysFiltered), dictionary = legendDictionary)
    }
  }

  return (orderedValuesNames)
}


#' Check whether a given shape identifier is relevant
#'
#' @param shape.identifier a shape identifier (either of "line", "arrow", integer -> fixed shape defined by integer)
#' @return whether the identifier is relevant
#' @examples
#' eneRgymaps::is.relevant.shape.identifier ("arrow")
#' eneRgymaps::is.relevant.shape.identifier (shape.identifier = 16)
#' @export
is.relevant.shape.identifier <- function (shape.identifier) {
  allowedStringShapes <- c ("line", "arrow")
  if (shape.identifier %in% allowedStringShapes) {
    return (TRUE)
  } else {
    return ((eneRgymaps::integers.all(dataset = c (shape.identifier))) && (shape.identifier >= 0))
  }
}
