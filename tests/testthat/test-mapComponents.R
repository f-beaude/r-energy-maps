testthat::context("Test geographic maps components functions")

testthat::test_that("Testing world background", {
  eneRgycache::initialise.cache()

  testthat::expect_true ({bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE); bg; TRUE})
  testthat::expect_true ({bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = FALSE); bg; TRUE})

  # no border depicted
  testthat::expect_true ({bg <- eneRgymaps::world.background(fill.color = "grey", border.color = NULL, only.keep.Europe = TRUE); bg; TRUE})

  # ISO2 to ignore
  testthat::expect_true ({bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE, iso2s.to.ignore = c ("BY", "FR", "DZ", "EG", "LY", "MA", "RU", "TN", "TR", "UA")); bg; TRUE})

  # Message: some ISO2 to ignore are not present in the initial data set
  testthat::expect_message (eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE, iso2s.to.ignore = c ("BY", "FR", "DZ", "EG", "LY", "MA", "RU", "TN", "TR", "UA", "KT", "YY")))
})

testthat::test_that("Testing add.geographic.entity.names", {
  eneRgycache::initialise.cache()

  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country"); plot; TRUE})

  # setting date for geographic entity name
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Bidding zone", date = as.Date("2017-01-01")); plot; TRUE})

  # if font.size = 0, the plot should be identical to the input
  plotIdentical <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country", font.size = 0)
  testthat::expect_identical(bg, plotIdentical)

  # custom items to keep
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country", items.to.keep = c("DE", "PL", "ES")); plot; TRUE})

  # custom items to remove
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country", items.to.remove = c("DE", "PL", "ES")); plot; TRUE})

  # predefined initiative
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country", political.initiatives = c ("EFTA", "EU")); plot; TRUE})

  # LFC areas
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Control area"); plot; TRUE})

  # Bidding-zones
  testthat::expect_true ({plot <- eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Bidding zone"); plot; TRUE})

  # error: cannot define both items.to.keep and items.to.remove
  testthat::expect_error (eneRgymaps::add.geographic.entity.names(plot = bg, geographic.entity.name = "Country", items.to.keep = c("DE", "PL", "ES"), items.to.remove = c("DK")))
})

testthat::test_that("Testing configure.colors", {
  eneRgycache::initialise.cache()

  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  pricesData <- utils::read.csv(file = file.path(eneRgymaps::example.data.dir(), "Prices.csv"), header = TRUE,
                                fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
    dplyr::filter(`year` == 2019) %>%
    dplyr::mutate(`price` = dplyr::if_else(`area_code` == "10YCA-BULGARIA-R", NA, `price`))
  BZsShapes <- eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone")
  pricesDataGeographic <- base::merge (x = pricesData, by.x = "area_code",
                                       y = BZsShapes, by.y = "eic_code", all = FALSE)

  continuousVariableMap <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                          fill.color.field = "price",
                                                                          border.color = "white")

  discreteVariableMap <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                 fill.color.field = "discrete_value",
                                                                 border.color = "white")

  # do nothing
  testthat::expect_equal(continuousVariableMap, eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = NULL,
                                                                             configure.type = "fill", field.name = "price", color.palette = "Blues"))
  testthat::expect_equal(discreteVariableMap, eneRgymaps::configure.colors(geographic.map = discreteVariableMap, data = pricesData,
                                                                             configure.type = "fill", field.name = NULL, color.palette = "Blues"))

  # adding color palette
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", color.palette = "Blues"); plot; TRUE})

  # adding palette and legend
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", color.palette = "Blues", legend.name = "Prices"); plot; TRUE})

  # customising the NA color
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", color.palette = "Blues", na.color = "red"); plot; TRUE})

  # standard gradient colors
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green")); plot; TRUE})

  # gradient colors with absolute thresholds
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green"),
                                                               gradient.values = c(25, 75)); plot; TRUE})

  # gradient colors with relative thresholds
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green"),
                                                               gradient.perc.values = c(0, 1.1)); plot; TRUE})

  # min and max values
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", color.palette = "Blues",
                                                               min.value = 35, max.value = 50, legend.name = "Prices"); plot; TRUE})

  # discrete values with list of colors
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = discreteVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "discrete_value", colors.list = eneRgymaps::palette.colors.sample(length = 3),
                                                               legend.name = "Discrete value"); plot; TRUE})

  # discrete values with color palette
  testthat::expect_true ({plot <- eneRgymaps::configure.colors(geographic.map = discreteVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "discrete_value", color.palette = "Blues",
                                                               legend.name = "Discrete value"); plot; TRUE})

  # warning: the absolute color gradient thresholds do not cover the whole scope of values
  testthat::expect_warning (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                               configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green"),
                                                               gradient.values = c(35, 45)))

  # warning: the relative color gradient thresholds do not cover the whole scope of values
  testthat::expect_warning (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                         configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green"),
                                                         gradient.perc.values = c(0.1, 0.75)))

  # error: field name not in data set
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "myField", color.palette = "Blues"))

  # error: invalid configure.type
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "shades", field.name = "price", color.palette = "Blues"))

  # error: gradient.colors must include more than one color
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "price", gradient.colors = c("yellow"),
                                                       gradient.perc.values = c(0)))

  # error: only min.value defined (without max.value)
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                               configure.type = "fill", field.name = "price", color.palette = "Blues",
                               min.value = 35, legend.name = "Prices"))

  # error: only max.value defined (without min.value)
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "price", color.palette = "Blues",
                                                       max.value = 50, legend.name = "Prices"))

  # error: both gradient.values and gradient.perc.values defined
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "price", gradient.colors = c("yellow", "green"),
                                                       gradient.values = c(25, 75), gradient.perc.values = c(0.1, 0.75)))

  # error: defined both palette and colors.list
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "price",
                                                       color.palette = "Blues", colors.list = eneRgymaps::palette.colors.sample(length = 3)))

  # error: defined both palette and gradient.colors
  testthat::expect_error (eneRgymaps::configure.colors(geographic.map = continuousVariableMap, data = pricesData,
                                                       configure.type = "fill", field.name = "price",
                                                       color.palette = "Blues", gradient.colors = c("yellow", "green"),
                                                       gradient.perc.values = c(0.1, 0.75)))
})

testthat::test_that("Testing add.fill.border", {
  eneRgycache::initialise.cache()
  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  pricesData <- utils::read.csv(file = file.path(eneRgymaps::example.data.dir(), "Prices.csv"), header = TRUE,
                                fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
    dplyr::filter(`year` == 2019)
  BZsShapes <- eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone")
  pricesDataGeographic <- base::merge (x = pricesData, by.x = "area_code",
                                 y = BZsShapes, by.y = "eic_code", all = FALSE)

  # basic add.fill.and.border (continuous variable)
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price",
                                                                         border.color = "white"); map; TRUE})

  # add.fill.and.border (discrete variable)
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "discrete_value", fill.keep.NA = FALSE,
                                                                         border.color = "white"); map; TRUE})

  # add.fill.and.border (customised border.width and border.linetype)
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price", fill.keep.NA = FALSE,
                                                                         border.color = "white", border.width = 5, border.linetype = "dashed"); map; TRUE})
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price", fill.keep.NA = FALSE,
                                                                         border.color = "white", border.width = 5, border.linetype = 3313); map; TRUE})

  # fixed fill color
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color = "red", border.color = "white"); map; TRUE})

  # keep NAs
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price", fill.keep.NA = TRUE,
                                                                         border.color = "white"); map; TRUE})

  # no border
  testthat::expect_true ({map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price",
                                                                         border.color = "white", border.width = 0); map; TRUE})


  # error : both fill.color and fill.color.field defined
  testthat::expect_error(map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                        fill.color.field = "price", fill.color = "red",
                                                                        border.color = "white"))

  # error : non-numeric border.width
  testthat::expect_error (map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price",
                                                                         border.color = "white", border.width = "five"))

  # error : negative width
  testthat::expect_error (map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "price",
                                                                         border.color = "white", border.width = -1))

  # error : fill.color.field is not in the data set
  testthat::expect_error (map <- bg %>% eneRgymaps::add.fill.and.border (data = pricesDataGeographic,
                                                                         fill.color.field = "anotherPrice",
                                                                         border.color = "white"))
})


testthat::test_that("Testing add.title.caption", {
  eneRgycache::initialise.cache()
  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "yellow", only.keep.Europe = TRUE)

  # standard title
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (title.text = "My title"); myUpdatedMap; TRUE})

  # customised title
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (title.text = "My title", title.size = 25, title.color = "forestgreen", title.face = "bold"); myUpdatedMap; TRUE})

  # customised title with float size
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (title.text = "My title", title.size = 21.5, title.color = "forestgreen"); myUpdatedMap; TRUE})

  # standard caption
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (caption.text = "A caption"); myUpdatedMap; TRUE})

  # customised caption
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (caption.text = "A caption", caption.size = 13.1, caption.color = "red", caption.face = "italic"); myUpdatedMap; TRUE})

  # customised title and caption
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.title.caption (title.text = "My title", title.size = 25, title.color = "forestgreen", caption.text = "A caption", caption.face = "bold"); myUpdatedMap; TRUE})

  # neither title nor caption
  testthat::expect_true (identical (bg, eneRgymaps::add.title.caption (geographic.map = bg)))
  testthat::expect_true (identical (bg, eneRgymaps::add.title.caption (geographic.map = bg, title.text = "An invisible title", title.size = 0)))
  testthat::expect_true (identical (bg, eneRgymaps::add.title.caption (geographic.map = bg, caption.text = "An invisible caption", caption.size = 0)))

  # error : non-numeric title size
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, title.text = "My title", title.size = "ten"))

  # error : negative title size
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, title.text = "My title", title.size = -1))

  # error : bad title color name
  testthat::expect_error (suppressWarnings(eneRgymaps::add.title.caption (geographic.map = bg, title.text = "My title", title.color = "myNewBlue")))

  # error: bad title face
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, title.text = "My title", title.face = "horizontal"))

  # error : non-numeric caption size
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, caption.text = "My title", caption.size = "ten"))

  # error : negative caption size
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, caption.text = "My title", caption.size = -1))

  # error : bad caption color name
  testthat::expect_error (suppressWarnings(eneRgymaps::add.title.caption (geographic.map = bg, caption.text = "My title", caption.color = "myNewBlue")))

  # error: bad caption face
  testthat::expect_error (eneRgymaps::add.title.caption (geographic.map = bg, caption.text = "My title", caption.face = "horizontal"))
})


testthat::test_that("Testing add.shapes", {
  eneRgycache::initialise.cache()
  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  exampleDataDir <- eneRgymaps::example.data.dir()

  lineCoordinateIdentifiers <- c (NA, NA, "long1", "lat1", "long2", "lat2")
  pointCoordinateIdentifiers <- c ("long1", "lat1", rep.int(NA, 4))

  LinesData <- readxl::read_excel(path = file.path(exampleDataDir, "Lines.xlsx")) %>%
    dplyr::mutate(`loading` = dplyr::if_else(`flow` >= `Fmax`, "overload",
                                      dplyr::if_else(`flow` >= 0.9 * `Fmax`, "high load", "normal")),
           `shape` = dplyr::if_else(`flow` >= `Fmax`, 16, 17))

  # no data => nothing to do
  testthat::expect_equal(bg, bg %>%
                                eneRgymaps::add.shapes (data = NULL, shape = "arrow", color = "green", coordinates.identifiers = lineCoordinateIdentifiers))

  # add arrows/lines (fixed shapes)
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = "arrow", color = "green", coordinates.identifiers = lineCoordinateIdentifiers); myUpdatedMap; TRUE})

  testthat::expect_equal(bg %>%
                                eneRgymaps::add.shapes (data = LinesData, shape = "arrow", color = "green", coordinates.identifiers = lineCoordinateIdentifiers),
                              bg %>%
                                eneRgymaps::add.arrows (data = LinesData, color = "green", coordinates.identifiers = lineCoordinateIdentifiers))

  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = "line", color = "blue", coordinates.identifiers = lineCoordinateIdentifiers); myUpdatedMap; TRUE})

  testthat::expect_equal(bg %>%
                                eneRgymaps::add.shapes (data = LinesData, shape = "line", color = "blue", coordinates.identifiers = lineCoordinateIdentifiers),
                              bg %>%
                                eneRgymaps::add.lines (data = LinesData, color = "blue", coordinates.identifiers = lineCoordinateIdentifiers))

  # dynamic shape
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape.field.name = "shape", color = "green", coordinates.identifiers = pointCoordinateIdentifiers); myUpdatedMap; TRUE})

  # dynamic color
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = 17, color.field.name = "loading", colors.list = c("orange", "green", "red"), coordinates.identifiers = pointCoordinateIdentifiers); myUpdatedMap; TRUE})

  # dynamic color with legend
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = 17, color.field.name = "loading",
                            colors.list = c("orange", "green", "red"),
                            coordinates.identifiers = pointCoordinateIdentifiers,
                            legend.color.name = "Colors", legend.color.keys = c ("overload", "high load", "normal"),
                            legend.color.values = c ("Overload", "High load - no overload", "standard load")); myUpdatedMap; TRUE})

  # color legend keeping NAs
  linesDataWithNA <- rbind(LinesData, list("NALine", 5565500, 840000.0, 5589461, 1424833, 600, 900, NA,17))
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = linesDataWithNA, shape = 17, color.field.name = "loading",
                            colors.list = c("orange", "green", "red"),
                            coordinates.identifiers = pointCoordinateIdentifiers,
                            legend.color.name = "Colors", legend.keep.na = TRUE); myUpdatedMap; TRUE})

  # color legend removing NAs
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = linesDataWithNA, shape = 17, color.field.name = "loading",
                            colors.list = c("orange", "green", "red"),
                            coordinates.identifiers = pointCoordinateIdentifiers,
                            legend.color.name = "Colors", legend.keep.na = FALSE); myUpdatedMap; TRUE})

  # fixed width
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = "arrow", color = "green", width = 5, coordinates.identifiers = lineCoordinateIdentifiers); myUpdatedMap; TRUE})

  # dynamic width
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.shapes (data = LinesData, shape = "arrow", color = "green", width.field.name = "shape", coordinates.identifiers = lineCoordinateIdentifiers); myUpdatedMap; TRUE})

  # error: missing/too many coordinate identifiers
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", coordinates.identifiers = c(NA, NA)))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", coordinates.identifiers = c(lineCoordinateIdentifiers, NA, NA)))

  # error: coordinate identifiers not in data
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                          color = "green", coordinates.identifiers = c(NA, NA, "aLong", "aLat", "anothLong", "anothLat")))


  # error: duplicate definition of shape, width, color
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", color.field.name = "loading", coordinates.identifiers = lineCoordinateIdentifiers))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow", shape.field.name = "shape",
                                                  color = "green", coordinates.identifiers = lineCoordinateIdentifiers))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", width = 15, width.field.name = "shape", coordinates.identifiers = lineCoordinateIdentifiers))

  # error: field names not in data
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape.field.name = "arrow",
                                                  color = "green", coordinates.identifiers = lineCoordinateIdentifiers))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color.field.name = "green", coordinates.identifiers = lineCoordinateIdentifiers))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", width.field.name = "missing", coordinates.identifiers = lineCoordinateIdentifiers))

  # error: bad shape identifiers
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape.field.name = "id",
                                                  color = "green", coordinates.identifiers = lineCoordinateIdentifiers))

  # error: bad width
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", width = "abc", coordinates.identifiers = lineCoordinateIdentifiers))
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", width = -1, coordinates.identifiers = lineCoordinateIdentifiers))

  # error: bad width min/max
  testthat::expect_error (eneRgymaps::add.shapes (geographic.map = bg, data = LinesData, shape = "arrow",
                                                  color = "green", width.field.name = "shape",
                                                  width.min.value = 20, width.max.value = 15,
                                                  coordinates.identifiers = lineCoordinateIdentifiers))

  # error: forbidden arguments linked with lines
  testthat::expect_error (eneRgymaps::add.lines (geographic.map = bg, data = LinesData,
                                                 shape.field.name = "shape",
                                                 color = "green", coordinates.identifiers = lineCoordinateIdentifiers))

  testthat::expect_error (eneRgymaps::add.lines (geographic.map = bg, data = LinesData,
                                                 legend.shape.name = "test",
                                                 color = "green", coordinates.identifiers = lineCoordinateIdentifiers))
  # error: forbidden arguments linked with arrows
  testthat::expect_error (eneRgymaps::add.arrows (geographic.map = bg, data = LinesData,
                                                 shape.field.name = "shape",
                                                 color = "green", coordinates.identifiers = lineCoordinateIdentifiers))

  testthat::expect_error (eneRgymaps::add.arrows (geographic.map = bg, data = LinesData,
                                                 legend.shape.name = "test",
                                                 color = "green", coordinates.identifiers = lineCoordinateIdentifiers))
})


testthat::test_that("Testing add.labels", {
  eneRgycache::initialise.cache()
  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  pointCoordinateIdentifiers <- c ("centroid_longitude", "centroid_latitude")

  pricesData <- utils::read.csv(file = file.path(eneRgymaps::example.data.dir(), "Prices.csv"), header = TRUE,
                                fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
    dplyr::filter(`year` == 2019)
  BZsShapes <- eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", with.centroids = TRUE)
  pricesDataGeographic <- base::merge (x = pricesData, by.x = "area_code",
                                 y = BZsShapes, by.y = "eic_code", all = FALSE)

  # no data => nothing to do
  testthat::expect_equal(bg, bg %>%
                                eneRgymaps::add.labels (data = NULL, field.names = c("price"), with.frame = TRUE))

  # no field name => nothing to do
  testthat::expect_equal(bg, bg %>%
                                eneRgymaps::add.labels (data = pricesDataGeographic, field.names = NULL, with.frame = TRUE))

  # add one standard label
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE);
    myUpdatedMap; TRUE})

  # add one label (with frame)
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = TRUE);
    myUpdatedMap; TRUE})

  # add multiple labels (default separator)
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "discrete_value"),
                            coordinates.identifiers = pointCoordinateIdentifiers, with.frame = FALSE); myUpdatedMap; TRUE})

  # add multiple labels (one separator)
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "discrete_value"),
                            field.separators = c("-"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE); myUpdatedMap; TRUE})

  # add multiple labels (multiple separators)
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "discrete_value"),
                            field.separators = c("-", ";"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE); myUpdatedMap; TRUE})

  # rounding digits
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), rounding.ndigits = 0, coordinates.identifiers = pointCoordinateIdentifiers, with.frame = FALSE);
    myUpdatedMap; TRUE})
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), rounding.ndigits = 2, coordinates.identifiers = pointCoordinateIdentifiers, with.frame = FALSE);
    myUpdatedMap; TRUE})
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), rounding.ndigits = -1, coordinates.identifiers = pointCoordinateIdentifiers, with.frame = FALSE);
    myUpdatedMap; TRUE})

  # customize font size and color
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE, font.color = "green", font.size = 4);
    myUpdatedMap; TRUE})
  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE, font.color = "#808000", font.size = 3.5);
    myUpdatedMap; TRUE})

  # warning: same field depicted multiple times
  testthat::expect_warning ({myUpdatedMap <- bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE);
    myUpdatedMap; TRUE})

  # error: bad length of coordinates.identifier
  testthat::expect_error (bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = c(pointCoordinateIdentifiers[[1]]),
                            with.frame = FALSE))
  testthat::expect_error (bg %>%
                            eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = c(pointCoordinateIdentifiers, "price"),
                                                    with.frame = FALSE))

  # error: coordinate identifiers not in data set
  testthat::expect_error (bg %>%
                            eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = c("long", "lat"),
                                                    with.frame = FALSE))

  # error: field name not in data set
  testthat::expect_error (bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("volume"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE))

  # error: bad length of field separators
  testthat::expect_error (bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "discrete_value"),
                            field.separators = c("-", "--", "---"), coordinates.identifiers = pointCoordinateIdentifiers,
                            with.frame = FALSE))

  # error: field separators are not string
  testthat::expect_error (bg %>%
                            eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price", "percentage_change", "discrete_value"),
                                                    field.separators = c(1), coordinates.identifiers = pointCoordinateIdentifiers,
                                                    with.frame = FALSE))

  # error: rounding digit is not an integer
  testthat::expect_error (bg %>%
    eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                            rounding.ndigits = 1.5, with.frame = FALSE))

  # error: bad font size
  testthat::expect_error (bg %>%
                            eneRgymaps::add.labels (data = pricesDataGeographic, field.names = c("price"), coordinates.identifiers = pointCoordinateIdentifiers,
                                                    with.frame = FALSE, font.size = -1))
})


testthat::test_that("Testing set.frame.coordinates", {
  eneRgycache::initialise.cache()
  bg <- eneRgymaps::world.background(fill.color = "grey", border.color = "white", only.keep.Europe = TRUE)

  testthat::expect_true ({myUpdatedMap <- bg %>%
    eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.min = 15, latitude.max = 55);
  myUpdatedMap; TRUE})

  # error: one attribute is not set
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.max = 45, latitude.min = 15, latitude.max = 55))
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10,latitude.min = 15, latitude.max = 55))
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.max = 55))
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.min = 15))

  # error: one attribute is not numeric
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.min = 15, latitude.max = "North pole"))

  # error: longitude.min >= longitude.max
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = -10, latitude.min = 15, latitude.max = 55))
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = 60, longitude.max = 45, latitude.min = 15, latitude.max = 55))

  # error: latitude.min >= latitude.max
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.min = 15, latitude.max = 15))
  testthat::expect_error (myUpdatedMap <- bg %>%
                            eneRgymaps::set.frame.coordinates (longitude.min = -10, longitude.max = 45, latitude.min = 15, latitude.max = 0))
})
