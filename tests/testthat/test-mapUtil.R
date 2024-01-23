testthat::context("Test geographic maps utility functions")

DEATEic <- eneRgymaps::eic.code.merged.AT.DE()
DELUEic <- eneRgymaps::eic.code.DELU()
ATEic <- eneRgymaps::eic.code.AT()
DELUTSOsEics <- c(eneRgymaps::eic.codes.DE.TSOs(), eneRgymaps::eic.code.LU.TSO())

testthat::test_that("Testing generic shapes", {
  eneRgycache::initialise.cache()

  testthat::expect_equal (eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE), eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE, scale = 10))
  testthat::expect_true ({eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE, scale = 50); TRUE})
  testthat::expect_true ({eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE, scale = 110); TRUE})
  testthat::expect_error (eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE, scale = 75))

  testthat::expect_true ({eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE); TRUE})
  testthat::expect_true ({eneRgymaps::countries.shapes.cached (only.keep.Europe = FALSE); TRUE})

  world <- eneRgymaps::countries.shapes.cached (only.keep.Europe = TRUE, scale = 110)
  testthat::expect_true ("LU" %in% world[["iso2"]])
  testthat::expect_true ("Luxembourg" %in% world[["name"]])
  testthat::expect_true ("DE" %in% world[["iso2"]])
  testthat::expect_true ("Germany" %in% world[["name"]])
  testthat::expect_true ("DELU" %in% world[["iso2"]])
  testthat::expect_true ("Germany - Luxembourg" %in% world[["name"]])

  testthat::expect_true ("Cyprus" %in% world[["name"]])
  testthat::expect_false ("Cyprus, Northern" %in% world[["name"]])
  testthat::expect_false ("Cyprus No Mans Area" %in% world[["name"]])

  testthat::expect_true ({eneRgymaps::bidding.zones.shapes.cached(date = base::Sys.Date()); TRUE})
  testthat::expect_equal(eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2017-12-05")),
                         eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2016-01-01")))
  testthat::expect_true ({eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2019-01-05")); TRUE})
  testthat::expect_true ({eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2028-06-05")); TRUE})

  # check that the presence of the merged or split AT/DE bidding-zones is robust
  biddingZonesBeforeDEATSplit <- eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2016-01-01"))
  testthat::expect_true (DEATEic %in% biddingZonesBeforeDEATSplit[["eic_code"]])
  testthat::expect_false (DELUEic %in% biddingZonesBeforeDEATSplit[["eic_code"]])
  testthat::expect_false (ATEic %in% biddingZonesBeforeDEATSplit[["eic_code"]])

  biddingZonesAfterDEATSplit <- eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2020-01-01"))
  testthat::expect_false (DEATEic %in% biddingZonesAfterDEATSplit[["eic_code"]])
  testthat::expect_true (DELUEic %in% biddingZonesAfterDEATSplit[["eic_code"]])
  testthat::expect_true (ATEic %in% biddingZonesAfterDEATSplit[["eic_code"]])

  # error: no bidding-zone shape for 1945
  testthat::expect_error(eneRgymaps::bidding.zones.shapes.cached(date = as.Date("1945-05-08")))
  # error: date is not under the right format
  testthat::expect_error(eneRgymaps::bidding.zones.shapes.cached(date = "yesterday"))

  testthat::expect_true ({eneRgymaps::control.areas.shapes.cached(); TRUE})

  # check whether both DE/LU (as a whole) and individual DE TSO shapes are present in control areas
  caShapes <- eneRgymaps::control.areas.shapes.cached()
  testthat::expect_true (DELUEic %in% caShapes[["eic_code"]])
  testthat::expect_true (all (DELUTSOsEics %in% caShapes[["eic_code"]]))
})

testthat::test_that("Testing variables names formatting", {
  varNamesOk <- c ("AName", "Another.name", "my_var", "testvar456")
  varNamesAt <- c ("aTest", "@2")
  varNamesBackslash <- c ("aTest", "`no`")
  varNamesSpace <- c ("aTest", "no value")

  testthat::expect_true (eneRgymaps::coherent.map.variable.names.formatting(variable.names = varNamesOk))
  testthat::expect_false (eneRgymaps::coherent.map.variable.names.formatting(variable.names = varNamesAt))
  testthat::expect_false (eneRgymaps::coherent.map.variable.names.formatting(variable.names = varNamesBackslash))
  testthat::expect_false (eneRgymaps::coherent.map.variable.names.formatting(variable.names = varNamesSpace))
})

testthat::test_that("geographic shapes", {
  eneRgycache::initialise.cache()

  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE), eneRgymaps::countries.shapes.cached(only.keep.Europe = TRUE))
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = FALSE), eneRgymaps::countries.shapes.cached(only.keep.Europe = FALSE))
  testthat::expect_false (identical (eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = FALSE), eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE)))

  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE, date = as.Date("2016-11-11")),
                          eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE, date = as.Date("2022-11-11")))

  # world should have Afghanistan, but not if only Europe is kept
  testthat::expect_true ("AF" %in% eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = FALSE) [["iso2"]])
  testthat::expect_false ("AF" %in% eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE) [["iso2"]])

  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = base::Sys.Date()), eneRgymaps::bidding.zones.shapes.cached(date = base::Sys.Date()))
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = as.Date("2016-11-11")), eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2016-11-11")))
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = as.Date("2016-11-11"), only.keep.Europe = TRUE),
                          eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = as.Date("2016-11-11"), only.keep.Europe = FALSE))
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = as.Date("2025-12-25")), eneRgymaps::bidding.zones.shapes.cached(date = as.Date("2025-12-25")))
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Control area"), eneRgymaps::control.areas.shapes.cached())
  testthat::expect_equal (eneRgymaps::geographic.shapes(geographic.entity.name = "Control area", only.keep.Europe = TRUE),
                          eneRgymaps::geographic.shapes(geographic.entity.name = "Control area", only.keep.Europe = FALSE))

  # error : no geographic entity name defined
  testthat::expect_error (eneRgymaps::geographic.shapes())

  # error : bad geographic entity name
  testthat::expect_error (eneRgymaps::geographic.shapes(geographic.entity.name = "house"))

  # error : requesting country shapes without specifying only.keep.Europe
  testthat::expect_error (eneRgymaps::geographic.shapes(geographic.entity.name = "Country"))
  testthat::expect_error (eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = NULL))
})

testthat::test_that("Adding centroids", {
  eneRgycache::initialise.cache()

  centroidsCountries1 <- eneRgymaps::add.centroids(geographic.shapes = eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = FALSE))
  testthat::expect_true(all(c("centroid", "centroid_longitude", "centroid_latitude") %in% colnames(centroidsCountries1)))
  centroidsCountries2 <- eneRgymaps::add.centroids(geographic.shapes = eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE))
  testthat::expect_true(all(c("centroid", "centroid_longitude", "centroid_latitude") %in% colnames(centroidsCountries2)))

  centroidsBZs <- eneRgymaps::add.centroids(geographic.shapes = eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = base::Sys.Date()))
  testthat::expect_true(all(c("centroid", "centroid_longitude", "centroid_latitude") %in% colnames(centroidsBZs)))

  centroidsCAs <- eneRgymaps::add.centroids(geographic.shapes = eneRgymaps::geographic.shapes(geographic.entity.name = "Control area"))
  testthat::expect_true(all(c("centroid", "centroid_longitude", "centroid_latitude") %in% colnames(centroidsCAs)))

  # error: no "geometry" column in data
  polygonData <- eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE) %>%
    dplyr::rename(`polygons` = `geometry`)
  testthat::expect_error(eneRgymaps::add.centroids(geographic.shapes = polygonData))

  # error: add.centroids already run on the considered data
  testthat::expect_error(eneRgymaps::add.centroids(geographic.shapes = centroidsCAs))

  # error "centroid_longitude" already in the data
  dataWithColumn <- eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE) %>%
    dplyr::mutate(`centroid_longitude` = 2)
  testthat::expect_error(eneRgymaps::add.centroids(geographic.shapes = dataWithColumn))
})

testthat::test_that("cleaning duplicate geographic shapes - bidding zone", {
  geographic.entity.name <- "Bidding zone"

  eneRgycache::initialise.cache()

  geographicData <- eneRgymaps::geographic.shapes(geographic.entity.name = geographic.entity.name, date = as.Date("2019-02-20"))

  # clean.map.shapes.duplicates does not apply to bidding-zones, thus the following 4 data frames should be identical
  cleanedData1 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData2 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = TRUE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData3 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = FALSE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData4 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = TRUE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)
  # merging German TSOs areas only applies to control areas, thus the following 4 data frames should be identical
  cleanedData11 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = TRUE)
  cleanedData21 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = TRUE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = TRUE)
  cleanedData31 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = FALSE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = TRUE)
  cleanedData41 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = "eic_code", GB.NI.in.map = TRUE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = TRUE)

  testthat::expect_true (identical (cleanedData1, cleanedData2))
  testthat::expect_true (identical (cleanedData1, cleanedData3))
  testthat::expect_true (identical (cleanedData1, cleanedData4))

  testthat::expect_true(identical(cleanedData1, cleanedData11))
  testthat::expect_true(identical(cleanedData11, cleanedData21))
  testthat::expect_true(identical(cleanedData11, cleanedData31))
  testthat::expect_true(identical(cleanedData11, cleanedData41))
})


testthat::test_that("cleaning duplicate geographic shapes - countries", {
  geographic.entity.name <- "Country"
  idColumnName <- "iso2"

  eneRgycache::initialise.cache()

  geographicData <- eneRgymaps::geographic.shapes(geographic.entity.name = geographic.entity.name, only.keep.Europe = TRUE)

  # separating DE/LU TSOs does not apply to countries, thus the following data frames should be identical
  cleanedData1 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData11 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = TRUE)
  testthat::expect_true (identical (cleanedData1, cleanedData11))

  testthat::expect_true (all (c ("GB", "IE", "IE (SEM)", "NI", "UK") %in% geographicData[[idColumnName]]))

  testthat::expect_true ("UK" %in% cleanedData1[[idColumnName]])
  testthat::expect_false ("GB" %in% cleanedData1[[idColumnName]])
  testthat::expect_false ("NI" %in% cleanedData1[[idColumnName]])
  testthat::expect_false ("IE (SEM)" %in% cleanedData1[[idColumnName]])

  cleanedData3 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = TRUE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)

  testthat::expect_false (identical (cleanedData1, cleanedData3))

  testthat::expect_false ("UK" %in% cleanedData3[[idColumnName]])
  testthat::expect_true ("GB" %in% cleanedData3[[idColumnName]])
  testthat::expect_true ("NI" %in% cleanedData3[[idColumnName]])
  testthat::expect_false ("IE (SEM)" %in% cleanedData3[[idColumnName]])

  cleanedData4 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)

  testthat::expect_false (identical (cleanedData1, cleanedData4))
  testthat::expect_false (identical (cleanedData3, cleanedData4))

  testthat::expect_false ("NI" %in% cleanedData4[[idColumnName]])
  testthat::expect_false ("IE" %in% cleanedData4[[idColumnName]])
  testthat::expect_true ("IE (SEM)" %in% cleanedData4[[idColumnName]])

  cleanedData5 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = TRUE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)

  testthat::expect_false (identical (cleanedData1, cleanedData5))
  testthat::expect_false (identical (cleanedData3, cleanedData5))
  testthat::expect_false (identical (cleanedData4, cleanedData5))

  testthat::expect_false ("UK" %in% cleanedData5[[idColumnName]])
  testthat::expect_false ("NI" %in% cleanedData5[[idColumnName]])
  testthat::expect_false ("IE" %in% cleanedData5[[idColumnName]])
  testthat::expect_true ("IE (SEM)" %in% cleanedData5[[idColumnName]])
  testthat::expect_true ("GB" %in% cleanedData5[[idColumnName]])
})


testthat::test_that("cleaning duplicate geographic shapes - Control areas", {
  geographic.entity.name <- "Control area"
  idColumnName <- "eic_code"

  eneRgycache::initialise.cache()

  geographicData <- eneRgymaps::geographic.shapes(geographic.entity.name = geographic.entity.name)

  # merging/splitting AT/DE, GB/NI, IE (SEM) does not apply to control areas, so that the 8 data sets should be identical
  cleanedData1 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData3 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData5 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = TRUE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData7 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = TRUE, IE.SEM.in.map = TRUE, separate.DE.LU.TSOs.in.map = FALSE)

  testthat::expect_true (identical (cleanedData1, cleanedData3))
  testthat::expect_true (identical (cleanedData1, cleanedData5))
  testthat::expect_true (identical (cleanedData1, cleanedData7))

  cleanedData11 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE)
  cleanedData12 <- geographicData %>%
    eneRgymaps::clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name, id.column.name = idColumnName, GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = TRUE)

  testthat::expect_true (all (c (DELUTSOsEics, DELUEic) %in% geographicData[[idColumnName]]))

  testthat::expect_true (DELUEic %in% cleanedData11[[idColumnName]])
  testthat::expect_false (any (DELUTSOsEics %in% cleanedData11[[idColumnName]]))

  testthat::expect_false (DELUEic %in% cleanedData12[[idColumnName]])
  testthat::expect_true (all (DELUTSOsEics %in% cleanedData12[[idColumnName]]))
})

testthat::test_that("cleaning duplicate geographic shapes - errors", {
  eneRgycache::initialise.cache()

  geographicData <- eneRgymaps::geographic.shapes(geographic.entity.name = "Country", only.keep.Europe = TRUE)

  # error : id.column.name not set
  testthat::expect_error (eneRgymaps::clean.map.shapes.duplicates (geographic.data = geographicData, geographic.entity.name = "Country", GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE))

  # error: id.column.name not in geographic.data
  testthat::expect_error (eneRgymaps::clean.map.shapes.duplicates (geographic.data = geographicData, geographic.entity.name = "Country", id.column.name = "aName", GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE))

  # error : bad geographic entity name
  testthat::expect_error (eneRgymaps::clean.map.shapes.duplicates (geographic.data = geographicData, geographic.entity.name = "City", id.column.name = "iso2", GB.NI.in.map = FALSE, IE.SEM.in.map = FALSE, separate.DE.LU.TSOs.in.map = FALSE))
})

testthat::test_that("flip negative border values", {
  eneRgycache::initialise.cache()

  borderDataPath <- file.path(eneRgymaps::example.data.dir(), "Flows.xlsx")
  borderDataRaw <- readxl::read_excel (path = borderDataPath)

  bordersToFlip <- c ("SI > AT", "DE > FR", "CZ > DE", "GR > TR")
  borderDataWithNegative <- borderDataRaw %>%
    dplyr::mutate(`avg_1` = dplyr::if_else(`border` %in% bordersToFlip, -`Average`, `Average`))

  borderDataManuallyFlipped <- borderDataWithNegative %>%
    dplyr::mutate (`to_flip` = `avg_1` < 0,
                   `separator_pos` = stringr::str_locate (string = `border`, pattern = " > ") [1],
                   `area_in` = substr(x = `border`, start = `separator_pos` + 3, stop = nchar (`border`)),
                   `area_out` = substr(x = `border`, start = 1, stop = `separator_pos` - 1),
                   `tmp_area` = `area_out`,
                   `tmp_area_code` = `out_area_code`,
                   `border` = dplyr::if_else (`to_flip`, paste0 (`area_in`, " > ", `area_out`), `border`),
                   `area_out` = dplyr::if_else (`to_flip`, `area_in`, `area_out`),
                   `area_in` = dplyr::if_else (`to_flip`, `tmp_area`, `area_in`),
                   `out_area_code` = dplyr::if_else (`to_flip`, `in_area_code`, `out_area_code`),
                   `in_area_code` = dplyr::if_else (`to_flip`, `tmp_area_code`, `in_area_code`),
                   `avg_1` = dplyr::if_else (`to_flip`, - `avg_1`, `avg_1`)) %>%
    dplyr::select (- c (`to_flip`, `separator_pos`, `tmp_area`, `tmp_area_code`, `area_in`, `area_out`))

  borderDataAutomaticallyFlipped <- eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                                      in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                                      label.value.field.name = "avg_1")

  borderDataAutomaticallyFlipped2 <- eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                                       in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                                       label.value.field.name = "avg_1", width.value.field.name = "avg_1")

  borderDataAutomaticallyFlipped3 <- eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                                       in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                                       width.value.field.name = "avg_1")

  testthat::expect_true (all.equal(borderDataManuallyFlipped, borderDataAutomaticallyFlipped))
  testthat::expect_true (all.equal (borderDataManuallyFlipped, borderDataAutomaticallyFlipped2))
  testthat::expect_true (all.equal (borderDataManuallyFlipped, borderDataAutomaticallyFlipped3))


  # error : missing plotData argument
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (id.field.name = "border",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                  width.value.field.name = "avg_1"))

  # error : missing id field name argument
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative,
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                  width.value.field.name = "avg_1"))

  # error : missing in id field name argument
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  out.id.field.name = "out_area_code",
                                                  width.value.field.name = "avg_1"))

  # error : missing out id field name argument
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "in_area_code",
                                                  width.value.field.name = "avg_1"))

  # error : no field name defined among width and label
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code"))


  # error : two different field names defined for width and value
  borderDataDoubleFields <- borderDataWithNegative %>%
    dplyr::mutate (`avg_2` = `avg_1`)
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataDoubleFields, id.field.name = "border",
                                    in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                    label.value.field.name = "avg_1", width.value.field.name = "avg_2"))


  # error : id field name not in data set
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "AFieldName",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                  label.value.field.name = "avg_1"))

  # error : in field name not in data set
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "AFieldName", out.id.field.name = "out_area_code",
                                                  label.value.field.name = "avg_1"))

  # error : out field name not in data set
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "AFieldName",
                                                  label.value.field.name = "avg_1"))

  # error : flipping (label) field name not in data set
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                  label.value.field.name = "AFieldName"))

  # error : flipping (width) field name not in data set
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = borderDataWithNegative, id.field.name = "border",
                                                  in.id.field.name = "in_area_code", out.id.field.name = "out_area_code",
                                                  width.value.field.name = "AFieldName"))

  # error : forbidden field name ("toReverse")
  dataForbiddenField1 <- borderDataWithNegative %>%
    dplyr::rename (`toReverse` = `in_area_code`)
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = dataForbiddenField1, id.field.name = "border",
                                                    in.id.field.name = "toReverse", out.id.field.name = "out_area_code",
                                                    label.value.field.name = "avg_1"))

  # error : forbidden field name ("initialInIdFieldName")
  dataForbiddenField2 <- borderDataWithNegative %>%
    dplyr::rename (`initialInIdFieldName` = `in_area_code`)
  testthat::expect_error (eneRgymaps::flip.negative.oriented.border.values (plotData = dataForbiddenField2, id.field.name = "border",
                                                                            in.id.field.name = "initialInIdFieldName", out.id.field.name = "out_area_code",
                                                                            label.value.field.name = "avg_1"))
})


testthat::test_that("Labels of geographic entities", {
  eneRgycache::initialise.cache()

  testthat::expect_true ({eneRgymaps::geographic.entity.labels (geographic.entity.name = "Country"); TRUE})
  testthat::expect_true ({eneRgymaps::geographic.entity.labels (geographic.entity.name = "Bidding zone"); TRUE})
  testthat::expect_true ({eneRgymaps::geographic.entity.labels (geographic.entity.name = "Control area"); TRUE})

  # error: bad geographic entity name
  testthat::expect_error (eneRgymaps::geographic.entity.labels (geographic.entity.name = "Region"))

  # check the EU, EC, and EFTA delineation
  ecCountriesIso2 <- c ("AL", "BA", "MD", "ME", "MK", "RS", "UA", "XK") # observers (such as NO) not included
  eftaCountriesIso2 <- c ("CH", "NO") # other EFTA countries not yet included in data
  euCountriesISO2 <- c ("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU",
                       "IE", "IT", "LT", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "GB", "NI")

  ECCountries <- eneRgymaps::geographic.entity.labels (geographic.entity.name = "Country", political.initiatives = c("EC"))
  testthat::expect_equal(sort(ECCountries [["id"]]), sort (ecCountriesIso2))

  EFTACountries <- eneRgymaps::geographic.entity.labels (geographic.entity.name = "Country", political.initiatives = c("EFTA"))
  testthat::expect_equal(sort(EFTACountries [["id"]]), sort (eftaCountriesIso2))

  EUCountries <- eneRgymaps::geographic.entity.labels (geographic.entity.name = "Country", political.initiatives = c("EU"))
  testthat::expect_equal(sort(EUCountries [["id"]]), sort (euCountriesISO2))

  # check the conversion of double space to \n
  NGLabel <- eneRgymaps::geographic.entity.labels (geographic.entity.name = "Control area") %>%
    dplyr::filter (`id` == "10YGB----------A") %>%
    dplyr::select (`label`)
  testthat::expect_equal (NGLabel[["label"]], "National\nGrid")

  # error : bad geographic entity name
  testthat::expect_error (eneRgymaps::geographic.entity.labels (geographic.entity.name = "Jurisdiction"))
})

testthat::test_that("Geographic points of borders", {
  eneRgycache::initialise.cache()

  testthat::expect_true ({eneRgymaps::geographic.border.points (); TRUE})
})

testthat::test_that("Enrich data with geographic coordinates", {
  eneRgycache::initialise.cache()

  borderData <- readxl::read_excel (path = file.path(eneRgymaps::example.data.dir(), "Flows.xlsx"))
  CountriesAssessment <- read.csv (file = file.path(eneRgymaps::example.data.dir(), "Countries_assessment.csv"),
                                   fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE)

  borderCoordinates <- eneRgymaps::geographic.border.points()

  # enriching border data
  coordinatesBorderFields <- c ("coor_cent_long", "coor_cent_lat", "coor_out_long", "coor_out_lat", "coor_in_long", "coor_in_lat")
  testthat::expect_true ({enrichedBorderData <- enrich.with.geographic.coordinates (data = borderData, data.id.field.name = c ("out_area_code", "in_area_code"),
                                                                                    geographic.coordinates.data = eneRgymaps::geographic.border.points(),
                                                                                    geographic.coordinates.data.id.field.name = c ("out_area_code", "in_area_code"),
                                                                                    geographic.coordinates.data.field.names = coordinatesBorderFields); TRUE})
  testthat::expect_equal (sort (colnames (enrichedBorderData)), sort (c (colnames (borderData), coordinatesBorderFields)))
  testthat::expect_true (all (c ("out_area_code", "in_area_code") %in% colnames (enrichedBorderData)))

  # NULL data : nothing to do
  nullData <- NULL
  testthat::expect_equal (nullData, eneRgymaps::enrich.with.geographic.coordinates (data = nullData, data.id.field.name = "Country",
                                                                                   geographic.coordinates.data = countryCoordinates,
                                                                                   geographic.coordinates.data.id.field.name = "id",
                                                                                   geographic.coordinates.data.field.names = coordinatesCountryFields))

  # error : data id field not in data set
  testthat::expect_error (eneRgymaps::enrich.with.geographic.coordinates (data = CountriesAssessment, data.id.field.name = "MyCountry",
                                                                          geographic.coordinates.data = countryCoordinates,
                                                                          geographic.coordinates.data.id.field.name = "id",
                                                                          geographic.coordinates.data.field.names = coordinatesCountryFields))

  # error : geographic data id field not in geographic data set
  testthat::expect_error (eneRgymaps::enrich.with.geographic.coordinates (data = CountriesAssessment, data.id.field.name = "Country",
                                                                          geographic.coordinates.data = countryCoordinates,
                                                                          geographic.coordinates.data.id.field.name = "Noid",
                                                                          geographic.coordinates.data.field.names = coordinatesCountryFields))

  # error : geographic coordinate field not in geographic data set
  testthat::expect_error (eneRgymaps::enrich.with.geographic.coordinates (data = CountriesAssessment, data.id.field.name = "Country",
                                                                          geographic.coordinates.data = countryCoordinates,
                                                                          geographic.coordinates.data.id.field.name = "id",
                                                                          geographic.coordinates.data.field.names = c (coordinatesCountryFields, "altitude")))

  # error : coordinate field already present in data set
  CountriesAssessmentWithLong <- CountriesAssessment %>%
    dplyr::mutate (`longitude` = 1)
  testthat::expect_error (eneRgymaps::enrich.with.geographic.coordinates (data = CountriesAssessmentWithLong, data.id.field.name = "Country",
                                                                          geographic.coordinates.data = countryCoordinates,
                                                                          geographic.coordinates.data.id.field.name = "id",
                                                                          geographic.coordinates.data.field.names = coordinatesCountryFields))

  # error : duplicate data id field name
  testthat::expect_error (enrich.with.geographic.coordinates (data = borderData, data.id.field.name = c ("out_area_code", "out_area_code"),
                                                            geographic.coordinates.data = eneRgymaps::geographic.border.points(),
                                                            geographic.coordinates.data.id.field.name = c ("out_area_code", "in_area_code"),
                                                            geographic.coordinates.data.field.names = coordinatesBorderFields))

  # error : duplicate geographic data id field name
  testthat::expect_error (enrich.with.geographic.coordinates (data = borderData, data.id.field.name = c ("out_area_code", "in_area_code"),
                                                              geographic.coordinates.data = eneRgymaps::geographic.border.points(),
                                                              geographic.coordinates.data.id.field.name = c ("out_area_code", "out_area_code"),
                                                              geographic.coordinates.data.field.names = coordinatesBorderFields))

  # error : duplicate geographic data field names
  testthat::expect_error (enrich.with.geographic.coordinates (data = borderData, data.id.field.name = c ("out_area_code", "in_area_code"),
                                                              geographic.coordinates.data = eneRgymaps::geographic.border.points(),
                                                              geographic.coordinates.data.id.field.name = c ("out_area_code", "out_area_code"),
                                                              geographic.coordinates.data.field.names = c ("coor_cent_long", "coor_cent_lat", "coor_cent_long")))
})


testthat::test_that("test relevant legend position", {
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = "none"))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = "left"))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = "bottom"))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = "right"))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = "top"))

  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.0, 0.0)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5, 0.0)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.0, 0.5)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5, 0.75)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (1.0, 0.0)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.0, 1.0)))
  testthat::expect_true (eneRgymaps::is.relevant.legend.position (legend.position = c (0.314, 0.789)))

  # coordinates should only be between 0 and 1
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (-0.5, 0.1)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.2, -0.75)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (1.1, 0.5)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.1, 2.0)))

  # coordinates should be numeric
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.0, NA)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5, NULL)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (NA, NULL)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5, "one")))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c ("zero", "one")))

  # exactly two parameters are expected are allowed
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5, 0.5, 0.75)))
  testthat::expect_false (eneRgymaps::is.relevant.legend.position (legend.position = c (0.5)))
})


testthat::test_that("Relevant shape identifier", {
  testthat::expect_true(eneRgymaps::is.relevant.shape.identifier (shape.identifier = "arrow"))
  testthat::expect_true(eneRgymaps::is.relevant.shape.identifier (shape.identifier = "line"))

  testthat::expect_true(eneRgymaps::is.relevant.shape.identifier (shape.identifier = 16))
  testthat::expect_true(eneRgymaps::is.relevant.shape.identifier (shape.identifier = 17))
  testthat::expect_true(eneRgymaps::is.relevant.shape.identifier (shape.identifier = 0))

  testthat::expect_false(eneRgymaps::is.relevant.shape.identifier (shape.identifier = "triangle"))
  testthat::expect_false(eneRgymaps::is.relevant.shape.identifier (shape.identifier = "myShape"))
  testthat::expect_false(eneRgymaps::is.relevant.shape.identifier (shape.identifier = -5))
})
