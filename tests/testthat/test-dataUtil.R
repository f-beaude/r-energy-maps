testthat::context("Test data utilities")

testthat::test_that("Testing save data", {

  myKeys <- c ("a", "b", "c", "d")
  myValues <- c (1, 2, 3, 4)

  tmpOutputDir <- tempdir()
  outputPath <- file.path(tmpOutputDir, "test_data")
  testthat::expect_true ({eneRgymaps::save.data(dataset = myKeys, destination.path = outputPath); TRUE})
  testthat::expect_true ({eneRgymaps::save.data(dataset = myValues, destination.path = outputPath); TRUE})

  errorOutputPath <- file.path(tmpOutputDir, "AFolderWhichDoesn'tExist", "my_data")

  # error : the output folder doesn't exist
  testthat::expect_error (eneRgymaps::save.data(dataset = myKeys, destination.path = errorOutputPath))
})

testthat::test_that("Testing element mapping", {

  myKeys <- c ("a", "b", "c", "d")
  myValues <- c (1, 2, 3, 4)

  testthat::expect_equal (eneRgymaps::map.element(identifier = "a", keys.vector = myKeys, values.vector = myValues), 1)
  testthat::expect_equal (eneRgymaps::map.element(identifier = "d", keys.vector = myKeys, values.vector = myValues), 4)
  testthat::expect_equal (eneRgymaps::map.element(identifier = "z", keys.vector = myKeys, values.vector = myValues), NA)
  testthat::expect_equal (eneRgymaps::map.element(identifier = 1, keys.vector = myKeys, values.vector = myValues), NA)

  testthat::expect_equal (eneRgymaps::map.element(identifier = 2, keys.vector = myValues, values.vector = myKeys), "b")
  testthat::expect_equal (eneRgymaps::map.element(identifier = 3, keys.vector = myValues, values.vector = myKeys), "c")
  testthat::expect_equal (eneRgymaps::map.element(identifier = 15, keys.vector = myValues, values.vector = myKeys), NA)
  testthat::expect_equal (eneRgymaps::map.element(identifier = "c", keys.vector = myValues, values.vector = myKeys), NA)

  # error : the keys dictionary has a duplicate key ("a")
  keysDuplicate <- c ("a", "b", "c", "a")
  testthat::expect_error (eneRgymaps::map.element (identifier = "b", keys.vector = keysDuplicate, values.vector = myValues))

  # error : the keys and value dictionaries do not have the same length
  valuesAdded <- c (1, 2, 3, 4, 5)
  testthat::expect_error (eneRgymaps::map.element (identifier = "a", keys.vector = myKeys, values.vector = valuesAdded))
})


testthat::test_that("Testing dictionaries", {

  dict.keys <- c ("a", "b", "c", "d")
  dict.values <- c (1, 2, 3, 4)

  dict1 <- eneRgymaps::create.dictionary (keys = dict.keys, values = dict.values)
  dict2 <- eneRgymaps::create.dictionary (keys = dict.values, values = dict.keys)

  # error : duplicate key
  testthat::expect_error (eneRgymaps::create.dictionary (keys = c ("a", "b", "a", "d"), values = dict.values))

  # error : keys and values do not have the same size
  testthat::expect_error (eneRgymaps::create.dictionary (keys = c ("a", "b", "d"), values = dict.values))

  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "a", dictionary = dict1), 1)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "b", dictionary = dict1), 2)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "c", dictionary = dict1), 3)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "d", dictionary = dict1), 4)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = NULL, dictionary = dict1), NULL)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = NA, dictionary = dict1), NA)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "ghjk", dictionary = dict1), NA)

  eneRgymaps::add.dictionary.keys (dictionary = dict1, keys = c ("ghjk"), values = c (15))
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "ghjk", dictionary = dict1), 15)

  # error : the key already exists
  testthat::expect_error (eneRgymaps::add.dictionary.keys (dictionary = dict1, keys = c ("ghjk"), values = c (15)))

  # no error because force = TRUE => replace existing keys
  eneRgymaps::add.dictionary.keys (dictionary = dict1, keys = c ("ghjk"), values = c (22), force = TRUE)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "ghjk", dictionary = dict1), 22)

  # remove the key
  eneRgymaps::remove.dictionary.keys (dictionary = dict1, keys = c ("ghjk"))
  # warning because the key doesn't exist any longer
  testthat::expect_warning (eneRgymaps::remove.dictionary.keys(dictionary = dict1, keys = c ("ghjk")))
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "ghjk", dictionary = dict1), NA)

  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "1", dictionary = dict2), "a")
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "2", dictionary = dict2), "b")
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "3", dictionary = dict2), "c")
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "4", dictionary = dict2), "d")
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = NULL, dictionary = dict2), NULL)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = NA, dictionary = dict2), NA)
  testthat::expect_equal (eneRgymaps::get.dictionary.value(key = "6", dictionary = dict2), NA)

  values <- c ("a", "b", "d", "c", "a", "a", NA)
  values2 <- c (1, 2, 4, 4, 3, 1, 2)

  refVals <- c (sapply (X = c (1, 2, 4, 3, 1, 1), FUN = toString), NA)
  testthat::expect_equal (eneRgymaps::apply.dictionary(keys = values, dictionary = dict1), refVals)

  refVals2 <- c ("a", "b", "d", "d", "c", "a", "b")
  testthat::expect_equal (eneRgymaps::apply.dictionary(keys = values2, dictionary = dict2), refVals2)

  # error : at least one key does not belong to the dictionary
  testthat::expect_error (eneRgymaps::apply.dictionary (keys = c ("e", "b", "a", "d"), dictionary = dict1))
  testthat::expect_error (eneRgymaps::apply.dictionary (keys = c (1, 2, 3, 8, 0), dictionary = dict2))
})


testthat::test_that("Checking whether a vector only contains integers", {
  intVec <- c (1, 8, -10)
  strVect <- c ("a", "test")
  dblVec <- c (1.5, 3.1415)
  mixVec1 <- c (1, 2.5)
  mixVec2 <- c (2, "ko")

  testthat::expect_true (eneRgymaps::integers.all (intVec))
  testthat::expect_false (eneRgymaps::integers.all (strVect))
  testthat::expect_false (eneRgymaps::integers.all (dblVec))
  testthat::expect_false (eneRgymaps::integers.all (mixVec1))
  testthat::expect_false (eneRgymaps::integers.all (mixVec2))
})
