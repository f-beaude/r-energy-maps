#' Generic data utility functions

#' Save a generated data set as a .csv file
#'
#' Save a generated data set (linked with a plot) as a .csv file
#' @param dataset a data set
#' @param destination.path the path in which to save the graph (without file extension)
#' @param na.string the string to use to write NA values
#' @param ... additional arguments for write.csv
#' @importFrom utils write.csv
#' @examples
#' save.data (dataset, file.path("C:", "data"))
#' save.data (dataset = myData, destination.path = file.path("C:", "data"), na.string = "NA")
#' @export
save.data <- function(dataset, destination.path, na.string = "", ...) {
  outputFileName <- base::basename (destination.path)
  outputDirectory <- base::dirname (destination.path)
  if ((is.null(outputDirectory)) | (! dir.exists(outputDirectory))) {
    stop(paste("Unable to save data set : the output directory does not exist", outputDirectory, sep=" "))
  }

  utils::write.csv(dataset, file = paste(destination.path, ".csv", sep = ""), row.names = FALSE,
                   na = na.string, fileEncoding = "UTF-8", ...)
}

#' Map an element from a dictionary : check whether the element is in the keys, if so return the value (otherwise return NA)
#'
#' @param identifier the identifier value
#' @param keys.vector the keys vector in which to look for the id
#' @param values.vector the values vector to look for the value when the key has been found
#' @return the value (or NA if not found)
#' @examples
#' map.element (identifier = "France", keys.vector = countryNames, values.vector = countryEICCodes)
#' @export
map.element <- function (identifier, keys.vector, values.vector) {
  if (! identifier %in% keys.vector) {
    return (NA)
  }

  # sanity checks
  # the keys vector should not contain duplicate keys
  stopifnot (length (keys.vector) == length (unique(keys.vector)))
  # the keys and values vectors should have the same size
  stopifnot (length (keys.vector) == length (values.vector))

  return (values.vector [[which(identifier == keys.vector)[[1]]]])
}

#' Create a dictionary, based on keys and values
#'
#' @param keys a vector of keys
#' @param values a vector of values
#' @return dictionary mapping keys and values
#' @importFrom hash hash
#' @examples
#' create.dictionary (keys = c ("France", "Poland"), values = c (12, 25))
#' @export
create.dictionary <- function (keys, values) {
  # keys and values shall have the same size
  stopifnot (length (keys) == length (values))

  # there shall be no duplicate key (there may be duplicate values)
  stopifnot (length (unique (keys)) == length (keys))

  newDic <- hash::hash()
  eneRgymaps::add.dictionary.keys (dictionary = newDic, keys = keys, values = values)

  return (newDic)
}

#' Add keys (and values) to a given dictionary
#'
#' @param dictionary the dictionary to update
#' @param keys a vector of keys to add
#' @param values a vector of values to add
#' @param force whether to force the update (i.e. replace already existing keys)
#' @return nothing (the dictionary is directly updated)
#' @importFrom hash keys .set
#' @examples
#' add.dictionary.keys (dictionary = myDic, keys = c (12, 13), values = c ("a", "b"))
#' add.dictionary.keys (dictionary = myDic, keys = c (12), values = c ("b"), force = TRUE)
#' @export
add.dictionary.keys <- function (dictionary, keys, values, force = FALSE) {
  stopifnot (force | (! any (keys %in% hash::keys (dictionary))))
  hash::.set (hash = dictionary, keys = keys, values = values)
}

#' Remove keys from a given dictionary
#'
#' @param dictionary the dictionary to update
#' @param keys a vector of keys to remove
#' @return nothing (the dictionary is directly updated)
#' @importFrom hash del
#' @examples
#' remove.dictionary.keys (dictionary = myDic, keys = c (12, 13))
#' @export
remove.dictionary.keys <- function (dictionary, keys) {
  hash::del(x = keys, hash = dictionary)
}


#' Apply a vector of keys to a dictionary
#'
#' @param keys a vector of keys
#' @param dictionary a dictionary
#' @return a vector of values (obtained applying the keys to the dictionary)
#' @importFrom hash keys
#' @importFrom stats na.omit
#' @examples
#' apply.dictionary (keys = c ("France", "Germany", "Spain", dictionary = myDic)
#' @export
apply.dictionary <- function (keys, dictionary) {
  stopifnot (all ((stats::na.omit(unique(keys))) %in% hash::keys (dictionary)))

  return (unlist (lapply (X = keys, FUN = function(k)
    eneRgymaps::get.dictionary.value (key = k, dictionary = dictionary)), use.names = FALSE))
}

#' Retrieve a given value from a dictionary
#'
#' @param key a key
#' @param dictionary a dictionary
#' @return a value (or NA if the key does not belong to the dictionary)
#' @importFrom hash keys
#' @examples
#' get.dictionary.value (key = "France", dictionary = myDic)
#' @export
get.dictionary.value <- function (key, dictionary) {
  if (is.null (key)) {
    return (key)
  } else if (is.na (key)) { # two different ifs to avoid warning when testing is.na (NULL)
    return (key)
  }

  # the dictionary only handles string keys
  keyStr <- toString (key)
  if (keyStr %in% hash::keys (dictionary)) {
    return (dictionary[[keyStr]])
  } else {
    return (NA)
  }
}

#' Check whether all elements within a vector are integers
#'
#' @param dataset a data set
#' @param na.rm whether to ignore NA
#' @return whether all elements are integers
#' @examples
#' integers.all (myData)
#' integers.all (dataset = myDatas, na.rm = TRUE)
#' @export
integers.all <- function(dataset, na.rm = FALSE) {
  isAllInt <- try(expr = all(dataset == floor(dataset), na.rm = na.rm), silent = TRUE)
  if(inherits(isAllInt, "try-error"))
  {
    isAllInt <- FALSE
  }
  return (isAllInt)
}

#' Check if a data.table is NULL (either NULL or data.table::as.data.table(NULL))
#' @param data a data.table to test
#' @return whether the data.table is NULL
#' @importFrom data.table is.data.table
#' @examples
#' is.null.data.table (data.table::as.data.table(NULL))
#' is.null.data.table (NULL)
is.null.data.table <- function(data) {
  if (is.null(data)) {
    return(TRUE)
  }
  stopifnot(data.table::is.data.table(data))
  return(length(data) == 0)
}
