#' INSTALLING THE PACKAGES
#' This file enables easy package installation
#' The underlying configuration comes from the configuration file (see config/example.yaml)

# Install the generic R packages required for installing this R package
if (! require("config")) install.packages("config")
if (! require("devtools")) install.packages("devtools")
if (! require("remotes")) install.packages("remotes")
if (! require("withr")) install.packages("withr")

# retrieve the generic configuration file (returns NULL if the config file doesn't exist)
packageInstallConfig <- config::get(value = "package-install", file = "r-config.yaml",
                               config = Sys.getenv("R_CONFIG_ACTIVE", "default"))

# NULL means installing packages in the default package installation directory,
# otherwise, install everything in the destination path
destinationPath <- NULL
if ((! is.null(packageInstallConfig)) && (! is.null(packageInstallConfig[["directory"]]))) {
  destinationPath <- file.path(packageInstallConfig$directory)
  stopifnot (dir.exists(destinationPath))
}

# avoid compiling packages from sources (to save time)
options(install.packages.compile.from.source = "never")

# Generic installation function (including external dependencies from rnaturalearthhires and r-energy-cache)
install_all_packages <- function() {
  remotes::install_github(repo = "ropensci/rnaturalearthhires")

  #' Reference to use within the r-energy-cache package
  #' (e.g. whether a specific version is requested, "HEAD" for latest available)
  refCachePackage <- "HEAD"
  remotes::install_github(repo = "f-beaude/r-energy-cache", ref = refCachePackage,
                          dependencies = TRUE, upgrade = "never",
                          build_manual = TRUE, build_vignettes = TRUE)

  #' Reference to use within the r-energy-maps package
  #' (e.g. whether a specific version is requested, "HEAD" for latest available)
  refMapsPackage <- "HEAD"
  remotes::install_github(repo = "f-beaude/r-energy-maps", ref = refMapsPackage,
                          dependencies = TRUE, upgrade = "never",
                          build_manual = TRUE, build_vignettes = TRUE)
}

# if destination path is set, customise the destination directory
if (is.null(destinationPath)){
  install_all_packages()
} else {
  withr::with_libpaths(new = destinationPath, code = install_all_packages())
}
