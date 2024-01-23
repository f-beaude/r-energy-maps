Energy maps
============

Purpose
========

This package enables plotting geographic maps related to the electricity sector, depicting information related to countries, bidding-zones or load frequency control areas (mainly in the European Union).

License
========
THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
The data provided for the automated tests and examples is for illustration purpose only. Shapefiles and EIC codes are the property (and subject to license) of their respective owner.

See DESCRIPTION for detailed license.

Installing the package
=====================

The package may be installed either
* based on Github
* locally with [RStudio](https://www.rstudio.com/)

Github install
----------------
1) download the [installation file](util/install/install_package.R)
2) run the installation file
3) run the [examples](inst/examples/maps.R) to check that the package works properly

RStudio install
----------------
1) download the full package
2) open the [R project](eneRgymaps.Rproj) in R Studio
3) build the package using RStudio
In RStudio, use Build > Clean and Install. 
When building for the first time, if you get an error "ERROR: a 'NAMESPACE' file is required", build the NAMESPACE file first with Build > Document, before running Clean and Install again.
See [here](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages) for more help.
4) run the [examples](inst/examples/maps.R) to check that the package works properly

If you need help, please contact [Francois Beaude](mailto:Francois.Beaude@gmail.com)
