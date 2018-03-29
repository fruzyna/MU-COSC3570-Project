~~~~
Program:               MATH_3570_Project
Version:               1.2
Author:                Cade Dombrowski, David Helminiak, Reid Holben, Liam Fruzyna
Date Created:          6 March 2018
Date Last Modified:    24 March 2018
Purpose:               Clean and analyze data found for weather, traffic and crime within Chicago for years 2013-2015
Versioning Plan:       1.0: Data may be opened and viewed
                       1.1: All data clean and combined
                       1.2: Basic visualizations
                       1.3: Model development
                       2.0: Final project deliverable
External Datasets:     https://data.cityofchicago.org/Transportation/Chicago-Traffic-Tracker-Congestion-Estimates-by-Se/n4j6-wkkf
                       https://data.cityofchicago.org/Transportation/Chicago-Traffic-Tracker-Historical-Congestion-Esti/77hq-huss
                       https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
                       NOAA Station GHCND:USW00014819 Weather Infomration from 2001-01-01 00:00 - 2018-02-28 23:59
~~~~    

## SETUP

NOTES: Download additional dataset: https://data.cityofchicago.org/Transportation/Chicago-Traffic-Tracker-Congestion-Estimates-by-Se/n4j6-wkkf for segment IDs

#### Package installations if needed
~~~~
install.packages("geosphere")
install.packages("tidyverse")
install.packages("mice")
install.packages("dplyr")
install.packages("geosphere")
install.packages("maps")
install.packages("ggmap")
install.packages("sf")
install.packages("sp")
install.packages("maptools")
install.packages("doparallel")
install.packages("foreach")
install.packages("tidyverse")
install.packages("sf")
install.packages("sp")
install.packages("maptools")
~~~~

#### IF ERRORS OCCUR FOR sf PACKAGE

On Mac OSX
~~~~
brew unlink gdal
brew tap osgeo/osgeo4mac && brew tap --repair
brew install proj
brew install geos
brew install udunits
brew install gdal2 --with-armadillo --with-complete --with-libkml --with-unsupported
brew link --force gdal2
~~~~
  
On Ubuntu 16.04
~~~~
sudo apt-get install python-setuptools python-dev build-essential
sudo apt install gdal-bin python-gdal python3-gdal
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev 
~~~~

On a RHL Based Distro
~~~~
dnf install proj-devel proj-epsg udunits2-devel geos-devel gdal-devel
install.packages('udunits2', type = 'source', repo = 'cran.rstudio.com', configure.args='--with-udunits2-include=/usr/include/udunits2')
~~~~
