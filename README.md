EVA_stationobs
==============
UERRA common evaluation procedure: assessing uncertainties in reanalysis by evaluation against station observations.

This package uses German station observations to compare against global (ERA20C and ERA-Interim) and regional (COSMO HErZ) reanalyses by plotting and analysing station and co-located reanalysis time series. The verification so far is performed on monthly means.

Station observations are downloaded by the package during run-time from the CDC-ftp server of DWD. The reanalysis data had been downloaded from ECMWF (global reanalyses) and directly from Uni Bonn (regional reanalysis) and post processed to a time series of monthly means. These data files are necessary for the package to run properly. The post-processed files can be accessed through ECMWF's server **ecgate** at /scratch/ms/de/ded3/data/EVA_stationobs/.

Further data sources may be added later, i.e., other regional reanalyses or station observations when or if available.

So far, this package (EVAstatobsR) is distributed as a source package, i.e., the pure source code is provided.
With the package "devtools" it can be installed; RStudio provides easy access to build functionality.
The folder EVA_statobs provides the environment under which the package is useable. It includes:
 * the run script *AnalyseWindspeed.R* and its configuration file *Settings.R*
 * folders from which data is read and into which data and results will be written during run-time
  * ./data/, ./DWD_station_data/, ./output/

Installation:
-------------
1. System requirements: Since this package handles netCDF files the following libraries need to be installed on your system:
 * netcdf-bin
 * libnetcdf-dev

2. Necessary R dependencies:
 * netcdf
 * netcdf.tools
 * xts

3. Nice-to-have R dependencies:
 * devtools

4. Clone github repository:
 * git clone https://github.com/UERRA-EVA/EVA_stationobs.git

5. Build the package:
 * in RStudio use **Build -> Build and Reload**
 * with devtools use **build()** and then **install()**

Configuration in **Settings.R**
---------------------------
The *Settings.R* file holds the settings of variable configurations to the script.
Please refer to the explanations within this file for further details.


Running the package:
--------------------

In order to run the program , the file "./AnalyseWindspeed.R" needs to be executedwith its settings file Settings.R needs to be run.
In "Settings.R" data files and plotting options need to be set. The folders holding the data and the output plots need to be specified.
