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
 * in RStudio use *Build -> Build and Reload*
 * with devtools use **build()** and then **install()**

Configuration in *Settings.R*
---------------------------
The *Settings.R* file holds the settings of variable configurations to the script.
Please refer to the explanations within this file for further details.


Running the package:
--------------------
In order to run the package, execute the main script "AnalyseWindspeed.R" together with its configuration file *Settings.R*.
 * ./AnalyseWindspeed.R Settings.R

The verification can be run on four different pre-defined time periods.
For each time period, four different analyses can be performed:
 * plot monthly mean time series of station data and reanalyses, their anomalies, and correlations of 10m wind speed
 * the same as above for 100m wind speed between ERA20C and COSMO HErZ reanalyses
 * the same as the first for specific monthss only
 * calculate an S_score between two PDFs and determine their overlapping following Mayer et al., 2015 (determine whether the PDFs belong to the same distribution with the Smirnov test will be added soon)
