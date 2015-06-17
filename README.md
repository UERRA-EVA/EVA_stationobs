EVA_stationobs
==============
UERRA common evaluation procedure: assessing uncertainties in reanalysis by evaluation against station observations.

Station observations are taken from the CDC-ftp server of DWD and cover Germany. A further data source may be added later.

Running the package:
--------------------
Verifiaction is performed against monthly means of ERA20C and ERA-Interim global reanalyses as well as the COSMO HErZ regional reanalysis.
These data files need to be downloaded prior to running this package. They can be accessed at ECMWF's server ecgate at /scratch/ms/de/ded3/data/EVA_stationobs/.

So far, this package is distributed as a source package, i.e., the pure source code is provided.
With tha package "devtools" it can be installed; RStudio provides easy access to build functionality.

After installation, the file "./AnalyseWindspeed.R Settings.R" with its settings file Settings.R needs to be run.
In "Settings.R" data files and plotting options need to be set. The folders holding the data and the output plots need to be specified.


