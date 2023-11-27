# ICES_JMS-RS-detection-methods
Script for the paper: "Poor performance of regime shift detection methods in marine ecosystems"


Here is a brief overview of the contents of the repository:

Main contains : 
- Two "main" scripts :
  - Main_script_applications.R: the script containing the necessary to apply the different detection methods to the 32 Norwegian Sea time series
  - Main_script.R: script containing the necessary to produce (or just use the Surrogates generated for this paper) the surrogates for each time series and apply the different detection methods and produce the data in a readible format
- Libraries.R : Contains all the libraries needed for these analyses, to be loaded into your environment before beginning
- Functions.F: all the functions I created to optimise the code, to be loaded into your environment before beginning
- Oct_plotFin.R: all the code to produce the plots in the paper
- 30oct_prettyTables.R : script to produce the html tables included in the Supplementary Information
- 3 folders:
  - Data: the original time series data used, the surrogates used and the omnibused (normalised) version of the time series used. Also a file (Strucc_SFts) containing the start and end date of each time series, necessary for some functions
  - Data_results: the results data inputted for the figures
  - Tables: All tables included in the Supplementary Information
 
  
