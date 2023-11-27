
# if (!requireNamespace(package_name, quietly = TRUE)) {
#   # If not installed, install the package
#   install.packages(package_name)
# }

# Load theses libraries 

library(readr) # open csv files
library(zoo) ## interpolating NA 
library(tseries) # generate surrogates
library(EnvCpt) # envcpt changepoint detection method
library(tidyverse) # pipeline
library(dplyr) # select() and sumerize
library(strucchange) # Fstat ... functions
library(rshift) # STARS
library(beepr) # makes sounds when finished running
library(ggplot2) # make plots
library(formattable) # function for plot y scales
library(scales) # ggplot scales percentage
library(ggpubr) # grid.arrange for ggplots
library(ggExtra) # distribution plot on the side of the time-series plots
library(cowplot)
library(patchwork) # plotting ggplots side-by-side!
library(purrr)
library(grid) # textGrob

# chronological clustering 
library(rioja)
library(vegan)
library(adespatial)

library(plotly)

library(ggchicklet)


# make pretty html tables 

library(kableExtra)
