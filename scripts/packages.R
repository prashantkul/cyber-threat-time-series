# Install packages if not already installed
required_packages <- c("tidyverse", "corrplot", "lubridate", "forecast", "ggplot2", "dplyr", "corrplot")
installed_packages <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load libraries
library(tidyverse)
library(corrplot)
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)
library(corrplot)

setwd("/Users/prashantkulkarni/Documents/source-code/time-series/cyber-threat-time-series")
