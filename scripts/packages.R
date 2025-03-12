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
library(stats)  # for stl()
setwd("/Users/prashantkulkarni/Documents/source-code/time-series/cyber-threat-time-series")

library(zoo)
library(forecast)
# Install if not already installed
install.packages("skimr")

library(skimr)
# Install if not already installed
install.packages("summarytools")

library(summarytools)
install.packages("kableExtra")
library(kableExtra)


install.packages("prophet")
library(prophet)

