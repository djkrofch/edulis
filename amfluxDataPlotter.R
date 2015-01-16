### This script produces some simple diagnostic plots from the output of amfluxFileReader.R 
### 
### 
### Author: Dan Krofcheck
### Email: djkrofch@unm.edu
### Date Modified: 15 - Jan - 2015

## Load required libraries
library(ggplot2)

# First specify the directory where the data are located
dataDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/Products/'
outputDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/Plots/'

# Add that to the file name to read in
toRead  <- paste(dataDir, 'AllAmfluxData_Daily_Reichstein.csv', sep = '')

# Read in the desired file
rawdata <- read.table(toRead, sep = ',', head = TRUE)

# Create a vector of sitenames from the file as well as a list of the headers
sites <- levels(rawdata$SITE)
all_vars <- names(rawdata) #note here we don't want to plot SITE, YEAR, or DOY, so we exclude the first 3 columns
vars_len <- length(all_vars)
vars <- all_vars[4:vars_len]

for(i in 1:length(sites)){
# Subset the file by site
dataSub <- subset(rawdata, SITE == sites[i])

# Specify the directory in which the plots will be output
outputSiteDir <- paste(outputDir, sites[i], '/', sep = '') 

# Create the directory for the plots if it does not already exist (otherwise, it will yield a warning, 
# but won't impact or stop the script
dir.create(outputSiteDir)

# Determine some quick dimensions for plots to account for differing number of years between sites
len_years <- max(rawdata$YEAR) - min(rawdata$YEAR)
height <- 4
width <- 4 * len_years

    for(k in 1:length(vars)){
	fname <- paste(outputSiteDir, sites[i], vars[k], '.jpg', sep = '_')
	ggplot(dataSub, aes_string('DOY', vars[k])) + geom_point() + geom_line() +
	facet_grid(~YEAR) + theme_bw() + ggsave(fname, height = height, width = width)
    }
}


