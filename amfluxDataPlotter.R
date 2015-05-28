### This script produces some simple diagnostic plots from the output of amfluxFileReader.R 
### 
### 
### Author: Dan Krofcheck
### Email: djkrofch@unm.edu
### Date Modified: 27 - Feb- 2015

## Load required libraries
library(ggplot2)
library(reshape)
library(grid)

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

# Special Plots --- Some variables we want to view together, on the same set of axes (for convenience and context).
# Here, we specify which column names should be treated in a special way, and then deal with plotting those
# after the bulk of the plots
reqVars <- c('YEAR','DOY')
special <- c('TA_mean','TA_min','TA_max')
special_TA <- c('TA_mean','TA_min','TA_max')
special_FC <- c('FC','FC_day','FC_night')
special_GPP <- c('FC','GPP','RE')
special_ET <- c('ET','PET')
special_PRECIP <- c('PRECIP.x', 'PRECIP.y')

for(i in 1:length(sites)){
# Subset the file by site
dataSub <- subset(rawdata, SITE == sites[i])
specialSub_TA <- dataSub[c(reqVars, special_TA)]
specialSub_FC <- dataSub[c(reqVars, special_FC)]
specialSub_GPP <- dataSub[c(reqVars, special_GPP)]
specialSub_ET<- dataSub[c(reqVars, special_ET)]
specialSub_PRECIP<- dataSub[c(reqVars, special_PRECIP)]
TA <- melt(specialSub_TA, c('YEAR','DOY'))
FC <- melt(specialSub_FC, c('YEAR','DOY'))
ET <- melt(specialSub_ET, c('YEAR','DOY'))
GPP <- melt(specialSub_GPP, c('YEAR','DOY'))
PRECIP <- melt(specialSub_PRECIP, c('YEAR','DOY'))

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

# TA plots
fname <- paste(outputSiteDir, sites[i], 'TA_plots', '.jpg', sep = '_')
ggplot(TA, aes(DOY, value)) + geom_point(aes(color = variable)) + geom_line(aes(alpha = 0.2)) +
facet_grid(~YEAR) + theme_bw() + 
scale_color_manual(values = c('black','blue','red'), labels = c('TA mean','TA min','TA max'))  +
guides(color=guide_legend(title='Air Temperature'), alpha = FALSE) + ylab('Temp (degC)') + xlab('Day of Year') +
ggsave(fname, height = height, width = width)

# FC plots
fname <- paste(outputSiteDir, sites[i], 'FC_timebins', '.jpg', sep = '_')
ggplot(FC, aes(DOY, value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable,alpha = 0.2)) +
facet_grid(~YEAR) + theme_bw() + 
scale_color_manual(values = c('black','blue','red'), labels = c('FC','FC day','FC night'))  +
guides(color=guide_legend(title='Fc'), alpha = FALSE) + ylab('gC day') + xlab('Day of Year') + 
ggsave(fname, height = height, width = width)

# FC component plots
fname <- paste(outputSiteDir, sites[i], 'FC_components', '.jpg', sep = '_')
ggplot(GPP, aes(DOY, value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable,alpha = 0.2)) +
facet_grid(~YEAR) + theme_bw() + 
scale_color_manual(values = c('black','blue','red'), labels = c('FC','GPP','RE'))  +
guides(color=guide_legend(title='Fc'), alpha = FALSE) + ylab('gC day') + xlab('Day of Year') +
ggsave(fname, height = height, width = width)

# ET
fname <- paste(outputSiteDir, sites[i], 'ETplots', '.jpg', sep = '_')
P1 <- ggplot(ET, aes(DOY, value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable,alpha = 0.2)) +
facet_grid(~YEAR) + theme_bw() + 
scale_color_manual(values = c('black','red'), labels = c('ET','PET'))  +
guides(color=guide_legend(title='ET'), alpha = FALSE) + ylab('EvapoTranspiration (mm)') + xlab('Day of Year') +
ggsave(fname, height = height, width = width)

# PRECIP 
fname <- paste(outputSiteDir, sites[i], 'PRECIPplots', '.jpg', sep = '_')
P1 <- ggplot(PRECIP, aes(DOY, value)) + geom_point(aes(color = variable)) + geom_line(aes(color = variable,alpha = 0.2)) +
facet_grid(~YEAR) + theme_bw() + 
scale_color_manual(values = c('black','red'), labels = c('PRECIP','PRECIP_f'))  +
guides(color=guide_legend(title='PRECIP'), alpha = FALSE) + ylab('PRECIP (mm)') + xlab('Day of Year') +
ggsave(fname, height = height, width = width)
}


