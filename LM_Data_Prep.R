# Read in the raw RapidEye VI csv, generated in the iPython notebook
# We just need to convert the data into a format similar to the 
# Landsat data, and parse the timestamp into YEAR and DOY.
# The final step in this script performs linear interpolation on the Landsat data,
# and then picks the Landsat ETM dates which coinside with RapidEye acquisition dates.
# Finally, all of the control and girdle ETM and RE data are concatenatead
# and saved to CSV.
#
#
# This script was written in support of the linear modeling manuscript, tentatively
# titled 'remote sensing based simple models of GPP in both disturbed and undisturbed
# pinon-juniper woodlands in the Southwestern US.
#
# Author: Dan Krofcheck
# Edited: 4 - 6 - 2015

# Load required libraries 
library(ggplot2)
library(reshape)

# Load output of iPython notebook RE zonal stats analysis
dataDir <- '/home/nikko/Research/RE_Linear_Modeling/'
ETMcsv <- 'allETMdata.csv'
REcsv <- 'RapidEye_VI.csv'

# Load Ameriflux daily file so that the corresponding dates and data values
# can be appended to the RS data for the modeling bits.
fluxdata <- read.table('/home/nikko/Research/Data/TowerData/AmerifluxFiles/Products/AllAmfluxData_Daily_Reichstein.csv',sep = ',', head = TRUE)

# subset out only control and girdle flux data
sitedata <- fluxdata[which(fluxdata$SITE == 'Mpj' | fluxdata$SITE == 'Mpg'), ]

# Establish a 'YEAR DOY' column in the sitedata dataframe for quick subsetting later
sitedata <- data.frame(sitedata, YEAR_TS = paste(sitedata$YEAR, sitedata$DOY))

# Read in the RE and ETM data from the output of the zonal stats workflow
rawdata <- read.table(paste(dataDir, REcsv, sep = ''), sep = ',', head = TRUE)
ETMrawdata <- read.table(paste(dataDir, ETMcsv, sep = ''), sep = ',', head = TRUE)

# Parse the datetime from the Landsat ETM data
rawdata$TS  <- as.POSIXlt(strptime(rawdata$date, format = '%Y%m%d'))

# Clean up headers and save the outputs
RE_1<- data.frame(YEAR = rawdata$TS$year + 1900, DOY = rawdata$TS$yday, 
	Control = rawdata$controlVI, Girdle= rawdata$girdleVI, INDEX = rawdata$index,
	SENSOR = 'RapidEye')

# For some reason, the DOY from the POSIX timestamp gets the DOY wrong by one day. So,
# Add one to the DOY
RE_1$DOY <- RE_1$DOY + 1

REdata <- melt(RE_1, c('YEAR','DOY','INDEX', 'SENSOR'))
names(REdata) <- c('YEAR','DOY','INDEX','SENSOR','SITE','VI')
write.table(REdata, 'RapidEye_VI_TS.csv', sep = ',', row.names = FALSE)

# We need to define the day of year vectors for each year which
# we want to keep the Landsat interpolated data. 
CDOY2009 <- REdata[which(REdata$SITE == 'Control' & REdata$YEAR == 2009 & REdata$INDEX == 'NDVI'), 2]
GDOY2009 <- REdata[which(REdata$SITE == 'Girdle' & REdata$YEAR == 2009 & REdata$INDEX == 'NDVI'), 2]

# Need to do linear interpolation on the Landsat data
control <- ETMrawdata[which(ETMrawdata$SITE == 'Control'), ]
girdle <- ETMrawdata[which(ETMrawdata$SITE == 'Girdle'), ]

# 2009 data ----
# Control
control <- control[which(control$YEAR == 2009), ]

NDVIc <- control[which(control$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Control',YEAR = 2009,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
NDVI_C <- NDVI_C[NDVI_C$DOY %in% CDOY2009, ]
EVIc <- control[which(control$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Control',YEAR = 2009,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
EVI_C <- EVI_C[EVI_C$DOY %in% CDOY2009, ]
TEMPc <- control[which(control$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Control',YEAR = 2009,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
TEMP_C <- TEMP_C[TEMP_C$DOY %in% CDOY2009, ]
NDWIc <- control[which(control$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Control',YEAR = 2009,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')
NDWI_C <- NDWI_C[NDWI_C$DOY %in% CDOY2009, ]
Control2009VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

# Girdle 
girdle <- girdle[which(girdle$YEAR == 2009), ]

NDVIc <- girdle[which(girdle$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Girdle',YEAR = 2009,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
EVIc <- girdle[which(girdle$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Girdle',YEAR = 2009,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
TEMPc <- girdle[which(girdle$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Girdle',YEAR = 2009,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
NDWIc <- girdle[which(girdle$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Girdle',YEAR = 2009,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')
Girdle2009VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

# 2010 data ----
control <- ETMrawdata[which(ETMrawdata$SITE == 'Control'), ]
girdle <- ETMrawdata[which(ETMrawdata$SITE == 'Girdle'), ]
# Control
control <- control[which(control$YEAR == 2010), ]

NDVIc <- control[which(control$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Control',YEAR = 2010,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
EVIc <- control[which(control$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Control',YEAR = 2010,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
TEMPc <- control[which(control$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Control',YEAR = 2010,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
NDWIc <- control[which(control$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Control',YEAR = 2010,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')
Control2010VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

# Girdle 
girdle <- girdle[which(girdle$YEAR == 2010), ]

NDVIc <- girdle[which(girdle$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Girdle',YEAR = 2010,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
EVIc <- girdle[which(girdle$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Girdle',YEAR = 2010,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
TEMPc <- girdle[which(girdle$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Girdle',YEAR = 2010,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
NDWIc <- girdle[which(girdle$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Girdle',YEAR = 2010,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')
Girdle2010VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

# 2011 data ----
control <- ETMrawdata[which(ETMrawdata$SITE == 'Control'), ]
girdle <- ETMrawdata[which(ETMrawdata$SITE == 'Girdle'), ]
# Control
control <- control[which(control$YEAR == 2011), ]

NDVIc <- control[which(control$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Control',YEAR = 2011,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
EVIc <- control[which(control$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Control',YEAR = 2011,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
TEMPc <- control[which(control$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Control',YEAR = 2011,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
NDWIc <- control[which(control$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Control',YEAR = 2011,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')
Control2011VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

# Girdle 
girdle <- girdle[which(girdle$YEAR == 2011), ]

NDVIc <- girdle[which(girdle$INDEX == 'NDVI'), ]
NDVI_interpC <- approx(NDVIc$DOY, NDVIc$VI, 1:365)
NDVI_C <- data.frame(SITE = 'Girdle',YEAR = 2011,
	VI = NDVI_interpC$y, DOY = 1:365, INDEX = 'NDVI')
EVIc <- girdle[which(girdle$INDEX == 'EVI'), ]
EVI_interpC <- approx(EVIc$DOY, EVIc$VI, 1:365)
EVI_C <- data.frame(SITE = 'Girdle',YEAR = 2011,
	VI = EVI_interpC$y, DOY = 1:365, INDEX = 'EVI')
TEMPc <- girdle[which(girdle$INDEX == 'TEMP'), ]
TEMP_interpC <- approx(TEMPc$DOY, TEMPc$VI, 1:365)
TEMP_C <- data.frame(SITE = 'Girdle',YEAR = 2011,
	VI = TEMP_interpC$y, DOY = 1:365, INDEX = 'TEMP')
NDWIc <- girdle[which(girdle$INDEX == 'NDWI'), ]
NDWI_interpC <- approx(NDWIc$DOY, NDWIc$VI, 1:365)
NDWI_C <- data.frame(SITE = 'Girdle',YEAR = 2011,
	VI = NDWI_interpC$y, DOY = 1:365, INDEX = 'NDWI')

Girdle2011VI <- rbind(NDVI_C, EVI_C, TEMP_C, NDWI_C)

allETMdata <- rbind(Control2009VI, Girdle2009VI, Control2010VI, Girdle2010VI,
	Control2011VI, Girdle2011VI)
allETMdata <- data.frame(allETMdata, SENSOR = 'LandsatETM')

ggplot(allETMdata, aes(DOY, VI)) + geom_line(aes(color = factor(YEAR))) + facet_wrap(SITE~INDEX, scales = 'free') +
theme_bw()

allControlETM <- allETMdata[which(allETMdata$SITE == 'Control'), ]
allControlETMts <- data.frame(allControlETM, DOYTS = paste(allControlETM$YEAR, allControlETM$DOY))
allControlRE <- REdata[which(REdata$SITE == 'Control'), ]
allControlREts <- data.frame(allControlRE, DOYTS = paste(allControlRE$YEAR, allControlRE$DOY))

allGirdleETM <- allETMdata[which(allETMdata$SITE == 'Girdle'), ]
allGirdleETMts <- data.frame(allGirdleETM, DOYTS = paste(allGirdleETM$YEAR, allGirdleETM$DOY))
allGirdleRE <- REdata[which(REdata$SITE == 'Girdle'), ]
allGirdleREts <- data.frame(allGirdleRE, DOYTS = paste(allGirdleRE$YEAR, allGirdleRE$DOY))

# Subset the days from the interpolated ETM data for which we have RE data
ETMcontrolSub <- allControlETMts[allControlETMts$DOYTS %in% allControlREts$DOYTS,]
ETMgirdleSub <- allGirdleETMts[allGirdleETMts$DOYTS %in% allGirdleREts$DOYTS,]

ggplot(ETMcontrolSub, aes(DOY, VI)) + geom_line(aes(color = factor(YEAR))) + facet_wrap(SITE~INDEX, scales = 'free') + theme_bw()
ggplot(ETMgirdleSub, aes(DOY, VI)) + geom_line(aes(color = factor(YEAR))) + facet_wrap(SITE~INDEX, scales = 'free') + theme_bw()

# Subset the fluxdata in a similar fashion
fluxGirdle <- sitedata[sitedata$SITE == 'Mpg', ]
fluxGirdle$SITE <- 'Girdle'
fluxG <- fluxGirdle[fluxGirdle$YEAR_TS %in% allGirdleREts$DOYTS, ]
fluxControl <- sitedata[sitedata$SITE == 'Mpj', ]
fluxControl$SITE <- 'Control'
fluxC <- fluxControl[fluxControl$YEAR_TS %in% allControlREts$DOYTS, ]
fluxData <- rbind(fluxG, fluxC)
fluxDatam <- melt(fluxData, c('YEAR','DOY','YEAR_TS','SITE'))
fluxDataSub <- data.frame(fluxDatam, SENSOR = 'Tower')
names(fluxDataSub) <- c('YEAR','DOY','YEAR_TS','SITE','VARIABLE','VALUE','SENSOR')

girdleData <- rbind(allGirdleREts, ETMgirdleSub)
controlData <- rbind(allControlREts, ETMcontrolSub)
allLMdata <- rbind(girdleData, controlData)
names(allLMdata) <- c('YEAR','DOY','VARIABLE','SENSOR','SITE','VALUE','YEAR_TS')

LMdataComplete <- rbind(allLMdata, fluxDataSub)
write.table(LMdataComplete, 'AllLinearModelingData_v2.csv', row.names = FALSE)

