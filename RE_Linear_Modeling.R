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
# Edited: 4 - 8 - 2015

# Load required libraries 
library(ggplot2)
library(reshape)

# Load data output from Landsat Data Prep script
LMdatat <- read.table('/home/nikko/Research/RE_Linear_Modeling/AllLinearModelingData.csv',sep = ' ', head = TRUE)

# Control
# LMdata <- LMdatat[LMdatat$SITE == 'Control', ]
LMdata <- LMdatat

# Create sensor subsets (for ETM, RE, and Tower data)
ETMdata <- LMdata[LMdata$SENSOR == 'LandsatETM', ]
REdata <- LMdata[LMdata$SENSOR == 'RapidEye', ]
Towerdata <- LMdata[LMdata$SENSOR == 'Tower', ]

TowerData <- data.frame(YEAR = Towerdata$YEAR[Towerdata$VARIABLE == 'PAR'], DOY = Towerdata$DOY[Towerdata$VARIABLE == 'PAR'],
			SITE = Towerdata$SITE[Towerdata$VARIABLE == 'TA_mean'],TA = Towerdata$VALUE[Towerdata$VARIABLE == 'TA_mean'],
			PAR = Towerdata$VALUE[Towerdata$VARIABLE == 'PAR'], GPP = Towerdata$VALUE[Towerdata$VARIABLE == 'GPP'],
			RG = Towerdata$VALUE[Towerdata$VARIABLE == 'Rg'])

RapidEyeData <- data.frame(YEAR = REdata$YEAR[REdata$VARIABLE == 'NDVI'], DOY = REdata$DOY[REdata$VARIABLE == 'NDVI'],
			SITE = REdata$SITE[REdata$VARIABLE == 'NDVI'],
			NDVIRE = REdata$VALUE[REdata$VARIABLE == 'NDVI'], NDRE = REdata$VALUE[REdata$VARIABLE == 'NDRE'])

LandsatData <- data.frame(YEAR = ETMdata$YEAR[ETMdata$VARIABLE == 'NDVI'], DOY = ETMdata$DOY[ETMdata$VARIABLE == 'NDVI'],
			SITE = ETMdata$SITE[ETMdata$VARIABLE == 'NDVI'],
			NDVILS = ETMdata$VALUE[ETMdata$VARIABLE == 'NDVI'], NDWI = ETMdata$VALUE[ETMdata$VARIABLE == 'NDWI'])

modelData <- merge(TowerData, RapidEyeData, by = c('SITE','YEAR','DOY'))
modelData <- merge(modelData, LandsatData, by = c('SITE','YEAR','DOY'))
#write.table(modelData, 'LMdataComplete.csv', row.names = FALSE)
modelData <- read.table('LMdataComplete_new.csv',head = TRUE, sep = ',')

modelData <- modelData[modelData$SITE == 'Control', ]
#modelData <- modelData[abs(modelData$ANGLE) >= 7, ]
#modelData <- modelData[modelData$YEAR == '2009', ]
#ggplot(modelData, aes(NDVILS, GPP)) + geom_point()

NDVI_LS  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVILS + modelData$NDVILS*modelData$TA)
NDVI_LSw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVILS + modelData$NDWI + modelData$TA*modelData$NDVILS)
NDVI_RE  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVIRE + modelData$RG * modelData$TA)
NDVI_REw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVIRE + modelData$NDWI + modelData$RG*modelData$TA)
NDRE_RE  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDRE)
NDRE_REw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDRE + modelData$NDWI)

modelOutputC <- data.frame(modelData[, 1:7], NDVILS = fitted(NDVI_LS), NDVILSW = fitted(NDVI_LSw), 
		NDVIRE = fitted(NDVI_RE), NDVIREW = fitted(NDVI_REw), NDRE = fitted(NDRE_RE),
		NDREW = fitted(NDRE_REw))

labs <- matrix(nrow = 6, ncol = 1) 
labs[1,1] <- summary(NDVI_LS)$adj.r.squared
labs[2,1] <- summary(NDVI_LSw)$adj.r.squared
labs[3,1] <- summary(NDVI_RE)$adj.r.squared
labs[4,1] <- summary(NDVI_REw)$adj.r.squared
labs[5,1] <- summary(NDRE_RE)$adj.r.squared
labs[6,1] <- summary(NDRE_REw)$adj.r.squared


fittextC <- data.frame(x = 1, y = 6, SITE = 'Control', MODEL = c('NDVILS','NDVILSW','NDVIRE','NDVIREW',
			'NDRE','NDREW'), fits = round(labs, digits = 3)) 

modelData <- read.table('LMdataComplete_new.csv',head = TRUE, sep = ',')

modelData <- modelData[modelData$SITE == 'Girdle', ]
#modelData <- modelData[abs(modelData$ANGLE) >= 7, ]
#modelData <- modelData[modelData$YEAR == '2009', ]
#ggplot(modelData, aes(NDVILS, GPP)) + geom_point()

NDVI_LS  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVILS + modelData$NDVILS*modelData$RG)
NDVI_LSw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVILS + modelData$NDWI + modelData$RG*modelData$NDVILS)
NDVI_RE  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVIRE + modelData$RG * modelData$TA)
NDVI_REw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDVIRE + modelData$NDWI + modelData$RG*modelData$NDVIRE)
NDRE_RE  <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDRE + modelData$TA* modelData$RG)
NDRE_REw <- lm(modelData$GPP ~ modelData$RG + modelData$TA + modelData$NDRE + modelData$NDWI + modelData$NDWI* modelData$RG)

labs <- matrix(nrow = 6, ncol = 1) 
labs[1,1] <- summary(NDVI_LS)$adj.r.squared
labs[2,1] <- summary(NDVI_LSw)$adj.r.squared
labs[3,1] <- summary(NDVI_RE)$adj.r.squared
labs[4,1] <- summary(NDVI_REw)$adj.r.squared
labs[5,1] <- summary(NDRE_RE)$adj.r.squared
labs[6,1] <- summary(NDRE_REw)$adj.r.squared

fittextG <- data.frame(x = 1, y = 6, SITE = 'Girdle', MODEL = c('NDVILS','NDVILSW','NDVIRE','NDVIREW',
			'NDRE','NDREW'), fits = round(labs, digits = 3)) 

modelOutputG <- data.frame(modelData[,1:7], NDVILS = fitted(NDVI_LS), NDVILSW = fitted(NDVI_LSw), 
		NDVIRE = fitted(NDVI_RE), NDVIREW = fitted(NDVI_REw), NDRE = fitted(NDRE_RE),
		NDREW = fitted(NDRE_REw))

modelOutputGm <- melt(modelOutputG, c('SITE','YEAR','DOY','TA','PAR','GPP','RG'))
modelOutputCm <- melt(modelOutputC, c('SITE','YEAR','DOY','TA','PAR','GPP','RG'))
modelOutput <- rbind(modelOutputGm, modelOutputCm)
names(modelOutput) <- c('SITE','YEAR','DOY','TA','PAR','GPP','RG','MODEL','GPP_Est')

allFits <- rbind(fittextC, fittextG)

ggplot(modelOutput, aes(GPP, GPP_Est)) + geom_point(aes(shape = factor(YEAR), color = factor(DOY)), size = 4) +
		facet_grid(MODEL~SITE) + 
		geom_text(data = allFits, aes(x = x, y = y, label = fits)) + theme_bw() + 
		geom_smooth(method = lm, se = FALSE)

		
ggplot(modelOutputC, aes(NDVILS, GPP)) + geom_point() + facet_grid(~SITE)
