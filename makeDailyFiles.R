### This script reads in all Ameriflux files in a directory, either gapfilled or
###  with_gaps, and produces 2 files:
### 1) a concatenated 30 minute file, and 2) a concatenated daily file. The daily 
### file variables now include most of the commonly used variables, including now ET, PET, and
### Penman-Monteith stomatal conductance / atmospheric conductance variables. 
### 
### Author: Dan Krofcheck
### Email: djkrofch@unm.edu
### Date Modified: 23 - Feb - 2015



### ---------------- Input Data -------------------------- ###

# First specify the directory where the data are located
dataDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/'

# Specify whether you want to look at gapfilled or with_gaps files
# Here, we are just specifying a pattern that exists in the file names which
# differentiates the files from one another
fileType <- 'gapfilled'

# Gapfilled precip has been an effort since Nov 2014. We now have some first cut efforts
# at this product. The data are daily at this point, so we will read it in here,
# and merge it with the daily file after the Ameriflux file processing is complete.
gapfilled_precip <- 'dailyprecip.csv'


### ---------------- Function Definitions ---------------------- ###

# Several helper functions designed to be used with the Penman-Monteith
# calculations, and (although not yet implemented) the Priestly-Taylor
# calculations for ET and PET. Current the P-T calculations are done in-line
# and will be moved up here for organization reasons later on.



calc_eta <- function(, arg2, ... ){
statements
return(object)
}

# Specify the column names we want to appear in the daily file
# Currently, edits here require edits inside the dailyfile
# Need to have the loop dynamically generated via a function, using this list as inputs ultimately.
dailynames <- c('SITE','YEAR','DOY','TA_mean','TA_min','TA_max','FC','FC_day','FC_night','H','LE','PRECIP',
	'RH','PA','VPD_day','VPD_day_min','VPD_day_max','RNET','PAR','Rg','Rg_out','Rlong_in','Rlong_out',
	'RE','GPP','GAP_qual','LUE','WUE', 'ET','PET')

### ------------------------------------------------------------ ###

# Generate a list of file names that exist in our data directory 
fileList  <- list.files(dataDir, pattern = fileType)
fullNames <- paste(dataDir, fileList, sep = '')

# Read in the first Ameriflux file. Note here we skip all the non-data rows when we read it in. This would not be required if we didn't have the extra metadata rows.
firstFile <- data.frame(read.table(fullNames[1], sep = ',', skip = 5))

# A little trick to deal with the column names for the Ameriflux Files
colNames <- read.table(fullNames[1], sep = ',', skip = 3, nrows = 1, head = TRUE)

# Now we assign the column names to the first file that we read in
names(firstFile) <- names(colNames)

# Now we have a variable names firstFile, which has the correct number of columns with the correct column names. Next, we are going to extract the site name from the file, and append it as a column named 'SITE'
split1 <- strsplit(fileList[1], '-') # splits the filename wherever a '-' exists
split2 <- strsplit(split1[[1]][2], '_') # splits the second chunk of that name wherever a '_' exists
sitename <- split2[[1]][1] # picks only the first chunk of the second split

# Finally, create a new dataframe with a new column 'SITE' having the value 'sitename' for all of its rows
AmfluxFile <- data.frame(firstFile, SITE = sitename)
AmfluxFile[AmfluxFile == -9999] <- NaN

# Now we will loop through every file in our list and concatenate them to eachother, as well as add the site column to each one as we go.
for(i in 1:length(fileList)){

    # Creating some output can be useful to determine on which file a loop breaks. here we just use the message() function to do so.
    message(paste('reading file', i, 'of', length(fileList)),"\r",appendLF=FALSE)
    flush.console()

    # Note we included 'i' as the loop iterator in the variable to be read in

    fluxfile <- data.frame(read.table(fullNames[i], sep = ',', skip = 5))
    split1 <- strsplit(fileList[i], '-') 
    split2 <- strsplit(split1[[1]][2], '_')
    sitename <- split2[[1]][1] 

    # Assigning the names seems to be required by rbind() below
    names(fluxfile) <- names(colNames)

    # Append the sitename
    fluxfile <- data.frame(fluxfile, SITE = sitename)
    fluxfile[fluxfile == -9999] <- NaN


    # To create the daily files, we need to first figure out how many days are in each file (as this will determine the row numbers).
    numdays <- max(fluxfile$DOY)
    dailyfile <- data.frame(matrix(nrow = numdays, ncol = length(dailynames)))
    names(dailyfile) <- dailynames

    # umol/m2/s conversion to gC for carbon fluxes
    convert <- 12.01 * 1800 / 1000000 # gC per 30 minute flux interval

    ### ----------- Daily Amflux File Generation ------------ ###

    for(d in 1:numdays){ 
	message(paste('processing day', d, 'of', numdays),"\r",appendLF=FALSE)

	# Subset only the rows for a single day
	thisday <- fluxfile[which(fluxfile$DOY == d),]

	# Subset rows within this day that are daylight only, determine the length of that variable
	daytime <- thisday[which(thisday$Rg > 10), ]
	nighttime <- thisday[which(thisday$Rg <= 11), ]
	day_obs <- length(daytime$DOY)

	dailyfile[d,1] <- sitename 
	dailyfile[d,2] <- thisday$YEAR[1]
	dailyfile[d,3] <- d
	dailyfile[d,4] <- mean(thisday$TA, na.rm = TRUE)
	dailyfile[d,5] <- min(thisday$TA, na.rm = TRUE)
	dailyfile[d,6] <- max(thisday$TA, na.rm = TRUE)
	dailyfile[d,7] <- sum(thisday$FC * convert, na.rm = TRUE)
	dailyfile[d,8] <- sum(daytime$FC * convert, na.rm = TRUE)
	dailyfile[d,9] <- sum(nighttime$FC * convert, na.rm = TRUE)
	dailyfile[d,10] <- mean(daytime$H, na.rm = TRUE) 
	dailyfile[d,11] <- mean(daytime$LE, na.rm = TRUE)
	dailyfile[d,12] <- sum(thisday$PRECIP, na.rm = TRUE)
	dailyfile[d,13] <- mean(thisday$RH, na.rm = TRUE)
	dailyfile[d,14] <- mean(thisday$RH, na.rm = TRUE)
	dailyfile[d,15] <- mean(daytime$VPD, na.rm = TRUE)
	dailyfile[d,16] <- min(daytime$VPD, na.rm = TRUE)
	dailyfile[d,17] <- max(daytime$VPD, na.rm = TRUE)
	dailyfile[d,18] <- sum(thisday$RNET, na.rm = TRUE)
	dailyfile[d,19] <- sum(thisday$PAR, na.rm = TRUE)
	dailyfile[d,20] <- sum(thisday$Rg, na.rm = TRUE)
	dailyfile[d,21] <- sum(thisday$Rg_out, na.rm = TRUE)
	dailyfile[d,22] <- sum(thisday$Rlong_in, na.rm = TRUE)
	dailyfile[d,23] <- sum(thisday$Rlong_out, na.rm = TRUE)
	dailyfile[d,24] <- sum(thisday$RE * convert, na.rm = TRUE)
	dailyfile[d,25] <- sum(thisday$GPP * convert, na.rm = TRUE)
	dailyfile[d,26] <- sum(thisday$FC_flag) / 48
	dailyfile[d,27] <- sum(thisday$GPP * convert) / sum(thisday$Rg)

	# ET calculation
	# lambda - value of vapor latent heat flux
	lambda = 2501-2.4*dailyfile[d,4]

	# H and LE, just referenced from the daily file above, for equation clarity
	H = dailyfile[d,10]
	LE = dailyfile[d,11]

	# ET (mm)
	ET = sum(day_obs * 1800 * LE / (1000 *lambda), na.rm = TRUE)

	# Priestly-Taylor constant
	alphaPT = 1.26

	# Saturation vapor pressure temp curve (Tetens, 1930)
	slopeSAT = (2508.3 / (dailyfile[d,4] + 237.3)^2) * exp(17.3 * dailyfile[d,4] / (dailyfile[d,4] + 237.3))

	# Psychometric constant (kPa / degC)
	PSI = 0.066

	# LE potential and ET potential (from Priestly-Taylor). Note here, Rn-G is substituted with H + LE
	LEpot = alphaPT *(slopeSAT* (H + LE) / (slopeSAT + PSI))
	ETpot = LEpot * 1800 * day_obs / (1000 * lambda)
	
	# Write the ET related variables to the daily file
	
	dailyfile[d,28] <- sum(thisday$GPP * convert) / ET 
	dailyfile[d,29] <- ET
	dailyfile[d,30] <- ETpot


	# Penman-Monteith (inverted) to get out surface and canopy conductances - NEED UNITS
	rho_a <- 1.225 # density of dry air (10 deg C, sea level --- not ideal, need to replace!)
	c_p <- 1024 # specific heat of air at constant temperature -- not ideal, need to replace!
	lam <- LE # latent heat of vaporization of water
	rho_w <- 1000 # Kg m^-3  density of water
	vpd <- dailyfile[d, 15] # vapor pressure deficit
	ga <- 0.033 # aerodynamic conductance (from Zhang et al., 2008 -- needs to be a function of
		# u*, per Leuning et al., 2008
	psi <- # Psychrometric constant (66.5 Pa K(-1))
	B <- H / LE # bowen ratio (H / LE)


	# Juant et al., 2007 method ------------------------- ###
	Kb <- # Stephan-Boltzman constant
	albedo <- dailyfile[d,21] / dailyfile[d,20] # Rg_out / Rg
	emissivity <- -0.16 * albedo + 0.99
	t_s <- (dailyfile[d,23] / (Kb * emissivity) ^(0.25) # RLong_out / (stephan-boltzman * epsilon)^.25
	t_s <- t_s - 273.15 # convert K to degC
	ga <- H / (rho * c_p * (t_s - dailyfile[d,4]))
    } 

    ### ----------------------------------------------------- ###

# This if/else block is how we are growing the variable AmfluxFileDaily over time, and still initialize the first pass of the loop to a clean dataframe with the right dimensions (note here however there is no limit for the number of rows we are allowing the variable to become, which can cause some issues. Ideally we would preallocate the entire thing, and this step would not be required (and our code would be a bit faster).

    if(i == 1){ # Here, the loop iterator (i) is in reference to the outer loop. This indexes the number of the Amerifluxfile we have opened currently.
	AmfluxFileDaily <- dailyfile
    }else{
	AmfluxFileDaily <- rbind(AmfluxFileDaily, dailyfile)
    }
    # rbind() or concatenate by row, our first file, and the new file. Note here that we call this variable the same thing as our first variable. In this way it grows with every iteration of the loop. Not the best programming practice but it does the trick.
    AmfluxFile <- rbind(AmfluxFile, fluxfile)
}

# Finally, write the result to a single csv. Here, the row.names argument stops R from including a pesky first column of numerical row numbers.. which is of no use for us. This will be a large text file, ane may take some time to write.
message('Writing concatenated 30 minute file', "\r", appendLF=FALSE)
write.table(AmfluxFile, 'AllAmfluxData_30min_Reichstein.csv', sep = ',', row.names = FALSE)
message('Writing concatenated daily file', "\r", appendLF=FALSE)
write.table(AmfluxFileDaily, 'AllAmfluxData_Daily_Reichstein.csv', sep = ',', row.names = FALSE)
