### This script reads in all Ameriflux files in a directory, either gapfilled or
###  with_gaps, and produces 2 files:
### 1) a concatenated 30 minute file, and 2) a concatenated daily file. The daily 
### file variables now include most of the commonly used variables, including now ET, PET, and
### Penman-Monteith stomatal conductance / atmospheric conductance variables. 
### 
### Author: Dan Krofcheck
### Email: djkrofch@unm.edu
### Date Modified: 03 - Mar - 2015

library(reshape)

### ---------------- Input Data -------------------------- ###

# First specify the directory where the data are located
dataDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/'
outDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/Products/'

# Specify whether you want to look at gapfilled or with_gaps files
# Here, we are just specifying a pattern that exists in the file names which
# differentiates the files from one another
fileType <- 'gapfilled'

# Gapfilled precip has been an effort since Nov 2014. We now have some first cut efforts
# at this product. The data are daily at this point, so we will read it in here,
# and merge it with the daily file after the Ameriflux file processing is complete.

# Constants which are used for various steps of the calulations below
SECONDS_PER_HALF_HOUR <- 60*30
LV_WATER <- 2.257e6;      # J Kg-1
CP_DRY_AIR <- 1006;   # J Kg-1 K-1
STEFAN_BOLTZMANN_CONST <- 5.670373e-8; #J s-1 m−2 K−4
GRAMS_C_PER_MICROEINSTEIN<- 12.01 * SECONDS_PER_HALF_HOUR / 1000000 # gC per 30 minute flux interval
PRIESTLY_TAYLOR_COEFF <- 1.74
Rd <- 286.9 # dry air gas const; J Kg-1 K-1
Rw <- 461.5; # water vapor gas const; J Kg-1 K-1

### ---------------- Function Definitions ---------------------- ###

# Several helper functions designed to be used with the Penman-Monteith
# calculations, and (although not yet implemented) the Priestly-Taylor
# calculations for ET and PET. Current the P-T calculations are done in-line
# and will be moved up here for organization reasons later on.
#
# Also need to move these function definitions from this file into another
# script, and included as a package --- on the TO DO list.
#
# These functions represent work by Dan Krofcheck, Tim Hilton, and Andy Fox.
# for the University of New Mexico, Biology Dept.

# T must be in Kelvin
get_es <- function( T ){
T_s <- 373.16 # steam_point temperature (K)
log_10_e <- ( -7.90298 * ( ( T_s/ T ) - 1 ) ) + 
    5.02808 * log10( T_s / T ) - 
    1.3816e-7 * 10^( 11.344 * ( 1 - ( T / T_s ) ) - 1 ) + 
    8.1328e-3 * 10^( ( -3.49149 * ( ( T_s / T ) - 1 ) ) - 1 ) + 
    log10( 1013.246 )
e_s <- 10^(log_10_e)
return(e_s)
}

calc_albedo <- function(ROut, RIn){
albedo <- ROut / RIn
print('calculating albedo')
return(albedo)
}

calc_emissivity <- function(albedo){
epsilon <- -0.16 * albedo + 0.99
print('calculating emissivity')
return(epsilon)
}

calc_Ts <- function(epsilon, Rlong_out){
Ts <- (Rlong_out / (STEFAN_BOLTZMANN_CONST * epsilon)) ^ (0.25)
Ts <- Ts - 273.15 # Convert K to degC
print('calculating surface temperature')
return(Ts)
}

# Moist air density calculation.
# T must be in Kelvin, p in Pascals, and RH in ratio (not %)
calc_moist_air_density <- function( T, p, RH ){
epsilon <- Rd / Rw
w <- (calc_ws(T, p) / 100 ) * RH # Mixing ratio of water vapor
e <- (w / (w + epsilon) ) * p # Vapor pressure
rho <- (p / (Rd * T)) * (1 - (( e / p) * (1 - epsilon))) # Wallace & Hobbs eqs 2.13 - 2.16
return(rho)
}

# T must be in Kelvin, p in Pascals (not kPa)
calc_ws <- function(T, p ){
epsilon <- Rd / Rw
e_s <- get_es(T)
w_s <- epsilon * (e_s / (p - e_s)) # Wallace & Hobbs eqs 2.64 and 2.14
return(w_s)
}

# T must be in Kelvin, p in Pascals, and RH in ratio (not %)
calc_cp <- function(T, p, RH){
w_s <- calc_ws( T, p ) # saturation water vapor mixing ratio
w <- ( RH ) * w_s # water vator mixing ratio, Wallace & Hobbs eq. 2.66
c_p <- CP_DRY_AIR * ( 1.0 + ( 0.84 * w ) ); # Stull eq. 3.2
return(c_p)
}

# T_a is in degrees Celcius
calc_delta <- function(T_a){
#T_ref <- ( -50:1:100 ) + 273.15  # -50C to 100C converted to K
#es_kPa <- get_es( T_ref ) / 10.0 # saturation vapor pressure in kPa
#delta <- approx( T_ref, des_dt, T_a + 273.15 )
slopeSAT <- (2508.3 / (T_a + 237.3)^2) * exp(17.3 * T_a / (T_a + 237.3)) # temporary calc
return(slopeSAT)
}

calc_ga <- function(T_a, p, RH, H, T_s){
#c_p =1024;
c_p = calc_cp( T_a + 273.15, p, RH );
ga = H / ( rho * c_p * ( T_s - T_a ) )
return(ga)
}

calc_eta <- function(T_a, p, RH){
c_p <- calc_cp( T_a + 273.15, p, RH )
#c_p <- 1024;
gamma <- c_p / LV_WATER; # pyschrometric constant
delta <- calc_delta( T_a )
rho <- calc_moist_air_density(T_a + 273.15, p, RH)
eta <- ( rho * c_p * ga ) / 
      ( 1 - ( alpha_PTc * ( delta / ( delta  + gamma ) ) ) )
return(eta)
}

calc_alphaPT <- function(T_a, H, LE, p, RH){
c_p = calc_cp( T_a + 273.15, p, RH );
p_kPa = p/1000
epsilon = Rd / Rw
delta <- calc_delta( T_a )
gamma = (c_p  * p_kPa) / ( LV_WATER * epsilon ) 
alpha_PTc <- (H + LE) * (( delta + gamma) / delta)
alpha_PTc <- (1 + (1 / (delta / gamma)) / (1 + (H / LE)))
return(alpha_PTc)
}

calc_gc <- function(T,  p, RH, VPD, H, LE){
epsilon = Rd / Rw
p_kPa = p/1000
gamma = (c_p  * p_kPa) / ( LV_WATER * epsilon ) 
bowen = H / LE
term1 = ( ( ( delta / gamma ) * bowen ) - 1 ) * ( 1.0 / ga );
term2 = ( ( 1012 * rho ) / ( LE * gamma ) ) * VPD;
gc = ( 1.0 / ( term1 + term2 ) );
R_star = 8.3144621 # uiversal gas const
mol_2_umol = 1000.0  # factor to convert mol to umol
T_a_K = T + 273.15  # convert C to K
conv_factor = ( p / ( R_star * T_a_K ) ) * mol_2_umol
gc = gc * conv_factor  # convert gc from m/s to mmol/m2/s
return(gc)
}

calc_omega <- function( T_a, p, RH, VPD, H, LE, gc, ga){
p_kPa = p/1000
epsilon = Rd / Rw
delta <- calc_delta( T_a )
gamma = (c_p  * p_kPa) / ( LV_WATER * epsilon ) 
omega <- ((delta/gamma) + 1) / (delta/gamma + 1 + (ga/gc))
return(omega)
}

# Specify the column names we want to appear in the daily file
# Currently, edits here require edits inside the dailyfile loop.
# Need to have the loop dynamically generated via a function, using this list as inputs ultimately.
dailynames <- c('SITE','YEAR','DOY','TA_mean','TA_min','TA_max','FC','FC_day','FC_night','H','LE','PRECIP',
	'RH','PA','VPD_day','VPD_day_min','VPD_day_max','RNET','PAR','Rg','Rg_out','Rlong_in','Rlong_out',
	'RE','GPP','GAP_qual','LUE','WUE', 'ET','PET', 'es','ws','albedo','emissivity','surface_temp','delta',
	'rho','eta','cp','ga','gc', 'alphaPT', 'omega')

### ------------------------------------------------------------ ###

# Generate a list of file names that exist in our data directory 
fileList  <- list.files(dataDir, pattern = fileType)
fullNames <- paste(dataDir, fileList, sep = '')

# Read in the first Ameriflux file. Note here we skip all the non-data rows when we read it in. This would not be required if we didn't have the extra metadata rows.
firstFile <- data.frame(read.table(fullNames[1], sep = ',', skip = 5))

# Read in the gapfilled precip data
# Note here we rename the columns to play nice with the dailyfile site names
# so that when we merge by site and DOY, alls well.
#gapfilled_P <- data.frame(read.table(paste(dataDir,gapfilled_precip, sep = ''), sep = ',', head = TRUE))
#names(gapfilled_P) <- c('TS','Vcm','Vcp','Wjs','Mpj','Mpg','Seg','Sen','Ses','YEAR','DOY')
#melted_precip <- melt(gapfilled_P, c('TS','YEAR','DOY'))
#names(melted_precip) <- c('TS','YEAR','DOY','SITE','PRECIP')

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

    ### ----------- Daily Amflux File Generation ------------ ###

    for(d in 1:numdays){ 
	message(paste('processing day', d, 'of', numdays),"\r",appendLF=FALSE)

	# Subset only the rows for a single day
	thisday <- fluxfile[which(fluxfile$DOY == d),]

	# Subset rows within this day that are daylight only, determine the length of that variable
	daytime <- thisday[which(thisday$Rg > 10), ]
	nighttime <- thisday[which(thisday$Rg <= 11), ]
	day_obs <- length(daytime$DOY)
	PM_interval <- thisday[which(thisday$HRMIN > 930 & thisday$HRMIN < 1630),]

	dailyfile[d,1] <- sitename 
	dailyfile[d,2] <- thisday$YEAR[1]
	dailyfile[d,3] <- d
	dailyfile[d,4] <- mean(thisday$TA, na.rm = TRUE)
	dailyfile[d,5] <- min(thisday$TA, na.rm = TRUE)
	dailyfile[d,6] <- max(thisday$TA, na.rm = TRUE)
	dailyfile[d,7] <- sum(thisday$FC * GRAMS_C_PER_MICROEINSTEIN, na.rm = TRUE)
	dailyfile[d,8] <- sum(daytime$FC * GRAMS_C_PER_MICROEINSTEIN, na.rm = TRUE)
	dailyfile[d,9] <- sum(nighttime$FC * GRAMS_C_PER_MICROEINSTEIN, na.rm = TRUE)
	dailyfile[d,10] <- mean(daytime$H, na.rm = TRUE) 
	dailyfile[d,11] <- mean(daytime$LE, na.rm = TRUE)
	dailyfile[d,12] <- sum(thisday$PRECIP, na.rm = TRUE)
	dailyfile[d,13] <- mean(thisday$RH, na.rm = TRUE)
	dailyfile[d,14] <- mean(thisday$PA, na.rm = TRUE)
	dailyfile[d,15] <- mean(daytime$VPD, na.rm = TRUE)
	dailyfile[d,16] <- min(daytime$VPD, na.rm = TRUE)
	dailyfile[d,17] <- max(daytime$VPD, na.rm = TRUE)
	dailyfile[d,18] <- sum(thisday$RNET, na.rm = TRUE)
	dailyfile[d,19] <- sum(thisday$PAR, na.rm = TRUE)
	dailyfile[d,20] <- mean(thisday$Rg, na.rm = TRUE)
	dailyfile[d,21] <- mean(thisday$Rg_out, na.rm = TRUE)
	dailyfile[d,22] <- mean(thisday$Rlong_in, na.rm = TRUE)
	dailyfile[d,23] <- mean(thisday$Rlong_out, na.rm = TRUE)
	dailyfile[d,24] <- sum(thisday$RE * GRAMS_C_PER_MICROEINSTEIN, na.rm = TRUE)
	dailyfile[d,25] <- sum(thisday$GPP * GRAMS_C_PER_MICROEINSTEIN, na.rm = TRUE)
	dailyfile[d,26] <- sum(thisday$FC_flag) / 48
	dailyfile[d,27] <- sum(thisday$GPP * GRAMS_C_PER_MICROEINSTEIN) / sum(thisday$Rg)

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
	
	dailyfile[d,28] <- sum(thisday$GPP * GRAMS_C_PER_MICROEINSTEIN) / ET 
	dailyfile[d,29] <- ET
	dailyfile[d,30] <- ETpot

	es <- get_es(PM_interval$TA + 273.15)
	ws <- calc_ws(PM_interval$TA + 273.15, PM_interval$PA * 1000)
	albedo <- calc_albedo(PM_interval$Rg_out, PM_interval$Rg)
	epsilon <- calc_emissivity(albedo)
	Ts <- calc_Ts(epsilon, PM_interval$Rlong_out)
	delta <- calc_delta(PM_interval$TA)
	rho <- calc_moist_air_density(PM_interval$TA + 273.15, PM_interval$PA * 1000, PM_interval$RH)
	c_p <- calc_cp(PM_interval$TA + 273.15, PM_interval$PA * 1000, PM_interval$RH)
	ga <- calc_ga(PM_interval$TA, PM_interval$PA * 1000, PM_interval$RH, PM_interval$H, Ts)
	gc <- calc_gc(PM_interval$TA, PM_interval$PA * 1000, PM_interval$RH, PM_interval$VPD, PM_interval$H, Ts)
	alpha_PTc <- calc_alphaPT(PM_interval$TA, PM_interval$H, PM_interval$LE, PM_interval$PA * 1000, PM_interval$RH)
	alpha_PTc <- alpha_PTc[which((alpha_PTc > 0) & (alpha_PTc < 5))]
	eta <- calc_eta(PM_interval$TA, PM_interval$PA * 1000, PM_interval$RH)
	eta <- eta[which((eta < 200) & (eta > -1000))]
	omega <- calc_omega(PM_interval$TA, PM_interval$PA * 1000, PM_interval$RH, PM_interval$VPD, PM_interval$H, Ts, ga, gc)
	ga <- ga[which((ga > 0) & (ga < 0.2))]
	gc <- gc[which((gc > 0) & (gc < 250))]

	dailyfile[d,31] <- mean(es, na.rm = TRUE)
	dailyfile[d,32] <- mean(ws, na.rm = TRUE)
	dailyfile[d,33] <- mean(albedo, na.rm = TRUE)
	dailyfile[d,34] <- mean(epsilon, na.rm = TRUE) 
	dailyfile[d,35] <- mean(Ts, na.rm = TRUE) 
	dailyfile[d,36] <- mean(delta, na.rm = TRUE) 
	dailyfile[d,37] <- mean(rho, na.rm = TRUE) 
	dailyfile[d,38] <- mean(eta, na.rm = TRUE) 
	dailyfile[d,39] <- mean(c_p, na.rm = TRUE) 
	dailyfile[d,40] <- mean(ga, na.rm = TRUE) 
	dailyfile[d,41] <- mean(gc, na.rm = TRUE) 
	dailyfile[d,42] <- mean(alpha_PTc, na.rm = TRUE) 
	dailyfile[d,43] <- mean(omega, na.rm = TRUE) 

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
write.table(AmfluxFileDaily, paste(outDir, 'AllAmfluxData_Daily_Reichstein.csv', sep = ''), sep = ',', row.names = FALSE)
## end

