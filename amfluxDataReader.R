### This script reads in all Ameriflux files in a directory, either gapfilled or with_gaps, and produces 2 files:
### a concatenated 30 minute file, and a concatenated daily file. The daily file variables are bare bones at this point,
### producing only SITE, YEAR, and DOY columns. This is simple placeholder.

# First specify the directory where the data are located
dataDir <- '/home/nikko/Research/Data/TowerData/AmerifluxFiles/'

# Specify whether you want to look at gapfilled or with_gaps files
# Here, we are just specifying a pattern that exists in the file names which
# differentiates the files from one another
fileType <- 'gapfilled'

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

# Specify the column names we want to appear in the daily file
    dailynames <- c('SITE','YEAR','DOY')

    # To create the daily files, we need to first figure out how many days are in each file (as this will determine the row numbers).
    numdays <- max(fluxfile$DOY)
    dailyfile <- data.frame(matrix(nrow = numdays, ncol = length(dailynames)))
    names(dailyfile) <- dailynames

    for(d in 1:numdays){ 
	message(paste('processing day', d, 'of', numdays),"\r",appendLF=FALSE)
	thisday <- fluxfile[which(fluxfile$DOY == d),]
	dailyfile[d,1] <- sitename 
	dailyfile[d,2] <- thisday$YEAR[1]
	dailyfile[d,3] <- d


    } 

# This if/else block is how we are growing the variable AmfluxFileDaily over time, and still initialize the first pass of the loop to a clean dataframe with the right dimensions (note here however there is no limit for the number of rows we are allowing the variable to become, which can cause some issues. Ideally we would preallocate the entire thing, and this step would not be required (and our code would be a bit faster).

    if(i == 1){ #note here, the loop iterator (i) is in reference to the outer loop. This indexes the number of the Amerifluxfile we have opened currently.
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
