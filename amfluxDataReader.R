### This script is designed to simply read Ameriflux formatted files and concatenate the results into a single csv. 

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

    # rbind() or concatenate by row, our first file, and the new file. Note here that we call this variable the same thing as our first variable. In this way it grows with every iteration of the loop. Not the best programming practice but it does the trick.
    AmfluxFile <- rbind(AmfluxFile, fluxfile)
}

# Finally, write the result to a single csv. Here, the row.names argument stops R from including a pesky first column of numerical row numbers.. which is of no use for us. This will be a large text file, ane may take some time to write.
write.table(AmfluxFile, 'AllAmfluxData_30min_Reichstein.csv', sep = ',', row.names = FALSE)
