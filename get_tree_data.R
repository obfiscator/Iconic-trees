#source("/Users/mcgrath/NOENCRYPT/PROJECTS/DATA/DATA_INC/Iconic-trees/get_tree_data.R")
#setwd("/Users/mcgrath/NOENCRYPT/PROJECTS/DATA/DATA_INC/Iconic-trees/")
########################################################
# These variables can be changed to match your system.

# This is the directory that stores all the downloaded data
data.dir <- "/Users/mcgrath/DATA/TREE_RING/"

# What type of data are we looking for?
data.file.type <- "crd"
#data.file.type <- "rwl"


########################################################
# These libraries should not be changed.
# Use some important libraries for accessing the API
library(httr)
library(jsonlite)
library(dplR) # For tree rings
########################################################


# It appears that treating strings as factors is problematic.  This string helps.
options(stringsAsFactors = FALSE)



##################################################################
# Things below this line should not be changed unless debugging. #
##################################################################

# Create a function to download all the header data for all the tree ring studies
initialize.data <- function(){
  # Store the information of the tree API, which is on NOAA's website.
  url  <- "https://www.ncdc.noaa.gov/"
  path <- "paleo-search/study/search.json?dataPublisher=NOAA&dataTypeId=18&headersOnly=true"
  
  # Now we download the information from the API
  raw.result <- GET(url = url, path = path)
  
  # Need to change the content from Unicode to something legible
  this.raw.content <- rawToChar(raw.result$content)
  
  # Take the raw string and parse it into something we can work with
  this.content <- fromJSON(this.raw.content)
  
  return(this.content)
}


# This is the first command to get the list of all tree ring studies
this.content <- initialize.data()

# I am going to loop over all the studies this returned.  This should be all the tree ring studies, but I still need to check to see if there is data available.
study.number.list <- this.content[["study"]][["xmlId"]]
nstudies <- length(study.number.list)

# Try a function here, since this part could take a while.  
# This takes all the header information and searches it for data files, downloading those to the current directory.
# get.data(1,2,study.number.list)
get.data <- function(start_y,end_y,study.number.list) {
  
  # Check to see if the data directory exists.  The directory itself is defined globally.
  dir.create(data.dir) # dir.create will not do anything if it exists
  
  # I will want to loop through all these studies to find the ones which have data I need in them.
  for(istudy in seq(start_y,end_y)){
    url <- "https://www.ncdc.noaa.gov/"
    path <- paste("paleo-search/study/search.json?xmlId=",study.number.list[istudy],sep="")
    
    new.result <- GET(url = url, path = path)
    new.raw.content <- rawToChar(new.result$content, multiple = FALSE)
    this.new.content <- fromJSON(iconv(new.raw.content,to="UTF-8")) # I have had some issues with files not being encoded properly
    
    # Write the .json header data to a file.  I just used this to look at the header data.
    #write(prettify(new.raw.content), paste("xmlId",study.number.list[istudy],".json",sep=""))
    
    # Now I want to search the header data and find all mentions of "http\S+crd", since this should be a web-accessible location of the .crn data file.  Similarly for the .rwl file, depending on what data.file.type we are using.
    
    # This finds a URL.  Some extra escapes are needed since this is passed though the function.
    print(paste("On study: ",study.number.list[istudy],istudy))
    if(identical(data.file.type, "crd")){
      regexp.str <- "http[\\d|a-z|A-Z|/|:|\\.]+crn"
    }else if(identical(data.file.type, "rwl")){
      regexp.str <- "http[\\d|a-z|A-Z|/|:|\\.]+rwl"
    }else{
      print(paste("I do not recognize the data.file.type option:",data.file.type))
      browser()
    }
    
    m <- regexpr(paste("\"",regexp.str,"\"",sep=""), new.raw.content, perl=TRUE)
    #browser()
    
    if(m[1] == -1){
      print("No match here")
    }else{
      url.finds <- regmatches(new.raw.content, m)
      
      # Need to strip off leading and trailing quotation marks
      for(ielement in seq(1,length(url.finds))){
        url.finds[ielement] <- gsub("^.*http","http",url.finds[ielement])
        url.finds[ielement] <- gsub("crn.*$","crn",url.finds[ielement])
        
        
        # Now I wish to download these .crn files and put them into the local directory
        #browser()
        print(paste("Downloading file: ",url.finds[ielement],""))
        destination.file <- gsub(".+/(\\S+crn)$","\\1",url.finds[ielement])
        destination.file <- paste(data.dir,destination.file,sep="")
        print(paste("Creating file: ",destination.file,""))
        download.file(url.finds[ielement], destfile = destination.file, method = "libcurl")
        
      }
    }
  }
}

# Now I should have a set of data files in a data directory somewhere.
# This will open them all up and combine them into the data set I really want
combine.data <- function() {
  # move to the data directory
  setwd(data.dir)
  
  # Create the header of the table we are making.  This depends on the data format.
  if(identical(data.file.type, "crd")){
    cyears <- as.character(seq(1950,1955)) # just an arbitrary range for the moment
    cyears <- paste("Y",cyears,sep="") # column names can't be numbers.  I depend on this YNNNN format below.
    table.header <- c("siteID","speciesID","latitude","longitude",cyears)
    # Create an empty matrix
    df.matrix <- matrix(data = NA, nrow=1, ncol=length(table.header))
    colnames(df.matrix) <- table.header
  }else if(identical(data.file.type, "rwl")){
    print(paste("No option yet for .rwl:",data.file.type))
    browser()
    
  }else{
    print(paste("I do not recognize the data.file.type option:",data.file.type))
    browser()
  }  
  
  # Find all the data files in this directory
  if(identical(data.file.type, "crd")){
    file.list <- list.files(pattern = "*.crn")
  }else if(identical(data.file.type, "rwl")){
    file.list <- list.files(pattern = "*.rwl")
  }else{
    print(paste("I do not recognize the data.file.type option:",data.file.type))
    browser()
  }
  
  # For all these files, loop through them and read in the data they contain.
  nfiles <- length(file.list)
  file.numbers <- seq(1,nfiles)
  ###
  print("TRUNCATING FILES")
  file.numbers <- seq(1,576)
  ###
  
  #### This is to debug a specific file
  #file.numbers <- seq(516,nfiles)
  ####
  for(ifile in file.numbers){
    
    print(paste("Working on file: ",ifile,file.list[ifile]))
    
    # I have found some files do not conform to standards.  I put in workarounds where I can, but for these files there is no way to create a workaround that wouldn't break other functionality.
    if(file.list[ifile] %in% c("ca556x.crn", "ca560x.crn", "newz103.crn", "newz107.crn", "newz110.crn", "newz111.crn", "newz113.crn")){
      print("Skipping")
      next()
    }
    # For each file, I want to add the information inside to a table.
    if(identical(data.file.type, "crd")){
      
      # The data in the crn file: Lon, lat, Species, Growth...
      # Where "growth is given for each year
      # And then another table: Lon, lat, Species, Nsamples
      # Where "Nsamples" is given for each year
      temp.df <- read.crn.file(file.list[ifile])
      
     if(temp.df$success){ # this means we actually found data in a proper format
       # take on the data to the existing data we've found
       # this requires some work to make sure the years add up
       # Find the range of years in the main dataframe.  This is pretty simple due to the format of the headers.
       full.df.firstyear <- table.header[5]
       full.df.firstyear <- as.integer(substr(full.df.firstyear,2,5))
       full.df.lastyear <- table.header[length(table.header)]
       full.df.lastyear <- as.integer(substr(full.df.lastyear,2,5))
       if(is.na(full.df.firstyear) || is.na(full.df.lastyear)){
         print("Had a problem converting years to integers: AXHF")
         print(table.header[5])
         print(table.header[length(table.header)])
         stop()
       }
       
       # Do something simple, but perhaps CPU intensive.  Grow a table based on the lower year and the higher year between the table and the new vector.
       # Then copy over all new values.  THIS COULD BE OPTIMIZED.
       lowest.year <- min(full.df.firstyear,temp.df$first.year)
       highest.year <- max(full.df.lastyear,temp.df$last.year)
       new.df.years <- seq(lowest.year,highest.year)
       cyears <- as.character(new.df.years) # just an arbitrary range for the moment
       cyears <- paste("Y",cyears,sep="") # column names can't be numbers.  I depend on this YNNNN format below.
       table.header <- c("siteID","speciesID","latitude","longitude",cyears)
       # I need to add one row to the existing matrix to store the new site data.
       # First, check to see if all the elements in the current df.matrix are NA.  If so, we haven't added any data yet.
       if(sum(is.na(df.matrix)) == nrow(df.matrix)*ncol(df.matrix)){
         if(nrow(df.matrix) != 1){
           # We should not have all NAs if we have more than 1 row!
           print("Odd dataframe matrix with added information")
           browser()
         }
         # If everything is NA, we have not yet added any information.  So we only need one row.
         df.matrix <- matrix(data = NA, nrow=1, ncol=length(table.header))
         colnames(df.matrix) <- table.header
         # Copy the identity data over
         df.matrix[1,1] <- temp.df$siteID
         df.matrix[1,2] <- temp.df$speciesID
         df.matrix[1,3] <- as.integer(temp.df$latitude)
         df.matrix[1,4] <- as.integer(temp.df$longitude)
         
         # Now the growth data, just changing values for years which we actually have new data
         new.crn.year.index <- 0
         for(iyear in seq(1,length(new.df.years))){
           current.year <- new.df.years[iyear]
           if((current.year <= temp.df$last.year) && (current.year >= temp.df$first.year)){
             # We have a new datapoint for this year
             new.crn.year.index <- new.crn.year.index + 1
             if(current.year == temp.df$crn.df$years.indices[new.crn.year.index]){
               df.matrix[1,4+iyear] <- as.integer(temp.df$crn.df$growth.indices[new.crn.year.index])
             }else{
               print("I am putting growth data into the wrong place!")
               browser()
             }
           }
         }
       }else{
         # Here we actually add a row
         df.matrix.new <- matrix(data = NA, nrow=nrow(df.matrix)+1, ncol=length(table.header))
         colnames(df.matrix.new) <- table.header
         
         # First, we need to copy over the old df.matrix.  This is not trivial since the number of columns may have changed.  
         # Thankfully, we just need to loop over the columns and R does this easily.
         df.matrix.new[1:nrow(df.matrix),colnames(df.matrix)] <- df.matrix[,colnames(df.matrix)]
         
         
         # Then, we add the data from the latest site
         irow.new <- nrow(df.matrix)+1
         # Copy the identity data over
         df.matrix.new[irow.new,1] <- temp.df$siteID
         df.matrix.new[irow.new,2] <- temp.df$speciesID
         df.matrix.new[irow.new,3] <- as.integer(temp.df$latitude)
         df.matrix.new[irow.new,4] <- as.integer(temp.df$longitude)
         
         # Now the growth data, just changing values for years which we actually have new data
         new.crn.year.index <- 0
         for(iyear in seq(1,length(new.df.years))){
           current.year <- new.df.years[iyear]
           if((current.year <= temp.df$last.year) && (current.year >= temp.df$first.year)){
             # We have a new datapoint for this year
             new.crn.year.index <- new.crn.year.index + 1
             if(current.year == temp.df$crn.df$years.indices[new.crn.year.index]){
               df.matrix.new[irow.new,4+iyear] <- as.integer(temp.df$crn.df$growth.indices[new.crn.year.index])
             }else{
               print("I am putting growth data into the wrong place!")
               browser()
             }
           }
         }
         
         # destory the old matrix and rename the new
         rm(df.matrix)
         df.matrix <- df.matrix.new
         rm(df.matrix.new)
       }

     }
      
    }else if(identical(data.file.type, "rwl")){
      print(paste("Have not yet writting the .rwl routine for this:",data.file.type))
      browser()
    }else{
      print(paste("I do not recognize the data.file.type option:",data.file.type))
      browser()
    }
  }
  
  # Write all of this to a CSV file to be read in later
  write.table(df.matrix, file = "combined_data.csv",sep = ",", col.names=TRUE, row.names = FALSE)
}

# This will open up a .crn file and return a list which includes a data.frame
# There is a package to do this, dplR, but the read.crn routine there doesn't pull out all the information I want.
read.crn.file <- function(file.name, encoding = getOption("encoding")) {
  
  con <- file(file.name, encoding = encoding)
  on.exit(close(con))
  
  # Determine if the header is there.  The header has information I want.
  hdr1 <- readLines(con, n = 1)
  # How long is this line?  If it's more than 200 characters, we probably have a formatting problem.
  if(nchar(hdr1) > 200){
    print(paste("Really long header line in: ",file.name))
    browser()
  }else if(nchar(hdr1) < 62){
    print(paste("Too short of header line in: ",file.name))
    browser()
  }
  
  # Characters 1-6 are the site ID
  siteID <- substr(hdr1,1,6)
  # Characters 62--65 are the species code.
  speciesID <- substr(hdr1,62,65)
  # I want the species code.  If this does not exist, I skip this file.
  if(identical(speciesID,"")){
    print(paste("Cannot find speciesID for file:",file.name))
    browser()
  }
  
  # The latitude and longitude should be on line 2
  # Note: lat-lons are in degrees and minutes, ddmm or dddmm.  There is a dash between them.
  hdr2 <- readLines(con, n = 2)
  if (length(hdr2) < 2){
    print(paste("Why does this file have less than 2 lines? :",file.name))
    stop()
  }
  hdr2 <- hdr2[2]
  latitude <- as.integer(substr(hdr2,48,51))
  longitude <- as.integer(substr(hdr2,53,57))
  
  # Based on the years the records cover, we can create vectors to store the information
  first.year <- as.integer(substr(hdr2,68,71))
  last.year <- as.integer(substr(hdr2,73,76))
  
  # Did we actually find integers?
  if(is.na(first.year) || is.na(last.year)){
    print(paste("Could not find the years. :",file.name))
    print(paste(first.year,last.year))
    print(hdr2)
    crn.list <- list(success=FALSE)
    return(crn.list)
  }
  # Calculate the total number of years of data that we have.
  total.years <- last.year - first.year +1
  if(total.years < 1){
    print(paste("Something appears to be wrong with the years :",file.name))
    print(paste(first.year,last.year))
    print(hdr2)
    crn.list <- list(success=FALSE)
    return(crn.list)
  }
  growth.indices <- integer(total.years)
  years.indices <- seq(first.year,last.year)
  # This is a counter which we'll use to keep track of where we are
  current.year <- first.year
    
  # Read in the whole file now
  whole.file <- readLines(con, n = -1)
  if (length(whole.file) < 4){ # the first three lines should be the header, and then the data everything afterwards
    print(paste("Why does this file have less than 4 lines? :",file.name))
    stop()
  }
  for(iline in seq(4,length(whole.file))){
    # characters 1-6 are the siteID.  7-10 is the first year of the decade. The rest of the line should be groups of 7 integers.  How many groups depends on how many years we have in this decade, but the maximimum is 10
    # occasionally lines appear to be blank.  If so, skip this line.
    if(nchar(whole.file[iline]) == 0){
      next()
    }
      
    # What if the line is unusually short?
    if(nchar(whole.file[iline]) < 17){
      print(paste("This line appears to be missing records:",file.name))
      print(paste(iline, " : ",whole.file[iline]))
      browser()
    }
    test.year <- as.numeric(substr(whole.file[iline],7,10))
    if(is.na(test.year)){
      print(paste("Problem converting test.year in file: ",file.name))
      print(whole.file[iline])
      crn.list <- list(success=FALSE)
      return(crn.list)
    }

    # We may have a problem with our bookkeeping
    # Although there is a known bad case with this.  If the records start in, say, 1422, the year read in on the first line of this file will be 1422, with NA entries for 1420 and 1421.  The next line will start on 1430. So we make an exception for line 4 of records which are not divisible by 10.
    if(iline == 4){
      if((test.year %% 10) != 0){
        test.year <- test.year - test.year %% 10
        current.year <- current.year - current.year %% 10
      }
    }
    if(test.year != current.year){
      print(paste("Our years are not matching up!"))
      print(paste(test.year,current.year))
      print(whole.file[iline])
      browser()
      stop()
    }
    # Grab the data here.  Each line should have ten data points; missing data is 9990
    for(icstart in seq(11,80,by=7)){
      if(icstart > nchar(whole.file[iline])){
        break()
      }
      # The standard says the first four integers in the group are the growth index, the second three are the number of samples
      icend <- icstart + 4
      growth.index <- as.integer(substr(whole.file[iline],icstart,icend))
      
      # What is the vector position that corresponds to this year?
      year.index <- match(current.year,years.indices)
      growth.indices[year.index] <- growth.index
      current.year <- current.year + 1
      
      # Sometimes there are more records than years.
      if(current.year > last.year){
        break()
      }
    }
    # Sometimes there are more records than years.
    if(current.year > last.year){
      break()
    }
  } # This is the loop over the whole file
  
  # Mark data as not available
  growth.indices[growth.indices == 9990 ] <- NA
  
  # Now let's put all of this into a data frame
  crn.df <- data.frame(years.indices, growth.indices)
  
  crn.list <- list(success=TRUE,siteID=siteID, speciesID=speciesID, longitude=longitude, latitude=latitude, first.year=first.year, last.year=last.year, crn.df=crn.df)
  
  crn.list
}