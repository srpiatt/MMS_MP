# GetDataFunctions
# Authors: Samantha Piatt, Marek Petrik, Daroc Alden, Jeremy Walker

# ------------------------------------------------- Open, Save, and Process Data
# Create data frame from mms and sitl data.
#   This is the original function created to parse and merge the feature and
#   response data. This is the second version, which incorporates the
#   convert.time function from the evaluation document.
#
# Returns a data frame containing merged feature set with sitl response columns.
merge_sitl <- function(mms, sitl) {
  outdata.m <- clean_mms(na.omit(read.csv(mms)))
  outdata.s <- clean_sitl(read.csv(sitl))
  outdata.a <- merge_data(outdata.m, outdata.s)
  return(outdata.a[order(outdata.a$Time),])
}

# Merges a sitl data frame into the passed in mms data frame.
merge_data <- function(mms, sitl){
  # Order data for faster processing. 
  mms <- mms[order(mms$Time),]
  sitl <- sitl[order(sitl$Start),]
  slvl <- levels(sitl$Comments)
  
  # populate Y in mms
  startPoint = min(sitl$Start)
  endPoint = max(sitl$End)
  
  com <- mms$Comments
  
  for(x in 1:nrow(mms)){
    # only compare if mms time is less than max sitl end time
    if(mms$Time[x] >= endPoint) break
    
    # only loop through these if current mms row time is larger or equal to 
    #   sitl min start time.
    if(mms$Time[x] >= startPoint){
      for(i in 1:nrow(sitl)){
        if(mms$Time[x] >= sitl$Start[i] & mms$Time[x] <= sitl$End[i]){
          mms$Priority[x] <- sitl$Priority[i]
          mms$Selected[x] <- 1
          mms$Comments[x] <- sitl$Comments[i]
          com[x] <- trimws(slvl[as.numeric(sitl$Comments[i])])
        }
      }
    }
  }
  
  mms$Comments <- com
  return(mms)
}

# Convert time fields into a difference in seconds
convert.time <- function(x)
  as.integer(difftime(x, "2000-01-01 00:00:00.000000", units = "secs"))

# Cleans the mms input data frame from csv file.
#
# Returns a data frame containing dates formated in seconds, an X ID field, 
#   as well as Selected and Priority fields with defaults of 0.
clean_mms <- function(mms){
  mms$Date <- mms$Time
  mms$Time <- convert.time(mms$Time)
  mms[,"Priority"] <- 0
  mms[,"Selected"] <- 0
  mms[,"Comments"] <- "None"
  
  #mms <- data.frame(mms, X = mms$Time/100)
  return(mms)
}

# Cleans the sitl input data frame from csv file.
#
# Returns a data frame containing start and end times standardized.
clean_sitl <-  function(sitl){
  sitl$Start <- convert.time(stringr::str_replace_all(sitl$Start, "([T])", " "))
  sitl$End <- convert.time(stringr::str_replace_all(sitl$End, "([T])", " "))
  
  renamed <- data.frame(Status = sitl$Status, Start = sitl$Start, 
                        End = sitl$End, Comments = sitl$Reason, 
                        Priority = sitl$Priority)
  return(renamed)
}

# If the merged data file already exists, it is loaded. Otherwise, the mms and 
#   sitl files are loaded, merged, and then saved.
#
# Returns a data.frame containing the merged data.
open_merged <- function(day){
  localFile = paste("../experimentation/data/merged_", day['date'], ".Rds", sep="")
  sitl = paste("../", day['set'], "/sitl_", day['date'], ".csv", sep="")
  mms = paste("http://data.rmdp.xyz/mms/", day['set'], 
              "/mms_", day['date'], ".csv", sep="")
  
  data= NULL
  if(!file.exists(localFile)){
    data <- merge_sitl(mms, sitl)
    saveRDS(data, localFile)
  } else {
    data <- readRDS(localFile)
  }
  return(data)
}

# Alterate data opening function that loads existing Rds data file or creates a 
#   new one if it doesn't already exist.
#
# Retuns a data frame contianing saved or retrieved data.
open_data <- function(source, destination){
  fix.na <- function(x, replacement){
    replace(x, is.na(x), replacement)
  }
  if(!file.exists(destination)){
    # load and pre-process the data
    data <- readr::read_csv(source)
    data <- dplyr::mutate(data, Orbit=cumsum(fix.na(data$Time - lag(data$Time),4) > 100))
    saveRDS(data, destination)
  } else {
    data <- readRDS(destination)
  }
  return(data)
}

# Loads a local RDS file containing mms data from a specific orbit, or creates the 
#    local data from a source data file.
#
# Retuns a data frame contianing saved or retrieved data.
load_orbit <- function(source, destination, orbit){
  dest = paste(orbit, destination, sep='')
  if(!file.exists(dest)){
    data <- open_data(source, destination)
    data <- data %>% dplyr::mutate(DES.N=pmax(DES.N,0.001), Orbit=as.factor(Orbit)) %>% filter(Orbit == orbit)
    saveRDS(data, dest)
  } else {
    data <- readRDS(dest)
  }
  return(data)
}

# ------------------------------------------------------------------- Split Data
# Calculate indexes for training subset.
#
# Returns a vector of row numbers to include in the training subset.
select_train <- function(indata, size, seed) {
  select = floor(size * nrow(indata))
  set.seed(seed)
  return(sample(seq_len(nrow(indata)), size = select))
}

# Split data set into training data.
#
# Returns a data frame containing the training subset of indata.
split_train <- function(indata, size = 0.75, seed = 0){
  points = select_train(indata, size, seed)
  train <- indata[points, ]
  return(train)
}

# Split data set into test data.
#
# Returns a data frame containing the test subset of indata.
split_test <- function(indata, size = 0.75, seed = 0){
  points = select_train(indata, size, seed)
  test <- indata[-points, ]
  return(test)
}

# Select all columns in a list, or columns not in the list.
# 
# Returns a list containing remaining columns.
subtract <- function(clist, selection) clist[!(clist %in% selection)]