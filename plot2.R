library(dplyr)

## Load and cached text file data 
## Return as a data.frame
## @param m: inetrnal loaded data.frame cache
## set: Set the internal cached data.frame
## get: Get the internal cached data.frame
## load: Read and store data for the internal cached data.frame
cacheData <- function(m = NULL){
  
  # Set the internal cached data.frame
  set <- function(y){
    m <<- y
  }
  
  # Get the internal cached data.frame
  get <- function() {
    if(is.null(m))
      return(0)
    return(m)
  }
  
  # Return the subset between date ranges
  getsubset <- function(minDate = "2007/02/01", maxDate = "2007/02/02"){
    if(is.null(m))
      return(0)
    
    # Return the subset
    return(subset(m, Date >= as.Date(minDate) & Date <= as.Date(maxDate))) 
  }
  
  # Read and store data for the internal cached data.frame
  load <- function(filename){
    print("loading data...")
    
    # Read file only if "m" is NULL and filename is valid
    if(!is.null(filename) & is.null(m)){
      m <<- read.csv(filename, header=TRUE, sep=";", stringsAsFactors = FALSE)
    
      # Convert to Date format
      print('converting to Date...')
      m$Date <<- as.Date(m$Date, format="%d/%m/%Y")
    }
  }
  
  # Return methods 
  return(
    list(
      set = set,
      get = get,
      getsubset = getsubset,
      load = load
    )
  )
}

# Main program proper
# Load and cache data
d <- cacheData()
d$load("household_power_consumption.txt")

# Get the subset (Date between 2007/02/01 - 2007/02/02 only)
t <- d$getsubset()

# Get clean data (has no "?")
gbp <- as.numeric(t$Global_active_power[t$Global_active_power != '?'])

# Set-up devices and settings
png(filename="plot2.png", width=480, height=480, units='px')
op <- par(mar = rep(2, 4), bg=NA)

# Get unique time per day
s <- t %>%
  mutate(
    datetime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
    gbp = as.numeric(Global_active_power != '?')
  )

# Plot the graph and render to PNG
plot(s$datetime, gbp, type="l", ylab="Global Active Power (kilowatts)", xlab="")
par(op)
dev.off()