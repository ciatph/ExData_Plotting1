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
  
  # Convert a string date to class Date
  convert <- function(x){
    # return(as.Date(x, format='%d/%m/%Y'))
    return(as.Date(as.character(x), format="%Y/%m/%d"))
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
grp <- as.numeric(t$Global_reactive_power[t$Global_reactive_power != '?'])
voltage <- as.numeric(t$Voltage[t$Voltage != '?'])

# Set-up devices and settings
#par(mfrow = c(2, 2), mar = c(4,5,2,1))
png(filename="plot4.png", width=480, height=480, units='px')
op <- par(mfcol=c(2, 2), mfrow = c(2, 2), mar = c(4,4,4,2), bg=NA)

# Get unique time per day
s <- t %>%
  mutate(
    datetime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
    gbp = as.numeric(Global_active_power != '?'),
    grp = as.numeric(Global_reactive_power != '?')
  )

# Plot the graph and render to PNG
# Plot 1
plot(s$datetime, gbp, type="l", ylab="Global Active Power (kilowatts)", xlab="datetime")

# Plot 2
plot(s$datetime, voltage, type="l", ylab="Voltage", xlab="datetime")

# Plot 3
plot(s$datetime, s$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="")
lines(s$datetime, s$Sub_metering_2, col="red")
lines(s$datetime, s$Sub_metering_3, col="blue")
legend("topright", col=c("black","red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), lty=1, bty="n")

# Plot #4
plot(s$datetime, grp, type="l", ylab="Global Active Power (kilowatts)", xlab="datetime")
par(op)
dev.off()