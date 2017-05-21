# *********************************************************************************************************
# plot3.R code : This file generates an exploratory line graph for 
# an 3 sub-meter readings by a household
# for the days 01-Feb-2007 and 02-Feb-2007
# 
# Process :
#   1) Code downloads dataset from the given URLdecode and unzips the datafile into a specific datapath
#   2) The txt file data read into data.table using fread()
#   3) A subset is taken for 01-Feb-2007 and 02-Feb-2007
#   4) A new column called datetime is created which combines Date and Time columns for further analysis
#   5) Line graph generated for 3 sub-meter readings for the specified date time
#   6) Generated plot is saved into plot3.png file
#   7) all connections are closed and temp files deleted
# *********************************************************************************************************

plot3 <- function()
{
  ## Load data.table library for creating and using data.table structures
  suppressPackageStartupMessages(library(data.table))
  
  #declare static variables
  temp_datapath <- paste("./tempdata",Sys.Date(), sep = "")
  file_link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  txt_filename <- "household_power_consumption.txt"
  
  # Create the temp datapath
  if(!file.exists(temp_datapath)) { dir.create(temp_datapath) }
  
  #download file and unzip it to the temp data path
  download.file(file_link, destfile = paste(temp_datapath,"/datafile.zip", sep=""))
  unzip(paste(temp_datapath,"/datafile.zip", sep=""), overwrite = T, exdir = temp_datapath)
  
  print("***File Downloaded and unzipped***")
  
  #read the file into data.table using fread. This method is faster compared to read.table
  pwrusg_all <- fread(paste(temp_datapath,"/",txt_filename, sep=""), 
                      sep = ";", header = T, stringsAsFactors = F, na.strings = c("?"), 
                      colClasses = c("Date", "POSIXlt","numeric","numeric","numeric", "numeric",
                                     "numeric","numeric","numeric"))
  
  print("***File read into a data table***")
  
  # Convert date and time within the data.table for easier manipulation of Date and Time
  pwrusg_all[,Date := as.Date(Date,"%d/%m/%Y")]
  
  # Subset the data table to extract data for only 01-Feb-2007 and 02-Feb-2007
  pwrusg_filt <- pwrusg_all[Date >= "2007-02-01" & Date <= "2007-02-02"]
  
  # Add a new column DateTime to combine date and time columns
  pwrusg_filt[,DateTime := strptime(paste(Date, Time, sep = " "), "%Y-%m-%d %H:%M:%S")]
  
  
  # generate line graph and copy it to plot3.png file
  #----------------------------------------------------
  # select the output device and set the plot width and height
  png("plot3.png",width = 480, height = 480, units = "px")
  
  # develop the line chart and annotate it
  
  with(pwrusg_filt, {
    plot(DateTime,Sub_metering_1, type="l", xlab="Date Time", ylab="Energy sub metering")
    points(DateTime, Sub_metering_2, col = "red", type = "l")
    points(DateTime, Sub_metering_3, col = "blue", type = "l")
    legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col = c("black", "red", "blue"), lty = c(1,1,1), lwd = c(1,1,1))
  })
  
  # close connection to png file 
  dev.off()
  
  print("***Required plot generated and stored in .png file under working dir***")
  # Clean-up : delete the txt file and delete the temp data folder
  unlink(temp_datapath, recursive = T)
  
}