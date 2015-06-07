# Plot histogram for Global Active Power.
#
# Functions:
# 1) plot1
#      Usage
#        plot1(dataDir = "./")
#      Arguments
#        dataDir - directory where data file is located. Defaults to current working directory.
#      Output
#        Creates the plot1.ping in the value passed for dataDir.
# 2) createTempFile
#      So as not to waste any time, the first time any of the scripts 
#      are called, a temp file called "data.txt" is created which is a filtered version
#      of household_power_consumption.txt.
#

createTempFile <- function(dataDir){
    if(!file.exists("data.txt")){
      data <- read.table("household_power_consumption.txt",header = TRUE,sep=";", colClasses = "character")
      data[,1] <- as.Date(data[,1],format="%d/%m/%Y")
      data <- data[data$Date == as.Date("1/2/2007",format="%d/%m/%Y") | data$Date == as.Date("2/2/2007",format="%d/%m/%Y") ,]
      write.table(data,col.names=TRUE,row.names=FALSE,file="data.txt",sep=";")    
    }
}

plot1 <- function(dataDir = "./"){
    
    setwd(dataDir)
    createTempFile(dataDir)
    data <- read.table("data.txt",header = TRUE,sep=";", colClasses = "character")
    data$Global_active_power2 <- as.numeric(data$Global_active_power)
    png(file = "plot1.png", width = 480, height = 480)
    hist(data$Global_active_power2,main="Global Active Power",xlab ="Global Active Power (kilowatts)",col = "red")
    dev.off()
    
}