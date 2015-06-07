# Plot multiple graphs in one PNG.
#
# Functions:
# 1) plot4
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

plot4 <- function(dataDir = "./"){
    
    setwd(dataDir)
    createTempFile(dataDir)
    data <- read.table("data.txt",header = TRUE,sep=";", colClasses = "character")
    
    data$date_time <- paste(data[,1],data[,2],sep = " ")
    data$date_time2 <- strptime(data$date_time,format="%Y-%m-%d %H:%M:%S")
    
    data$Global_active_power2 <- as.numeric(data$Global_active_power)
    
    data$Voltage <- as.numeric(data$Voltage)
    
    data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
    data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
    data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
    
    data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
    
    png(file = "plot4.png", width = 480, height = 480)
    par(mfrow = c(2, 2)) 
    
    with(data,{
         
         plot(date_time2,Global_active_power2,type="l",xlab="",ylab="Global Active Power")
    
         plot(date_time2,Voltage,type="l",xlab="datetime")          
         
         plot (date_time2,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering")
         lines(date_time2,data$Sub_metering_2, col = "red")
         lines(date_time2,data$Sub_metering_3, col = "blue")    
         legend("topright",bty="n", legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"),  lwd=2)
         
         plot(date_time2,Global_reactive_power,type="l",xlab="datetime")          
    })
    dev.off()
    
}