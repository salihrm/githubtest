
#load package
library(fitdistrplus)


# import the data file from any logic. 

dataanylogic <- read.table("C:\\Users\\Salih\\Downloads\\LogProcessing.txt", header=TRUE, sep = ";")


n = nrow(dataanylogic)


# read and separate the data vectors.

Tstart <- dataanylogic$Fri.Nov.12.06.01.32.CET.2021
Tend <- dataanylogic$Fri.Nov.12.06.02.29.CET.2021
NrItems <- dataanylogic$X13.0

# create vectors to save the results 

processingtime=c()
processingtimeforautomatic=c()
processingtimeregular=c()


# loop to read the rows V1,V2 then delete the charterers. 
# change the date format to time format.
# calculate the time difference between the start and the end time.


for (i in 1:(n-1))
{
    
    datavar1 <- Tstart[i]
    daravar2 <- Tend[i]
    

    disp <- gsub("[a-zA-Z]", "", datavar1)
    disp2 <- gsub("[a-zA-Z]", "", daravar2)


    z <- as.POSIXct(disp,format="%d %H:%M:%OS %Y")
    op <- options(digits.secs = 3)
    zo <- gsub("[a-zA-Z]", "", z)

  
    z2 <- as.POSIXct(disp2,format="%d %H:%M:%OS %Y")
    op2 <- options(digits.secs = 3)
    zo2 <- gsub("[a-zA-Z]", "", z2)
    
    
    var1 <- zo
    var2 <- zo2
    
    
    
    answer<- difftime(var2, var1, units="secs")
    
    processingtime <- c(processingtime, answer)
    
    
    
# compare the number of items then calculate the required processing time
    
    if (NrItems[i] >= (20)) {
      
      processingtimeregular <- c(processingtimeregular, answer)
      
    }
    
    else {
      
      processingtimeforautomatic <- c(processingtimeforautomatic, answer)
      
    } 
} 

#plot the histogram
hist(processingtime, col = "red", breaks = 100, main = "processingtime")
hist(processingtimeregular, col = "green", breaks = 100, main = "regular")
hist(processingtimeforautomatic, col = "lightblue", breaks = 200, main = "automatic")



#  fit the data set distribution
 fitl <- fitdist(processingtimeregular, distr = "lnorm")
 fitn <- fitdist(processingtimeregular, distr = "norm")
 fitg <- fitdist(processingtimeregular, distr = "gamma")
 fitll <- fitdist(processingtimeforautomatic, distr = "lnorm")
 fitnn <- fitdist(processingtimeforautomatic, distr = "norm")
 fitgg <- fitdist(processingtimeforautomatic, distr = "gamma")
 
 
#view the summary of the fit
 summary(fitl)
 summary(fitn)
 summary(fitg)
 summary(fitll)
 summary(fitnn)
 summary(fitgg)
 
# produce plots to visualize the fit
 plot(fitl)
 plot(fitn)
 plot(fitg)
 plot(fitll)
 plot(fitnn)
 plot(fitgg)
