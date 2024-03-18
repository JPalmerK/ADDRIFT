library(tidyr)
library(ggplot2)
library(dplyr)
rm(list=ls())
# Add noise level data
csv_directory='F:\\GPS_CSV-20230923T045356Z-001\\MorroBay Mar 2023 Noise Files'
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)
csv_files<- csv_files[1:length(csv_files)-1]
dataframes_list <- list()


# Loop through each CSV file load, change name
for (csv_file in csv_files) {
  # Read the CSV file
  df <- read.csv(csv_file, header = TRUE)
  
  # Extract the first and eighth columns
  colnames(df)[1]<-'UTC'
  
  # Extract the first 10 characters from the filename
  file_name <- substr(basename(csv_file), 1, 10)
  
  # Create a 'DriftName' column with the extracted filename
  df$DriftName <- file_name
  
  # Append the dataframe to the list
  dataframes_list[[file_name]] <- df
}



# Combine all dataframes into a single dataframe
noiseDf <- bind_rows(dataframes_list)
noiseDf$datetime_posix <- as.POSIXct(noiseDf$UTC, 
                                     format = "%Y-%m-%dT%H:%M:%OSZ",
                                     tz='UTC')


library(tidyr)

keycol <- "Measurement"
valuecol <- "Value"
gathercols <- colnames(noiseDf)[2:24]

data_long= gather_(noiseDf, keycol, valuecol, gathercols)


# Get the frequency values
foo <- data.frame(do.call('rbind', 
                          strsplit(as.character(data_long$Measurement),'_',
                                   fixed=TRUE)))
data_long$CenterF = foo$X2
measurementNames = as.factor(unique(data_long$Measurement))

data_long$Measurement= as.factor(data_long$Measurement)
data_long$Measurement <- ordered(data_long$Measurement, levels = measurementNames)
data_long$MeasDummy = as.numeric(data_long$Measurement)

ggplot(data_long)+
  facet_wrap(~DriftName,nrow = 8)+
  geom_path(aes(x = datetime_posix,
                y= Value, color =Measurement ))+
  ylab('1/3 Octave Level')+
  scale_color_discrete()+
  theme_bw()
  
ggplot(data_long[data_long$DriftName =='ADRIFT_046',])+
  geom_tile(aes(x= datetime_posix, y= MeasDummy, fill = Value))+
  scale_fill_viridis_c()

ggplot(data_long)+
  facet_wrap(~DriftName,nrow = 8)+
  geom_tile(aes(x= datetime_posix, y= MeasDummy, fill = Value))+
  scale_y_continuous(breaks = c(0,5,10,15,20),
    labels= unique(data_long$CenterF)[c(0,5,10,15,20)+1])+
  scale_fill_viridis_c()+
  theme_bw()+
  labs(x = "", 
       y = "Frequency (Hz)", 
   fill = expression(paste(
     "Third Octave\nLevel (dB re 1",
                     mu, 'pa)')))+
  ggtitle('Morro Bay Spring 2023')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(as.POSIXct(c("2023-03-15 00:00:00",
                    "2023-03-15 09:00:00"),
                format = "%Y-%m-%d %H:%M:%OS", 
                tz= 'UTC'))
  





#############################################
# Plot area monitored throughout deployment
#############################################


# Pick a few third octave levels
measurments = unique(data_long$MeasDummy)[c(1, 10, 17, 23)]

plotNLData = data_long[data_long$MeasDummy %in% measurments,]


for (ii in 1:lenght(measurments)){
  
  # Get the noise level data
  datasub = plotNLData[plotNLData$MeasDummy == measurments[ii],]
  
  
  
}





#############################################
# Peak finding
#########################################
library(zoo)



argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(w, span) {
  peaks <- argmax(x, y, w=w, span=span)
  
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]), col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
}


#####################################################
# Get all peaks in all spectrograms
#####################################################

ThridOctPeaks = ls()

measurements=unique(data_long$Measurement)
dataSub = data_long[data_long$DriftName =='ADRIFT_046',]
dataSub= subset(dataSub, Measurement ==measurements[1])


y = dataSub$Value
x = 1:nrow(dataSub)
w=1
span=0.02

peaks = argmax(x,y, w=w, span = span)
test(w, span)

# For the first 5 third octave levels, find the peaks
dataSub = data_long[data_long$DriftName =='ADRIFT_046',]
keepVals=NaN
x = 1:length(unique(dataSub$UTC))

for(ii in 1:8){
  dataVals = subset(dataSub, Measurement ==measurements[ii])
  y = dataVals$Value[x]
  peaks = argmax(x,y , w=w, span = span)
  
  test(w, span)
  
    # As we go up in frequency there should be fewer peaks
    keepVals=c(keepVals, peaks$x)
}

keepVals=sort(keepVals)
diffVls =  c(0, diff(keepVals))



# Find indices of duplicated values or values with a difference of <= 1
filter_indices <- duplicated(keepVals) | c(0, diff(keepVals)) <= 2

combined_result <- combine_values(keepVals, tolerance = 3)


#ThridOctPeaks[[1]]$DriftName = 
#ThridOctPeaks[[1]]$PeakTime = dataSub$datetime_posix[filtered_vector]





plot(x, y, type='l')
points(x[filtered_vector], 
       peaks$y.hat[filtered_vector], 
       col="Red", pch=19, cex=1.25)



boundaryData = data.frame(x1=dataSub$datetime_posix[filtered_vector]-minutes(2),
                          x2=dataSub$datetime_posix[filtered_vector]+minutes(2))
boundaryData$y1= 1
boundaryData$y2= 4


plotData = subset(dataSub, MeasDummy %in% c(1:5) )
#plotData= plotData[x,]

ggplot(dataSub)+
  geom_tile(aes(x= datetime_posix, y= MeasDummy, fill = Value))+
  scale_fill_viridis_c()+
  geom_rect(data=boundaryData,
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
            color="red", alpha=0.5) +
  xlim(c(min(plotData$datetime_posix), 
         min(plotData$datetime_posix)+hours(10)))
  






library(cardidates)



AA = peakwindow(vals,)
ind <- AA$smd.indices
lines(x[ind], vals[ind], col="red", lwd=2)


## generate test data with 3 peaks
set.seed(123)
x <- seq(0, 360, length = 20)
y <- abs(rnorm(20, mean = 1, sd = 0.1))
y[5:10] <- c(2, 4, 7, 3, 4, 2)
y <- c(y, 0.8 * y, 1.2 * y)
x <- seq(0, 360, along = y)
y[6] <- y[7]   # test case with 2 neighbouring equal points

## plot the test data
plot(x, y, type="b")

## identify the "spring mass development"
peaks <- peakwindow(x, y)
ind <- peaks$smd.indices
lines(x[ind], y[ind], col="red", lwd=2)


## now fit the cardinal dates
fit <- fitweibull6(peaks$smd.x, peaks$smd.y)
CDW(fit)
plot(fit)
  #########################################
# Correlation in time
#########################################


DrifExtents <- data_long %>%                             
  group_by(DriftName) %>%
  summarise_at(vars(datetime_posix),
               list(min = min, max=max))


minTime = max(DrifExtents$min)
maxTime = max(DrifExtents$max)

CorrData = data.frame(UTC = seq(minTime, maxTime, by = '2 min'))

# Put noise level predictions on the same timescale
for(ii in 1:length(unique(DrifExtents$DriftName))){
  
  NLsub = subset(noiseDf, DriftName== unique(DrifExtents$DriftName)[ii])
    
  NL_FUN <-  approxfun(NLsub$datetime_posix, NLsub$TOL_500)
  
  preds = data.frame(NL = NL_FUN(CorrData$UTC))
  colnames(preds)<-unique(DrifExtents$DriftName)[ii]
  
  CorrData= cbind(CorrData, preds)
  
}



# Correlation matrix
Pairwise_Correlation=cor(tempdf[,2:11], use="pairwise.complete.obs")
summary(Pairwise_Correlation[Pairwise_Correlation<1])

corrgram(Pairwise_Correlation, 
         lower.panel=panel.pie, upper.panel=NULL, 
         type = 'corr',
         main="2013 Noise Levels Correlations ") 

ccf(subset(noiseDf, DriftName %in$ c('ADRIFT_046','ADRIFT_047', select='TOL_125'), y-variable name)
  
  
library(corrplot)

M<-cor(noiseDf[,c()])




