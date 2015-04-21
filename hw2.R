# =====================================================================
# CSE487/587
# Author: Sai Srinath
# Email: saisrina@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
#install.packages("forecast")
#install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data
# local path /Users/saisrinath/Projects/small
library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO
#set working directory
setwd('/gpfs/courses/cse587/spring2015/data/hw2/data')
#list to store filenames
fnames=dir(pattern="*.csv")


MAE_arima_list=list()
MAE_hw_list=list()
MAE_lm_list=list()
filename_list=list()

#going to iterate over all the files 
for(k in 1:length(fnames)){
  filename=fnames[k]
  
  
  # if file is not empty
  if(file.info(filename)[1]>0) {
    
    
    nrows=sapply(filename,function(f) nrow(read.csv(f)))
    no_of_rows=sum(nrows)
    if(no_of_rows>=754){
      # read one csv file into variable (DO NOT EDIT)
      textData=read.csv(file=filename, header=T)
      
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE_arima = matrix(NA,1,length(testData))
      MAE_hw = matrix(NA,1,length(testData))
      MAE_lm = matrix(NA,1,length(testData))
      
      # apply ARIMA model (DO NOT EDIT)
      arima_fitData = auto.arima(trainData,seasonal=FALSE,lambda=NULL,approximation=TRUE)
      hw_fitData = HoltWinters(trainData,gamma=FALSE)
      lm_fitData = tslm(trainData~trend)
      
      
      # apply forecast(DO NOT EDIT)
      arima_forecastData = forecast(arima_fitData, h=length(testData))
      hw_forecastData = forecast(hw_fitData, h=length(testData))
      lm_forecastData = forecast(lm_fitData, h=length(testData))
      
      # print variable and see what is in the result data set
      #print(forecastData)
      
      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE_arima[1,i] = abs(arima_forecastData$mean[i] - testData[i])
        MAE_hw[1,i] = abs(hw_forecastData$mean[i] - testData[i])
        MAE_lm[1,i] = abs(lm_forecastData$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      #print(sum(MAE_arima[1,1:10]))
      #print(sum(MAE_hw[1,1:10]))
      #print(sum(MAE_lm[1,1:10]))
      
      
      
      
      MAE_arima_list[k]=sum(MAE_arima[1,1:10])
      MAE_hw_list[k]=sum(MAE_hw[1,1:10])
      MAE_lm_list[k]=sum(MAE_lm[1,1:10])
      filename_list[k]=filename
    }
    
    
    
  }
  
}

df_arima=data.frame(unlist(filename_list),unlist(MAE_arima_list))
df_hw=data.frame(unlist(filename_list),unlist(MAE_hw_list))
df_lm=data.frame(unlist(filename_list),unlist(MAE_lm_list))

final_arima=df_arima[order(unlist(MAE_arima_list)),]
final_hw=df_hw[order(unlist(MAE_hw_list)),]
final_lm=df_lm[order(unlist(MAE_lm_list)),]
top10_arima=head(final_arima,n=10)
top10_hw=head(final_hw,n=10)
top10_lm=head(final_lm,n=10)
print("The top 10 stocks with the minimum sum of MAE using ARIMA Model:")
print(top10_arima)
print("The top 10 stocks with the minimum sum of MAE using Holt Winters Model:")
print(top10_hw)
print("The top 10 stocks with the minimum sum of MAE using Linear Regression Model:")
print(top10_lm)
setwd('/gpfs/courses/cse587/spring2015/students/saisrina/hw2')

jpeg("arima.jpg")
plot(top10_arima[1:10,2], col = "blue",xaxt='n')
lines(top10_arima[1:10,2], lw = 2, col = "red")
axis(1, at=1:10, top10_arima$unlist.filename_list[1:10])
dev.off()
jpeg("hw.jpg")
plot(top10_hw[1:10,2], col = "blue",xaxt='n')
lines(top10_hw[1:10,2], lw = 2, col = "red")
axis(1, at=1:10, top10_hw$unlist.filename_list[1:10])
dev.off()
jpeg("lm.jpg")
plot(top10_lm[1:10,2], col = "blue",xaxt='n')
lines(top10_lm[1:10,2], lw = 2, col = "red")
axis(1, at=1:10, top10_lm$unlist.filename_list[1:10])
dev.off()
}

