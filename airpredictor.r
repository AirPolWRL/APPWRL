#setting the working directory
setwd("C:/Data/AirData")
#reading dataset
datatable <- read.table("azzsdata2022.txt", header = TRUE, fill = TRUE)
#creating data vectors
vecso2 <- datatable$so2
vecpm10 <- datatable$pm10
vecno2 <- datatable$no2
vecnox <- datatable$nox
vecco <- datatable$co
vecno <- datatable$no
vecpm25 <- datatable$pm25
vect <- datatable$t
vecp <- datatable$p
vecrh <- datatable$rh
veccity <- datatable$city
vectime <- datatable$time

#creating relations with linear regression, drawing diagrams, saving .png files
png(file = "linregpm10pm25new.png")
plot(vecpm25, vecpm10,col = "blue",main = "PM2.5 – PM10",
abline(lm(vecpm10~vecpm25)),cex = 1.3,pch = 16,xlab = "PM2.5 microgram/m3",ylab = "PM10 microgram/m3")
dev.off()

png(file = "linregnoxno2.png")
plot(vecnox, vecno2,col = "blue",main = "NOX - NO2",
abline(lm(vecno2~vecnox)),cex = 1.3,pch = 16,xlab = "NOX microgram/m3",ylab = "NO2 microgram/m3")
dev.off()

png(file = "linregso2no2.png")
plot(vecso2, vecno2,col = "blue",main = "SO2 - NO2",
abline(lm(vecno2~vecso2)),cex = 1.3,pch = 16,xlab = "SO2 microgram/m3",ylab = "NO2 microgram/m3")
dev.off()

png(file = "linregcono.png")
plot(vecco, vecno,col = "blue",main = "CO - NO",
abline(lm(vecno~vecco)),cex = 1.3,pch = 16,xlab = "CO miligram/m3",ylab = "NO microgram/m3")
dev.off()

png(file = "linregcono2.png")
plot(vecco, vecno2,col = "blue",main = "CO - NO2",
abline(lm(vecno2~vecco)),cex = 1.3,pch = 16,xlab = "CO miligram/m3",ylab = "NO2 microgram/m3")
dev.off()

png(file = "linregconox.png")
plot(vecco, vecnox,col = "blue",main = "CO - NOx",
abline(lm(vecnox~vecco)),cex = 1.3,pch = 16,xlab = "CO microgram/m3",ylab = "NOx microgram/m3")
dev.off()

png(file = "linregtempnox.png")
plot(vecnox, vect,col = "blue",main = "Temp - NOx",
abline(lm(vect~vecnox)),cex = 1.3,pch = 16,xlab = "Temperature C",ylab = "NOx mikrogram/m3")
dev.off()

png(file = "linregrhnox.png")
plot(vecnox, vecrh,col = "blue",main = "Relative Humidity - NOx",
abline(lm(vecrh~vecnox)),cex = 1.3,pch = 16,xlab = "Relative Humidity Percentage",ylab = "NOx microgram/m3")
dev.off()

png(file = "linregrhco.png")
plot(vecco, vecrh,col = "blue",main = "Relative Humidity - CO",
abline(lm(vecrh~vecco)),cex = 1.3,pch = 16,xlab = "CO miligram/m3",ylab = "Relative Humidity Percentage")
dev.off()

png(file = "linregtempco.png")
plot(vecco, vect,col = "blue",main = "Temp - CO",
abline(lm(vect~vecco)),cex = 1.3,pch = 16,xlab = "CO miligram/m3",ylab = "Temperature C")
dev.off()

png(file = "linregpm10conew.png")
plot(vecpm10, vecco,col = "blue",main = "PM10 - CO",
abline(lm(vecpm10~vecco)),cex = 1.3,pch = 16,xlab = "PM10 microgram/m3",ylab = "CO miligram/m3")
dev.off()

png(file = "linregpm25conew.png")
plot(vecpm25, vecco,col = "blue",main = "PM25 - CO",
abline(lm(vecpm25~vecco)),cex = 1.3,pch = 16,xlab = "PM25 microgram/m3",ylab = "CO miligram/m3")
dev.off()

png(file = "linregpm10so2new.png")
plot(vecpm10, vecso2,col = "blue",main = "PM10 – SO2",
abline(lm(vecpm10~vecso2)),cex = 1.3,pch = 16,xlab = "PM10 microgram/m3",ylab = "SO2 microgram/m3")
dev.off()

png(file = "linregtempso2new.png")
plot(vect, vecso2,col = "blue",main = "Temperature – SO2",
abline(lm(vect~vecso2)),cex = 1.3,pch = 16,xlab = "SO2 microgram/m3",ylab = "Temperature C")
dev.off()

png(file = "linregtempconew.png")
plot(vect, vecco,col = "blue",main = "Temperature – CO",
abline(lm(vect~vecco)),cex = 1.3,pch = 16,xlab = "CO microgram/m3",ylab = "Temperature C")
dev.off()

png(file = "linregnoxno.png")
plot(vecnox, vecrh,col = "blue",main = "NOx - NO",
abline(lm(vecrh~vecnox)),cex = 1.3,pch = 16, xlab = "NO microgram/m3",ylab = "NOx microgram/m3")
dev.off()

png(file = "linregnonox.png")
plot(vecnox, vecno,col = "blue",main = "NO - NOx",
abline(lm(vecno~vecnox)),cex = 1.3,pch = 16, xlab = "NO microgram/m3",ylab = "NOx microgram/m3")
dev.off()

png(file = "linregno2nox.png")
plot(vecnox, vecno2,col = "blue",main = "NOx – NO2",
abline(lm(vecno2~vecnox)),cex = 1.3,pch = 16, xlab = "NO2 microgram/m3",ylab = "NOx microgram/m3")
dev.off()

png(file = "linregno2nox.png")
plot(vecnox, vecno2,col = "blue",main = "NO2 – NOx",
abline(lm(vecno2~vecnox)),cex = 1.3,pch = 16, xlab = "NOx microgram/m3",ylab = "NO2 microgram/m3")
dev.off()

#predicting air pollutant values with linear regression, creating data vectors for prediction
datatableforprediction <- read.table("azzsdatatest.txt", header = TRUE, fill = TRUE)
vecpm10pred <- datatableforprediction$pm10
vecpm25pred <- datatableforprediction$pm25
veccopred <- datatableforprediction$co
vecnopred <- datatableforprediction$no
vecso2pred <- datatableforprediction$so2
vecnoxpred <- datatableforprediction$nox
vecno2pred <- datatableforprediction$no2

#fiting the linear model
linear_model_pm10<-lm(vecpm25~vecpm10,data = datatable)
#creating a data frame
variable_pm10<-data.frame(vecpm10pred)
#predicts the future values
predict(linear_model_pm10,newdata = variable_pm10)
predict(linear_model_pm10,newdata = variable_pm10, interval ='confidence')
#printing the model results
linear_model_pm10

#fiting the linear model
linear_model_pm25<-lm(vecpm10~vecpm25,data = datatable)
#creating a data frame
variable_pm25<-data.frame(vecpm25pred)
#predicts the future values
predict(linear_model_pm25,newdata = variable_pm25)
predict(linear_model_pm25,newdata = variable_pm25, interval ='confidence')
#printing the model results
linear_model_pm25

#fiting the linear model
linear_model_co<-lm(vecno~vecco,data = datatable)
#creating a data frame
variable_co<-data.frame(veccopred)
#predicts the future values
predict(linear_model_co,newdata = variable_co)
predict(linear_model_co,newdata = variable_co, interval ='confidence')
#printing the model results
linear_model_co

#fiting the linear model
linear_model_no<-lm(vecco~vecno,data = datatable)
#creating a data frame
variable_no<-data.frame(vecnopred)
#predicts the future values
predict(linear_model_no,newdata = variable_no)
predict(linear_model_no,newdata = variable_no, interval ='confidence')
#printing the model results
linear_model_no

#fiting the linear model
linear_model_so2<-lm(vecso2~vecno2,data = datatable)
#creating a data frame
variable_so2<-data.frame(vecso2pred)
#predicts the future values
predict(linear_model_so2,newdata = variable_so2)
predict(linear_model_so2,newdata = variable_so2, interval ='confidence')
#printing the model results
linear_model_so2

#fiting the linear model
linear_model_nox<-lm(vecnox~vecco,data = datatable)
#creating a data frame
variable_nox<-data.frame(vecnoxpred)
#predicts the future values
predict(linear_model_nox,newdata = variable_nox)
predict(linear_model_nox,newdata = variable_nox, interval ='confidence')
#printing the model results
linear_model_nox

#fiting the linear model
linear_model_no2<-lm(vecno2~vecco,data = datatable)
#creating a data frame
variable_no2<-data.frame(vecno2pred)
#predicts the future values
predict(linear_model_no2,newdata = variable_no2)
predict(linear_model_no2,newdata = variable_no2, interval ='confidence')
#printing the model results
linear_model_no2

#fiting the linear model
linear_model_nox<-lm(vecnox~vecno,data = datatable)
#creating a data frame
variable_nox<-data.frame(vecnoxpred)
#predicts the future values
predict(linear_model_nox,newdata = variable_nox)
predict(linear_model_nox,newdata = variable_nox, interval ='confidence')
#printing the model results
linear_model_nox

#fiting the linear model
linear_model_nox<-lm(vecnox~vecno2,data = datatable)
#creating a data frame
variable_nox<-data.frame(vecnoxpred)
#predicts the future values
predict(linear_model_nox,newdata = variable_nox)
predict(linear_model_nox,newdata = variable_nox, interval ='confidence')
#printing the model results
linear_model_nox