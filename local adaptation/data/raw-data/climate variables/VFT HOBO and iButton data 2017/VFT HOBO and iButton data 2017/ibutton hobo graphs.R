library(chron)
ibutt <- read.csv("/Users/Allison/Desktop/ibutt.csv", header= TRUE)
ibutt$finaldate.time = as.POSIXlt(as.character(ibutt$Date.Time), format= "%m/%d/%y %H:%M")

hobo <- read.csv("/Users/Allison/Desktop/hobo.csv", header= TRUE)
hobo$finaldate.time = as.POSIXlt(as.character(hobo$Date.time), format= "%m/%d/%y %H:%M")
hobo$water <- (hobo$water1+ hobo$water2)/2
hobo$water[which(hobo$water< -100)] <- hobo$water2[which(hobo$water< -100)]

plot(ibutt$Value ~as.numeric(ibutt$finaldate.time), xaxt= "n", type= "l", ylab= "Temperature (degrees C)", xlab= "Date")
axis.POSIXct(side= 1, x= ibutt$finaldate.time)
plot(hobo$water*30 ~as.numeric(hobo$finaldate.time), type= "l", xaxt= "n",ylab= "Water content (m^3/m^3)",xlab= "Date")
axis.POSIXct(side= 1, x= ibutt$finaldate.time, xlab= "Date")
