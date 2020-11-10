#input (CH4_concentration[ppm], Pressure[hPa], Temperature[C] )
input <- read.csv("danetest.csv", header = FALSE)
#input <- c(4,  1020, 15)
#function to convert ppm to mg/m^3
c  = input[,2]*(16.04/((8.314462*(input[,4]+273))/(input[,3]*10)))/100
input <- cbind(input, c*49)
#slope calculation
linearmod <- lm(c~V1, data=input)
#Flux calculation [mg/m^3]
Flux= (linearmod$coefficients[2])*49 #[mg/s]
plot(input$V1, input$`c * 49`)
plot(input$V1, input$V2)
#teraz w druga strone:
#mamy jakis flux [mg/m^3], i chce znalezc slope:

flux = (1:100)*1000 #[mg/s]
rozmiar=600

slope = flux[]
ch4mass = 63.61
ch4ppm = ch4mass/0.683/49
flow=1060/3600
i=2
j=1
for(j in 1:10){
for(i in 2:rozmiar ){
 ch4mass[i]=ch4mass[i-1]+flux[j] + flow*(63.61/49) -flow*(ch4mass[i-1]/49)
 ch4ppm[i]=(ch4mass[i])/0.683/49
 
}
  #plot( 1:rozmiar, ch4mass[1:rozmiar], type='l', xlim=c(0.0,rozmiar), ylim=c(0.0,10000), xlab='s', ylab='mg', col = "green")
  plot( 1:rozmiar, ch4ppm[1:rozmiar]/1000, type='l', xlim=c(0.0,rozmiar), ylim=c(0.0,50), xlab='s', ylab='promil', col = "green")
  
  par(new=T)
  
}
par(new=F)




