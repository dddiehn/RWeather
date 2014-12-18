mapplot <- function(data,x,y,z,c){
  library(rgl)
  library(akima)
  xset <- data[[x]]
  yset <- data[[y]]
  zset <- data[[z]]
  xlabel <- colnames(data)[x]
  ylabel <- colnames(data)[y]
  zlabel <- colnames(data)[z]
  cset = data[[c]]
  cset = cut(c, breaks=64)
  cols = rainbow(64)[as.numeric(cset)]
  df <- data.frame(x=xset,
                   y=yset,
                   z=zset,
                   color=cols)
  grad <- terrain.colors(100)
  plot3d(x=df$x,y=df$y,z=df$z,
         xlab=xlabel, ylab=ylabel, zlab=zlabel, col=df$color, main="WEATHER", type="s", size=1)
  s=interp(df$x,df$y,df$z,duplicate="mean")
  
  #surfaces <- c(length(xset))
  #for(i in 1:length(zset)){
   # surfaces[i]=interp(x,c,y)
    #surface3d(x=surface$x,y=s$y,z=zset[i],col=grad[surfaces[i]$c],alpha=0.1)  
  #}
  surface3d(x=s$x,y=s$y,z=(s$z),col=grad[s$z],alpha=0.75)
  
  
}

#xtest <- c(1,1,1,5,5,5,3,3,3,4,1,9,8,3,6,1,7,9,5)
#ytest <- c(1,1,3,2,3,4,3,4,5,9,5,1,3,5,4,7,9,5,6)
#ztest <- c(1,2,3,1,2,3,1,2,3,1,2,3,4,5,6,7,8,9,5)
#ctest <- c(10,20,30,15,25,35,20,30,40,10,50,30,40,60,20,10,50,20,30)

getWeatherData <- function(code){
  library("weatherData")
  #showAvailableColumns("NRT", "2014-04-04")
  #dat <- getWeatherForYear(code, 2013)
  dat <- d3<- getWeatherForDate(code, start_date="2014-01-01",
                                end_date = "2014-01-30",
                                opt_detailed = TRUE,
                                opt_all_columns = TRUE)
  
  return(dat)
}