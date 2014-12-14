mapplot <- function(xset,yset,zset,c,name,xlabel,ylabel,zlabel){
  library(rgl)
  library(akima)
  
  cset = c
  cset = cut(c, breaks=64)
  cols = rainbow(64)[as.numeric(cset)]
  df <- data.frame(x=xset,
                   y=yset,
                   z=zset,
                   color=cols)
  grad <- terrain.colors(5)
  plot3d(x=df$x,y=df$y,z=df$z,
         xlab=xlabel, ylab=ylabel, zlab=zlabel, col=df$color, main="test", type="s", size=1)
  s=interp(df$x,df$y,df$z,duplicate="mean")
  
  #surfaces <- c(length(xset))
  #for(i in 1:length(zset)){
   # surfaces[i]=interp(x,c,y)
    #surface3d(x=surface$x,y=s$y,z=zset[i],col=grad[surfaces[i]$c],alpha=0.1)  
  #}
  surface3d(x=s$x,y=s$y,z=(s$z),col=grad[s$z],alpha=0.75)
  
  
}

xtest <- c(1,1,1,5,5,5,3,3,3)
ytest <- c(1,1,3,2,3,4,3,4,5)
ztest <- c(1,2,3,1,2,3,1,2,3)
ctest <- c(10,20,30,15,25,35,20,30,40)

getWeatherData <- function(){
  library("weatherData")
  showAvailableColumns("NRT", "2014-04-04")
  
}

getHistoricalWeather <- function(airport.code="SFO", date="Sys.Date()")
{
  base.url <- 'http://api.wunderground.com/api/{f02bab890e85dbd8}/'
  # compose final url
  final.url <- paste(base.url, 'history_', date, '/q/', airport.code, '.json', sep='')
  
  
  # reading in as raw lines from the web service
  conn <- url(final.url)
  raw.data <- readLines(conn, n=-1L, ok=TRUE)
  # Convert to a JSON
  weather.data <- fromJSON(paste(raw.data, collapse=""))
  close(conn)
  return(weather.data)
}