library(gWidgets2)
library(gWidgets2RGtk2)
library(akima)
library(digest)
library(memoise)
library(rgl)
library(RWeather)
library(weatherData)

win <- gwindow("Weather Data", visible=TRUE)
nb <- gnotebook(cont=win)
group <- ggroup(horizontal = FALSE, container=nb, label = "Local")
focus(group)<-TRUE

#### LOCAL TAB ############

infogroup <- ggroup(horizontal = FALSE, container=group)

locationgroup <- ggroup(horizontal = TRUE, container=infogroup)
addSpring(locationgroup)
locationtext <- glabel("", container=locationgroup)
font(locationtext) <- c(size = 16)
addSpring(locationgroup)
changelocation <- gbutton("Change Location", container=locationgroup, handler = function(h,...){
  delete(infogroup, locationgroup)
  add(infogroup, zipgroup, anchor = 0,0)
})
addSpring(locationgroup)
delete(infogroup, locationgroup)


zipgroup <- ggroup(horizontal = TRUE, container=infogroup, anchor = 0,0)
addSpring(zipgroup)
ziplabel <- glabel("Zip Code:", container=zipgroup)
zipedit <- gedit("", container=zipgroup, expand = TRUE)
zipbutton <- gbutton("Enter", container=zipgroup, expand = TRUE, handler=function(h,...) {
  
  zipcode <- svalue(zipedit)
  # retrieve weather information from the zipcode 
  zipweather <- getWeatherFromYahoo(zipcode)
  
  current <- zipweather[[1]]
  
  location <- current[["location"]]
  city <- location[["city"]]
  region <- location[["region"]]
  country <- location[["country"]]
  
  units <- current[["units"]]
  units_temperature <- units[["temperature"]]
  units_distance <- units[["distance"]]
  units_pressure <- units[["pressure"]]
  units_speed <- units[["speed"]]
  
  wind <- current[["wind"]]
  wind_chill <- wind[["chill"]]
  wind_direction <- wind[["direction"]]
  wind_speed <- wind[["speed"]]
  
  atmosphere <- current[["atmosphere"]]
  humidity <- atmosphere[["humidity"]]
  visibility <- atmosphere[["visibility"]]
  pressure <- atmosphere[["pressure"]]
  rising <- atmosphere[["rising"]]
  
  astronomy <- current[["astronomy"]]
  sunrise <- astronomy[["sunrise"]]
  sunset <- astronomy[["sunset"]]
  
  condition <- current[["condition"]]
  condition_text <- condition[["text"]]
  condition_code <- condition[["code"]]
  temperature <- condition[["temp"]]
  date <- condition[["date"]]
  
  svalue(locationtext) <- sprintf("%s, %s, %s", city, region, country)
  svalue(datetext) <- date
  svalue(condtext) <- condition_text
  svalue(temptext) <- sprintf("%s %s", temperature, units_temperature)
  svalue(windtext) <- sprintf("%s %s coming from the %s", wind_speed, units_speed, getDirection(wind_direction))
  svalue(chilltext) <- sprintf("%s %s", wind_chill, units_temperature)
  svalue(humtext) <- sprintf("%s%%", humidity)
  svalue(vistext) <- sprintf("%s %s", visibility, units_distance)
  svalue(presstext) <- sprintf("%s %s, rising %s %s", pressure, units_pressure, rising, units_pressure)
  svalue(sunrtext) <- sunrise
  svalue(sunstext) <- sunset
  add(infogroup, locationgroup, anchor= 0,0)
  delete(infogroup, zipgroup)
  
})
addSpring(zipgroup)
addSpring(group)

#Converts degrees into readable direction
getDirection <- function(direction_text){
  dirtext <- "ERROR" #returns an error if the function doesn't work properly
  direction <- as.numeric(direction_text)
  if(direction >= 348.75 || direction <= 11.25)
    dirtext <- "N"
  else if(direction <= 33.75)
    dirtext <- "NNE"
  else if(direction <= 56.25)
    dirtext <- "NE"
  else if(direction <= 78.75)
    dirtext <- "ENE"
  else if(direction <= 101.25)
    dirtext <- "E"
  else if(direction <= 123.75)
    dirtext <- "ESE"
  else if(direction <= 146.25)
    dirtext <- "SE"
  else if(direction <= 168.75)
    dirtext <- "SSE"
  else if(direction <= 191.25)
    dirtext <- "S"
  else if(direction <= 213.75)
    dirtext <- "SSW"
  else if(direction <= 236.25)
    dirtext <- "SW"
  else if(direction <= 258.75)
    dirtext <- "WSW"
  else if(direction <= 281.25)
    dirtext <- "W"
  else if(direction <= 303.75)
    dirtext <- "WNW"
  else if(direction <= 326.25)
    dirtext <- "NW"
  else
    dirtext <- "NNW"
  return(dirtext)
}

#weathertext <- gtext(container=group)

dategroup <- ggroup(horizontal = TRUE, container=infogroup)
datetext = glabel("", container=dategroup)

curgroup <- ggroup(horizontal = FALSE, container=group)

condgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(condgroup)
condlabel <- glabel("Conditions Outside:", container=condgroup)
condtext <- glabel("", container=condgroup)
addSpring(condgroup)

tempgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(tempgroup)
templabel <- glabel("Temperature:", container=tempgroup)
temptext <- glabel("", container=tempgroup)
addSpring(tempgroup)

windgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(windgroup)
windlabel <- glabel("Wind:", container=windgroup)
windtext <- glabel("", container=windgroup)
addSpring(windgroup)

chillgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(chillgroup)
chilllabel <- glabel("Wind Chill:", container=chillgroup)
chilltext <- glabel("", container=chillgroup)
addSpring(chillgroup)

humgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(humgroup)
humlabel <- glabel("Humidity:", container=humgroup)
humtext <- glabel("", container=humgroup)
addSpring(humgroup)

visgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(visgroup)
vislabel <- glabel("Visibility:", container=visgroup)
vistext <- glabel("", container=visgroup)
addSpring(visgroup)

pressgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(pressgroup)
presslabel <- glabel("Pressure:", container=pressgroup)
presstext <- glabel("", container=pressgroup)
addSpring(pressgroup)

sunrgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(sunrgroup)
sunrlabel <- glabel("Sunrise:", container=sunrgroup)
sunrtext <- glabel("", container=sunrgroup)
addSpring(sunrgroup)

sunsgroup <- ggroup(horizontal = TRUE, container=curgroup)
addSpring(sunsgroup)
sunslabel <- glabel("Sunset:", container=sunsgroup)
sunstext <- glabel("", container=sunsgroup)
addSpring(sunsgroup)
#forecasts <- w[[2]]
#print(current[[1]])
addSpring(group)

###### HISTORICAL TAB #############

#encompasses the whole historical page
overallgroup <-ggroup(horizontal = FALSE, container = nb, label = "Historical")

#airport code
airportgroup <- ggroup(horizontal = TRUE, container = overallgroup)
addSpring(airportgroup)
glabel("Airport Code:", container = airportgroup)
airportedit <- gedit("CMX", container = airportgroup, expand = TRUE)
addSpring(airportgroup)

#encompasses the datesgroup and enter button in a horizontal fashion
innergroup <- ggroup(horizontal = TRUE, container = overallgroup)
addSpring(innergroup)

#encompasses both dates in a vertical fashion
datesgroup <- ggroup(horizontal = FALSE, container = innergroup)

enterbutton <- gbutton("Enter", container = innergroup, expand = TRUE, handler = function(h,...){
  #Make sure the day of the month entered is valid for the start date
  if(svalue(startmonths) == 2 && ((svalue(startdays)) == 29 || (svalue(startdays)) ==  30 || (svalue(startdays)) == 31))
    sday <- 29
  else if((svalue(startmonths) == 4 || svalue(startmonths) == 6 || svalue(startmonths) == 8 || svalue(startmonths) == 9 ||
             svalue(startmonths) == 11) && svalue(startdays) == 31)
    sday <- 30
  else
    sday <- svalue(startdays)
  #Format the start date yyyy-mm-dd
  startdate <- paste(svalue(startyears), paste("-", paste(svalue(startmonths), paste("-", sday))))
  
  #Make sure the day of the month entered is valid for the end date
  if(svalue(endmonths) == 2 && ((svalue(enddays)) == 29 || (svalue(enddays)) ==  30 || (svalue(enddays)) == 31))
    eday <- 29
  else if((svalue(endmonths) == 4 || svalue(endmonths) == 6 || svalue(endmonths) == 8 || svalue(endmonths) == 9 ||
             svalue(endmonths) == 11) && svalue(enddays) == 31)
    eday <- 30
  else
    eday <- svalue(enddays)
  #Format the end date yyyy-mm-dd
  enddate <- paste(svalue(endyears), paste("-", paste(svalue(endmonths), paste("-", eday))))
  
  #data <- getWeatherData("ORD")
  
  dummydata <- c("x", "y", "z")
  colornames <- c("red", "blue", "yellow")
  # Replace dummydata in the following comboboxes with colnames(data)
  # Also replace colornames with... whatever it's supposed to be? Numbers?
  
  graphgroup <- ggroup(horizontal = TRUE, container = overallgroup)
  addSpring(graphgroup)
  
  glabel("X:", container = graphgroup)
  xgraph <- gcombobox(dummydata, container = graphgroup)
  addSpring(graphgroup)
  
  glabel("Y:", container = graphgroup)
  ygraph <- gcombobox(dummydata, container = graphgroup)
  addSpring(graphgroup)
  
  glabel("Z:", container = graphgroup)
  zgraph <- gcombobox(dummydata, container = graphgroup)
  addSpring(graphgroup)
  
  glabel("Color:", container = graphgroup)
  colors <- gcombobox(colornames, container = graphgroup, expand = TRUE)
  addSpring(graphgroup)
  
  graphbutton <- gbutton("Graph", container = graphgroup, handler = function(h,...){
    #Do you graphy stuff here
  })
})
addSpring(innergroup)

#lists to put in the date comboboxes
yearitems <- c(2010, 2011, 2012, 2013, 2014)
monthitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
dayitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)

#start date
startdategroup <- ggroup(horizontal = TRUE, container = datesgroup)
startdategroup$SetSizeRequest(500,500)
addSpring(startdategroup)
glabel("Start Date:", container = startdategroup)
startyears <- gcombobox(yearitems, container = startdategroup, expand = TRUE)
startmonths <- gcombobox(monthitems, container = startdategroup, expand = TRUE)
startdays <- gcombobox(dayitems, container = startdategroup, expand = TRUE)
addSpring(startdategroup)

#end date
enddategroup <- ggroup(horizontal = TRUE, container = datesgroup)
addSpring(enddategroup)
glabel(" End Date:", container = enddategroup)
endyears <- gcombobox(yearitems, container = enddategroup, expand = TRUE)
endmonths <- gcombobox(monthitems, container = enddategroup, expand = TRUE)
enddays <- gcombobox(dayitems, container = enddategroup, expand = TRUE)
addSpring(enddategroup)




#### PLOTTING ####
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

getWeatherData <- function(code, start_date="2014-01-01", end_date="2014-01-30"){
  library("weatherData")
  #showAvailableColumns("NRT", "2014-04-04")
  #dat <- getWeatherForYear(code, 2013)
  dat <- d3<- getWeatherForDate(code, start_date=start_date,
                                end_date=end_date,
                                opt_detailed = TRUE,
                                opt_all_columns = TRUE)
  
  return(dat)
}