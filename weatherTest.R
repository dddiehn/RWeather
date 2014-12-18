library(gWidgets2)
library(gWidgets2RGtk2)
library(akima)
library(digest)
library(memoise)
library(rgl)
library(RWeather)
library(weatherData)

win <- gwindow("Weather Data", visible=TRUE, expand=TRUE)
win.SetSizeRequest (500,200);
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
airportedit <- gedit("", container = airportgroup, expand = TRUE)
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