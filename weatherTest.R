library(gWidgets2RGtk2)

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
#   zipcode <- svalue(zipedit)
#   zipweather <- getWeatherFromYahoo(zipcode)
#   
#   current <- zipweather[[1]]
#   
#   location <- current[["location"]]
#   city <- location[["city"]]
#   region <- location[["region"]]
#   country <- location[["country"]]
#   
#   units <- current[["units"]]
#   units_temperature <- units[["temperature"]]
#   units_distance <- units[["distance"]]
#   units_pressure <- units[["pressure"]]
#   units_speed <- units[["speed"]]
#   
#   wind <- current[["wind"]]
#   wind_chill <- wind[["chill"]]
#   wind_direction <- wind[["direction"]]
#   wind_speed <- wind[["speed"]]
#   
#   atmosphere <- current[["atmosphere"]]
#   humidity <- atmosphere[["humidity"]]
#   visibility <- atmosphere[["visibility"]]
#   pressure <- atmosphere[["pressure"]]
#   rising <- atmosphere[["rising"]]
#   
#   astronomy <- current[["astronomy"]]
#   sunrise <- astronomy[["sunrise"]]
#   sunset <- astronomy[["sunset"]]
#   
#   condition <- current[["condition"]]
#   condition_text <- condition[["text"]]
#   condition_code <- condition[["code"]]
#   temperature <- condition[["temp"]]
#   date <- condition[["date"]]
#   
#   svalue(locationtext) <- sprintf("%s, %s, %s", city, region, country)
#   svalue(datetext) <- date
#   svalue(condtext) <- condition_text
#   svalue(temptext) <- sprintf("%s %s", temperature, units_temperature)
#   svalue(windtext) <- sprintf("%s %s coming from the %s", wind_speed, units_speed, getDirection(wind_direction))
#   svalue(chilltext) <- sprintf("%s %s", wind_chill, units_temperature)
#   svalue(humtext) <- sprintf("%s%%", humidity)
#   svalue(vistext) <- sprintf("%s %s", visibility, units_distance)
#   svalue(presstext) <- sprintf("%s %s, rising %s %s", pressure, units_pressure, rising, units_pressure)
#   svalue(sunrtext) <- sunrise
#   svalue(sunstext) <- sunset
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

overallgroup <-ggroup(horizontal = FALSE, container = nb, label = "Historical")

airportgroup <- ggroup(horizontal = TRUE, container = overallgroup)
addSpring(airportgroup)
glabel("Airport Code:", container = airportgroup)
airportedit <- gedit("", container = airportgroup, expand = TRUE)
addSpring(airportgroup)

innergroup <- ggroup(horizontal = TRUE, container = overallgroup)
addSpring(innergroup)
hgroup <- ggroup(horizontal = FALSE, container = innergroup)

#addSpring(innergroup)
enterbutton <- gbutton("Enter", container = innergroup, expand = TRUE, handler = function(h,...){
  startdate <- paste(svalue(startyears), paste("-", paste(svalue(startmonths), paste("-", svalue(startdays)))))
  print(startdate)
})
addSpring(innergroup)

yearitems <- c(2010, 2011, 2012, 2013, 2014)
monthitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
dayitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)

startdategroup <- ggroup(horizontal = TRUE, container = hgroup)
addSpring(startdategroup)
glabel("Start Date:", container = startdategroup)
startyears <- gcombobox(yearitems, container = startdategroup, expand = TRUE)
startmonths <- gcombobox(monthitems, container = startdategroup, expand = TRUE)

# if(svalue(months) == 1 || svalue(months) == 3 || svalue(months) == 5 || svalue(months) == 7 ||
#      svalue(months) == 8 || svalue(months) == 10 || svalue(months) == 12)
#   dayitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
# else if(svalue(months) == 2)
#   dayitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
# else
#   dayitems <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

startdays <- gcombobox(dayitems, container = startdategroup, expand = TRUE)
addSpring(startdategroup)

enddategroup <- ggroup(horizontal = TRUE, container = hgroup)
addSpring(enddategroup)
glabel(" End Date:", container = enddategroup)
endyears <- gcombobox(yearitems, container = enddategroup, expand = TRUE)
endmonths <- gcombobox(monthitems, container = enddategroup, expand = TRUE)
enddays <- gcombobox(dayitems, container = enddategroup, expand = TRUE)
addSpring(enddategroup)
