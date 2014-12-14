(require gWidgets2)
(require RWeather)


win <- gwindow("Weather Data", visible=TRUE)
group <- ggroup(horizontal = FALSE, container=win)
zipgroup <- ggroup(horizontal = TRUE, container=group)
ziplabel <- glabel("Zip Code:", container=zipgroup)
zipedit <- gedit("", container=zipgroup, width=5)
zipbutton <- gbutton("Run", container=zipgroup, handler=function(h,...) {
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
  
  
})

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
infogroup <- ggroup(horizontal = FALSE, container=group)
locationgroup <- ggroup(horizontal = TRUE, container=infogroup)
locationlabel = glabel("Location:", container=locationgroup)
locationtext = glabel("", container=locationgroup)

dategroup <- ggroup(horizontal = TRUE, container=infogroup)
datetext = glabel("", container=dategroup)


curgroup <- ggroup(horizontal = FALSE, container=group)

condgroup <- ggroup(horizontal = TRUE, container=curgroup)
condlabel <- glabel("Conditions Outside:", container=condgroup)
condtext <- glabel("", container=condgroup)

tempgroup <- ggroup(horizontal = TRUE, container=curgroup)
templabel <- glabel("Temperature:", container=tempgroup)
temptext <- glabel("", container=tempgroup)

windgroup <- ggroup(horizontal = TRUE, container=curgroup)
windlabel <- glabel("Wind:", container=windgroup)
windtext <- glabel("", container=windgroup)

chillgroup <- ggroup(horizontal = TRUE, container=curgroup)
chilllabel <- glabel("Wind Chill:", container=chillgroup)
chilltext <- glabel("", container=chillgroup)

humgroup <- ggroup(horizontal = TRUE, container=curgroup)
humlabel <- glabel("Humidity:", container=humgroup)
humtext <- glabel("", container=humgroup)

visgroup <- ggroup(horizontal = TRUE, container=curgroup)
vislabel <- glabel("Visibility:", container=visgroup)
vistext <- glabel("", container=visgroup)

pressgroup <- ggroup(horizontal = TRUE, container=curgroup)
presslabel <- glabel("Pressure:", container=pressgroup)
presstext <- glabel("", container=pressgroup)

sunrgroup <- ggroup(horizontal = TRUE, container=curgroup)
sunrlabel <- glabel("Sunrise:", container=sunrgroup)
sunrtext <- glabel("", container=sunrgroup)

sunsgroup <- ggroup(horizontal = TRUE, container=curgroup)
sunslabel <- glabel("Sunset:", container=sunsgroup)
sunstext <- glabel("", container=sunsgroup)
#forecasts <- w[[2]]
#print(current[[1]])
