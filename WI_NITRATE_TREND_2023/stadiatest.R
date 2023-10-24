library(leaflet)

map <- leaflet() %>%
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/{variant}/{z}/{x}/{y}{r}.png?api_key={apikey}",
    attribution = paste('&copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://www.stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
    options = tileOptions(variant='stamen_toner_lite', apikey = 'YOUR-API-KEY')
  )  %>%
  fitBounds(lng1 = -92.155801, lat1 = 42.210618, lng2 = -91.543313, lat2 = 45.642400)

#map
