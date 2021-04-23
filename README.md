### UFO Sightings In Illinois 
#### A leaflet-based RShiny app for lookin' at UFO sightings

I found [this](https://www.kaggle.com/NUFORC/ufo-sightings) fun dataset (originally scraped [here](https://github.com/planetsig/ufo-reports) by planetsig) and thought it would make a great app while I was teaching myself how to use leaflet.

Found the corny vintage UFO gif on the [GifCities](https://gifcities.org/) search engine

#### What's in here:

complete.csv is the uncleaned data.

data_prep.R guides you through how I prepped the data for the app, including extracting colors with regex and dealing with messy/inconsistent location data.

within "appitself", you'll find app.R (the app itself) and the cleaned data.

www holds the gif and additional CSS/HTML files to make it look nice!
