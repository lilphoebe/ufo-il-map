#data prep

ufo <- read.csv("complete.csv")

#examine blanks
blanks <- ufo %>%
  filter(country == "") %>%
  filter(state != "")
blanks

#assign correct countries to blanks
canada_sts <- ufo %>%
  filter(country == "ca")

canstates <- unique(canada_sts$state)
canstates <- canstates[-8]

us_sts <- ufo %>%
  filter(country == "us")

usstates <- unique(us_sts$state)
usstates <- as.vector(usstates)

for (i in 1:nrow(blanks)) {
  obs <- blanks[i,]
  if (obs$state %in% usstates) {blanks[i,]$country <- "us"}
  else if (obs$state %in% canstates) {blanks[i,]$country <- "ca"}
  else next
}

us_blanks <- blanks %>%
  filter(country == "us")

unique(us_blanks$country)
#ok ready to load back in
#binding in fixed data

ufo <- ufo %>%
  rbind(us_blanks)


#removing data with no lat/long coordinates
ufo <- ufo %>%
  filter(country == "us") %>%
  mutate(latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude),
         !is.na(latitude)) 

#more bad location data
ufo <- ufo %>%
  filter(!longitude == 0) %>%
  filter(!latitude == 0)
nrow(ufo)

#filtering alaska/hawaii
ufo <- ufo %>%
  filter(!state %in% c("ak", "hi"))
nrow(ufo)

#time to deal with all the outliers with bad coordinates...
easties <- ufo %>%
  filter(longitude > -67)

nrow(easties)

#flights, puerto rico, boats, mistakes --
#no need to save these

ufo <- ufo %>%
  filter(longitude <= -67.2)
nrow(ufo)

southies <- ufo %>%
  filter(latitude < 24.4)


#mistakes, puerto rico, etc
#allowing for estimates of key west

northies <- ufo %>%
  filter(latitude > 49)


#canada mistakes, camping, etc.

ufo <- ufo %>%
  filter(latitude >= 24.4) %>%
  filter(latitude <49.5)

nrow(ufo)


#parsing datetime and cleaning up time data
ufo <- ufo %>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"))

ufo <- ufo %>%
  filter(datetime > "1999-01-01 02:30:00")

#setting blank shapes to "unknown"
ufo <- ufo %>%
  mutate(across(c("shape"), ~ifelse(.=="", "unknown", as.character(.))))

#extract color!!

pat <- "([Ss]ilver)|([Gg]reen)|([Bb]lue)|([Aa]mber)|([Rr]ed)|([Oo]range)|([Oo]rangish)|([Ww]hite)|([Pp]urple)|([Yy]ellow)|([Cc]opper)|([Bb]lack)|([Ss]ilver)|([Bb]luish)|([Tt]urquoise)|([Tt]eal)|([Pp]ink)"
ufo <- ufo %>%
  mutate(colors = str_extract(comments, pattern = pat)) %>%
  mutate(colors = str_to_lower(colors))

#haha didn't have to deal with uppercase after all, duh

ufo$colors[is.na(ufo$colors)] <- "colorless" #eliminating color NAs

#getting picky with colors

ufo$colors[ufo$colors=="bluish"] <- "blue"
ufo$colors[ufo$colors=="orangish"] <- "orange"

ufo <- ufo %>%
  mutate(colors = as.factor(colors))

#lat/long data is a mix of precise coordinates and default for the listed city.
#so we'll jitter to avoid overlap on our map.
uj <- ufo %>%
  mutate(latitude = jitter(latitude, factor = 0.001),
         longitude = jitter(longitude, factor = 0.001))

#make label category for pop-ups on leaflet map

uj <- uj %>%
  mutate(label = as.character(paste(colors, shape, strftime(datetime, "%m/%d/%Y"))))

#our gorgeous color palette

cov2 <- colorFactor(palette = c("#FF0A33", "#FFFFFF", "#DEDEE3","#FFAD62","#6CC678","#F49097","#CF8D4F", "#D5EBEB","#2BA3DE", "#5D5B5D", "#F5E960","#FFC833","#C3B2FB", "#00AEB8","#44DFCD"), 
                    levels = c("red", "white", "colorless", "orange", "green", "pink", "copper", "silver", "blue", "black", "yellow", "amber", "purple", "teal", "turquoise"))


#clean up our comments section a little more by removing unicode - leave the fun caps and stuff!
ujc <- uj %>% 
  mutate(comments = str_replace_all(comments, "&#44", ",")) %>%
  mutate(comments = str_replace_all(comments, "&quot;", "'")) %>% 
  mutate(comments = str_replace_all(comments, "&#33", "!")) %>%
  mutate(comments = str_replace_all(comments, "&#39", "'"))

#i was still having issues with overlapping dots so I used this:

#a table of how many dots have identical lat or long:

dups <- data.frame(table(c(round(ujc$longitude,digits=4))))
dup_longs <- dups %>%
  filter(Freq > 1) %>%
  arrange(Freq) %>%
  filter(Freq > 1) %>%
  select(Var1)

nrow(dup_longs)

#then a loop to jitter just those overlapping data.
#i did it for frequencies >8 with a larger factor, the slowly down to >1.
#and modified the factor to mess with actual precise info the minimum amount.

for (row in 1:nrow(ujc)) {
  var <- round(ujc$longitude[row], digits = 4)
  
  if (var %in% dup_longs$Var1) {
    ujc$longitude[row] <- jitter(ujc$longitude[row], factor = .003)
  }
  else next
}

#same for latitudes

duplat <- data.frame(table(c(round(ujc$latitude,digits=4))))
dup_lt <- duplat %>%
  filter(Freq > 1) %>%
  arrange(Freq) %>%
  filter(Freq > 1) %>%
  select(Var1)

nrow(dup_lt)

for (row in 1:nrow(ujc)) {
  var <- round(ujc$latitude[row], digits = 4)
  
  if (var %in% dup_lt$Var1) {
    ujc$latitude[row] <- jitter(ujc$latitude[row], factor = .003)
  }
  else next
}


#pare down to IL:
il_uf <- ujc %>%
  filter(state == "il")

#write the csv!
write.csv(cleaned, file = "clean_il_ufo.csv")

