# import library readr
library(readr)
# import library dplyr
library(dplyr)
# import library ggplot2
library(ggplot2)
# import library ggmap and set API key, 
# Reference: https://www.littlemissdata.com/blog/maps
# Reference: https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
library(ggmap)


# read data frame from local
df.wine1 <- read_csv("Downloads/wine-reviews/winemag-data_first150k.csv")
df.wine2 <- read_csv("Downloads/wine-reviews/winemag-data-130k-v2.csv")

# Observe data
summary(df.wine1)
summary(df.wine2)

# choose remain data
keeps.wine <- c("country", "description", "points", "price", "province", "variety", "winery")
# combine data frame with rbind
# df.wine <- rbind(df.wine1[keeps.wine], df.wine2[keeps.wine])
df.wine <- bind_rows(df.wine1, df.wine2)[keeps.wine]
View(df.wine)

# Fliter NA vlaue
df.wine <- na.omit(df.wine)

# data frame dim and summary
dim(df.wine)
summary(df.wine)

# Find top 10 rating countries
table.country <- table(df.wine$country)
df.country <- data.frame(table.country)
colnames(df.country) <- c("Country", "Freq")
df.country.top10 <- head(arrange(df.country, desc(Freq)), 10)
df.wine <- subset(df.wine, df.wine$country %in% df.country.top10$Country)

# Find top 10 rating province
table.province <- table(df.wine$province)
df.province <- data.frame(table.province)
colnames(df.province) <- c("Province", "Freq")

# Set your API key
ggmap::register_google(key = "SET YOUR KEY HERE")

# Darw world map. Reference: https://github.com/dkahle/ggmap/issues/164
# Reference: https://stackoverflow.com/questions/11201997/world-map-with-ggmap
map.world <- ggmap(get_stamenmap(bbox = c(left = -180, bottom = -80, right = 179.9999, top = 85), zoom = 3))

# Get every latitude and longitude of every country
for (i in 1:nrow(df.country.top10)) {
  latlng = geocode(as.character(df.country$Country[i]))
  df.country$latitude[i] = as.numeric(latlng)[2]
  df.country$longitude[i] = as.numeric(latlng)[1]
}
# draw world map and put circle for each country
map.world + geom_point(aes(x=longitude, y=latitude), data=df.country.top10, col="red", alpha=0.4, size=df.country.top10$Freq / 1000) + scale_size_continuous(range = range(df.country.top10$Freq))

# Get every latitude and longitude of every province
for (i in 1:nrow(df.province)) {
  latlng = geocode(as.character(df.province$Province[i]))
  df.province$latitude[i] = as.numeric(latlng)[2]
  df.province$longitude[i] = as.numeric(latlng)[1]
}
# draw world map and put circle for each province
map.world + geom_point(aes(x=longitude, y=latitude), data=df.province, col="red", alpha=0.4, size=df.province$Freq / 1000) + scale_size_continuous(range = range(df.province$Freq))

# Find most/last rating winery
table.winery <- table(df.wine$winery)
df.winery <- data.frame(table.winery)
colnames(df.winery) <- c("Winery", "Freq")

# Find max review winery
winery.max <- df.winery[which.max(df.winery$Freq),]

# Find top 5 average point country
df.country.average <- data.frame(aggregate(df.wine[,3], list(df.wine$country), mean))
colnames(df.country.average) <- c("Country", "Average")
tail(arrange(df.country.average, Average), 5)

# Find top 5 average point province
#df.province.average <- data.frame(aggregate(df.wine[,3], list(df.wine$province), mean))
#colnames(df.province.average) <- c("Province", "Average")
#tail(arrange(df.province.average, Average), 5)

# Find max and min price
price.max <- df.wine[which.max(df.wine$price),]