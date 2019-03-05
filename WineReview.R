# import library readr
library(readr)
# read data frame from local
df.wine1 <- read_csv("Downloads/wine-reviews/winemag-data_first150k.csv")
df.wine2 <- read_csv("Downloads/wine-reviews/winemag-data-130k-v2.csv")

# choose remain data
keeps.wine <- c("country", "description", "points", "price", "province", "variety", "winery")
# combine data frame with rbind
df.wine <- rbind(df.wine1[keeps.wine], df.wine2[keeps.wine])

# Fliter NA vlaue
df.wine <- na.omit(df.wine)

# data frame dim andsummary
dim(df.wine)
summary(df.wine)

# import library dplyr
library(dplyr)
# import library ggplot2
library(ggplot2)
# import library ggmap and set API key, 
# Reference: https://www.littlemissdata.com/blog/maps
# Reference: https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
library(ggmap)

# Find top 5 rating countries
table.country <- table(df.wine$country)
df.country <- data.frame(country=table.country)
colnames(df.country) <- c("Country", "Freq")
tail(arrange(df.country, Freq), 5)

# Find top 5 rating province
table.province <- table(df.wine$province)
df.province <- subset(data.frame(table.province))
colnames(df.province) <- c("Province", "Freq")
tail(arrange(df.province, Freq), 5)

# Find max and min price
price.max <- df.wine[which.max(df.wine$price),]
price.min <- df.wine[which.min(df.wine$price),]

# Get every latitude and longitude of every country
for (i in 1:nrow(df.country)) {
  latlng = geocode(as.character(df.country$Country[i]))
  df.country$latitude[i] = as.numeric(latlng)[2]
  df.country$longitude[i] = as.numeric(latlng)[1]
}


