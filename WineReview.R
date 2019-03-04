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

# import library dplyr
library(dplyr)
# import library ggplot2
library(ggplot2)

# 1. Get country table
table.country <- table(df.wine$country)
df.country <- subset(data.frame(table.country), Freq > 1000)
tail(arrange(df.country, Freq), 5)

