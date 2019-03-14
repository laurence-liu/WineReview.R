# import library readr
library(readr)
# import library dplyr
library(dplyr)
# import library ggplot2
library(ggplot2)
# import tidyverse
# library(tidyverse)
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
keeps.wine <- c("country", "description", "points", "price", "province", "region_1","variety", "winery")
# combine data frame with rbind
# df.wine <- rbind(df.wine1[keeps.wine], df.wine2[keeps.wine])
df.wine <- bind_rows(df.wine1, df.wine2)[keeps.wine]
View(df.wine)

# Fliter NA vlaue and delete duplicated value
df.wine <- distinct(na.omit(df.wine))
# df.wine <- na.omit(df.wine)

# data frame dim and summary
dim(df.wine)
summary(df.wine)

table.country <- table(df.wine$country)
df.country <- data.frame(table.country)
colnames(df.country) <- c("Country", "Freq")
df.country <- arrange(df.country, desc(Freq))
View(df.country)

# Set your API key
ggmap::register_google(key = "SET YOUR KEY HERE")

# Darw world map. Reference: https://github.com/dkahle/ggmap/issues/164
# Reference: https://stackoverflow.com/questions/11201997/world-map-with-ggmap
# map.world <- get_googlemap(center = c(lon = 0, lat = 0), zoom = 1, maptype="roadmap", size = c(2040, 1844), scale = 2) %>% ggmap(.)

# Get every latitude and longitude of every country
# for (i in 1:nrow(df.country)) {
#  latlng = geocode(as.character(df.country$Country[i]))
#  df.country$latitude[i] = as.numeric(latlng)[2]
#  df.country$longitude[i] = as.numeric(latlng)[1]
# }
# draw world map and put circle for each country
# map.world + geom_point(aes(x=longitude, y=latitude), data=df.country, col="red", alpha=0.4, size=df.country$Freq / 1000) + scale_size_continuous(range = range(df.country$Freq))

# Get all US wine
df.wine.US <- arrange(subset(df.wine, df.wine$country == "US"), desc(price))

# Get all US provinces
table.province.US <- table(df.wine.US$province)
df.province.US <- data.frame(table.province.US)
colnames(df.province.US) <- c("Province", "Freq")
df.province.US <- arrange(df.province.US, desc(Freq))

# Get every latitude and longitude of every provinces
for (i in 1:nrow(df.province.US)) {
  latlng = geocode(as.character(df.province.US$region[i]))
  df.province.US$latitude[i] = as.numeric(latlng)[2]
  df.province.US$longitude[i] = as.numeric(latlng)[1]
}

# Get USA center
center.USA <- as.numeric(geocode("United States"))
# Draw USA map
map.USA <- ggmap(get_googlemap(center=center.USA, scale=2, zoom=4), extent="normal")

map.USA + geom_point(aes(x=longitude, y=latitude), data=df.province.US, col="red", alpha=0.4, size=df.province.US$Freq / 1000) + scale_size_continuous(range = range(df.province.US$Freq))

df.state <- map_data("state")
colnames(df.state) <- c("long", "lat", "group", "order", "Province", "subregion")

# Left_join two sheet
df.wine.state.separate <- left_join(df.state, df.province.US %>% mutate(Province=tolower(Province)))
# Draw heat map
ggplot(df.wine.state.separate, aes(long, lat)) + geom_polygon(aes(group = group, fill = Freq)) + coord_quickmap() + scale_fill_gradientn(colours=rev(heat.colors(3)))

# Draw scatter plot of price and point
ggplot(df.wine.US, aes(x=points, y=price)) + geom_point() + geom_smooth(method=lm)

# Hist and Normal Distribution of points
ggplot(df.wine.US, aes(x=points)) + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..)) + stat_function(fun=dnorm, color="red", args=list(mean=mean(df.wine.US$points), sd=sd(df.wine.US$points)))

# Level price
df.wine.US$level[20 > df.wine.US$price] ='$'
df.wine.US$level[20 < df.wine.US$price & df.wine.US$price <= 40] ='$$'
df.wine.US$level[40 < df.wine.US$price & df.wine.US$price <= 80] ='$$$'
df.wine.US$level[80 < df.wine.US$price] ='$$$$'

# Create level data frame
table.level <- table(df.wine.US$level)
df.level <- data.frame(table.level)
colnames(df.level) <- c("Level", "Freq")
df.level <- arrange(df.level, desc(Freq))

# Draw level Pie Chart
library(scales)
df.level <- df.level %>%  mutate(per=`Freq`/sum(`Freq`)) %>% arrange(desc(Level))
df.level$label <- percent(df.level$per)
ggplot(data=df.level) +
  geom_bar(aes(x="", y=per, fill=Level), stat="identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

# Level poits
df.wine.US$rating[85 > df.wine.US$points] ='Poor'
df.wine.US$rating[85 < df.wine.US$points & df.wine.US$points <= 95] ='Good'
df.wine.US$rating[95 <= df.wine.US$points] ='Excellent'

# Create rating data frame
table.rating <- table(df.wine.US$rating)
df.rating <- data.frame(table.rating)
colnames(df.rating) <- c("Rating", "Freq")
df.rating <- arrange(df.rating, desc(Freq))

# Draw rating pie chart
df.rating <- df.rating %>%  mutate(per=`Freq`/sum(`Freq`)) %>% arrange(desc(Rating))
df.rating$label <- percent(df.rating$per)
ggplot(data=df.rating) +
  scale_fill_manual(values=c('#B388FF', '#82B1FF', '#FFD180')) + 
  geom_bar(aes(x="", y=per, fill=Rating), stat="identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))


# Which variety is the most described “fruity”?
df.fuzzy.fruity <- df.wine.US[grep("fruity", df.wine.US$description),]
table.variety.fruity <- table(df.fuzzy.fruity$variety)
df.variety.fruity <- data.frame(table.variety.fruity)
colnames(df.variety.fruity) <- c("Variety", "Freq")
df.variety.fruity <- arrange(df.variety.fruity, desc(Freq))
max.variety.fruity <- df.variety.fruity[which.max(df.variety.fruity$Freq),]
df.wine.variety.fruity <- df.wine.US[df.wine.US$variety == max.variety.fruity$Variety[1],]

table.province.variety.fruity <- table(df.wine.variety.fruity$province)
df.province.variety.fruity <- data.frame(table.province.variety.fruity)
colnames(df.province.variety.fruity) <- c("region", "Freq")
df.province.variety.fruity <- arrange(df.province.variety.fruity, desc(Freq))

df.province.variety.fruity <- right_join(select(df.province.US, region, latitude, longitude), df.province.variety.fruity)

map.USA + geom_point(aes(x=longitude, y=latitude), data=df.province.variety.fruity, col="red", alpha=0.4, size=df.province.variety.fruity$Freq / 300) + scale_size_continuous(range = range(df.province.variety.fruity$Freq))

# Correlation between prices and points
points.model <- lm(price ~ points, data = df.wine.US)
summary(points.model)
attributes(points.model)
plot(df.wine.US$price~df.wine.US$points)
abline(points.model,col='red')

# Create C/P Value, using histogram and boxplot, find and remove outliers

df.wine.US$CPvalue <- df.wine.US$points/df.wine.US$price

p1 <- ggplot(data = df.wine.US, aes(x = CPvalue)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange'))
boxplot(df.wine.US$CPvalue,main= "CPvalue Boxplot", pch=19, xlab="CPvalue")
boxplot(df.wine.US$CPvalue)$out
boxplot(df.wine.US$CPvalue, plot=FALSE)$out
outliers <- boxplot(df.wine.US$CPvalue, plot=FALSE)$out
print(outliers)
df.wine.US[which(df.wine.US$CPvalue %in% outliers),]
df.wine.US <- df.wine.US[-which(df.wine.US$CPvalue %in% outliers),]
df.wine.US
boxplot(df.wine.US$CPvalue,main= "CPvalue Boxplot w/o outliers", pch=19, xlab="CPvalue")

#Wine topvalue
topvalue <- subset(df.wine.US, CPvalue>7.9)
topvalue
topvalue[,c("points", "province", "region_1", "winery", "variety")]

table.value.province <- table(topvalue$province)
table.value.province

table.value.region <- table(topvalue$region_1)
table.value.region

table.value.variety <- table(topvalue$variety)
table.value.variety
