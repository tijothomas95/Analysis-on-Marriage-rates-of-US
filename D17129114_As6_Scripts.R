library(rgdal)
library(rgeos)
library(ggplot2)
library(dplyr)
library(rgdal)

library(geojsonio)
spdf <- geojson_read("us_states_hexgrid.geojson.json",  what = "sp")

library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
us_marriage_rate <- tidy(spdf, region = "google_name")

library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = us_marriage_rate, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

#Visualisation - 1 ----------Marriage rates of US
# Found here: https://www.cdc.gov/nchs/nvss/marriage-divorce.html
data=read.table("https://www.r-graph-gallery.com/wp-content/uploads/2017/12/State_Marriage_Rates.csv", header=T, sep=",", na.strings="---")

colnames(data)
colnames(us_marriage_rate)
names(data)<-c("id","y_2015","y_2014","y_2013","y_2012","y_2011","y_2010","y_2009","y_2008","y_2007","y_2006","y_2005","y_2004","y_2003","y_2002","y_2001","y_2000","y_1999","y_1995","y_1990")

# Merge geospatial and numerical information
us_marriage_rate = us_marriage_rate %>%
  left_join(. , data, by=c("id"="id"))

View(us_marriage_rate)
# Make a first chloropleth map
ggplot() +
  geom_polygon(data = us_marriage_rate, aes(fill =  y_2015, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


# Prepare binning
us_marriage_rate$bin = cut( us_marriage_rate$y_2015 , breaks=c(seq(5,10), Inf), labels=c("5-6", "6-7", "7-8", "8-9", "9-10", "10+" ), include.lowest = TRUE )

# Prepare a color scale coming from the viridis color palette
library(viridis)
my_palette=rev(magma(8))[c(-1,-8)]

# plot
ggplot() +
  geom_polygon(data = us_marriage_rate, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=4, alpha=0.6) +
  theme_void() +
  scale_fill_manual(
    values=my_palette,
    name="Wedding per 1000 people in 2015",
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)
  ) +
  ggtitle( "A map of marriage rates in US, state by state" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
