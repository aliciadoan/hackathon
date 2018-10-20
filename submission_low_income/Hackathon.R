library(rgdal)
library(raster)

df <- read.csv("C:/Users/596049/hackathon/data/tabular_data/moco_adu.csv", stringsAsFactors = F)

shp <- tracts(state="Maryland", county = c("Montgomery"), cb = TRUE, year = 2016, refresh = TRUE)
geo.set <- geo.make(state = "MD", county =c("Montgomery") , tract = "*")

key <- "419466df6f925dc41d274311d79e26efad0acb2a"
# Authenticate API key
api.key.install(key = key)

# Pull unemployment data from ACS
pull <- acs.fetch(endyear = 2016, span = 5, geography = geo.set,
                  table.name = "B23001", col.names = "pretty")

geo <- pull@geography
est <- pull@estimate

# Merge, drop NAMES column, remove row names
data <- cbind(geo, est)
saveRDS(data,"census_data.RDS")
drop_var <- names(data)[1]
#data$GEOID <- paste0(data$state,"0",data$county, data$tract)
data <- data[, !names(data) %in% drop_var]
rownames(data) <- NULL

# Select only total unemployment
data <- data[, c(1:3, seq(from = 4, to = 175, by = 7))]

# Rename total colum
colnames(data)[4] <- "total"

# Calculate row sums
data <- data %>% mutate(unemployment = rowSums(.[5:length(data)])) %>%
  mutate(percent = round((unemployment/total) * 100, 2)) %>%
  select(state, county, tract, unemployment, percent)

# Create GEOID
data[, 1:2] <- sapply(data[, 1:2], function(x) as.character(x))
data$state <- str_pad(data$state, width = 2, side = "left", 
                      pad = "0")
data$county <- str_pad(data$county, width = 3, side = "left",
                       pad = "0")
data$GEOID <- paste0(data$state, data$county, data$tract)

# Create choropleth map with ggplot 
# Convert sp object to sf object
gg_shp <- fortify(shp, region = "GEOID")

# Turn generated "id" to a factor GEOID column for merging
gg_shp$GEOID <- factor(gg_shp$id)
gg_shp$id <- NULL

#dplyr merge
library(dplyr)
df_income <- df[c("GEOID","MED_HH_INCOME")]
data2 <- left_join(data,df_income, by="GEOID")

# Combine 
data2$GEOID <- as.numeric(data$GEOID)

plot.data <- merge(gg_shp, data2)

ggplot(plot.data, aes(x = long, y = lat,
                      group = group)) +
  geom_polygon(aes(fill = MED_HH_INCOME)) +
  scale_fill_distiller(type = "div", palette = "Red",
                       direction = -1) + 
  coord_equal() + ggtitle("Median Income in\n Montogomery County in 2016") + theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5))

##################################################################################################################################################################

shp <- tracts(state="Maryland", county = c("Montgomery"), cb = TRUE, year = 2016, refresh = TRUE)
geo.set <- geo.make(state = "MD", county =c("Montgomery") , tract = "*")

# Pull unemployment data from ACS
pull <- acs.fetch(endyear = 2016, span = 5, geography = geo.set,
                  table.name = "B23001", col.names = "pretty")

geo <- pull@geography
est <- pull@estimate

pull_rent <- acs.fetch(endyear = 2016, span = 5, geography = geo.set,
                       table.name = "B25064", col.names = "pretty")
geo_rent <- pull_rent@geography
est_rent <- pull_rent@estimate

df$income_mid_age <- NULL
df$income_mid_age[df$MED_HH_INCOME>=76665&df$MED_HH_INCOME<=137979] <- df$LIV_ARR_ALONE_35_64[df$MED_HH_INCOME>=76665&df$MED_HH_INCOME<=137979]
live_alone_35_64 <- df$income_mid_age

# Merge, drop NAMES column, remove row names
data_inc <- cbind(geo,live_alone_35_64)
data_rent <- cbind(geo_rent, est_rent)
data <- cbing(geo,est)
write.csv(data_rent, "data_rent.csv")
drop_var <- names(data)[1]
#data$GEOID <- paste0(data$state,"0",data$county, data$tract)
data <- data[, !names(data) %in% drop_var]
rownames(data) <- NULL

# Select only total unemployment
data <- data[, c(1:3, seq(from = 4, to = 175, by = 7))]

# Rename total colum
colnames(data)[4] <- "total"

# Calculate row sums
data <- data %>% mutate(unemployment = rowSums(.[5:length(data)])) %>%
  mutate(percent = round((unemployment/total) * 100, 2)) %>%
  select(state, county, tract, unemployment, percent)

# Create GEOID
data[, 1:2] <- sapply(data[, 1:2], function(x) as.character(x))
data$state <- str_pad(data$state, width = 2, side = "left", 
                      pad = "0")
data$county <- str_pad(data$county, width = 3, side = "left",
                       pad = "0")
data$GEOID <- paste0(data$state, data$county, data$tract)

# Create GEOID
data_rent[, 1:2] <- sapply(data_rent[, 1:2], function(x) as.character(x))
data_rent$state <- str_pad(data_rent$state, width = 2, side = "left", 
                      pad = "0")
data_rent$county <- str_pad(data_rent$county, width = 3, side = "left",
                       pad = "0")
data_rent$GEOID <- paste0(data_rent$state, data_rent$county, data_rent$tract)

# Create choropleth map with ggplot 
# Convert sp object to sf object
gg_shp <- fortify(shp, region = "GEOID")

# Turn generated "id" to a factor GEOID column for merging
gg_shp$GEOID <- factor(gg_shp$id)
gg_shp$id <- NULL

plot.data <- merge(gg_shp, data)
plot.data_rent <- merge(gg_shp, data_rent)
names(plot.data_rent)[12] <- "median_rent"


#% unemployed graph
p <- ggplot(plot.data, aes(x = long, y = lat,
                      group = group)) +
  geom_polygon(aes(fill = percent)) +
  scale_fill_distiller(type = "div", palette = "RdBu",
                       direction = -1) + 
  coord_equal() + ggtitle("% Unemployed in\n Montgomery County in 2016") + theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5))

p
library(plotly)
ggplotly(p)

plot.data_rent$median_rent[plot.data_rent$median_rent==-666666666] <- NA

#Median Rent Graph
p_rent <- ggplot(plot.data_rent, aes(x = long, y = lat,
                      group = group)) +
  geom_polygon(aes(fill = median_rent)) +
  scale_fill_distiller(type = "div", palette = "RdBu",
                       direction = -1) + 
  coord_equal() + ggtitle("Median Rent in\n Montgomery County in 2016") + theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5))

p_rent

library(plotly)
ggplotly(p_rent)

# Create GEOID
data_inc[, 1:2] <- sapply(data_inc[, 1:2], function(x) as.character(x))
data_inc$state <- str_pad(data_inc$state, width = 2, side = "left", 
                      pad = "0")
data_inc$county <- str_pad(data_inc$county, width = 3, side = "left",
                       pad = "0")
data_inc$GEOID <- paste0(data_inc$state, data_inc$county, data_inc$tract)

plot.data_inc <- merge(gg_shp, data_inc)
  
#Living ALone 35-64 Graphs
p_inc <- ggplot(plot.data_inc, aes(x = long, y = lat,
                           group = group)) +
  geom_polygon(aes(fill = live_alone_35_64)) +
  scale_fill_distiller(type = "div", palette = "RdBu",
                       direction = -1) + 
  coord_equal() + ggtitle("Middle Income 35-64 Living Alone in\n Montgomery County in 2016") + theme(legend.title = element_blank()) + theme(plot.title = element_text(hjust = 0.5))

p_inc
library(plotly)
ggplotly(p_inc)
