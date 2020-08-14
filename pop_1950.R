library(maps)
library(tidyverse)
library(maptools)
library(sf)
library(cartogram)
library(rgeos)
library(rgdal)
library(tmap)
library(broom)
library(mapproj)
library(RColorBrewer)
library(rcartocolor)


data(wrld_simpl)
plot(wrld_simpl)

afr = wrld_simpl[wrld_simpl$REGION==2,]
plot(afr)

afr <- spTransform(afr, CRS("+init=epsg:3395"))

#population data
datos <- read.csv("total_pop.csv", sep = ";", stringsAsFactors=FALSE)

datos <- datos %>% 
  gather(year, population, c(2:152)) %>% 
  select(NAME, year, population)


#remove "X"
datos$year <- str_remove_all(datos$year, "X")


#class(datos$population)
#class(datos$year)

datos$population <- as.numeric(gsub(",", ".", datos$population))
datos$year <- as.numeric(datos$year)
datos$population <- datos$population*1000

#merge
population_1950 <-
  wrld_simpl %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=robin") %>% 
  mutate(country_code = as.character(ISO3)) %>%
  left_join(datos) %>% 
  filter(year == 1950)


world_cont <- cartogram_cont(population_1950, "population", itermax = 5)


population_1950 <- tm_shape(world_cont) + tm_fill("population", 
                                   style = "jenks", 
                                   palette= "cividis",
                                   title="Population") +
  tm_raster("population", breaks=c(0, 55, 190, 430, 1.000, 1447),
            labels = c("0-14", "14-42", "42-83", "83-376", "376-554"),
            palette = terrain.colors(9), title="Population (millions)", midpoint = NA,
            legend.is.portrait = FALSE) +
  tm_layout(frame = FALSE,
            legend.show = FALSE,
            legend.title.color = "white",
            main.title.color = "white",
            main.title.size = 1.5,
            main.title.position = "center",
            main.title.fontfamily = "",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            title.size = 1.5) +
  tmap_options(bg.color = "#1E1E1E", legend.text.color = "white")

tmap_save(population_1950, "population_1950.png", width = 10, height=7)
