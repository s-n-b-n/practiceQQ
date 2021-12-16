```{r warning=FALSE, message=FALSE}
library(tidyverse)
library(tmap)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)
```

```{r}
shape <- st_read(here::here("practiceQ", "geo_export_71a6699f-f413-4a0e-be0a-b334cae8259a.shp"))%>%
  st_transform(., crs=7131)

graffiti <- read_csv(here::here("practiceQ", "Graffiti.csv"))

graffiti2<-graffiti%>%
  separate(., Point, c("A", "B"), sep = ",")

graffiti2$A<-parse_number(graffiti2$A) ## leading $ and grouping character , ignored
graffiti2$B<-parse_number(graffiti2$B) ## leading $ and grouping character , ignored

graffiti3<- graffiti2%>%
  filter(A !=	0 )%>%
  filter(B != 0)%>%
  st_as_sf(., coords = c("B", "A"),
           crs = 4326)

graffiti4<- graffiti3%>%
  filter(str_detect(Closed, "2019"))%>%
  #filter(str_detect(`Request Type`, "Building"))%>%
  st_transform(., crs=7131)

# spatial filter

graffiti_within <- graffiti4[shape,]

tmap_mode("plot")
tm_shape(shape) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(graffiti4) +
  tm_dots(col = "blue")


points_sf_joined <- shape%>%
  st_join(graffiti4)%>%
  add_count(geoid10)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  dplyr::select(geoid10 , neighborhood, density)%>%
  group_by(geoid10) %>%
  summarise(geoid10 = first(geoid10),
            neighborhood= first(neighborhood),
            density= first(density))

```

Now iwill read in census data

```{r}
library(readr)

census_family <- read_csv(here::here("practiceQ", "ACSST5Y2019.S1101_data_with_overlays_2021-11-25T162419.csv"))

  census_family2 <- census_family%>%
    clean_names()

census_family3 <- shape %>%
  mutate(joiner = paste("1400000US", geoid10, sep=""))
```