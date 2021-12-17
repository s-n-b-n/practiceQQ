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

graffiti5<- graffiti3%>%
  filter(str_detect(Closed, '2020'))

  
# spatial filter

graffiti_within <- graffiti4[shape,]
graffiti_within1 <- graffiti4[shape, , op=st_intersects]
shape_within <- shape[graffiti4,]

tmap_mode("plot")
tm_shape(shape) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(graffiti4) +
  tm_dots(col = "blue")


tmap_mode('plot')
tm_shape(shape_within)+
  tm_polygons(col=NA, alpha=0.5)+
  tm_shape(graffiti_within)+
  tm_dots(col='blue')

library(spatstat)

window <- as.owin(shape_within)

graffiti4SP <- graffiti4%>%
  as(., 'Spatial')

graffiti4SP.ppp <- ppp(x=graffiti4SP@coords[,1],
                       y=graffiti4SP@coords[,2],
                       window=window)

graffiti4SP.ppp%>%
  density(., sigma=100)%>%
  plot()


points_sf_joined <- shape_within%>%
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


points_sf_joined1 <- shape_within%>%
  st_join(graffiti4)%>%
  add_count(geoid10)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)

points_sf_joined2 <- points_sf_joined1 %>%
  add_count(geoid10, name='count')%>%
  dplyr::select(geoid10 , neighborhood, density, count)%>%
  group_by(geoid10) %>%
  summarise(geoid10 = first(geoid10),
            neighborhood= first(neighborhood),
            density= first(density),
            count=first(count))



tm_shape(points_sf_joined) +
  tm_polygons('density',
              style='jenks',
              palette='PuOr',
              midpoint=NA,
              popup.vars=c('neighborhood', 'density'),
              title='Graffiti Density')


```

Now iwill read in census data

```{r}
library(readr)

census_family <- read_csv(here::here("Data", "ACSDT5Y2019.B01003_data_with_overlays_2021-12-16T183055.csv"))

census_family2 <- census_family%>%
  clean_names()

census_family3 <- shape %>%
  mutate(joiner = paste("1400000US", geoid10, sep=""))

census_family4 <- shape_within %>%
  mutate(joiner = paste("1400000US", geoid10, sep=""))
#shape_within(지리데이터랑 그라피티 관계 있는 정보만 합쳐서 빼낸거)
#페이스트는 geoid10에 띄어쓰기 없이 어쩌고US 붙여서 새로운 joiner라는 변수 생성


census_family_merged <- census_family4 %>%
  left_join(census_family2,
            by=c('joiner' = 'geo_id')) %>%
  dplyr::distinct(joiner, .keep_all=T)%>%
  dplyr::rename(geo_id=joiner)


#points_sf_joined_pop1 <- points_sf_joined %>%
#  st_join(census_family_merged)%>%
#  dplyr::select(geoid10.x, neighborhood, density, geometry, b01003_001e)

mode(points_sf_joined_pop1$b01003_001e) <- 'numeric'


points_sf_joined_pop2 <- points_sf_joined2 %>%
  st_join(census_family_merged)%>%
  dplyr::select(geoid10.x, neighborhood, density, geometry, b01003_001e, count)

mode(points_sf_joined_pop2$b01003_001e) <- 'numeric'

dropsf <- census_family_merged%>%
  st_drop_geometry()#sf 파일을 dataframe으로

points_sf_joined_pop3 <- points_sf_joined2 %>%
    left_join(.,
            dropsf,
            by = c('geoid10'='geoid10'))%>%
  dplyr::select(geoid10, neighborhood, density, geometry, b01003_001e, count)

mode(points_sf_joined_pop3$b01003_001e) <- 'numeric' #캐릭터를 뉴메릭으로 변경

```
############

area_nb <- points_sf_joined %>%
  poly2nb(.,queen=T)

area.lw <- area_nb %>%
  nb2mat(., style="B")

sum(area.lw)
area.lw <- area_nb %>%
  nb2listw(., style='C')

I_area_global_density <- points_sf_joined %>%
  pull(density)%>%
  as.vector()%>%
  moran.test(.,area.lw)

I_area_global_density


I_area_local_density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., area.lw)%>%
  as_tibble()

slice_head(I_area_local_density, n=5)

points_sf_joined_localmoran <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_area_local_density$Ii))%>%
  mutate(density_Iz =as.numeric(I_area_local_density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

library(tmaptools)
library(RColorBrewer)

MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined_localmoran) +
  tm_polygons("density_I",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Graffiti in SF")
#############
q <- qplot(x = b01003_001e,
           y = count,
           data=points_sf_joined_pop3)

q + stat_smooth(method='lm', se=FALSE, size=1)

Regressiondata <- points_sf_joined_pop3 %>%
  dplyr::select(b01003_001e, count)

model1 <- Regressiondata %>%
  lm(count ~
       b01003_001e,
     data=.)
summary(model1)

library(broom)
tidy(model1)

ggplot(points_sf_joined_pop3, aes(x=b01003_001e)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1000) +
  geom_density(colour='red',
               size=1,
               adjust=1)

ggplot(points_sf_joined_pop3, aes(x=count)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 300) +
  geom_density(colour='red',
               size=1,
               adjust=1)

ggplot(points_sf_joined_pop3, aes(x=log(count)) +
  geom_histogram()

qplot(x = b01003_001e,
      y = log(count),
      data = points_sf_joined_pop3)
  
  
symbox(~count, 
       points_sf_joined_pop3, 
       na.rm=T,
       powers=seq(-3,3,by=.5))
