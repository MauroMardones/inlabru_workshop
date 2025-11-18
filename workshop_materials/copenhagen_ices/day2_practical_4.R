## ----child="practicals/spatial_data_types_areal.qmd"--------------------------

## -----------------------------------------------------------------------------

library(CARBayesdata)

data(pollutionhealthdata)
data(GGHB.IZ)




## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(sf)
library(ggplot2)
library(scico)


## -----------------------------------------------------------------------------
resp_cases <- merge(GGHB.IZ, pollutionhealthdata, by = "IZ")


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(dplyr)
resp_cases <- resp_cases %>% 
  mutate(SMR = observed/expected, .by = year )


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| fig-align: center
ggplot()+
  geom_sf(data=resp_cases,aes(fill=SMR))+
  facet_wrap(~year)+scale_fill_scico(palette = "roma")



## ----child="practicals/spatial_data_types_geostats.qmd"-----------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# For plotting
library(mapview)
library(ggplot2)
library(scico) # for colouring palettes

# Data manipulation
library(dplyr)
library(sdmTMB)

pcod_df = sdmTMB::pcod 
qcs_grid = sdmTMB::qcs_grid

library(sf)
pcod_sf =   st_as_sf(pcod_df, coords = c("lon","lat"), crs = 4326)


## -----------------------------------------------------------------------------
pcod_sf_proj <- st_transform(pcod_sf, crs = 32609)
st_crs(pcod_sf_proj)$units

plot(st_geometry(pcod_sf_proj))
## -----------------------------------------------------------------------------
pcod_sf_proj = st_transform(pcod_sf_proj,
                            gsub("units=m","units=km",
                                 st_crs(pcod_sf_proj)$proj4string)) 
st_crs(pcod_sf_proj)$units


## -----------------------------------------------------------------------------
pcod_sf = st_transform(pcod_sf,
                       crs = "+proj=utm +zone=9 +datum=WGS84 +no_defs +type=crs +units=km" )
st_crs(pcod_sf)$units



## -----------------------------------------------------------------------------

# add coast line from rnathure


ggplot()+
  geom_sf(data=pcod_sf %>% 
          filter(year == 2017),aes(color=factor(present))) +
   scale_color_manual(name="Occupancy status for the Pacific Cod",
                     values = c("green","tomato"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" )+
  theme_bw()
 
## -----------------------------------------------------------------------------
#| message: false
#| warning: false

library(terra)
depth_r <- rast(qcs_grid, type = "xyz")
depth_r


## -----------------------------------------------------------------------------
crs(depth_r) <- crs(pcod_sf)

ggplot()+ 
  geom_spatraster(data=depth_r$depth)+
  geom_sf(data=pcod_sf,aes(color=factor(present))) +
  facet_wrap(~year)+
    scale_color_manual(name="Occupancy status for the Pacific Cod",
                     values = c("black","orange"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" )




## ----child="practicals/spatial_data_types_points.qmd"-------------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# For plotting
library(ggplot2)
library(scico) # for colouring palettes

# Data manipulation
library(dplyr)




## -----------------------------------------------------------------------------
library(sf)
library(here)
shp_SGC <- st_read(
  here(
    "workshop_materials",
    "copenhagen_ices",
    "datasets",
    "SG_CairngormsNationalPark",
    "SG_CairngormsNationalPark_2010.shp"
  ),
  quiet = TRUE
)
## -----------------------------------------------------------------------------
shp_SGC <- shp_SGC %>% st_transform(crs = 27700)
st_crs(shp_SGC)$units

plot(st_geometry(shp_SGC))
## -----------------------------------------------------------------------------
shp_SGC <- st_transform(shp_SGC,
                        gsub("units=m","units=km",
                             st_crs(shp_SGC)$proj4string)) 
st_crs(shp_SGC)$units



ggplot()+
  geom_sf(data=shp_SGC)+
  theme_bw()



## -----------------------------------------------------------------------------
ringlett <- read.csv(here(
  "workshop_materials",
  "copenhagen_ices",
  "datasets",
  "bnm_ringlett.csv"))
head(ringlett)


## -----------------------------------------------------------------------------
ringlett_sf <- ringlett %>% 
  st_as_sf(coords = c("x","y"),
           crs = "+proj=longlat +datum=WGS84") 
shp_SGC_2 <- st_transform(shp_SGC, crs = "+proj=longlat +datum=WGS84")

st_crs(ringlett_sf)
st_crs(shp_SGC)



## -----------------------------------------------------------------------------
ringlett_CNP <- ringlett_sf[shp_SGC_2,] # crop to mainland


## -----------------------------------------------------------------------------
ggplot()+
  geom_sf(data=shp_SGC)+
  geom_sf(data=ringlett_CNP)


## -----------------------------------------------------------------------------
library(terra)
elevation_r <- rast(here(
  "workshop_materials",
  "copenhagen_ices",
  "datasets","Scotland_elev.tiff"))
crs(elevation_r) = crs(shp_SGC)
plot(elevation_r)

ggplot()+
  geom_spatraster(data=elevation_r)+
  scale_fill_scico(name = "Elevation",
                   palette = "glasgow",
                   na.value = "transparent" )+
  theme_bw()


## -----------------------------------------------------------------------------
elevation_r <- elevation_r %>% scale()


## -----------------------------------------------------------------------------
elev_CNP <- terra::crop(elevation_r,shp_SGC_2,mask=T)
plot(elev_CNP)

ggplot()+
  geom_spatraster(data=elev_CNP)+
  geom_sf(data=shp_SGC,fill=NA,color="black")+
  # añade los datos de ringlett
  geom_sf(data=ringlett_CNP, color= "black", fill="white",
          shape=21)+
  scale_fill_scico(name = "Elevation (scaled)",
                   palette = "nuuk",
                   na.value = "transparent" )+
  theme_bw()



