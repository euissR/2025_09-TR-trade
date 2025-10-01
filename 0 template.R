# packages ----------------------------------------------------------------

source("0 data.R")
# needs()

# data --------------------------------------------------------------------

abc <- read_excel("./data/.xlsx")  %>% 
  print()

# plot --------------------------------------------------------------------
labs(title = "",
     subtitle = "",
     caption = "Data: ")
ggsave_euiss("./img/abc.pdf",
             publication = "",
             w = "twocol",
             h = 1)

# data --------------------------------------------------------------------

raster <- raster("D:/data/raster/") %>%
  print()

# basemap --------------------------------------------------------------------

shp_countries <- euiss_gisco()
shp_borders <- euiss_gisco(shp = "BN")
shp_coast <- euiss_gisco(shp = "BN", bordertype = "COASTL")

# GISCO borders -----------------------------------------------------------

geom_sf(
  data = shp_borders,
  fill = NA,
  col = col_axis,
  linewidth = lwd_grid / 2,
  aes(linetype = POL_STAT == 0)
) +
  scale_linetype_manual(values = c(2, 1), guide = "none") +
  

# map projections ---------------------------------------------------------

# library(rgeos)
# set_do_poly_check(FALSE)

# equal earth projection
crs <- "+proj=eqearth +wktext"

coord_sf(
  crs = crs,
  xlim = c(-15000000, 17000000),
  ylim = c(-6500000, NA),
  expand = 0,
  datum = NA
) +
  
  # orthographic
  coord_sf(crs = "+proj=ortho +lon_0=10 +lat_0=50")

# ESRI:53032, Azimuthal Equidistant
coord_sf(crs = "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs")

# EU map
crs = 3035
# same as European-centric ETRS89 Lambert Azimuthal Equal-Area projection
coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
coord_sf(
  crs = 3035,
  xlim = c(2500000, 7000000),
  ylim = c(1000000, 5500000),
  datum = NA
)

# robinson
crs = "+proj=robin"
coord_sf(crs = crs, datum = NA) +
  coord_sf(crs = "+proj=robin", datum = NA)

coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs", datum = NA) +
  # without antarctica
  coord_sf(
    crs = "+proj=robin",
    datum = NA,
    ylim = c(-5900000, 9000000),
    expand = FALSE
  )

# shapes ------------------------------------------------------------------

# disputed borders (gisco)
geom_sf(
  data = euiss_gisco(shp = "BN"),
  aes(linetype = ifelse(POL_STAT != 0, "recognized", "disputed")),
  col = col_grid,
  stroke = lwd_grid / 2,
  show.legend = FALSE
)


# coastline
geom_sf(
  data = coast,
  size = lwd_grid,
  col = col_grid,
  fill = NA
)

# simplifying GADM ---------------------------------------------------------

fig14_region_gadm <- c()
foreach(i = 1:length(fig14_region$ISO3)) %do% {
  current <- getData("GADM",
                     country = fig14_region$ISO3[i],
                     level = 1,
                     path = "./data/shp/") %>%
    st_as_sf()
  
  fig14_region_gadm <- if (i == 1) {
    current
  } else {
    rbind(fig14_region_gadm, current)
  }
  
}

# giscoR ------------------------------------------------------------------
# params epsg, resolution (03, 10, 20, 60), region, country

coast <- gisco_coastallines %>%
  st_crop(
    gisco_countries %>%
      left_join(giscoR::gisco_countrycode %>%
                  select(ISO3_CODE, continent)) %>%
      filter(continent == "Africa")
  )

continent <- gisco_countries %>%
  left_join(giscoR::gisco_countrycode %>%
              select(ISO3_CODE, continent)) %>%
  filter(continent == "Africa")

# nedownload --------------------------------------------------------------

coast <- ne_coastline(scale = 110, returnclass = "sf")

countries <-
  ne_countries(scale = 110,
               country = data$country,
               returnclass = "sf") %>%
  select(admin) %>%
  print()

bbox <-
  ne_download(
    scale = 110,
    type = "wgs84_bounding_box",
    category  = "physical",
    returnclass = "sf"
  )