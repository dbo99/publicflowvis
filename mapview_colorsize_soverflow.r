library(mapview)
library(sf)


lat <- rep(34,16)
lon <- seq(-128, -126, length=16)
set.seed(1)
value <- c(-1000, -800, -600, -400, -200, -100, -50, -25, 
           25, 50, 100, 200, 400, 600, 800, 1000)


df <- data.frame(lat,lon, value) %>% select(-lat)
df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) %>% mutate(value_abs = abs(value)) 

pal <-  mapviewPalette("mapviewSpectralColors")
m <-   mapview(df["value"], 
             cex = df$value_abs/100,
             legend = TRUE,
             #col.regions = pal(100),
            col.regions = palfunc, at = at,
             layer.name = "value") 

m


  library(tidyverse)
library(sf)
library(mapview)

palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- RColorBrewer::brewer.pal(11, "RdBu")
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}


foo <- franconia %>% mutate(foo = rnorm(n()) + 2)

max_val = max(abs(foo$foo), na.rm = T)
n_val = max( length(unique(keep(foo$foo, ~.x > 0))),
             length(unique(keep(foo$foo, ~.x < 0))))
at = lattice::do.breaks(endpoints = c(-max_val, max_val), nint = 2 * n_val + 1)
p <- mapView(foo, zcol = 'foo', layer.name = "Example", col.regions = palfunc, at = at)
p


library(tidyverse)
library(mapview)
library(sf)

df <- data.frame(lat=rep(34,16), 
                 lon=seq(-128, -126, length = 16), 
                 value=c(-1000, -800, -600, -400, -200, -100, -50, 
                         -25, 25, 50, 100, 200, 400, 600, 800, 1000)) 

df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(value_abs = abs(value))

pal <-  mapviewPalette("mapviewSpectralColors")

mapview(df["value"], 
        cex = df$value_abs/100, 
        legend = TRUE,
        col.regions = pal(100), 
        layer.name = "value") 
