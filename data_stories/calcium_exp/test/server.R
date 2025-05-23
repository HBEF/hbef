library(readr)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(maptools)
library(plotly)

#Read in and clean grid coordinates for watersheds
w1_coords <- read.csv("w1_coords.csv")
w6_coords <- read.csv("w6_coords_revised.csv")
coordinates <- w6_coords[,c("x", "y")]
coordinates$y <- coordinates$y*(-1)
coordinates$x <- coordinates$x - 5
coordinates_w1 <- w1_coords[, c("x", "y")]
coordinates_w1$x <- coordinates_w1$x -10
coordinates_w1$y <- coordinates_w1$y - 34
coordsdf <- rbind(coordinates, coordinates_w1)
poly = c()
for (row in 1:nrow(coordsdf)){
  points <- rbind(c(coordsdf[[row, 1]], coordsdf[[row, 2]]),
                  c(coordsdf[[row, 1]] + 1, coordsdf[[row, 2]]),
                  c(coordsdf[[row, 1]] + 1, coordsdf[[row, 2]] - 1),
                  c(coordsdf[[row, 1]], coordsdf[[row, 2]] - 1))
  poly = c(poly, Polygons(list(Polygon(points)), paste("plot", row, sep = "")))
}
map <- SpatialPolygons(poly)
#Read in yearly data
w1_2006 <- read_csv("w1_2006veg.txt")
w6_2007 <- read.csv("w6_2007veg.csv")
w1_2001 <- read_csv("w1_2001veg.txt")
w6_2002 <- read_csv("w6_2002veg.txt")
w1_1996 <- read_csv("w1_1996veg.txt")
w6_1997 <- read_csv("w6_1997veg.txt")
w1_2011 <- read_csv("w1_2011veg.txt")
w6_2012 <- read.csv("w6_2012veg.csv")
#Create extra rows to fill in NAs for plots that
#aren't listed.
extra_row_2001 <-c(2001, 1, 144, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA,
                   NA, NA, NA) 
extra_row_2006 = c(2006, 1, 144, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA,
                   NA, NA, NA)
extra_row_1996 = c(1996, 1, 144, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA,
                   NA, NA, NA)
extra_row_2011_1 <- c(2011, 1, 131, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_2 <- c(2011, 1, 133, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_3 <- c(2011, 1, 137, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_4 <- c(2011, 1, 138, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_5 <- c(2011, 1, 139, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_6 <- c(2011, 1, 140, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_7 = c(2011, 1, 143, NA, NA, NA,
                     NA, NA, NA, NA, NA, NA,
                     NA, NA, NA)
extra_row_2011_8 = c(2011, 1, 144, NA, NA, NA,
                     NA, NA, NA, NA, NA, NA,
                     NA, NA, NA)
extra_row_2011_9 = c(2011, 1, 145, NA, NA, NA,
                     NA, NA, NA, NA, NA, NA,
                     NA, NA, NA)
extra_row_2011_10 = c(2011, 1, 146, NA, NA, NA,
                      NA, NA, NA, NA, NA, NA,
                      NA, NA, NA)
extra_row_2011_11 <- c(2011, 1, 149, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA,
                       NA, NA, NA)
extra_row_2011_12 <- c(2011, 1, 150, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA,
                       NA, NA, NA)
extra_row_2011_13 <- c(2011, 1, 151, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA,
                       NA, NA, NA)
w1_2006 <- rbind(w1_2006, extra_row_2006)
w1_2001 <- rbind(w1_2001, extra_row_2001)
w1_1996 <- rbind(w1_1996, extra_row_1996)
w1_2011 <- rbind(w1_2011, extra_row_2011_1,
                 extra_row_2011_2,
                 extra_row_2011_3,
                 extra_row_2011_4,
                 extra_row_2011_5,
                 extra_row_2011_6,
                 extra_row_2011_7,
                 extra_row_2011_8,
                 extra_row_2011_9,
                 extra_row_2011_10,
                 extra_row_2011_11,
                 extra_row_2011_12,
                 extra_row_2011_13)

#Function to ind the sum of the below ground and above ground
#biomass per meter squared for each plot per year
biomass <- function(df, num.plots, tib){
  totals = rep(NA, num.plots)
  for (p in 1:num.plots){
    if (tib == TRUE){
      abv.bmss <- df[df$Plot == p, "AbvBmss"]/625
      abv.bmss.df <- abv.bmss
      blw.bmss <- df[df$Plot == p, "BlwBmss"]/625
      blw.bmss.df <- blw.bmss
      if (nrow(abv.bmss) <= 0){
        abv.bmss.df <- 0
      }
      if (nrow(blw.bmss) <= 0){
        blw.bmss.df <- 0
      }
      totals[p] = sum(abv.bmss.df) + sum(blw.bmss.df)
    }else{
      abv.bmss <- df[df$Plot == p, "AbvBmss"]/625
      abv.bmss.df <- abv.bmss
      blw.bmss <- df[df$Plot == p, "BlwBmss"]/625
      blw.bmss.df <- blw.bmss
      if (length(abv.bmss) <= 0){
        abv.bmss.df <- 0
      }
      if (length(blw.bmss) <= 0){
        blw.bmss.df <- 0
      }
      totals[p] = sum(abv.bmss.df) + sum(blw.bmss.df)
    }
  }
  return(totals)
} 

biomass.sp <- function(df, num.plots, tib, sp){
  totals = rep(NA, num.plots)
  for (p in 1:num.plots){
    if (tib == TRUE){
      abv.bmss <- df[df$Plot == p & df$Species == sp, "AbvBmss"]/625
      abv.bmss.df <- abv.bmss
      blw.bmss <- df[df$Plot == p & df$Species == sp, "BlwBmss"]/625
      blw.bmss.df <- blw.bmss
      if (nrow(abv.bmss) <= 0){
        abv.bmss.df <- 0
      }
      if (nrow(blw.bmss) <= 0){
        blw.bmss.df <- 0
      }
      totals[p] = sum(abv.bmss.df) + sum(blw.bmss.df)
    }else{
      abv.bmss <- df[df$Plot == p & df$Species == sp, "AbvBmss"]/625
      abv.bmss.df <- abv.bmss
      blw.bmss <- df[df$Plot == p & df$Species == sp, "BlwBmss"]/625
      blw.bmss.df <- blw.bmss
      if (length(abv.bmss) <= 0){
        abv.bmss.df <- 0
      }
      if (length(blw.bmss) <= 0){
        blw.bmss.df <- 0
      }
      totals[p] = sum(abv.bmss.df) + sum(blw.bmss.df)
    }
  }
  return(totals)
}

cut.rename <- function(df, ws, plotnames){
  colnames(df) = c("year", "biomass1")
  df <- df %>%
    mutate(Biomass = cut(biomass1, breaks = c(0, 24, 32, 36, 40, 44,
                                              48, 5000)))
  df <- cbind(df, ws)
  colnames(df) <- c("Year", "Amount", "Biomass", "Watershed")
  rownames(df) <- plotnames
  return(df)
}

w6.biomass.2007 <- biomass(df = w6_2007, num.plots = 208, tib = FALSE)
w6.biomass.2002 <- biomass(df = w6_2002, num.plots = 208, tib = TRUE)
w6.biomass.1997 <- biomass(df = w6_1997, num.plots = 208, tib = TRUE)
w6.biomass.2012 <- biomass(df = w6_2012, num.plots = 208, tib = FALSE)
w1.biomass.2006 <- biomass(df = w1_2006, num.plots = 200, tib = TRUE)
w1.biomass.2001 <- biomass(df = w1_2001, num.plots = 200, tib = TRUE)
w1.biomass.1996 <- biomass(df = w1_1996, num.plots = 200, tib = TRUE)
w1.biomass.2011 <- biomass(df = w1_2011, num.plots = 200, tib = TRUE)



plotnames_w6_1997 <- paste("plot", 1:208, sep = "")
plotnames_w1_1996 <- paste("plot", 209:408, sep = "")
plotnames_w6_2002 <- paste("plot", 409:616, sep = "")
plotnames_w1_2001 <- paste("plot", 617:816, sep = "")
plotnames_w6_2007 <- paste("plot", 817:1024, sep = "")
plotnames_w1_2006 <- paste("plot", 1025:1224, sep = "")
plotnames_w6_2012 <- paste("plot", 1225:1432, sep = "")
plotnames_w1_2011 <- paste("plot", 1433:1632, sep = "")

#Creating vectors of years for the number of plots
w1.1996 <- rep(1996, 200)
w6.1996 <- rep(1996, 208)
w1.2001 <- rep(2001, 200)
w6.2001 <- rep(2001, 208)
w1.2006 <- rep(2006, 200)
w6.2006 <- rep(2006, 208)
w1.2011 <- rep(2011, 200)
w6.2011 <- rep(2011, 208)

#Creating data frames from the biomass and year data
df_w1_1996 <- data.frame(w1.1996, w1.biomass.1996, row.names = plotnames_w1_1996)
ws1 <- rep("Watershed 1", 200)
ws6 <- rep("Watershed 6", 208)
df_w1_1996 <- cut.rename(df = df_w1_1996, ws = ws1, 
                         plotnames = plotnames_w1_1996)
df_w6_1996 <- data.frame(w6.1996, w6.biomass.1997, row.names = plotnames_w6_1997)
df_w6_1996 <- cut.rename(df = df_w6_1996, ws = ws6,
                         plotnames = plotnames_w6_1997)
df_w1_2001 <- data.frame(w1.2001, w1.biomass.2001, row.names = plotnames_w1_2001)
df_w1_2001 <- cut.rename(df = df_w1_2001, ws = ws1,
                         plotnames = plotnames_w1_2001)
df_w6_2001 <- data.frame(w6.2001, w6.biomass.2002, row.names = plotnames_w6_2002)
df_w6_2001 <- cut.rename(df = df_w6_2001, ws = ws6,
                         plotnames = plotnames_w6_2002)
df_w1_2006 <- data.frame(w1.2006, w1.biomass.2006, row.names = plotnames_w1_1996)
df_w1_2006 <- cut.rename(df = df_w1_2006, ws = ws1,
                         plotnames = plotnames_w1_1996)
df_w6_2006 <- data.frame(w6.2006, w6.biomass.2007, row.names = plotnames_w6_1997)
df_w6_2006 <- cut.rename(df = df_w6_2006, ws = ws6,
                         plotnames = plotnames_w6_1997)
df_w1_2011 <- data.frame(w1.2011, w1.biomass.2011, row.names = plotnames_w1_2011)
df_w1_2011 <- cut.rename(df = df_w1_2011, ws = ws1,
                         plotnames = plotnames_w1_2011)
df_w6_2011 <- data.frame(w6.2011, w6.biomass.2012, row.names = plotnames_w6_2012)
df_w6_2011 <- cut.rename(df = df_w6_2011, ws = ws6,
                         plotnames = plotnames_w6_2012)
df = rbind(df_w6_2006, df_w1_2006)
mapdf <- SpatialPolygonsDataFrame(map, df)
proj4string(mapdf) <- CRS("+init=EPSG:4326")
#spplot(mapdf)
#grid.text("Watershed 6, 2002", x=unit(0.6, "npc"), y=unit(0.03, "npc"))
#grid.text("Watershed 1, 2001", x=unit(0.3, "npc"), y=unit(0.03, "npc"))
#mapdf@data$id <- rownames(mapdf@data)
#wsPoints <- fortify(mapdf, region = "id")
#wsDF <- merge(wsPoints, mapdf@data, by = "id")



shinyServer(function(input, output) {
  output$map.plot = renderPlot({
    spplot(mapdf, c("Biomass"), col.regions =  c("violet", "purple", 
                                   "blue", "green","orange", "yellow", "red"))
  })

})