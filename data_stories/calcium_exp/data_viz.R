library(readr)
library(dplyr)

w1.1996 <- read_csv("w1_1996veg.txt")
w1.2001 <- read_csv("w1_2001veg.txt")
w1.2006 <- read_csv("w1_2006veg.txt")
w1.2011 <- read_csv("w1_2011veg.txt")

w6.1997 <- read_csv("w6_1997veg.txt")
w6.2002 <- read_csv("w6_2002veg.txt")
w6.2007 <- read_csv("w6_2007veg.csv")
w6.2012 <- read_csv("w6_2012veg.csv")

species.count = data.frame(ws = c("Watershed 1",
                                  "Watershed 1",
                                  "Watershed 1",
                                  "Watershed 1",
                                  "Watershed 6",
                                  "Watershed 6",
                                  "Watershed 6",
                                  "Watershed 6"),
                           sc = c(length(unique(w1.1996$Species)),
                                  length(unique(w1.2001$Species)),
                                  length(unique(w1.2006$Species)),
                                  length(unique(w1.2011$Species)),
                                  length(unique(w6.1997$Species)),
                                  length(unique(w6.2002$Species)),
                                  length(unique(w6.2007$Species)),
                                  length(unique(w6.2012$Species))),
                           date = c(1996, 2001, 2006, 2011, 1997,
                                    2002, 2007, 2012))
count.species = function(df, species){
  num = df %>%
    filter(Species == species) %>%
    summarize(count = n())
  return(c(species, num$count))
}

species.div = sapply(unique(w1.1996$Species), count.species, df = w1.1996)
species.div = t(species.div)
species.div = as.data.frame(species.div)
colnames(species.div) = c("Species", "Count")
species.div$Count = as.numeric(species.div$Count)
diversity.1996 = (sum((species.div$Count)*((species.div$Count) - 1))/
                    (sum(species.div$Count)*(sum(species.div$Count) - 1)))
