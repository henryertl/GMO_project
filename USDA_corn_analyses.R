### downloaded excel spreadsheets from USDA
# https://quickstats.nass.usda.gov : census > crops > field crops > corn > corn,grain yield
# https://www.ers.usda.gov/data-products/adoption-of-genetically-engineered-crops-in-the-us/ > download dataset
## combined these two onto one spreadsheet and calculated sum of different types in excel ##
# goal: plot to visualize

# load libraries
library(ggplot2)
library(tidyverse)

theme_main <- function() {
  theme_bw() +
    theme(
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 30),
      strip.text = element_text(size = 30),
      legend.text= element_text(size = 20),
      legend.title = element_text(size = 30),
      plot.title = element_text(size = 25, face = "bold")
    )
}


# read in spreadsheet
df <- read.csv("/Users/henryertl/Documents/Devs/GMO_project/Data_tables/US_corn_yields_GMO_annual.csv", header = T)
colnames(df) <- c("Year", "Data_type", "Crop", "Yield_bush_acre", "GE_planted_insect", "GE_planted_herb", "GE_planted_stacked","GE_planted_total")

# subset year
df_1865 <- df[df$Year > 1865,]
# replace NAs with 0s
df_1865[is.na(df_1865)] <- 0

# plot
ggplot(df_1865, aes(x=Year)) +
  geom_point(aes(y=Yield_bush_acre)) +
  geom_smooth(aes(y=Yield_bush_acre),method="loess", color = "black") +
  xlab("Year") +
  ylab("Yield (Bushels/Acre)")


B <- df_1865[df_1865$Year > 1965,] %>%
ggplot(aes(x=Year)) +
  geom_point(aes(y=Yield_bush_acre, color=GE_planted_total)) +
  geom_smooth(aes(y=Yield_bush_acre),method="lm", color = "black") +
  #scale_color_gradient(low="blue", high="red", "% GMO Corn planted") +
  xlab("Year") +
  ylab("Yield (Bushels/Acre)")



df_1865[df_1865$Year > 1998,] %>%
ggplot() +
geom_line(aes(x=Year,y=GE_planted_insect), color = "blue") +
geom_line(aes(x=Year,y=GE_planted_herb), color = "red") +
geom_line(aes(x=Year, y=GE_planted_stacked), color = "green") +
geom_point(aes(x=Year,y=GE_planted_insect), color = "blue") +
geom_point(aes(x=Year,y=GE_planted_herb), color = "red") +
geom_point(aes(x=Year, y=GE_planted_stacked), color = "green")




#################

df <- read.delim("/Users/henryertl/Documents/Devs/GMO_project/Data_tables/Relevant_combined_corn.txt", header = T)
head(df)



# plot
ggplot(df) +
  geom_point(aes(x=Year, y=N.fertilizer.applied), color = "red") +
  geom_line(aes(x=Year, y=N.fertilizer.applied), color = "red") +
  geom_point(aes(x=Year, y=YIELD..BU...ACRE.), color = "blue") +
  geom_line(aes(x=Year, y=YIELD..BU...ACRE.), color = "blue")


  geom_smooth(aes(y=Yield_bush_acre),method="loess", color = "black") +
  xlab("Year") +
  ylab("Yield (Bushels/Acre)")
