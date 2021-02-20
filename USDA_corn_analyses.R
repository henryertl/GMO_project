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
  axis.title = element_text(size = 15),
  strip.text = element_text(size = 15),
  legend.text= element_text(size = 15),
  legend.title = element_text(size = 10),
  plot.title = element_text(size = 15, face = "bold")

)
}


## setwd
setwd("/Users/wittkopp_member/Code")

# read in spreadsheet
df <- read.delim("./GMO_project/Data_tables/Relevant_combined_corn.txt", header = T)
df$organic_acres_planted <- df$organic_acres_planted %>% as.numeric()

df[is.na(df)] <- 0

# plot
Z <- ggplot(df, aes(x=Year)) +
  geom_point(aes(y=YIELD_BU_ACRE, color=total_GMO_planted)) +
  geom_smooth(aes(y=YIELD_BU_ACRE),method="loess", color = "black") +
  scale_color_gradient(low="blue", high="red", "% GMO Corn planted") +
  xlab("Year") +
  ylab("USA Corn yield (Bushels/Acre)") +
  theme_main()

ggsave(Z, file = "./GMO_project/Figures/Yield_by_year_percGMO.pdf")
  



df_1865[df_1865$Year > 1998,] %>%
ggplot() +
geom_line(aes(x=Year,y=GE_planted_insect), color = "blue") +
geom_line(aes(x=Year,y=GE_planted_herb), color = "red") +
geom_line(aes(x=Year, y=GE_planted_stacked), color = "green") +
geom_point(aes(x=Year,y=GE_planted_insect), color = "blue") +
geom_point(aes(x=Year,y=GE_planted_herb), color = "red") +
geom_point(aes(x=Year, y=GE_planted_stacked), color = "green")
