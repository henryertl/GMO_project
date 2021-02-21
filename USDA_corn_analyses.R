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

setwd("/Users/henryertl/Documents/Devs")

# read in spreadsheet
df <- read.delim("./GMO_project/Data_tables/Relevant_combined_corn.txt", header = T)
df$organic_acres_planted <- df$organic_acres_planted %>% as.numeric()

# plot main graph
df_main <- df[is.na(df)] <- 0

Z <- df_main %>%
ggplot(aes(x=Year)) +
  geom_point(aes(y=YIELD_BU_ACRE, color=total_GMO_planted)) +
  geom_smooth(aes(y=YIELD_BU_ACRE),method="loess", color = "black") +
  scale_color_gradient(low="blue", high="red", "% GMO Corn planted") +
  xlab("Year") +
  ylab("USA Corn yield (Bushels/Acre)") +
  theme_main()

ggsave(Z, file = "./GMO_project/Figures/Yield_by_year_percGMO.pdf")

# look at chemical applications
## fertilizer
A <- df[df$Year > 1960,] %>%
ggplot(aes(x=Year)) +
geom_point(aes(y=N_fertilizer_applied)) +
geom_smooth(aes(y=N_fertilizer_applied), method="loess", color="black") +
theme_main() +
ylab("Nitrogen fertilizer applied (lb)")

## pesticides
B <- df[df$Year > 1960 & df$Year < 2020,] %>%
ggplot(aes(x=Year)) +
geom_point(aes(y=pesticides_used_perc_of_total), color = "dodgerblue4") +
geom_smooth(aes(y=pesticides_used_perc_of_total), method="loess", color="dodgerblue4") +
theme_main() +
geom_point(aes(y=(perc_GE_Bt_insecticide_only + perc_stacked)), color = "firebrick4") +
geom_smooth(aes(y=(perc_GE_Bt_insecticide_only + perc_stacked)), method = "loess", color="firebrick4") +
scale_y_continuous(name = "Chemical insecticides used (% of total acres planted)",
    sec.axis = sec_axis( trans=~., name="GMO insecticides used (% of total acres planted)")) +
theme(axis.title.y = element_text(color = "dodgerblue4", size=14, vjust=1.5),
    axis.text.y = element_text(size = 13, color = "dodgerblue4"),
    axis.title.y.right = element_text(color = "firebrick4", size=14, vjust=1.5),
    axis.text.y.right = element_text(size = 13, color = "firebrick4"),
    plot.title = element_text(hjust = 0.5)) +
ggtitle("Chemical vs GMO insecticide usage")
  ggsave(B, file = "./GMO_project/Figures/Chemical_GMO_pesticide_usage.pdf")


## herbicides
C <- df[df$Year > 1960 & df$Year < 2010,] %>%
ggplot(aes(x=Year)) +
geom_point(aes(y=herbicides_used_perc_of_total)) +
geom_smooth(aes(y=herbicides_used_perc_of_total), method="loess", color="black") +
theme_main() +
ylab("Herbicide used (% of total acres planted)")

D <- df[df$Year > 1995 & df$Year < 2010,] %>%
ggplot(aes(x=Year)) +
geom_point(aes(y=lb_Glyph_Acre)) +
geom_smooth(aes(y=lb_Glyph_Acre), method="loess", color="black")  +
geom_point(aes(y=lb_other_herb_acre), color = "red") +
geom_smooth(aes(y=lb_other_herb_acre), method="loess", color="red") +
theme_main() +
scale_y_continuous(name = "Glycophate used (lb)",
    sec.axis = sec_axis( trans=~., name="Other herbicice used (not glycophate))")) +
theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13),
    axis.text.y.right = element_text(size = 15, color = "red")
  )

E <- plot_grid(C, D)


# organic
G <- df[df$Year > 2004 & df$Year < 2012,] %>%
ggplot(aes(x=Year)) +
geom_point(aes(y=perc_organic_of_total)) +
geom_smooth(aes(y=perc_organic_of_total), method="lm", color="black") +
theme_main()
