### downloaded excel spreadsheets from USDA
# https://quickstats.nass.usda.gov : census > crops > field crops > corn > corn,grain yield
# https://www.ers.usda.gov/data-products/adoption-of-genetically-engineered-crops-in-the-us/ > download dataset
## combined these two onto one spreadsheet and calculated sum of different types in excel ##
# goal: plot to visualize

# load libraries
library(ggplot2)
library(tidyverse)

# read in spreadsheet
df <- read.csv("/Users/henryertl/Documents/General/US_corn_yields_GMO_annual.csv", header = T)
colnames(df) <- c("Year", "Data_type", "Crop", "Yield_bush_acre", "GE_planted_insect", "GE_planted_herb", "GE_planted_stacked","GE_planted_total")

# subset year
df_1865 <- subset(df, df$Year > 1865)
# replace NAs with 0s
df_1865[is.na(df_1865)] <- 0

# plot
ggplot(data = df_1865, aes(x=Year)) +
  geom_point(aes(y=Yield_bush_acre, color=GE_planted_total)) +
  geom_smooth(aes(y=Yield_bush_acre),method="loess", color = "black") +
  scale_color_gradient(low="blue", high="red", "% GMO Corn planted") +
  xlab("Year") +
  ylab("Yield (Bushels/Acre)")




#################





#########################
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

twobytwo <- matrix(ncol = 3, nrow = 4) %>% as.data.frame()
twobytwo[,1] <- c(rep("Conserved Grh motif", 2), rep("Variable Grh motif", 2))
twobytwo[,2] <- c(52,90,48,10)
twobytwo[,3] <- c("Diverged accessibility","Conserved accessibility","Diverged accessibility","Conserved accessibility")
colnames(twobytwo) <- c("Motif_Type", "Percentage", "Region_status")

A <- ggplot(data = twobytwo, aes(x=Region_status, y=Percentage, fill=Motif_Type)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") +
  theme_main()

ggsave(A,file="/Users/henryertl/Documents/Wittkopp_lab/AS_ATACseq/Figures/percent_variable_grh.pdf", width = 15, height = 15)



