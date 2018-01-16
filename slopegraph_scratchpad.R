library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(tidyr)
library(forcats)
library(viridis)

# https://rpubs.com/walkerke/slopegraph

# setwd
setwd("C:/Users/Stephen/Desktop/R/slopegraph")

# load data
list.files()
data <- read_csv("slopegraph_data.csv")
data

# create variable for difference in millions
# need to pivot data to wide format to get difference, then pivot back to long for ggplot
data <- data %>%
        spread(key = time, value = millions) %>%
        mutate(difference = `2015` - `2000`) %>%
        gather(key = time, value = value, c(`2000`, `2015`), -c(location, difference)) %>%
        gather(key = variable2, value = value2, c(difference, value)) %>%
        spread(key = variable2, value = value2) %>% rename(millions = value) %>%
        data.frame()

data

# create labels for dot icons on slopegraph
data <- data %>% mutate(label_left = str_c(location, " ", millions, " m", sep = ""), 
                                label_right = str_c(millions, " m", sep = ""))
data        

# use fct_relevel to assign new levels to location based on difference, for use when coloring
# https://blog.rstudio.com/2016/08/31/forcats-0-1-0/
str(factor(data$location))
data <- data %>% mutate(location_relevel = fct_reorder(factor(location), difference))
str(data$location_relevel)

# create slopegraph
ggplot(data) + 
        geom_line(aes(x = as.factor(time), y = millions, group = location_relevel, 
                      color = location_relevel), size = 2) + 
        geom_point(aes(x = as.factor(time), y = millions, color = location_relevel), size = 5) + 
        # theme_minimal(base_size = 18) + 
        scale_color_viridis(discrete = TRUE) + 
        xlab("") + 
        geom_text(data = data %>% filter(time == 2000, location != "south america"), 
                  aes(x = as.factor(time), y = millions, 
                color = location_relevel, label = label_left),
                size = 6, hjust = 1.1) +
        geom_text(data = data %>% filter(time == 2000, location == "south america"), 
                  aes(x = as.factor(time), y = millions, 
                color = location_relevel, label = label_left),
                  size = 6, hjust = 1.1, vjust = 1.5) +
        geom_text(data = data %>% filter(time == 2015), aes(x = as.factor(time), y = millions, 
                color = location_relevel, label = label_right),
                size = 6, hjust = -.25) +
        scale_y_continuous(trans = "log") +
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank(), 
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              plot.title = element_text(size = 18, hjust = .5, face = "bold")) + 
        ggtitle("Population change by region, 2000-2015")


# view color palettes
# display.brewer.all()


# code for adjusting label positions
# geom_text(data = subset(data, time == 2015 & location != "latin america"), 
#           aes(x = as.factor(time), y = millions, color = location, label = label15), 
#           size = 6, hjust = 1)