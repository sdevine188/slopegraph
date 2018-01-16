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
data <- read_csv("uscis_slopegraph_data.csv")
data


#################################################


# create variable for difference in millions
# need to pivot data to wide format to get difference, then pivot back to long for ggplot
data <- data %>% mutate(difference = `FY 2016` - `FY 2012`) %>%
        gather(key = Year, value = value, c(`FY 2016`, `FY 2012`), -c(Race, difference)) %>%
        data.frame()

data


####################################################


# since hawaiian race has NA for FY 2012, it has NA for difference
# this makes fct_relevel classify it as the last factor, which should be the highest difference
# so manually make difference zero, so it will be color-coded as lowest difference

data <- data %>% mutate(difference = case_when(Race == "Native Hawaiian / Pacific Islander" ~ 0, 
                                               TRUE ~ difference))
data


########################################################3


# create labels for dot icons on slopegraph
data <- data %>% mutate(label_left = str_c(Race, ": ", value, "%", sep = ""),
                        label_right = str_c(value, "%", sep = ""))

data

# update label_right for hawaii, so that Race is print on label_right, since there is no label_left for hawaii

# wrapping text
# data <- data %>%
#         mutate(label_right = case_when(Race == "Native Hawaiian / Pacific Islander" ~
#                                                str_c("Native Hawaiian /\n Pacific Islander", " ", value, "%", sep = ""), TRUE ~ label_right))
# data

# not wrapping text
data <- data %>%
        mutate(label_right = case_when(Race == "Native Hawaiian / Pacific Islander" ~
                                               str_c("Native Hawaiian / Pacific Islander:", " ", value, "%", sep = ""), TRUE ~ label_right))
data

# create race group to adjust label_right
adjust_label_right_race_group = c("Asian", "Two or more races", "American Indian / Alaska Native", 
                                  "Native Hawaiian / Pacific Islander")


#############################################################


# use fct_relevel to assign new levels to location based on difference, for use when coloring
# https://blog.rstudio.com/2016/08/31/forcats-0-1-0/
levels(factor(data$Race))
data %>% arrange(desc(difference)) %>% distinct(Race, difference)
data <- data %>% mutate(Race_relevel = fct_reorder(factor(Race), difference))
levels(factor(data$Race_relevel))


##################################################################

# create slopegraph
uscis_slopegraph <- ggplot(data) + 
        geom_line(aes(x = as.factor(Year), y = value, group = Race_relevel, 
                      color = Race_relevel), size = 2) + 
        geom_point(aes(x = as.factor(Year), y = value, color = Race_relevel), size = 5) + 
        theme_minimal(base_size = 18) +
        scale_color_viridis(discrete = TRUE) + 
        xlab("") + 
        geom_text(data = data %>% filter(Year == "FY 2012"), aes(x = as.factor(Year), y = value, label = label_left),
                  size = 6, hjust = 1, position = position_nudge(x = -.03)) +
        geom_text(data = data %>% filter(Year == "FY 2016", !(Race %in% adjust_label_right_race_group)), 
                  aes(x = as.factor(Year), y = value, label = label_right), 
                  size = 6, hjust = 0, position = position_nudge(x = .03)) +
        geom_text(data = data %>% filter(Year == "FY 2016", Race == "Asian"),
                  aes(x = as.factor(Year), y = value, label = label_right),
                  size = 6, hjust = 0, position = position_nudge(x = .03, y = -1.25)) +
        geom_text(data = data %>% filter(Year == "FY 2016", Race == "Two or more races"),
                  aes(x = as.factor(Year), y = value, label = label_right),
                  size = 6, hjust = 0, position = position_nudge(x = .03)) +
        geom_text(data = data %>% filter(Year == "FY 2016", Race == "American Indian / Alaska Native"),
                  aes(x = as.factor(Year), y = value, label = label_right), 
                  size = 6, hjust = 0, position = position_nudge(x = .03, y = 2)) +
        geom_text(data = data %>% filter(Year == "FY 2016", Race == "Native Hawaiian / Pacific Islander"),
                  aes(x = as.factor(Year), y = value, label = label_right),
                  size = 6, hjust = 0, position = position_nudge(x = .03)) +
        theme(legend.position = "none", 
              plot.background = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank(), 
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(),
              axis.text.x  = element_text(size = 18, color = "black"),
              plot.title = element_text(size = 20, hjust = .5, face = "bold")) + 
        ggtitle("Change in race of USCIS employees, FY 2012-FY 2016")

uscis_slopegraph


##############################################################


# save output
ggsave(filename = "uscis_slopegraph.pdf", plot = uscis_slopegraph, width = 17, height = 10, 
       units = "in", dpi = 900)


##############################################################
#############################################################
#################################################################


# version with legend using abbreviations for labels




#####################################################################
####################################################################
#####################################################################


# version two wrapping label text
# data2 <- data %>% mutate(label_left = str_c(Race, " ", value, "%", sep = ""),
#                         label_right = str_c(value, "%", sep = ""))
# 
# data2
# 
# data2 <- data2 %>%
#         mutate(label_right = case_when(Race == "Native Hawaiian / Pacific Islander" ~
#                 str_c("Native Hawaiian /\nPacific Islander", " ", value, "%", sep = ""), TRUE ~ label_right))
# 
# data2 <- data2 %>%
#         mutate(label_left = case_when(Race == "American Indian / Alaska Native" ~
#                         str_c("American Indian /\n Alaska Native", " ", value, "%", sep = ""),
#                 Race == "Black / African American" ~
#                         str_c("Black /\n African American", " ", value, "%", sep = ""),
#                 TRUE ~ label_left))
# data2
# 
# uscis_slopegraph <- ggplot(data2) +
#         geom_line(aes(x = as.factor(Year), y = value, group = Race_relevel,
#                       color = Race_relevel), size = 2) +
#         geom_point(aes(x = as.factor(Year), y = value, color = Race_relevel), size = 5) +
#         theme_minimal(base_size = 18) +
#         scale_color_viridis(discrete = TRUE) +
#         xlab("") +
#         geom_text(data = data2 %>% filter(Year == "FY 2012", 
#                 !(Race %in% c("American Indian / Alaska Native", "Black / African American", "Hispanic"))), 
#                 aes(x = as.factor(Year), y = value, label = label_left),
#                   size = 6, hjust = 1.1) +
#         geom_text(data = data2 %>% 
#                           filter(Year == "FY 2012", Race == "Hispanic"), 
#                   aes(x = as.factor(Year), y = value, label = label_left),
#                   size = 6, hjust = 0.5, position = position_nudge(x = -0.2, y = -1.3)) +
#         geom_text(data = data2 %>% 
#                           filter(Year == "FY 2012", Race == "American Indian / Alaska Native"), 
#                   aes(x = as.factor(Year), y = value, label = label_left),
#                   size = 6, hjust = 0.5, position = position_nudge(x = -0.25)) +
#         geom_text(data = data2 %>% 
#                           filter(Year == "FY 2012", Race == "Black / African American"), 
#                   aes(x = as.factor(Year), y = value, label = label_left),
#                   size = 6, hjust = 0.5, position = position_nudge(x = -0.25)) +
#         geom_text(data = data2 %>% filter(Year == "FY 2016", !(Race %in% adjust_label_right_race_group)),
#                   aes(x = as.factor(Year), y = value,
#                       label = label_right),
#                   size = 6, hjust = -0.25) +
#         geom_text(data = data2 %>% filter(Year == "FY 2016", Race == "Asian"),
#                   aes(x = as.factor(Year), y = value,
#                       label = label_right),
#                   size = 6, hjust = -0.25, vjust = 1.5) +
#         geom_text(data = data2 %>% filter(Year == "FY 2016", Race == "Two or more races"),
#                   aes(x = as.factor(Year), y = value,
#                       label = label_right),
#                   size = 6, hjust = -0.25) +
#         geom_text(data = data2 %>% filter(Year == "FY 2016", Race == "American Indian / Alaska Native"),
#                   aes(x = as.factor(Year), y = value,
#                       label = label_right),
#                   size = 6, hjust = -0.25, vjust = -1.25) +
#         geom_text(data = data2 %>% filter(Year == "FY 2016", Race == "Native Hawaiian / Pacific Islander"),
#                   aes(x = as.factor(Year), y = value,
#                       label = label_right),
#                   size = 6, hjust = -0.07, vjust = .25) +
#         theme(legend.position = "none",
#               plot.background = element_blank(),
#               panel.grid.major.y = element_blank(),
#               panel.grid.minor.y = element_blank(),
#               panel.grid.major.x = element_blank(),
#               axis.ticks.y = element_blank(),
#               axis.ticks.x = element_blank(),
#               axis.title.y = element_blank(),
#               axis.text.y = element_text(lineheight = 0.25, size = 12),
#               axis.text.x = element_text(size = 15, color = "black"),
#               plot.title = element_text(size = 18, hjust = .5, face = "bold")) +
#         ggtitle("Change in race of USCIS employees, FY 2012-FY 2016")
# 
# uscis_slopegraph
# 
# 
