
# import necessary libraries
library(tidyverse)
library(ggplot2)
library(rio)

################################## PLOT 1 ##################################

# load in the washington school data
load(file=url("https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/eduwa.rda"))

# explore variable of choice
class(eduwa$Student.Teacher.Ratio)
# Student.Teacher.Ratio variable is numeric
summary(eduwa$Student.Teacher.Ratio)
# min = 0.1, Q1 = 16.2, median = 18.3, mean = 18.95, Q3 = 20.2, max = 330, NAs = 329

# modify data to remove highest 2 outliers and 329 NAs
eduwa_new <- filter(eduwa, Student.Teacher.Ratio<100)
nrow(eduwa_new)
# new data only has 2096 rows -- 331 were removed

# calculate S.T.R outlier information
# IQR = 20.2 - 16.2 = 4
# lower cutoff = -(1.5 * 4) + 16.2 = 10.2 
# higher cutoff = 1.5 * 4 + 20.2 = 26.2
sum(eduwa_new$Student.Teacher.Ratio >= 10.2 & eduwa_new$Student.Teacher.Ratio <= 26.2)
# 1913 schools have non-outlying S.T.R.s 
# 185 schools have outlying S.T.R.s
sum(eduwa_new$Student.Teacher.Ratio < 10.2)
# 83 of the outlying S.T.R.s are low outliers
sum(eduwa_new$Student.Teacher.Ratio > 26.2)
# 102 of the outlying S.T.R.s are high outliers
1913/2098
# 91.2% of the schools have S.T.R.s between 10.2 and 26.2

# calculate S.T.R. national comparison
# use numbers from OECD to find 2022 national average S.T.R. for K-12 schools
(13.57 + 13.73 + 14.81 + 15 + 15) / 5
sum(eduwa_new$Student.Teacher.Ratio <= 14.4)
280/2098
# only 13.3% of schools are less than or equal to the 2022 national average

# create plot
base <- ggplot(data=eduwa_new, aes(x=Student.Teacher.Ratio))
# use a boxplot
basebox <- base + geom_boxplot()
# add title, subtitle, and caption
titlescap <- basebox +
  labs(title = "91.2% of 2,098 K-12 Schools in Washington State Have Student Teacher Ratios Between 10.2 and 26.2", 
       subtitle = "Only 13.3% of the Schools' Ratios are Below the 2022 U.S. National Average of 14.4 for K-12 Schools",
       caption = "Not Included: 2 Schools with Ratios Over 100 (129 and 330) and 329 Schools with No Reported Ratio\nSource: U.S. Department of Education") +
  theme(plot.title = element_text(size=9),
        plot.subtitle = element_text(size=8))
# modify axes titles and values, remove gridlines
xyaxes <- titlescap + 
  scale_x_continuous(breaks = c(0.1, 10.2, 14.4, 16.2, 18.3, 20.2, 26.2, 93.8)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank())
# add annotations (vertical lines and corresponding text) for outliers and average
uniplot <- xyaxes + 
  geom_vline(xintercept = 10.2, color='red', linetype="dotted") +  
  annotate(geom = 'text', label = "83 Low\nOutliers", 
           x = 5, y = .05, angle = 0) +
  geom_vline(xintercept = 26.2, color='red', linetype="dotted") +
  annotate(geom = 'text', label = "102 High Outliers",
           x = 60, y = .05, angle = 0) +
  geom_vline(xintercept = 14.4, color='blue', linetype="dotted") +
  annotate(geom = 'text', label = "U.S. K-12 National Avg.",
           x = 13, y = .21, angle = 90)

# view final plot
uniplot

# save plot
saveRDS(uniplot, file="uniplot.rds")

################################## PLOT 2 ##################################

# load in MA state police arrests data
ma_arrests <- rio::import("https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx",
                          which = 1)

# explore variables of choice 
# Arrest Type
class(ma_arrests$`Arrest Type`)
# Arrest Type is a character variable with 4 unique values, so convert to factor
ma_arrests <- mutate(ma_arrests, ArrestType = factor(x=`Arrest Type`, 
                                                     levels = c('M', 'W', 'F', 'O'),
                                                     labels = c('Misdemeanor', 'Warrant',
                                                                'Felony', 'Other')))
table(ma_arrests$ArrestType)
# Misdemeanor     Warrant      Felony       Other 
#    5441          1706         1251         180 
# M = Misdemeanor (less serious crime)
# W = Warrant (arrest with permission of judge)
# F = Felony (more serious crime)
# O = Other
sum(is.na(ma_arrests$ArrestType))
# 44 NA values
ggplot(ma_arrests, aes(x=ArrestType)) + geom_bar()
# many more misdemeanors than any other type

# Age
class(ma_arrests$Age)
# Age is a numeric variable ranging from 15 to 74
table(ma_arrests$Age)
summary(ma_arrests$Age)
#   Min.  1st Qu.  Median  Mean   3rd Qu.   Max.    NA's 
#  15.00   27.00   33.00   34.89   41.00   74.00     5 
hist(ma_arrests$Age)
# majority of ages fall between 20 and 40

# Arrest Type and Age
# remove NA values in both variables of interest
ma_arrests <- filter(ma_arrests, !is.na(ma_arrests$ArrestType) & 
                       !is.na(ma_arrests$Age))
tapply(ma_arrests$Age, ma_arrests$ArrestType, summary)
# $Misdemeanor (n=5441)
#  Min.  1st Qu.  Median  Mean   3rd Qu.   Max. 
# 15.00   27.00   34.00   35.28   41.00   74.00 
# $Warrant (n=1706)
#  Min.  1st Qu.  Median  Mean   3rd Qu.   Max. 
# 17.00   27.00   33.00   34.94   41.00   74.00 
# $Felony (n=1251)
#  Min.  1st Qu.  Median  Mean   3rd Qu.   Max. 
# 15.00   25.00   30.00   32.71   39.00   71.00 
# $Other (n=180)
#  Min.  1st Qu.  Median  Mean   3rd Qu.   Max. 
# 19.00   28.00   35.50   37.68   46.00   74.00 
# at a glance, the distributions of age look similar for each category

# create plot
base <- ggplot(data = ma_arrests, aes(x=reorder(ArrestType, desc(ArrestType)), y=Age))
# use a side-by-side boxplot with flipped axes and minimal theme
basebox <- base + geom_boxplot() + coord_flip() + theme_minimal()
# modify x and y axes
xyaxes <- basebox + scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70)) +
  labs(y = "Age (years)", x = NULL)
# add title, subtitle, and caption
titlescap <- xyaxes + 
  labs(title = "Felonies Concerningly Committed by Younger Demographic than Other Types of Crimes", 
       subtitle = "Arrests made by Massachusetts State Police between 01/01/2019 and 03/28/2020",
       caption = "Not Included: 49 Arrests With No Recorded Type or Age\nSource: Mass.gov") +
  theme(plot.title = element_text(size=9),
        plot.subtitle = element_text(size=8))
# add annotation for number of crimes of each type
biplot <- titlescap + 
  annotate(geom = 'text', label = "n = 5441", y = 18, x = 4.15, angle = 0) +
  annotate(geom = 'text', label = "n = 1706", y = 18, x = 3.15, angle = 0) +
  annotate(geom = 'text', label = "n = 1251", y = 18, x = 2.15, angle = 0) +
  annotate(geom = 'text', label = "n = 180", y = 18, x = 1.15, angle = 0)
# view final plot
biplot

# save plot
saveRDS(biplot, file="biplot.rds")
