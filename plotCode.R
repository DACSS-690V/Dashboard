# load in the washington school data
load(file=url("https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/eduwa.rda"))

# explore variable of choice
class(eduwa$Student.Teacher.Ratio)
# Student.Teacher.Ratio variable is numeric
summary(eduwa$Student.Teacher.Ratio)
# min = 0.1, Q1 = 16.2, median = 18.3, mean = 18.95, Q3 = 20.2, max = 330, NAs = 329

# modify data to remove highest 2 outliers and 329 NAs
library(tidyverse)
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
library(ggplot2)
# specify dataset and variable to plot
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
