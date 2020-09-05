#Final Project
#Alison Tomooka

setwd("C:/Users/akid6/OneDrive/Desktop/Spring 2020/ECON 256/FINAL Project")

library(tidyverse)

IPUMSdata <- read.csv("usa_00002.csv")
IPUMSstateData <- read.csv("usa_00003.csv")

eduIncome <- select(IPUMSdata, YEAR, EDUC, INCWAGE, POVERTY)
eduIncome <- filter(eduIncome, INCWAGE < 999998)

eduIncHawaii <- select(IPUMSstateData, YEAR, EDUC, INCWAGE, STATEFIP)
eduIncHawaii <- filter(eduIncHawaii, STATEFIP == 15)
eduIncHawaii <- filter(eduIncHawaii, INCWAGE < 999998)


eduIncome <- mutate(eduIncome, eduLevel = ifelse(EDUC <= 6, "High School or Less",
                                               ifelse(EDUC == 7, "1 Year",
                                                      ifelse(EDUC == 8, "2 Years",
                                                             ifelse(EDUC == 9, "3 Years",
                                                                    ifelse(EDUC == 10, "4 Years",
                                                                           "5 or More Years"))))))

eduIncHawaii <- mutate(eduIncHawaii, eduLevel = ifelse(EDUC <= 6, "High School or Less",
                                                 ifelse(EDUC == 7, "1 Year",
                                                        ifelse(EDUC == 8, "2 Years",
                                                               ifelse(EDUC == 9, "3 Years",
                                                                      ifelse(EDUC == 10, "4 Years",
                                                                             "5 or More Years"))))))


avgIncByEdu <- eduIncome %>%
  group_by(eduLevel) %>%
  summarize(avgIncome = mean(INCWAGE))

ggplot(avgIncByEdu, aes(x = eduLevel, y = avgIncome)) +
  geom_bar(stat = "identity", fill = "purple2") +
  ggtitle("Graph 1: Average Income by Education Level (2015-2018)") +
  xlab("Level of Post-Secondary Education") +
  ylab("Income in USD")




edu2015 <- filter(eduIncome, YEAR == 2015)

avgIncByEdu2015 <- edu2015 %>%
  group_by(eduLevel) %>%
  summarize('2015' = mean(INCWAGE))


edu2016 <- filter(eduIncome, YEAR == 2016)

avgIncByEdu2016 <- edu2016 %>%
  group_by(eduLevel) %>%
  summarize('2016' = mean(INCWAGE))


edu2017 <- filter(eduIncome, YEAR == 2017)

avgIncByEdu2017 <- edu2017 %>%
  group_by(eduLevel) %>%
  summarize('2017' = mean(INCWAGE))


edu2018 <- filter(eduIncome, YEAR == 2018)

avgIncByEdu2018 <- edu2018 %>%
  group_by(eduLevel) %>%
  summarize('2018' = mean(INCWAGE))


avgIncYearly <- left_join(avgIncByEdu2015, avgIncByEdu2016, "eduLevel")
avgIncYearly <- left_join(avgIncYearly, avgIncByEdu2017, "eduLevel")
avgIncYearly <- left_join(avgIncYearly, avgIncByEdu2018, "eduLevel")

avgIncYearly <- pivot_longer(avgIncYearly, c("2015", "2016", "2017", "2018"), "year")

ggplot(avgIncYearly, aes(x = year, y = value, group = eduLevel, color = eduLevel)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
  ggtitle("Graph 2: Change in Average Income by Post-Secondary Education Level (2015-2018)") +
  xlab("Year") +
  ylab("Average Income in USD") +
  theme_light() +
  theme(legend.title = element_blank())




medPovByEdu2015 <- edu2015 %>%
  group_by(eduLevel) %>%
  summarize('2015' = median(POVERTY))

medPovByEdu2016 <- edu2016 %>%
  group_by(eduLevel) %>%
  summarize('2016' = median(POVERTY))

medPovByEdu2017 <- edu2017 %>%
  group_by(eduLevel) %>%
  summarize('2017' = median(POVERTY))

medPovByEdu2018 <- edu2018 %>%
  group_by(eduLevel) %>%
  summarize('2018' = median(POVERTY))


medPovYearly <- left_join(medPovByEdu2015, medPovByEdu2016, "eduLevel")
medPovYearly <- left_join(medPovYearly, medPovByEdu2017, "eduLevel")
medPovYearly <- left_join(medPovYearly, medPovByEdu2018, "eduLevel")

medPovYearly <- pivot_longer(medPovYearly, c("2015", "2016", "2017", "2018"), "year")

ggplot(medPovYearly, aes(x = year, y = value, group = eduLevel, color = eduLevel)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
  ggtitle("Graph 3: Change in Median Poverty Level by Post-Secondary Education Level (2015-2018)") +
  xlab("Year") +
  ylab("Median Percentage of Poverty Threshold") +
  theme_light() +
  theme(legend.title = element_blank())



avgIncByEduH <- eduIncHawaii %>%
  group_by(eduLevel) %>%
  summarize(Hawaii = mean(INCWAGE))

avgIncByEduN <- eduIncome %>%
  group_by(eduLevel) %>%
  summarize(Nation = mean(INCWAGE))

avgIncByEduHvN <- left_join(avgIncByEduH, avgIncByEduN, "eduLevel")

avgIncByEduHvN <- pivot_longer(avgIncByEduHvN, c("Hawaii", "Nation"), "HvN")

ggplot(avgIncByEduHvN, aes(x = eduLevel, y = value, fill = HvN)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graph 4: Average Income by Education Level: Hawaii vs. Nation (2015-2018)") +
  xlab("Level of Post-Secondary Education") +
  ylab("Income in USD") +
  theme(legend.title = element_blank())



edu2015H <- filter(eduIncHawaii, YEAR == 2015)

avgIncByEdu2015H <- edu2015H %>%
  group_by(eduLevel) %>%
  summarize('2015' = mean(INCWAGE))


edu2016H <- filter(eduIncHawaii, YEAR == 2016)

avgIncByEdu2016H <- edu2016H %>%
  group_by(eduLevel) %>%
  summarize('2016' = mean(INCWAGE))


edu2017H <- filter(eduIncHawaii, YEAR == 2017)

avgIncByEdu2017H <- edu2017H %>%
  group_by(eduLevel) %>%
  summarize('2017' = mean(INCWAGE))


edu2018H <- filter(eduIncHawaii, YEAR == 2018)

avgIncByEdu2018H <- edu2018H %>%
  group_by(eduLevel) %>%
  summarize('2018' = mean(INCWAGE))


avgIncYearlyH <- left_join(avgIncByEdu2015H, avgIncByEdu2016H, "eduLevel")
avgIncYearlyH <- left_join(avgIncYearlyH, avgIncByEdu2017H, "eduLevel")
avgIncYearlyH <- left_join(avgIncYearlyH, avgIncByEdu2018H, "eduLevel")

avgIncYearlyH <- pivot_longer(avgIncYearlyH, c("2015", "2016", "2017", "2018"), "year")

ggplot(avgIncYearlyH, aes(x = year, y = value, group = eduLevel, color = eduLevel)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
  ggtitle("Graph 5: Average Income Change by Education Level in Hawaii (2015-2018)") +
  xlab("Year") +
  ylab("Average Income in USD") +
  theme_light() +
  theme(legend.title = element_blank())