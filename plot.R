#reading the data
#loading packages
library(lapply)
library(dplyr)
library(tidyr)
library(purrr)
library(microbenchmark)
library(tidyverse)
library(plyr)
library(janitor)
library(caret)
library(corrplot)

dfplot <- read.csv("C:/Users/mgharibdousti/Desktop/Dissertation/JuliaData - final_variables.csv")

dfplot1 <- read.delim2("C:/Users/mgharibdousti/Desktop/Dissertation/Data1000.dat")

setwd("C:/Users/mgharibdousti/Desktop/Dissertation")

dfplot$Utility<- as.numeric(dfplot$Utility)

plot(dfplot$Norm.LDRI,dfplot$Utility)
#caret::learning_curve_dat(dfplot$Norm.LDRI,dfplot$Utility)

c = 50

dfplot$Utility <- 1-exp((dfplot$Norm.LDRI/dfplot$Norm.MELD)/c)

xyplot(dfplot$Utility~dfplot$Norm.LDRI, type="s")
xyplot(dfplot$Utility~dfplot$Norm.MELD, type="s")

xyplot(dfplot$Utility~dfplot$Norm.LDRI,col = rgb(red = 255, green = 90, 
                                                 blue = 0, maxColorValue = 255),type="s")


xyplot(dfplot$Utility3~dfplot$Norm.MELD)

which(dfplot$Norm.MELD == 0)

library(BBmisc)
dfplot$Norm.MELD2<-  normalize(dfplot$Norm.MELD, method = "center")

dfplot$Norm.LDRI2<-  normalize(dfplot$Norm.LDRI, method = "center")

c = 0.5

dfplot$Utility2 <-  exp((dfplot$Norm.MELD2/dfplot$Norm.LDRI2)/c)

xyplot(dfplot$Utility2~dfplot$Norm.LDRI2,type="s")

#ggplot(mpg, aes(dfplot$Norm.LDRI,dfplot$Utility, colour = drv))

mdl1 <- lm(y ~ x, data = xy)
mdl2 <- lm(y ~ x + I(x^2), data = xy)
mdl3 <- lm(y ~ x + I(x^2) + I(x^3), data = xy)
mdl4 <- lm(y ~ I(x^2), data = xy)

prd <- data.frame(x = seq(0, 50, by = 0.5))

result <- prd
result$mdl1 <- predict(mdl1, newdata = prd)
result$mdl2 <- predict(mdl2, newdata = prd)
result$mdl3 <- predict(mdl3, newdata = prd)
result$mdl4 <- predict(mdl4, newdata = prd)
 
library(reshape2)
library(ggplot2)

result <-  melt(result, id.vars = "x", variable.name = "model",
                value.name = "fitted")
ggplot(result, aes(x = x, y = fitted)) +
  theme_bw() +
  geom_point(data = xy, aes(x = x, y = y)) +
  geom_line(aes(colour = model), size = 1)


dfplot1 <- read_excel("C:/Users/mgharibdousti/Desktop/Dissertation/KOOFT.xlsx")
dfplot1<- data.frame(dfplot1)

table(dfplot1$group)                       




# Load required R packages
library(tidyverse)
library(rstatix)
library(ggpubr)

# Prepare the data and inspect a random sample of the data
set.seed(1234)
dfplot1 %>% sample_n_by(group, size = 1)

dfplot1 %>%
  group_by(group) %>%
  get_summary_stats(Utility, type = "mean_sd")



#dfplot1$group<- as.numeric(dfplot1$group)
library(dplyr)
group_by(dfplot1, group) %>%
  summarise(
    count = n(),
    mean = mean(Utility, na.rm = TRUE),
    sd = sd(Utility, na.rm = TRUE),
    median = median(Utility, na.rm = TRUE),
    IQR = IQR(Utility, na.rm = TRUE)
  )

install.packages("ggpubr")


library("ggpubr")
ggboxplot(dfplot1, x = "group", y = "Utility", 
          color = "group",
          order = c("1", "2", "3", "4"),
          ylab = "Utility", xlab = "MELD Score Group")


library("ggpubr")
ggline(dfplot1, x = "group", y = "Utility", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "Utility", xlab = "MELD group")


kruskal.test(Utility ~ group, data = dfplot1)        


pairwise.wilcox.test(dfplot1$Utility, dfplot1$group,
                     p.adjust.method = "none")

kruskal.test(Utility ~ group, data = dfplot1)

#FOR C
#Obj.Value
dfplot2 <- read_excel("C:/Users/mgharibdousti/Desktop/Dissertation/1st-scenario-significant-test-c.xlsx")
dfplot2<- data.frame(dfplot2)

table(dfplot2$group)   


library("ggpubr")
ggboxplot(dfplot2, x = "group", y = "objective.value", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Objective value", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot2, x = "group", y = "objective.value", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Objective value", xlab = "Case Studies")


kruskal.test(objective.value ~ group, data = dfplot2)        


pairwise.wilcox.test(dfplot2$objective.value, dfplot2$group,
                     p.adjust.method = "BH")



#numberofmatchedpairs

library("ggpubr")
ggboxplot(dfplot2, x = "group", y = "Number.of.paired.matches", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Number of matched paires", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot2, x = "group", y = "Number.of.paired.matches", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Number of matched paires", xlab = "Case Studies")


kruskal.test(Number.of.paired.matches~ group, data = dfplot2)        


pairwise.wilcox.test(dfplot2$Number.of.paired.matches, dfplot2$group,
                     p.adjust.method = "BH")






#FOR P
#Obj.Value
dfplot3 <- read_excel("C:/Users/mgharibdousti/Desktop/Dissertation/1st-scenario-significant-test-p.xlsx")
dfplot3<- data.frame(dfplot3)

table(dfplot3$group)   


library("ggpubr")
ggboxplot(dfplot3, x = "group", y = "objective.value", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Objective value", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot3, x = "group", y = "objective.value", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Objective value", xlab = "Case Studies")


kruskal.test(objective.value ~ group, data = dfplot3)        


pairwise.wilcox.test(dfplot3$objective.value, dfplot3$group,
                     p.adjust.method = "BH")



#numberofmatchedpairs

library("ggpubr")
ggboxplot(dfplot3, x = "group", y = "Number.of.paired.matches", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Number of matched paires", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot3, x = "group", y = "Number.of.paired.matches", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Number of matched paires", xlab = "Case Studies")


kruskal.test(Number.of.paired.matches~ group, data = dfplot3)        


pairwise.wilcox.test(dfplot2$Number.of.paired.matches, dfplot3$group,
                     p.adjust.method = "BH")



library(readxl)
library(tidyverse)
SecondScenarioSensitivityAnalysis <- read_excel("C:/Users/mgharibdousti/Desktop/Dissertation/SecondScenarioSensitivityAnalysis.xlsx")
SecondScenarioSensitivityAnalysis <- data.frame(SecondScenarioSensitivityAnalysis)
dfplot3<- SecondScenarioSensitivityAnalysis

library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
dfplot3 %>%
  group_by(group) %>%
  get_summary_stats(Objective.value, type = "mean_sd")

dfplot3$Objective.value

library("ggpubr")
ggboxplot(dfplot3, x = "group", y = "Objective.value", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Objective value", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot3, x = "group", y = "Objective.value", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Objective value", xlab = "Case Studies")


kruskal.test(Objective.value ~ group, data = dfplot3)        


pairwise.wilcox.test(dfplot3$Objective.value, dfplot3$group,
                     p.adjust.method = "BH")







ThirdScenarioSensitivitAnalysisLAMBDAAAA
library(readxl)
library(tidyverse)
ThirdScenarioSA_P <- read_excel("C:/Users/mgharibdousti/Desktop/Dissertation/ThirdScenarioSA_P.xlsx")
ThirdScenarioSA_P <- data.frame(ThirdScenarioSA_P)
dfplot3<- ThirdScenarioSA_P

library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
dfplot3 %>%
  group_by(group) %>%
  get_summary_stats(Obj.Value, type = "mean_sd")

#dfplot3$Number.of.paired.matches

library("ggpubr")
ggboxplot(dfplot3, x = "group", y = "Obj.Value", 
          color = "group",
          order = c("1", "2", "3", "4", "5"),
          ylab = "Objective Value", xlab = "Case Studies")


library("ggpubr")
ggline(dfplot3, x = "group", y = "Obj.Value", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "5"),
       ylab = "Objective Value", xlab = "Case Studies")


kruskal.test(Obj.Value ~ group, data = dfplot3)        


pairwise.wilcox.test(dfplot3$Obj.Value, dfplot3$group,
                     p.adjust.method = "BH")