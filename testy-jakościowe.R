library(readr)
library(tidyverse)
library(ggstats)
library(ggstatsplot)
library(nortest)
czynniki_imputowane <- read_csv("czynniki-imputowane.csv")
cvm.test(czynniki_imputowane$Exam_Score)
ks.test(czynniki_imputowane$Exam_Score,pnorm,exact=NULL)
hist(czynniki_imputowane$Exam_Score)
boxplot(czynniki_imputowane$Exam_Score)
modelprosty2 <- lm(Exam_Score ~ Parental_Involvement + Access_to_Resources+Extracurricular_Activities
                   +Motivation_Level
                   +Internet_Access
                   +Family_Income
                   +Teacher_Quality
                   +School_Type
                   +Peer_Influence
                   +Learning_Disabilities
                   +Parental_Education_Level
                   +Distance_from_Home
                   +Gender,data = czynniki_imputowane)
anova(modelprosty2)

mod <- aov(formula = Exam_Score ~ Parental_Involvement + Access_to_Resources+Extracurricular_Activities
           +Motivation_Level
           +Internet_Access
           +Family_Income
           +Teacher_Quality
           +Peer_Influence
           +Learning_Disabilities
           +Parental_Education_Level
           +Distance_from_Home,
           data = czynniki_imputowane
)
ggcoefstats(mod)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Parental_Involvement,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Access_to_Resources,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Extracurricular_Activities,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Motivation_Level,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Internet_Access,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Peer_Influence,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Parental_Education_Level,
  y = Exam_Score
)
ggbetweenstats(
  data = czynniki_imputowane,
  x = Distance_from_Home,
  y = Exam_Score
)
