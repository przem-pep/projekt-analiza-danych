library(readr)
library(tidyverse)
library(ggstatsplot)
library(nortest)
czynniki_imputowane <- read_csv("czynniki-imputowane.csv")

lillie.test(czynniki_imputowane$Exam_Score)
ad.test(czynniki_imputowane$Exam_Score)
hist(czynniki_imputowane$Exam_Score)
boxplot(czynniki_imputowane$Exam_Score)
# rozklad nienormalny i wartosci odstajace - uzycie testow typu robust

# hours studied - wplyw
ggscatterstats(data = czynniki_imputowane, x = Hours_Studied, y = Exam_Score, type = "r")

# attendance - wplyw
ggscatterstats(data = czynniki_imputowane, x = Attendance, y = Exam_Score, type = "r")

# sleep hours - wplyw
ggscatterstats(data = czynniki_imputowane, x = Sleep_Hours, y = Exam_Score, type = "r")

# previous scores - wplyw
ggscatterstats(data = czynniki_imputowane, x = Previous_Scores, y = Exam_Score, type = "r")

# tutoring sessions - wplyw
ggscatterstats(data = czynniki_imputowane, x = Tutoring_Sessions, y = Exam_Score, type = "r")

# physical activity - wplyw
ggscatterstats(data = czynniki_imputowane, x = Physical_Activity, y = Exam_Score, type = "r")



