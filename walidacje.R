library(validate)
library(tidyverse)

dane <- read_csv("czynniki.csv")


# Należy najpierw zawrzeć przekodowanie zmiennych

dane$Parental_Involvement <- ifelse(dane$Parental_Involvement == "Low", 1,
                                    ifelse(dane$Parental_Involvement == "Medium", 2, 3))
dane$Parental_Involvement <- factor(dane$Parental_Involvement, levels = c(1, 2, 3),
                                    labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Access_to_Resources <- ifelse(dane$Access_to_Resources == "Low", 1,
                                   ifelse(dane$Access_to_Resources == "Medium", 2, 3))
dane$Access_to_Resources <- factor(dane$Access_to_Resources, levels = c(1, 2, 3),
                                   labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Extracurricular_Activities <- ifelse(dane$Extracurricular_Activities == "No", 0, 1)
dane$Extracurricular_Activities <- factor(dane$Extracurricular_Activities, levels = c(0, 1),
                                          labels = c("No", "Yes"))

dane$Motivation_Level <- ifelse(dane$Motivation_Level == "Low", 1,
                                ifelse(dane$Motivation_Level == "Medium", 2, 3))
dane$Motivation_Level <- factor(dane$Motivation_Level, levels = c(1, 2, 3),
                                labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Internet_Access <- ifelse(dane$Internet_Access == "No", 0, 1)
dane$Internet_Access <- factor(dane$Internet_Access, levels = c(0, 1), labels = c("No", "Yes"))

dane$Family_Income <- ifelse(is.na(dane$Family_Income), NA,
                             ifelse(dane$Family_Income == "Low", 1,
                                    ifelse(dane$Family_Income == "Medium", 2, 3)))
dane$Family_Income <- factor(dane$Family_Income, levels = c(1, 2, 3),
                             labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$Teacher_Quality <- ifelse(is.na(dane$Teacher_Quality), NA,
                               ifelse(dane$Teacher_Quality == "Low", 1,
                                      ifelse(dane$Teacher_Quality == "Medium", 2, 3)))
dane$Teacher_Quality <- factor(dane$Teacher_Quality, levels = c(1, 2, 3),
                               labels = c("Low", "Medium", "High"), ordered = TRUE)

dane$School_Type <- ifelse(dane$School_Type == "Public", 1, 2)
dane$School_Type <- factor(dane$School_Type, levels = c(1, 2), labels = c("Public", "Private"))

dane$Peer_Influence <- ifelse(dane$Peer_Influence == "Negative", 1,
                              ifelse(dane$Peer_Influence == "Neutral", 2, 3))
dane$Peer_Influence <- factor(dane$Peer_Influence, levels = c(1, 2, 3),
                              labels = c("Negative", "Neutral", "Positive"), ordered = TRUE)

dane$Learning_Disabilities <- ifelse(dane$Learning_Disabilities == "No", 0, 1)
dane$Learning_Disabilities <- factor(dane$Learning_Disabilities, levels = c(0, 1),
                                     labels = c("No", "Yes"))

dane$Parental_Education_Level <- ifelse(is.na(dane$Parental_Education_Level), NA,
                                        ifelse(dane$Parental_Education_Level == "High School", 1,
                                               ifelse(dane$Parental_Education_Level == "College", 2, 3)))
dane$Parental_Education_Level <- factor(dane$Parental_Education_Level, levels = c(1, 2, 3),
                                        labels = c("High School", "College", "Postgraduate"),
                                        ordered = TRUE)

dane$Distance_from_Home <- ifelse(is.na(dane$Distance_from_Home), NA,
                                  ifelse(dane$Distance_from_Home == "Far", 1,
                                         ifelse(dane$Distance_from_Home == "Moderate", 2, 3)))
dane$Distance_from_Home <- factor(dane$Distance_from_Home, levels = c(1, 2, 3),
                                  labels = c("Far", "Moderate", "Near"), ordered = TRUE)

dane$Gender <- ifelse(dane$Gender == "Female", 1, 2)
dane$Gender <- factor(dane$Gender, levels = c(1, 2), labels = c("Female", "Male"))


# Stworzenie reguł walidacyjnych

rules <- check_that(dane,
  
  # Reguły dla zmiennej Hours_Studied
  
  is.numeric(Hours_Studied), #1
  Hours_Studied >= 0, #2
  Hours_Studied <= 168, #3
  
  # Reguły dla zmiennej Attendance
  
  is.numeric(Attendance), #4
  Attendance >= 0, #5
  Attendance <= 100, #6
  
  # Reguły dla zmiennej Parental_Involvement
  
  is.factor(Parental_Involvement), #7
  Parental_Involvement %in% c("Low", "Medium", "High"), #8
  
  # Reguły dla zmiennej Access_to_Resources
  
  is.factor(Access_to_Resources), #9
  Access_to_Resources %in% c("Low", "Medium", "High"), #10
  
  # Reguły dla zmiennej Extracurricular_Activities
  
  is.factor(Extracurricular_Activities), #11
  Extracurricular_Activities %in% c("No", "Yes"), #12
  
  # Reguły dla zmiennej Sleep_Hours
  
  is.numeric(Sleep_Hours), #13
  Sleep_Hours >= 0, #14
  Sleep_Hours <= 24, #15
  
  # Reguły dla zmiennej Previous_Scores
  
  is.numeric(Previous_Scores), #16
  Previous_Scores >= 0, #17
  Previous_Scores <= 100, #18
  
  # Reguły dla zmiennej Motivation_Level
  
  is.factor(Motivation_Level), #19
  Motivation_Level %in% c("Low", "Medium", "High"), #20
  
  # Reguły dla zmiennej Internet_Access
  
  is.factor(Internet_Access), #21
  Internet_Access %in% c("No", "Yes"), #22
  
  # Reguły dla zmiennej Tutoring_Sessions
  
  is.numeric(Tutoring_Sessions), #23
  Tutoring_Sessions >= 0, #24
  (Tutoring_Sessions - floor(Tutoring_Sessions)) == 0, #25
  
  # Reguły dla zmiennej Family_Income
  
  is.factor(Family_Income), #26
  Family_Income %in% c("Low", "Medium", "High"), #27
  
  # Reguły dla zmiennej Teacher_Quality
  
  is.factor(Teacher_Quality), #28
  Teacher_Quality %in% c("Low", "Medium", "High"), #29
  
  # Reguły dla zmiennej School_Type
  
  is.factor(School_Type), #30
  School_Type %in% c("Public", "Private"), #31
  
  # Reguły dla zmiennej Peer_Influence
  
  is.factor(Peer_Influence), #32
  Peer_Influence %in% c("Negative", "Neutral", "Positive"), #33
  
  # Reguły dla zmiennej Physical_Activity
  
  is.numeric(Physical_Activity), #34
  Physical_Activity >= 0, #35
  Physical_Activity <= 168, #36
  
  # Reguły dla zmiennej Learning_Disabilities
  
  is.factor(Learning_Disabilities), #37
  Learning_Disabilities %in% c("No", "Yes"), #38
  
  # Reguły dla zmiennej Parental_Education_Level
  
  is.factor(Parental_Education_Level), #39
  Parental_Education_Level %in% c("High School", "College", "Postgraduate"), #40
  
  # Reguły dla zmiennej Distance_from_Home
  
  is.factor(Distance_from_Home), #41
  Distance_from_Home %in% c("Far", "Moderate", "Near"), #42
  
  # Reguły dla zmiennej Gender
  
  is.factor(Gender), #43
  Gender %in% c("Female", "Male"), #44
  
  # Reguły dla zmiennej Exam_Score
  
  is.numeric(Exam_Score), #45
  Exam_Score >= 0, #46
  Exam_Score <= 100 #47
)

summary(rules)
