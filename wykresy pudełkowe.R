czynniki <- read.csv("czynniki.csv")
View(czynniki)

install.packages("ggplot2")
library("ggplot2")

boxplot(czynniki$Hours_Studied,
        main = "Wykres pudełkowy godzin studiowania", 
        ylab = "Godziny studiowania",
        col = "yellow",
        border = "black")

boxplot(czynniki$Attendance,
        main = "Wykres pudełkowy obecności", 
        ylab = "Obecność [%]",
        col = "red",
        border = "black")

boxplot(czynniki$Sleep_Hours,
        main = "Wykres pudełkowy średniej liczby godzin snu w ciągu nocy", 
        ylab = "Średnia liczba godzin snu w ciągu nocy",
        col = "purple",
        border = "black")

boxplot(czynniki$Previous_Scores,
        main = "Wykres pudełkowy wyników z poprzednich egzaminów", 
        ylab = "Wyniki z poprzednich egzaminów",
        col = "blue",
        border = "black")

boxplot(czynniki$Tutoring_Sessions,
        main = "Wykres pudełkowy liczby sesji korepetycji w miesiącu", 
        ylab = "Liczba sesji korepetycji w miesiącu",
        col = "pink",
        border = "black")

boxplot(czynniki$Physical_Activity,
        main = "Wykres pudełkowy średniej liczby godzin aktywności fizycznej w tygodniu", 
        ylab = "Średnia liczba godzin aktywności fizycznej w tygodniu",
        col = "brown",
        border = "black")

boxplot(czynniki$Exam_Score,
        main = "Wykres pudełkowy wyników z egzaminu", 
        ylab = "Wynik z egzaminu [%]",
        col = "orange",
        border = "black")
