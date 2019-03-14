#title: shots-data
#description: shots data of the players
#input: stephen-curry.csv,andre-iguodala.csv,kevin-durant.csv,klay-thompson.csv,draymond-green.csv
#output:stephen-curry-summary.txt,andre-iguodala-summary.txt,kevin-durant-summary.txt,klay-thompson-summary.txt,draymond-green-summary.txt,shots-data.csv,shots-data-summary.txt

library(dplyr)

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)

curry$name <- "Stephen Curry"
iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"

curry$shot_made_flag[curry$shot_made_flag=="n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag=="y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag=="n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag=="y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag=="n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag=="y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag=="n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag=="y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag=="n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag=="y"] <- "shot_yes"

curry$minutes <- curry$period *12 - curry$minutes_remaining

iguodala$minutes <- iguodala$period *12 - iguodala$minutes_remaining

durant$minutes <- durant$period *12 - durant$minutes_remaining

thompson$minutes <- thompson$period *12 - thompson$minutes_remaining

green$minutes <- green$period *12 - green$minutes_remaining

sink(file="../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink(file="../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file="../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file="../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink(file="../output/draymond-green-summary.txt")
summary(green)
sink()

shotsdata <- rbind(curry,iguodala,durant,thompson,green)
View(shotsdata)

write.csv(shotsdata,"../data/shots-data.csv")

sink(file="../output/shots-data-summary.txt")
summary(shotsdata)

