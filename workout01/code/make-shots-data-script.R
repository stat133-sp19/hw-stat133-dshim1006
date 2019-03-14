##########################################################################################################
## Title        : Shots Data (Part 3)
## Description  : Create a csv file shots-data.csv that contains data of shots made by five players
## Input        :
##                - andre-iguodala.csv
##                - draymond-green.csv
##                - kevin-durant.csv
##                - klay-thompson.csv
##                - stephen-curry.csv
## Output       : shots-data.csv and other summaries of data in txt format
## Date         : 3/10/2019
## Author       : Jennifer Shim
##########################################################################################################


column_names <- c("team_name", "game_date", "season", "period", "minutes_remaining", "seconds_remaining", "shot_made_flag", "action_type", "shot_type", "shot_distance", "opponent", "x", "y")
data_types <- c("character", "character", "integer", "integer", "integer", "integer", "character", "character", "character", "integer", "character", "integer", "integer")

curry <- read.csv("../data/stephen-curry.csv", sep=",", col.names=column_names, colClasses=data_types, stringsAsFactors=FALSE)
curry$player <- c("Stephen Curry")
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$minute <- curry$period * 12 - curry$minutes_remaining

sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()



iguodala <- read.csv("../data/andre-iguodala.csv", sep=",", col.names=column_names, colClasses=data_types, stringsAsFactors=FALSE)
iguodala$player <- c("Andre Iguodala")
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$minute <- iguodala$period * 12 - iguodala$minutes_remaining

sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()




green <- read.csv("../data/draymond-green.csv", sep=",", col.names=column_names, colClasses=data_types, stringsAsFactors=FALSE)
green$player <- c("Draymond Green")
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$minute <- green$period * 12 - green$minutes_remaining

sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()




durant <- read.csv("../data/kevin-durant.csv", sep=",", col.names=column_names, colClasses=data_types, stringsAsFactors=FALSE)
durant$player <- c("Kevin Durant")
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$minute <- durant$period * 12 - durant$minutes_remaining

sink(file = "../output/kevin-durant-summary.txt")
summary(durant)
sink()



thompson <- read.csv("../data/klay-thompson.csv", sep=",", col.names=column_names, colClasses=data_types, stringsAsFactors=FALSE)
thompson$player <- c("Klay Thompson")
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$minute <- thompson$period * 12 - thompson$minutes_remaining

sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()


combined <- rbind(curry, iguodala, green, durant, thompson)

write.csv(
  x = combined, # R object to be exported
  file =  "../data/shots-data.csv")  # file path



sink(file = "../output/shots-data-summary.txt")
summary(combined)
sink()


