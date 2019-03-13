#title: make-shots-charts-script.R
#descricption: creation of shot charts
#inputs: what are the inputs required by the script?
#outputs: what are the outputs created when runnning the script?


library(ggplot2)
library(jpeg)
library(grid)


dat <- read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)               
                  
                  
                  
                  


court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

klay_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Klay Thompson (2016 season)") +
  theme_minimal()

pdf(file="../images/klay-thompson-shot-chart.pdf", width=6.5, height=5)
klay_shot_chart
dev.off()



kevin_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Kevin Durant (2016 season)") +
  theme_minimal()

pdf(file="../images/kevin-durant-shot-chart.pdf", width=6.5, height=5)
kevin_shot_chart
dev.off()




draymond_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Draymond Green (2016 season)") +
  theme_minimal()

pdf(file="../images/draymond-green-shot-chart.pdf", width=6.5, height=5)
draymond_shot_chart
dev.off()



andre_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Andre Iguodala (2016 season)") +
  theme_minimal()

pdf(file="../images/andre-iguodala-shot-chart.pdf", width=6.5, height=5)
andre_shot_chart
dev.off()



stephen_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Stephen Curry (2016 season)") +
  theme_minimal()

pdf(file="../images/stephen-curry-shot-chart.pdf", width=6.5, height=5)
stephen_shot_chart
dev.off()

gsw_shot_chart <- ggplot(data = dat) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x=x, y=y, color = shot_made_flag)) +
  ylim(-50, 420) + facet_wrap( ~player, ncol=3) +
  ggtitle("Shot Chart: GSW (2016 season)") +
  theme_minimal() + theme(legend.position="top", legend.title=element_blank())
  
pdf(file="../images/gsw-shot-charts.pdf", width=8, height=7)
gsw_shot_chart
dev.off()


ggsave(filename="../images/gsw-shot-charts.png", plot=gsw_shot_chart, width=8, height=7)
#png(filename = "../images/gsw-shot-charts.png", width=8, height=7)
#gsw_shot_chart
#dev.off()





