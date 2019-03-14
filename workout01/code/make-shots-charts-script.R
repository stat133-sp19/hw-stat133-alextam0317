#title:make-shot-charts-script
#description: charts of GSW's make shots information and each player's shooting details
#inputs: stephen-curry.csv, andre-iguodala.csv, kevin-durant.csv, klay-thompson.csv, draymond-green.csv, nba-court.jpg
#outputs:klay-thompson-shot-chart.pdf, stephen-curry-shot-chart.pdf, andre-iguodala-shot-chart.pdf, kevin-durant-shot-chart.pdf, draymond-green-shot-chart.pdf, gsw-shot-chart.pdf, gsw-shot-chart.png

shotsdata <- read.csv(file="../data/shots-data.csv")
library(ggplot2)
library(jpeg)
library(grid)

##Klay Thompson
# scatterplot
klay_scatterplot <- ggplot(data = thompson) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# shot chart with court background
ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()+
  ggsave("../images/klay-thompson-shot-chart.pdf",width=6.5,height=5)


##Stephen Curry
ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()+
  ggsave("../images/stephen-curry-shot-chart.pdf",width=6.5,height=5)

##Andre Iguodala
ggplot(data = Iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()+
  ggsave("../images/andre-iguodala-shot-chart.pdf",width=6.5,height=5)


##Kevin Durant
ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()+
  ggsave("../images/kevin-durant-shot-chart.pdf",width=6.5,height=5)

##Draymond Green
ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()+
  ggsave("../images/draymond-green-shot-chart.pdf",width=6.5,height=5)

##gsw facetted shot chart in pdf
ggplot(data = shotsdata) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal()+
  facet_wrap(~name,ncol=3)+
  ggsave("../images/gsw-shot-chart.pdf",width=6.5,height=5)

##save as png by hand (names R plot in images, for the report purpose)
ggplot(data = shotsdata) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal()+
  facet_wrap(~name,ncol=3)