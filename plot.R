# Dependencies
install.packages("ggplot2")
install.packages("plyr")
install.packages("gridExtra")

library(ggplot2)
library(plyr)
library(gridExtra)

# Load and configure data
data <- read.csv(
  'year_over_year_ppg.csv',
  header = TRUE
)
data$Team <- as.factor(data$Team)
str(data)

# Basic graphic parameters
par(
  omi = c(0.5, 0, 0, 0)
)

# Graphic 1 - Box plot of team PPGs, ordered alphabetically
plot(
  data$Team,
  data$PPG,
  ylim=c(0,3),
  las=2,
  main="PPG ranges by team in MLS",
  xlab="",
  ylab="Points per game",
  pch=19,
  col=colors.teams
)

# The next two commands will add a grid, with the ability to re-run the original
# plot above to render the box plots on top of the grid.
grid(lty=1,lwd=1,col="#e1e1e6")
par(
  new=T
)

# Reorder by median PPG
oind <- order(as.numeric(by(-data$PPG, data$Team, median)))
data$Team <- ordered(data$Team, levels=levels(data$Team)[oind])

# Graphic 2 - Replot using ordered colors, sorting teams by median PPG
plot(
  data$Team,
  data$PPG,
  ylim=c(0,3),
  las=2,
  main="PPG ranges by team in MLS",
  xlab="",
  ylab="Points per game",
  pch=19,
  col=colors.teams.ordered
)

# Graphic 3
# Tried to subset data using this expression, but it failed
#   data[data$Team %in% c("Columbus")]
ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG", x="Season", y="Points per game")

# Atlanta
atl <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Atlanta"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Atlanta"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.atl, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Atlanta United", x="Season", y="Points per game")

# Austin
aus <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Austin"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Austin"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.aus, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Austin FC", x="Season", y="Points per game")

# Charlotte
cha <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Charlotte"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Charlotte"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.cha, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Charlotte FC", x="Season", y="Points per game")

# Chicago
chi <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Chicago"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Chicago"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.chi, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Chicago Fire", x="Season", y="Points per game")

# Chivas USA
chv <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Chivas USA"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Chivas USA"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.chv, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Chivas USA", x="Season", y="Points per game")

# Cincinnati
cin <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Cincinnati"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Cincinnati"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.cin, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: FC Cincinnati", x="Season", y="Points per game")

# Colorado
col <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Colorado"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Colorado"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.col, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Colorado Rapids", x="Season", y="Points per game")

# Columbus
clb <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Columbus"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Columbus"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.clb, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Columbus Crew", x="Season", y="Points per game")

# Dallas
dal <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Dallas"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Dallas"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.dal, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Dallas Burn / FC Dallas", x="Season", y="Points per game")

# DC
dc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "DC"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "DC"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.dc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: DC United", x="Season", y="Points per game")

# Houston
hou <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Houston"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Houston"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.hou, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Houston Dynamo", x="Season", y="Points per game")

# Kansas City
kc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Kansas City"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Kansas City"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.kc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Kansas City Wizards / Sporting Kansas City", x="Season", y="Points per game")

# Los Angeles
la <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Los Angeles"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Los Angeles"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.la, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Los Angeles Galaxy", x="Season", y="Points per game")

# Los Angeles FC
lafc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Los Angeles FC"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Los Angeles FC"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.lafc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Los Angeles FC", x="Season", y="Points per game")

# Miami
mia <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Miami Fusion"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Miami Fusion"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.mia, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Miami Fusion", x="Season", y="Points per game")

# Inter Miami
imfc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Miami"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Miami"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.imfc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Inter Miami", x="Season", y="Points per game")

# Miami unified
ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Miami Fusion"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Miami Fusion"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.mia, size=3, shape=21, stroke=1) +
  geom_line(data = subset(data, Team == "Miami"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Miami"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.imfc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Miami Fusion / Inter Miami", x="Season", y="Points per game")

# Minnesota
min <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Minnesota"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Minnesota"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.min, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Minnesota United", x="Season", y="Points per game")

# Montreal
mtl <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Montreal"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Montreal"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.mtl, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Montreal Impact / CF Montreal", x="Season", y="Points per game")

# Nashville
nas <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Nashville"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Nashville"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.nas, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Nashville SC", x="Season", y="Points per game")

# New England
ne <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "New England"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "New England"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.ne, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: New England Revolution", x="Season", y="Points per game")

# New York
ny <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "New York"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "New York"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.ny, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Metrostars / New York Red Bulls", x="Season", y="Points per game")

# New York City
nyc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "New York City"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "New York City"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.nyc, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: New York City FC", x="Season", y="Points per game")

# Orlando
orl <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Orlando"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Orlando"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.orl, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Orlando City", x="Season", y="Points per game")

# Philadelphia
phi <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Philadelphia"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Philadelphia"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.phi, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Philadelphia Union", x="Season", y="Points per game")

# Portland
por <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Portland"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Portland"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.por, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Portland Timbers", x="Season", y="Points per game")

# Salt Lake
slc <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Salt Lake"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Salt Lake"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.rsl, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Real Salt Lake", x="Season", y="Points per game")

# San Jose
sj <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, (Team == "San Jose" & Season < "2006")), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, (Team == "San Jose" & Season < "2006")), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.sj, size=3, shape=21, stroke=1) +
  geom_line(data = subset(data, (Team == "San Jose" & Season > "2007")), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, (Team == "San Jose" & Season > "2007")), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.sj, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: San Jose Clash / San Jose Earthquakes", x="Season", y="Points per game")

# Seattle
sea <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Seattle"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Seattle"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.sea, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Seattle Sounders", x="Season", y="Points per game")

# St. Louis
stl <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "St. Louis"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "St. Louis"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.stl, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: St Louis City SC", x="Season", y="Points per game")

# Tampa Bay
tb <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Tampa Bay"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Tampa Bay"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.tb, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Tampa Bay Mutiny", x="Season", y="Points per game")

# Toronto
tor <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Toronto"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Toronto"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.tor, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Toronto FC", x="Season", y="Points per game")

# Vancouver
van <- ggplot(data) +
  geom_line(aes(x=Season, y=PPG, group=Team, colour = Team), col="gray") +
  geom_line(data = subset(data, Team == "Vancouver"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", size=1) +
  geom_point(data = subset(data, Team == "Vancouver"), aes(x=Season, y=PPG, group=Team, colour = Team), col="#000", fill=colors.van, size=3, shape=21, stroke=1) +
  coord_cartesian(ylim=c(0,3)) +
  labs(title="Year over year PPG: Vancouver Whitecaps", x="Season", y="Points per game")


grid.arrange(atl, aus, cha, chi, chv, cin, clb, col, dal, dc, hou, kc, la, lafc, mia, imfc, min, mtl, nas, ne, ny, nyc, orl, phi, por, sea, sj, slc, stl, tb, tor, van, ncol=6)
