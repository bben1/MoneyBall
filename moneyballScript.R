#Import the batting data
batting <- read.csv("R/Scripts/BattingData.csv")
print(head(batting))

#Add a column for the batting average
batting$BA <- batting$H / batting$AB

#Column for the on-base-percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

#Column for singles
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Column for slugging-average
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

#Load salary data
sal <- read.csv('Salaries.csv')

#Salary data only goes back until 1985 so need to use subset of batting data within this period
batting <- subset(batting,yearID >= 1985)

#Merge the two dataframes
combo <- merge(batting,sal,by=c('playerID','yearID'))

#Create a dataframe that contains info on the lost players to use as a benchmark when finding replacements
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )

#Adjust the dataframe (players left in 2001 so we only want that data and we only want relevant statistics)
lost_players <- subset(lost_players,yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

#Set upper limit for the salary that you are willing to pay prospective players
avail.players <- filter(avail.players,salary<7000000,OBP>0)

#The approximate average AB for the lost players was 490 so anything at least as high is good enough
avail.players <- filter(avail.players,AB >= 490)

#Sort the dataframe by on-base-percentage, showing the top 15 prospective players
possible <- head(arrange(avail.players,desc(OBP)),15)

#Display the available players that would be suitable replacements
possible <- possible[,c('playerID','OBP','AB','salary')]