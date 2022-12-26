setwd('/Users/mamane/Desktop/ICA 3')
data <- read.csv('live_streams.csv')
which.max(data$viewers)
data <- data[-which.max(data$viewers),]

plot(model_stdres, model_fitted)

nrows(data) # <- 1542
order_viewers <- data[order(data$viewers),]
#analysis

#Genre

# unique(data$genre)
#[1] "Sports"           "RealTimeStrategy" "Adventure"        "Racing"           "RolePlaying"     
#[6] "Puzzle"           "BoardGame"        "Simulation"   

Sports <- data[data$genre == 'Sports',]
RealTimeStrategy <- data[data$genre == 'RealTimeStrategy',]
Adventure <- data[data$genre == 'Adventure',]
Racing <- data[data$genre == 'Racing',]
RolePlaying <- data[data$genre == 'RolePlaying',]
Puzzle <- data[data$genre == 'Puzzle',]
BoardGame <- data[data$genre == 'BoardGame',]
Simulation <- data[data$genre == 'Simulation',]

summary(Sports$viewers)
summary(RealTimeStrategy$viewers)
summary(Adventure$viewers)
summary(Racing$viewers)
summary(RolePlaying$viewers)
summary(Puzzle$viewers)
summary(BoardGame$viewers)
summary(Simulation$viewers)

'''
> summary(Sports$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    744   10013   17702   20551   28892   73823 
> summary(RealTimeStrategy$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   29222   58455   71295  101624  265762 
> summary(Adventure$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1556   18658   43104   48818   65645  174173 
> summary(Racing$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   24272   55209   60084   89297  239221 
> summary(RolePlaying$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4093   58685  112080  142362  199725  632204 
> summary(Puzzle$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0    3874    9080   10681   14532   44890 
> summary(BoardGame$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   19116   55134   65400  107428  273363 
> summary(Simulation$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   24460   63848   72107  112673  262616
'''

frame_genre <- list(Sports$viewers, RealTimeStrategy$viewers,
                    Adventure$viewers, Racing$viewers,
                    RolePlaying$viewers, Puzzle$viewers,
                    BoardGame$viewers, Simulation$viewers)
attributes(frame_genre) <- list(row.names=1:204, class='data.frame')

boxplot(frame_genre, names = c('Sports', 'Real_T_Strat',' Adventure', 
                      'Racing', 'Role_P', 'Puzzle', 
                      'Board_G', 'Simulation'), ylab = 'Number of viewers', 
        main = 'Number of viewers depending on genre', xlab = 'Genre')

#Season
# unique(data$season)
# [1] "Spring" "Summer" "Autumn" "Winter"
Spring <- data[data$season == 'Spring',]
Summer <- data[data$season == 'Summer',]
Autumn <- data[data$season == 'Autumn',]
Winter <- data[data$season == 'Winter',]

summary(Spring$viewers)
summary(Summer$viewers)
summary(Autumn$viewers)
summary(Winter$viewers)
'''
> summary(Spring$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   11368   29342   46996   64959  632204 
> summary(Summer$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   10039   36952   56826   84286  469265 
> summary(Autumn$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   17363   45686   67347  100580  534997 
> summary(Winter$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   21446   59918   77414  115674  545451 
'''

frame_season <- list(Winter$viewers, Spring$viewers, 
                     Summer$viewers, Autumn$viewers)
attributes(frame_season) <- list(row.names=1:410, class='data.frame')
boxplot(frame_season, names = c("Winter", 'Spring', 'Summer', 'Autumn'),
        xlab = 'Season', ylab = 'number of viewers',
        main = 'NUmber of viewers depeding on Season')



#Host
#unique(data$host)
#[1] "Player3" "Player2" "Player5" "Player4" "Player1"
Player_1 <- data[data$host == 'Player1',]
Player_2 <- data[data$host == 'Player2',]
Player_3 <- data[data$host == 'Player3',]
Player_4 <- data[data$host == 'Player4',]
Player_5 <- data[data$host == 'Player5',]
'''
> max(Player_1$viewers)
[1] 632204
> max(Player_2$viewers)
[1] 497321
> max(Player_3$viewers)
[1] 442053
> max(Player_4$viewers)
[1] 369046
> max(Player_5$viewers)
[1] 545451
> min(Player_1$viewers)
[1] 0
> min(Player_2$viewers)
[1] 0
> min(Player_3$viewers)
[1] 0
> min(Player_4$viewers)
[1] 7901
> min(Player_5$viewers)
[1] 0
'''
#Player 4 has the smallest maximum of viewers but also the biggest minimum of viewers. They all experienced 0 viewers during a live stream, except him

frame_player <- list(Player_1$viewers, Player_2$viewers, 
                     Player_3$viewers, Player_4$viewers, 
                     Player_5$viewers)
attributes(frame_player) <- list(row.names = 1:329, class = 'data.frame')
boxplot(frame_player, names = sort(unique(data$host)),
        xlab = 'Hosts', ylab = 'Number of viewers', 
        main = 'Number of viewers depending on host')

#Days
Monday <- data[data$day == 'Mon',]
Tuesday <- data[data$day == 'Tue',]
Wednesday <- data[data$day == 'Wed',]
Thursday <- data[data$day == 'Thu',]
Friday <- data[data$day == 'Fri',]
Saturday <- data[data$day == 'Sat',]
Sunday <- data[data$day == 'Sat',]

summary(Monday$viewers)
summary(Tuesday$viewers)
summary(Wednesday$viewers)
summary(Thursday$viewers)
summary(Friday$viewers)
summary(Saturday$viewers)
summary(Sunday$viewers)

'''
> summary(Monday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   13049   35808   54064   73854  340663 
> summary(Tuesday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    744   13834   33373   52069   82892  258230 
> summary(Wednesday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   10121   28985   48017   74323  267012 
> summary(Thursday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1303   11489   30970   46974   67945  346992 
> summary(Friday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1348   13706   33754   57744   86746  442053 
> summary(Saturday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   28340   63195   98935  137022  632204 
> summary(Sunday$viewers)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0   28340   63195   98935  137022  632204 
'''

frame_days <- list(Monday$viewers, Tuesday$viewers, 
                   Wednesday$viewers, Thursday$viewers, 
                   Friday$viewers, Saturday$viewers,
                   Sunday$viewers)
attributes(frame_days) <- list(row.names = 1:236, class = 'data.frame')



#Player and genre of games
table(Player_1$genre)
table(Player_2$genre)
table(Player_3$genre)
table(Player_4$genre)
table(Player_5$genre)
Play_and_G <- data.frame(as.vector(table(Player_1$genre)), as.vector(table(Player_2$genre)),
                         as.vector(table(Player_3$genre)), as.vector(table(Player_4$genre)),
                         as.vector(table(Player_5$genre)), 
                         row.names = sort(unique(data$genre)))
colnames(Play_and_G) <- sort(unique(data$host))


#Player and season
table(Player_1$season)
table(Player_2$season)
table(Player_3$season)
table(Player_4$season)
table(Player_5$season)
Play_and_S <- data.frame(as.vector(table(Player_1$season)), as.vector(table(Player_2$season)),
                         as.vector(table(Player_3$season)), as.vector(table(Player_4$season)),
                         as.vector(table(Player_5$season)), 
                         row.names = sort(unique(data$season)))
colnames(Play_and_S) <- sort(unique(data$host))

#Player and days
table(Player_1$day)
table(Player_2$day)
table(Player_3$day)
table(Player_4$day)
table(Player_5$day)
Play_and_D <- data.frame(as.vector(table(Player_1$day)), as.vector(table(Player_2$day)),
                         as.vector(table(Player_3$day)), as.vector(table(Player_4$day)),
                         as.vector(table(Player_5$day)), 
                         row.names = sort(unique(data$day)))
colnames(Play_and_D) <- sort(unique(data$host))


boxplot(frame_days, names = c("Mon", "Tue", 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'), 
        xlab = 'Days', ylab = 'Number of viewers', 
        main = 'Number of viewers depending on day')

boxplot(frame_season, names = c("Winter", 'Spring', 'Summer', 'Autumn'),
        xlab = 'Season', ylab = 'number of viewers',
        main = 'NUmber of viewers depeding on Season')

boxplot(frame_player, names = sort(unique(data$host)),
        xlab = 'Hosts', ylab = 'Number of viewers', 
        main = 'Number of viewers depending on host')

boxplot(frame_genre, names = c('Sports', 'Real_T_Strat',' Adventure', 
                               'Racing', 'Role_P', 'Puzzle', 
                               'Board_G', 'Simulation'), 
        ylab = 'Number of viewers', xlab = 'Genre',
        main = 'Number of viewers depending on genre')


Play_and_S
Play_and_D
Play_and_G

#Guests >0
Guest_sup_0 <- data[data$guests >0, ]
No_Guest <- data[data$guests == 0, ]
par(mfrow = c(1,2))
plot(Guest_sup_0$viewers)
plot(No_Guest$viewers)
par(mfrow = c(1,1))

#More viewers where they are guests
plot(log(data$subscribers), log(data$viewers), main= 'Viewers against subscriber.',
     xlab = 'Log number of viewers', ylab = 'Log number of sunscribers')
plot(data$guests, data$viewers)
plot(data$ads_now, data$viewers)
plot(data$ads_last, data$viewers)

plot(data$subscribers, data$viewers, main = 'Viewers against subscriber',
     xlab = 'Number of subscribers', ylab = 'Number of Viewers')

part1 <- data[1:1000,]
part2 <- data[1000:1542,]



