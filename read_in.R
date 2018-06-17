##########
# LOAD LIBRARIES and read in data
##########
library(broom)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(doParallel)
library(caret)
library(glmnet)
library(reshape2)
library(plotly)
library(scales)
library(MASS)
library(ineq)
library(reldist)
library(plotly)
library(RColorBrewer)

# read in player stats
temp_2016 <- read_csv('data/player_stat_2015_16.csv')
temp_2017 <- read_csv('data/player_stat_2016_17.csv')
temp_2018 <- read_csv('data/player_stat_2018_reg.csv')

# clean up cols
clean_cols <- function(dat_player){
  names(dat_player) <- tolower(names(dat_player))
  names(dat_player) <- gsub(' ', '_', names(dat_player))
  names(dat_player)[5] <- 'teams'
  names(dat_player)[3] <- 'player'
  
  # convert date
  dat_player$date <- as.Date(dat_player$date, format = '%m/%d/%Y')
  
  # get year
  dat_player$year <- as.factor(format(dat_player$date, format = '%Y'))
  
  # create a month variable 
  dat_player$month <- month(as.POSIXlt(dat_player$date))
  
  return(dat_player)
}

temp_2016 <- clean_cols(temp_2016)
temp_2017 <- clean_cols(temp_2017)
temp_2018 <- clean_cols(temp_2018)

# keep only regular season data by removing "playoffs" from data_set column
temp_2016 <- temp_2016[!grepl('Playoffs', temp_2016$data_set),]
temp_2017 <- temp_2017[!grepl('Playoffs', temp_2017$data_set),]
temp_2018 <- temp_2018[!grepl('Playoffs', temp_2018$data_set),]


# get avg points per minute and subset by only keeping people who plahyed over 25 games

# 2016
temp_group_2016 <- temp_2016 %>%
  group_by(teams, player) %>%
  summarise(mean_min = mean(min, na.rm = T),
            mean_points = mean(pts, na.rm = T),
            games_played = n()) %>%
  mutate(mean_pts_per_min = round((mean_points/mean_min), 2)) %>%
  dplyr::filter(games_played > 15)

# 2017
temp_group_2017 <- temp_2017 %>%
  group_by(teams, player) %>%
  summarise(mean_min = mean(min, na.rm = T),
            mean_points = mean(pts, na.rm = T),
            games_played = n()) %>%
  mutate(mean_pts_per_min = round((mean_points/mean_min), 2)) %>%
  dplyr::filter(games_played > 15)

# 2018
temp_group_2018 <- temp_2018 %>%
  group_by(teams, player) %>%
  summarise(mean_min = mean(min, na.rm = T),
            mean_points = mean(pts, na.rm = T),
            games_played = n()) %>%
  mutate(mean_pts_per_min = round((mean_points/mean_min), 2)) %>%
  dplyr::filter(games_played > 15)

rm(temp_2016, temp_2017, temp_2018)


# essentially each country is a team and each player is a household with income equal to points per minute
# lets make a lorenz curve for each team by looping through each team and apply the Lc function and store in a list for later
# temp_data <- temp_group_2016
# i = 1
get_lonrenz_gini <- function(temp_data){
  
  # define a list to store loop results 
  lorenz_list <- list()
  
  # define unique set of teams
  nba_teams <- unique(temp_data$teams)
  
  # loop through each team 
  for(i in 1:length(nba_teams)){
    team_name <- nba_teams[i]
    sub_team <- temp_data[temp_data$teams == team_name,]
    temp_L <- Lc(sub_team$mean_pts_per_min)
    temp_g <- round(reldist::gini(sub_team$mean_pts_per_min),3)
    l_data <- as.data.frame(cbind(temp_L$p, temp_L$L))
    names(l_data) <- c('p', 'l')
    l_data$gini <- temp_g
    l_data$team_name <- unique(sub_team$teams)
    lorenz_list[[i]] <- l_data
    
  }
  
  final_data <- do.call('rbind', lorenz_list)
  
  return(final_data)
}

# get lorenz curves for each team
l_2016 <- get_lonrenz_gini(temp_group_2016)
l_2017 <- get_lonrenz_gini(temp_group_2017)
l_2018 <- get_lonrenz_gini(temp_group_2018)

# now we have the lorenz curve info for each team and year and the corresponding gini coefficent and team name.

# 1) gini scores (histogram) and of easter and western conference bar graph ranked
# 2) Top and bottom 3 L curves for each year, based on unweighted gini
# 3) Gini for entire nba2018 season alone (just 2018) - team avg points per minute
# 4) All NBA avg gini increase or decrease (all years)
# 5) salary cap gini comapred to other countries




# 1) hist of gini and east west ranked gini 

#-----2018
# 2018 top: bucks, rockets, okc (highest gini, most unequal)

# 2018 bottom: nets, memphis, SA

# plot gini on bar plot
# first get unique gini for each team
temp_gini <- l_2018 %>%
  group_by(team_name) %>%
  summarise(gini = unique(gini))

# histogram of whole league 
ggplot(temp_gini, 
       aes(x=gini)) +
  geom_histogram(binwidth = 0.02, 
                 colour="#2874A6", 
                 fill="#5D6D7E", 
                 alpha = 0.6) + 
  labs(x = 'Gini Coefficient',
       y = '# of teams', 
       title = 'NBA season 2018',
       subtitle = 'Distribution of gini coefficients') +
  theme_wsj(title_family = 12)

# ranked east and west gini 
east_teams <- c('Atlanta', 'Boston', 'Brooklyn', 'Charlotte', 'Chicago', 'Cleveland', 'Detroit', 'Indiana', 'Miami', 'Milwaukee', 'New York', 'Orlando',
                'Philadelphia', 'Toronto', 'Washington')
west_teams <- c('Houston', 'Golden State', 'Dallas', 'Denver', 'LA Clippers', 'LA Lakers', 'Memphis', 'Minnesota', 'New Orleans', 'Oklahoma City', 
                'Pheonix', 'Portland', 'Sacramento', 'San Antonio', 'Utah')

atlantic <- c('Toronto', 'Boston', 'Philadelphia', 'New York', 'Brooklyn')
central <- c('Cleveland', 'Indiana', 'Milwaukee', 'Detroit', 'Chicago')
southeast <- c('Miami', 'Washington', 'Charlotte', 'Orlando', 'Atlanta')

northwest <- c('Portland', 'Oklahoma City', 'Utah', 'Minnesota', 'Denver')
pacific <- c('Golden State', 'LA Clippers', 'LA Lakers', 'Sacramento', 'Phoenix')
southwest <- c('Houston', 'New Orleans', 'San Antonio', 'Dallas', 'Memphis')


# create variable in temp gini for east or west team 
temp_gini$conf <- ifelse(temp_gini$team_name %in% east_teams, 'East', 'West')

# create variable in temp gini for east or west team 
temp_gini$div <- ifelse(temp_gini$team_name %in% atlantic, 'Atlantic', 
                        ifelse(temp_gini$team_name %in% central, 'Central',
                               ifelse(temp_gini$team_name %in% southeast, 'Southeast',
                                      ifelse(temp_gini$team_name %in% northwest, 'Northwest',
                                             ifelse(temp_gini$team_name %in% pacific, 'Pacific', 'Southwest')))))

# get avg gini for east and west 
temp_avg_div <- temp_gini %>%
  group_by(div)%>%
  summarise(mean_gini = round(mean(gini), 2))

# get avg gini for east and west 
temp_avg_conf <- temp_gini %>%
  group_by(conf)%>%
  summarise(mean_gini = round(mean(gini), 2))

# pie chart
pie_plot <- function(temp_dat){
  
  # change to v1
  names(temp_dat)[1] <- 'V1'
  
  # get inside font 
  f <- list(
    family = "Ubuntu",
    size = 20,
    color = "white")
  
  # get color 
  if(length(unique(temp_dat$V1)) == 2){
    pie_cols <- c("#2874A6", "#5D6D7E")
     
  } else {
    pie_cols <- colorRampPalette(brewer.pal(9, 'Dark2'))(length(unique(temp_dat$V1)))

  }
  
  plot_ly(temp_dat, labels = ~V1, values = ~mean_gini,
          marker = list(colors = pie_cols),
                          type ='pie',
                          hole = 0.4,
                          textposition = 'inside',
                          insidetextfont = f,
                          textinfo = 'value',
                          hoverinfo = 'label')  %>%
    
    config(displayModeBar = F) %>%
    
    layout(title ='' , font = '', showlegend = T,
           annotations = list(
             showarrow = FALSE,
             text = 'Gini Coefficients',
             font = list(color = '#1F2023',
                         family = 'sans serif')), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

# here are charts
pie_plot(temp_avg_conf)
pie_plot(temp_avg_div)


# now do barplot ranking gini with both east and west serately 
temp_gini$gini <- round(temp_gini$gini, 2)
east_data <- temp_gini %>% dplyr::filter(conf == 'East')
west_data <- temp_gini %>% dplyr::filter(conf == 'West')

# use ggplot bar for east data
east_plot <- ggplot(data = east_data,
            aes(x = reorder(team_name, gini),
                y = gini)) +
  geom_bar(stat = 'identity', 
           colour = 'black', 
           fill = '#52A7DF',
           alpha = 0.8) +
  geom_text(aes(label = gini), hjust = 1.1) +
  labs(x = '', y = ' ', title = 'Highest gini coefficients',
       subtitle = 'Eastern conference') + 
  coord_flip() + theme_solarized()


# use ggplot bar for east data
west_plot <- ggplot(data = west_data,
            aes(x = reorder(team_name, gini),
                y = gini)) +
  geom_bar(stat = 'identity', 
           colour = 'black', 
           fill = '#DC4220',
           alpha = 0.8) +
  geom_text(aes(label = gini), hjust = 1.1) +
  labs(x = '', y = ' ', title = 'Highest gini coefficients',
       subtitle = 'Western conference') + 
  coord_flip() + theme_solarized()

###############################################################
# now get the Lc curves for top teams OKC vs memphis
okc <- l_2018 %>% dplyr::filter(team_name == 'Oklahoma City')
memphis <- l_2018 %>% dplyr::filter(team_name == 'Memphis')

# lc curve for okc 
ggplot(okc, aes(p, l)) +
  geom_smooth(se = F, color = '#DC4220', size = 2, alpha = 0.7) +
  geom_abline(a = 0, b = 1, linetype = 'dashed') +
  labs(x = 'Cumulative share of players from lowest to highest points per minute',
       y = 'Cumulative share of points per minute', title = 'Lorenz Curve',
       subtitle = 'Oklahoma City Thunder') + 
       theme_solarized(base_size = 15)

# lc curve for memphis
ggplot(memphis, aes(p, l)) +
  geom_smooth(se = F, color = 'darkblue', size = 2, alpha = 0.6) +
  geom_abline(a = 0, b = 1, linetype = 'dashed') +
  labs(x = 'Cumulative share of players from lowest to highest points per minute',
       y = 'Cumulative share of points per minute', title = 'Lorenz Curve',
       subtitle = 'Memphis Grizzlies') + 
  theme_solarized(base_size = 15)

##################################################
# Entire NBA wins as income
# read in team data
team_wins <- read_csv('data/team_win_percentage.csv')
team_wins$team <- gsub('*', '', team_wins$team, fixed = TRUE)

# get lorenz curve and gini coefficent
lc_team_wins <- Lc(team_wins$W)
lc_team_wins <-as.data.frame(cbind(lc_team_wins$p, lc_team_wins$L))
gini_team_wins <- round(reldist::gini(team_wins$W),3)


# lc curve for all nba
ggplot(lc_team_wins, aes(V1, V2)) +
  geom_smooth(se = F, color = 'darkblue', size = 2, alpha = 0.6) +
  geom_abline(a = 0, b = 1, linetype = 'dashed') +
  labs(x = 'Cumulative share of teams from lowest to highest wins',
       y = 'Cumulative share of wins', title = 'Lorenz Curve',
       subtitle = 'Entire NBA') + 
  theme_solarized(base_size = 15)

# plot total wins against gini coeficient
# use temp_gini and team_wins
length(unique(team_wins$team))
length(unique(temp_gini$team_name))

team_wins <- team_wins[order(team_wins$team),]
team_dat <- cbind(temp_gini, wins = team_wins$W)

# plot against each other with regression line
# lc curve for all nba
summary(lm(wins~gini, data = team_dat))
# y = 16.74 + 145.12*X
# if x is .15 then equation is 38.5
# if x is .25 then equation is 53.08
# so increasing x by .10, increases y on avg by 14.5 
ggplot(team_dat, aes(gini, wins)) +
  geom_point(color = 'darkblue', size = 2, alpha = 0.6) +
  geom_smooth(method = 'lm', linetype = 'dashed', color = '#DC4220')+
  labs(x = 'Gini coefficient',
       y = 'Total wins', title = 'Gini coefficient and wins',
       subtitle = '.42 correlation and .17 R-squared (from linear regression)') + 
  theme_solarized(base_size = 15)



##################################################
# Entire NBA total valuation
team_valuation <- read_csv('data/team_valuation.csv')

# edit out %, B, M, $
team_valuation$`Current Value` <- gsub('$', '', team_valuation$`Current Value`, fixed = TRUE)
team_valuation$`Current Value` <- gsub('B', '', team_valuation$`Current Value`, fixed = TRUE)
team_valuation$`Debt/Value` <- gsub('%', '', team_valuation$`Debt/Value`, fixed = TRUE)
team_valuation$Revenue <- gsub('$', '', team_valuation$Revenue, fixed = TRUE)
team_valuation$Revenue <- gsub('M', '', team_valuation$Revenue, fixed = TRUE)
team_valuation$`Operating Income` <- gsub('$', '', team_valuation$`Operating Income`, fixed = TRUE)
team_valuation$`Operating Income` <- gsub('M', '', team_valuation$`Operating Income`, fixed = TRUE)
team_valuation$`1-Yr Value Change`<- gsub('%', '', team_valuation$`1-Yr Value Change`, fixed = TRUE)
team_valuation$Rank<- gsub('#', '', team_valuation$Rank, fixed = TRUE)


# rename to capture info deleted 
names(team_valuation) <- c('rank', 'team', 'current_value_billions', 'one_yr_value_change', 'debt_over_value', 'revenue_millions', 'operating_income_millions')

# make numeric
team_valuation$current_value_billions <- as.numeric(team_valuation$current_value_billions)
team_valuation$one_yr_value_change <- as.numeric(team_valuation$one_yr_value_change)
team_valuation$debt_over_value <- as.numeric(team_valuation$debt_over_value)
team_valuation$revenue_millions <- as.numeric(team_valuation$revenue_millions)
team_valuation$operating_income_millions <- as.numeric(team_valuation$operating_income_millions)

# get lorenz curve and gini for current_value_billions, revenue_millions, operating_income_millions
team_val_lc <- Lc(team_valuation$operating_income_millions)
tot_val_lc <- as.data.frame(cbind(team_val_lc$p, team_val_lc$L))

tot_op_gini <- round(reldist::gini(team_valuation$operating_income_millions),3)
tot_rev_gini <- round(reldist::gini(team_valuation$revenue_millions),3)
tot_val_gini <- round(reldist::gini(team_valuation$current_value_billions),3)


# get team wins and plot each metric against them
team_valuation <- team_valuation[order(team_valuation$team),]
team_wins <- team_wins[order(team_wins$team),]

team_value <- as.data.frame(cbind(team_valuation, wins = team_wins$W))


summary(lm(wins~operating_income_millions, data = team_value))
cor(team_value$operating_income_millions, team_value$wins)
# y = 16.74 + 145.12*X
# if x is .15 then equation is 38.5
# if x is .25 then equation is 53.08
# so increasing x by .10, increases y on avg by 14.5 
summary(lm(wins~operating_income_millions, data = team_value))
cor(team_value$operating_income_millions, team_value$wins)

ggplot(team_value, aes(operating_income_millions, wins)) +
  geom_point(color = 'darkblue', size = 2, alpha = 0.6) +
  geom_smooth(method = 'lm', linetype = 'dashed', color = '#DC4220')+
  labs(x = 'Operating income (millions)',
       y = 'Total wins 2018', title = 'Operating income and wins',
       subtitle = 'Weak positive correlation: .14') + 
  theme_solarized(base_size = 15)


summary(lm(wins~current_value_billions, data = team_value))
cor(team_value$current_value_billions, team_value$wins)

ggplot(team_value, aes(current_value_billions, wins)) +
  geom_point(color = 'darkgreen', size = 2, alpha = 0.6) +
  geom_smooth(method = 'lm', linetype = 'dashed', color = '#DC4220')+
  labs(x = 'Current value (billions)',
       y = 'Total wins 2018', title = 'Current value and wins',
       subtitle = 'No strong correlation') + 
  theme_solarized(base_size = 15)


##################################################
# Entire NBA salary cap
team_cap <- read_csv('data/team_total_salary_cap.csv')

# clean up $
team_cap$active_cap<- gsub('$', '', team_cap$active_cap, fixed = TRUE)
team_cap$active_cap<- gsub(',', '', team_cap$active_cap, fixed = TRUE)
team_cap$total_cap<- gsub('$', '', team_cap$total_cap, fixed = TRUE)
team_cap$total_cap<- gsub(',', '', team_cap$total_cap, fixed = TRUE)

# make numeric
team_cap$active_cap <- as.numeric(team_cap$active_cap)
team_cap$total_cap <- as.numeric(team_cap$total_cap)

# get difference between active and total
team_cap$diff <- team_cap$total_cap - team_cap$active_cap


# get lorenz curve and gini for total_cap and active_cap
temp_lc <- Lc(team_cap$total_cap)
tot_cap_gini <- round(reldist::gini(team_cap$total_cap),3)
tot_cap_lc <- as.data.frame(cbind(temp_lc$p, temp_lc$L))

# get team wins and plot each againes total_cap, active_cap, and diff
team_cap <- team_cap[order(team_cap$team),]
team_wins <- team_wins[order(team_wins$team),]

team_cap_wins <- as.data.frame(cbind(team_cap, wins = team_wins$W))

summary(lm(wins~total_cap, data = team_cap_wins))
cor(team_cap_wins$wins, team_cap_wins$total_cap)

options(scipen = 999)
team_cap_wins$total_cap_mil <- round((team_cap_wins$total_cap)*.000001, 2)

ggplot(team_cap_wins, aes(total_cap_mil, wins, lty = 'Gini = .08')) +
  geom_point(color = 'black', size = 3, alpha = 0.6) +
  geom_smooth(method = 'lm', linetype = 'dashed', color = '#DC4220')+
  labs(x = 'Total cap (millions)',
       y = 'Total wins 2018', title = 'Total cap and wins',
       subtitle = '0.54 correlation and .30 R-squared from linear regression') + 
  theme_solarized(base_size = 15) + 


#######################################################
# get nba player salary
# nba_salaries_1.csv and nba_salaries_2.csv (former has up to 2017, latter has up to 2018. compare)
nba_salary <- read_csv('data/nba_salaries_2.csv')

# get rid of columns
nba_salary$season_end <- NULL
nba_salary$team <- NULL

# make year a character 
nba_salary$season_start <- as.character(nba_salary$season_start)

# loop through years and get gini and Lc object for each year
nba_years <- unique(nba_salary$season_start)
year_list <- list()
i = 1
for(i in 1:length(nba_years)){
  this_year <- nba_years[i]
  sub_season <- nba_salary[nba_salary$season_start == this_year,]
  temp_g <- round(reldist::gini(sub_season$salary),3)
  temp_L <- Lc(sub_season$salary)
  l_data <- as.data.frame(cbind(temp_L$p, temp_L$L))
  names(l_data) <- c('p', 'l')
  l_data$gini <- temp_g
  l_data$year <- unique(sub_season$season_start)
  year_list[[i]] <- l_data
  
}

nba_gini <- do.call('rbind', year_list)

# get year and gini
nba_gini_year <- nba_gini %>%
  group_by(year) %>%
  summarise(gini = unique(gini))

ggplot(nba_gini_year, aes(year, gini)) +
  geom_point(color = 'darkblue', size = 3, alpha = 0.6) +
  geom_smooth(method = 'lm', linetype = 'dashed', color = '#DC4220')+
  labs(x = '',
       y = 'Gini coefficient', title = 'NBA Salaries 1990 - 2018',
       subtitle = 'Gin coefficient over time') + 
  theme_solarized(base_size = 15) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# get year and Lc - 1990 and 2017
temp_90 <- nba_gini[nba_gini$year == '1990',]
temp_18 <- nba_gini[nba_gini$year == '2017',]


# lc curve for all nba
ggplot(temp_90, aes(p, l)) +
  geom_smooth(se = F, color = 'darkblue', size = 2, alpha = 0.6) +
  geom_abline(a = 0, b = 1, linetype = 'dashed') +
  labs(x = 'Cumulative share of teams from lowest to highest salaries',
       y = 'Cumulative share of salaries earned', title = 'Lorenz Curve NBA salaries',
       subtitle = 'Entire NBA 1990') + 
  theme_solarized()

# lc curve for all nba
ggplot(temp_18, aes(p, l)) +
  geom_smooth(se = F, color = 'orange', size = 2, alpha = 0.6) +
  geom_abline(a = 0, b = 1, linetype = 'dashed') +
  labs(x = 'Cumulative share of teams from lowest to highest salaries',
       y = 'Cumulative share of salaries earned', title = 'Lorenz Curve NBA salaries',
       subtitle = 'Entire NBA 2018') + 
  theme_solarized()


# 
# # get all year combined
# temp_all <- rbind(l_2016,
#                   l_2017,
#                   l_2018)
# 
# # group by team and get mean gini and gini_weighted
# temp_all <- temp_all %>%
#   group_by(team_name) %>%
#   summarise(gini = round(mean(gini, na.rm = T), 3),
#             gini_weighted = round(mean(gini_weighted, na.rm = T), 3))
# 
# 
# # define x and y axis labels for graph
# y_text <- list(
#   title = "Weighted",
#   showticklabels = TRUE,
#   tickangle = 0,
#   exponentformat = "E"
# )
# 
# x_text <- list(
#   title = "Unweighted",
#   showticklabels = TRUE,
#   tickangle = 0,
#   exponentformat = "E"
# )
# 
# 
# # plot weighted vs unweighted
# plot_ly(data = temp_all, x = ~gini, y = ~gini_weighted,
#         type = 'scatter',
#         mode = 'markers',
#         text = ~paste('Team: ', team_name,
#                       '<br> Unweighted Gini: ', gini,
#                       '<br> Weighted Gini: ', gini_weighted),
#         hoverinfo = 'text',
#         marker = list(size = 10,
#                       color = '#DF4217',
#                       line = list(color = '#1E59D8',
#                                   width = 1))) %>%
#   layout(title = 'Weighted vs Unweighted Gini Coef',
#          yaxis = y_text,
#          xaxis = x_text)
# 
# 
