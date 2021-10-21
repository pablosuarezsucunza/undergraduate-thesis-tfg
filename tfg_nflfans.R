library(tidyverse)
library(stringr)


#####################
#Import, clean and get df ready
#####################
         
fans = data.frame()     
for(i in 2011:2020){
  df = read.csv2(paste0('fans data/', i, ' fans.csv'), header = TRUE) %>% 
    select(-Total, -Home, -Away)%>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(cols = !Tm, names_to = 'Week', values_to = 'Attendence') %>% 
    slice(1:544) %>%
    mutate(Week = as.numeric(str_remove(Week, 'Week.')),
           Attendence = str_remove(Attendence, '[,.]'),
           year = rep(i, 544)) 
  fans = rbind(fans, df)
  print(i)
  remove(i, df)
}

fans = fans %>%  #get team names as in the other df to make uniting them easier
  mutate(Tm = case_when(Tm == 'San Diego Chargers' ~ 'Los Angeles Chargers',
                        Tm == 'St. Louis Rams' ~ 'Los Angeles Rams',
                        Tm == 'Oakland Raiders' ~ 'Las Vegas Raiders',
                        Tm == 'Washington Redskins' ~ 'Washington Football Team',
                        TRUE ~ Tm)) %>% 
  arrange(Tm) %>%
  mutate(Team = rep(c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET', 'GB', 
                      'HOU', 'IND', 'JAX', 'KC','LV', 'LAC', 'LAR', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 
                      'PHI', 'PIT', 'SF', 'SEA', 'TB', 'TEN', 'WAS'),
                    each = 170))


############
#get playoff attenndnace
############
#this was manually gathered for each playoff game, can be found in "playoff fands.csv"
pgames = read.csv2('fans data/playoff fans.csv', header = TRUE)




############
#Assign attendance to each individual game
###########

attendance = vector('list', nrow(nfl_filtered))
for(i in 1:nrow(nfl_selected)){
  df = fans %>%  filter(year == nfl_selected$season[i],
                        Week == nfl_selected$week[i],
                        Team %in% c(nfl_selected$home_team[i], nfl_selected$away_team[i]))
  if(nfl_selected$season_type[i] == 'REG'){
    attendance[[i]] = df$Attendence[1]
  } else{
    df2 = pgames %>% filter(game_id == nfl_selected$game_id[i])
    attendance[[i]] = df2$Attendance[1]
  }
  print(i)
  remove(i, df, df2)
}
attendance = data.frame(attendance = do.call(rbind, attendance))


remove(pgames,fans)

#########
#input into main df
########
nfl_selected =  cbind(nfl_selected, attendance)




