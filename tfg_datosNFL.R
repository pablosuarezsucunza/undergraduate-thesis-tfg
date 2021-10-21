library(tidyverse)
library(nflfastR)
library(rvest)
library(stringr)
library(zoo)



#original link to data
readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#getting the data
nfl = data.frame()
for (year in seq(2011,2020,1)) {
  nflseasondata <- readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_', year,'.rds')))
  nfl = rbind(nfl, nflseasondata)
  print(year)
  remove(nflseasondata, year)
}
remove(year)
nfl_variables = data.frame(colnames(nfl))


nfl_filtered = nfl %>% 
  filter(!is.na(epa),!is.na(wpa), !is.na(down), play_type %in% c("pass", "run"))
#--------------------------------------------------------------------------------------------------

#creating the "optimal play" variable
by_down_distance = nfl_filtered%>%  #groupng average epa for situation and play type
  filter(!is.na(epa), !is.na(down), play_type %in% c("pass", "run")) %>% 
  mutate(situation = paste0(down, "and", ydstogo)) %>% 
  group_by(situation, play_type) %>% 
  summarise(epa = mean(epa), wpa = mean(wpa, na.rm = TRUE), n = n())

#-------------------------------------------------------------------------------------------------
#find the pass advantage over the run each down and distance, if > 0 better to pass, if < 0 better run.

passing_advantage = data.frame() 
for (sit in unique(by_down_distance$situation)){
  this_one = by_down_distance %>% 
    filter(situation == sit)
  if(nrow(this_one) == 2){
    epa_diff = this_one$epa[1] - this_one$epa[2]
    wpa_diff = this_one$wpa[1] - this_one$wpa[2]
  } else {
    epa_diff = this_one$epa
    wpa_diff = this_one$wpa
  }
  situation = sit
  passing_advantage = rbind(passing_advantage, data.frame(situation, wpa_diff, epa_diff))
}   
remove(row, epa_diff, wpa_diff, situation, sit, this_one)
passing_advantage = passing_advantage %>% 
  mutate(down = as.numeric(str_sub(situation, 1, 1)),
         ydstogo = as.numeric(str_sub(situation, 5))) %>% 
  select(-situation)
 
#--------------------------------------------------------------------------------------------------
#putting the differences in the pbp dataset
epa_diff = vector("list", nrow(nfl_filtered)) 
wpa_diff = vector("list", nrow(nfl_filtered)) 
all_plays_difference = data.frame()
for(row in 1:nrow(nfl_filtered)){
  padvtg = passing_advantage %>% 
    filter(passing_advantage$down == as.numeric(paste(nfl_filtered$down[row])),
           passing_advantage$ydstogo == as.numeric(paste(nfl_filtered$ydstogo[row])))
  
  epa_diff[[row]] = as.numeric(paste(padvtg$epa[1]))
  wpa_diff[[row]] = as.numeric(paste(padvtg$wpa[1]))
  print(row)
}
all_plays_difference = cbind(rbind(all_plays_difference, do.call(rbind, epa_diff)),
                             wpa_diff = rbind(do.call(rbind, wpa_diff)))
all_plays_difference = all_plays_difference %>% rename(epa_diff = V1)
nfl_filtered = cbind(nfl_filtered, all_plays_difference)
remove(padvtg, row, epa_diff, wpa_diff)

#-----------------------------------------------------------------------------------------------

nfl_selected = nfl_filtered %>% #shaping the final dataset
  select(game_id, home_team, away_team, season_type, week, posteam, defteam, down, ydstogo, play_type,
         pass, rush, ep, epa, wp, wpa, epa_diff, wpa_diff) %>% 
  mutate(choice_epa = case_when(pass == 1 & epa_diff > 0 ~ "Right",
                                pass == 1 & epa_diff < 0 ~ "Wrong",
                                rush == 1 & epa_diff < 0 ~ "Right",
                                rush == 1 & epa_diff > 0 ~ "wrong"),
         choice_wpa = case_when(pass == 1 & wpa_diff > 0 ~ "Right",
                                pass == 1 & wpa_diff < 0 ~ "Wrong",
                                rush == 1 & wpa_diff < 0 ~ "Right",
                                rush == 1 & wpa_diff > 0 ~ "wrong"))%>% 
  mutate(choice_epa_no = ifelse(choice_epa == "Right", 1,0),
         choice_wpa_no = ifelse(choice_wpa == "Right", 1,0))
nfl_selected = nfl_selected %>% 
  mutate(season = str_sub(game_id, 1, 4))

mean(nfl_selected$choice_epa_no, na.rm = TRUE)
mean(nfl_selected$choice_wpa_no, na.rm = TRUE)



#----------------------------------------------------------------------------------------------------
#getting coaches and coaches experience

teams_by_season = nfl_selected %>% 
  group_by(posteam, across(season)) %>% 
  summarise(choice_epa_no = mean(choice_epa_no),
            choice_wpa_no = mean(choice_wpa_no), n = n()) %>% 
  mutate(team_name = case_when(posteam == "NE" ~ "NWE", posteam == "LV" ~ "RAI", posteam == "TEN" ~ "OTI",
                               posteam == "IND" ~ "CLT",posteam == "HOU" ~ "HTX",posteam == "BAL" ~ "RAV",
                               posteam == "KC" ~ "KAN",posteam == "LAC" ~ "SDG",posteam == "NO" ~ "NOR",
                               posteam == "TB" ~ "TAM",posteam == "NE" ~ "NWE",posteam == "GB" ~ "GNB",
                               posteam == "LA" ~ "RAM",posteam == "ARI" ~ "CRD",posteam == "SF" ~ "SFO",
                               class(coaches$posteam) == "character" ~ posteam)) %>% 
  mutate(team_name = tolower(team_name)) 

coaches = data.frame()   #gets link to every coach
for (team in unique(teams_by_season$team_name)){ #getting links to each coach
   for(year in unique(teams_by_season$season)){
     link = paste0("https://www.pro-football-reference.com/teams/", team,"/", year,".htm")
     page = read_html(link)
     record = page %>% html_nodes('.prevnext+ p') %>% html_text()
     coach_name = page %>% html_nodes('#meta p:nth-child(4) a') %>% html_text()
     coach_link = page %>% html_nodes('#meta p:nth-child(4) a') %>% html_attr('href')
     coaches = rbind(coaches, data.frame(record[1], coach_name[1], coach_link[1]))
   }
  print(team)
 }
remove(coach_name, coach_link, team, year, link, page, record)
coaches = coaches %>% 
  mutate(record.1. = ifelse(str_sub(coaches$record.1., 13, 13) == "-",
                          str_sub(coaches$record.1., 12, 12),
                          str_sub(coaches$record.1., 12, 13))) %>% 
  rename(record = record.1.) %>% 
  mutate(record = as.numeric(record))



experience = data.frame()   #gets expirience of every coach
for (cl in coaches$coach_link){
  link = paste0("https://www.pro-football-reference.com/", cl, "")
  page = read_html(link)
  exp = page %>% html_nodes('#coaching_results tfoot th') %>% html_text()
  years_coached = page %>% html_nodes('#coaching_results th.left a') %>% html_text()
  years_coached = paste(years_coached, sep = ',', collapse = ',')
  experience = rbind(experience, data.frame(cl, exp[1], years_coached[1]))
  print(cl)
} 
remove(link, page, exp, cl, years_coached)
experience = experience %>% 
  mutate(exp = ifelse(str_length(exp.1.) == 5, as.numeric(str_sub(exp.1., 1, 1)), 
                      as.numeric(str_sub(exp.1., 1, 2)))) %>% 
  select(-exp.1.)

experience = data.frame(team = teams_by_season$posteam, season = teams_by_season$season,
                        coach = coaches$coach_name.1., wins = coaches$record, 
                        years_coached = experience$years_coached.1., 
                        experience = experience$exp)


location = data.frame(str_locate(experience$years_coached, paste0(experience$season)))
experience = cbind(experience, location)
experience = cbind(experience, 
                   data.frame(years_coached_before = str_sub(experience$years_coached , 1, experience$end)))
experience = cbind(experience, 
                   data.frame(pastexperience = (str_length(experience$years_coached_before) + 1) / 5))
remove(location)
teams_by_season = cbind(teams_by_season,
                        data.frame(coach = experience$coach, wins = experience$wins,
                                   coachexp = experience$pastexperience))

    #input coaches exp in big dataset.
coachexp = vector("list", nrow(nfl_selected))
coach = vector("list", nrow(nfl_selected))
all_plays_coach = data.frame()
for (i in 1:nrow(nfl_selected)){
  pt = paste(nfl_selected$posteam[i])
  sea = as.numeric(paste(nfl_selected$season[i]))
  h = data.frame(experience) %>% 
    filter(team == pt, 
           season == sea)
  coachexp[[i]] = as.numeric(paste(h$pastexperience))
  coach[[i]] = paste(h$coach)
  print(i)
}
all_plays_coach = 
  all_plays_coach = rbind(all_plays_coach, cbind(V1 = do.call(rbind, coachexp),
                                                 V2 = do.call(rbind, coach)))
nfl_selected = nfl_selected %>% mutate(coachexp = as.numeric(all_plays_coach$V1),
                                       coach = all_plays_coach$V2)


remove(i, h, coachexp, pt, sea, coach, all_plays_coach)

#------------------------------------------------------------------------------------------------
#accounting for OCs

# ocs = data.frame()   #gets link to every OC
# for (team in unique(teams_by_season$team_name)){ #getting links to each coach
#   for(year in unique(teams_by_season$season)){
#     link = paste0("https://www.pro-football-reference.com/teams/", team,"/", year,".htm")
#     page = read_html(link)
#     record = page %>% html_nodes('.prevnext+ p') %>% html_text()
#     oc_name = page %>% html_nodes('p:nth-child(9) a') %>% html_text()
#     oc_link = page %>% html_nodes('p:nth-child(9) a') %>% html_attr('href')
#     ocs = rbind(ocs, data.frame(team, year, record[1], oc_name[1], oc_link[1]))
#   }
#   print(team)
# }
# remove(oc_name, oc_link, team, year, link, page, record)
# ocs = ocs %>%
#   mutate(record.1. = ifelse(str_sub(ocs$record.1., 13, 13) == "-",
#                             str_sub(ocs$record.1., 12, 12),
#                             str_sub(ocs$record.1., 12, 13))) %>%
#   rename(record = record.1.) %>%
#   mutate(record = as.numeric(record))


#------------------------------------------------------------------------------------------------
#creating new right/wrong variable
#
nfl_selected = nfl_selected %>% 
  mutate(degree_epa = ifelse(choice_epa_no == 1, abs(epa_diff), -abs(epa_diff)))



nfl_selected = nfl_selected %>% 
  mutate(degree_wpa = ifelse(choice_wpa_no == 1, abs(wpa_diff), -abs(wpa_diff)))



#----------------------------------------------------------------------------------------------------
#get current wins in each game
  # result > 0 is a home win, < 0 an away win and = 0 a tie.


game_result = nfl_filtered %>%    #determine if team with on offense won or loss
  select(game_id, home_team, away_team, posteam, result) %>% 
  mutate(posteam_win = case_when(result == 0 ~ 0.5,
                                home_team == posteam & result > 0 ~ 1,
                                away_team == posteam & result < 0 ~ 1,
                                home_team == posteam & result < 0 ~ 0,
                                away_team == posteam & result > 0 ~ 0)) %>% 
  select(game_id, posteam, posteam_win) %>% 
  group_by(game_id, across (posteam)) %>% 
  summarise(posteam_win = mean(posteam_win)) %>% 
  mutate(season = as.numeric(str_sub(game_id, 1, 4)),
         week = as.numeric(str_sub(game_id, 6, 7)))


cumwins = data.frame()
for (y in unique(game_result$season)){   #calculate accumulated wins
  df = game_result %>% 
    filter(season == y) %>% 
    select(posteam, posteam_win) %>% 
    group_by(posteam) %>% 
    mutate(acc_wins = cumsum(posteam_win)) %>%  
    mutate(week = as.numeric(str_sub(game_id, 6, 7))) 
  cumwins = rbind(cumwins, df)
  print(y)
  remove(y, df)
}

#input the wins into the main df
wins = vector('list', nrow(nfl_selected))
for (i in 1:nrow(nfl_selected)) {
  df = cumwins %>% filter(game_id == nfl_selected$game_id[i],
                          posteam == nfl_selected$posteam[i])
  wins[[i]] = df$acc_wins[1]
  
  print(i)
  remove(i,df)
}
do.call(rbind, wins)
nfl_selected = cbind(nfl_selected, data.frame(wins = do.call(rbind, wins)))

remove(game_result, wins)
#-----------------------------------------------------------------------------------------------------------
#Creating team strengths variable
team_strengths = nfl_selected %>% 
  group_by(season, posteam, play_type) %>% 
  summarise(epa = mean(epa),
            wpa = mean(wpa)) %>% 
  mutate(epa_lavg = case_when(play_type == 'pass' ~ mean(as.matrix(nfl_selected[nfl_selected$play_type == 'pass', 'epa'])),
                              play_type == 'run' ~ mean(as.matrix(nfl_selected[nfl_selected$play_type == 'run', 'epa']))),
         wpa_lavg = case_when(play_type == 'pass' ~ mean(as.matrix(nfl_selected[nfl_selected$play_type == 'pass', 'wpa'])),
                              play_type == 'run' ~ mean(as.matrix(nfl_selected[nfl_selected$play_type == 'run', 'wpa'])))) %>% 
  mutate(strength_epa = epa - epa_lavg,
         strength_wpa = wpa - wpa_lavg) %>% 
  select(season, posteam, play_type, strength_epa, strength_wpa)

  #input team strenghs for every play
strength_epa = vector("list", nrow(nfl_selected))
strength_wpa = vector("list", nrow(nfl_selected))

for (i in 1:nrow(nfl_selected)){
  data = team_strengths %>% 
    filter(posteam == nfl_selected$posteam[i],
          season == nfl_selected$season[i],
          play_type == nfl_selected$play_type[i])
  
  strength_epa[[i]] = as.numeric(paste(data$strength_epa))
  strength_wpa[[i]] = as.numeric(paste(data$strength_wpa))
  
  print(i)
}

all_plays_strength = cbind(data.frame(strenght_epa = do.call(rbind, strength_epa)),
                           data.frame(strenght_wpa = do.call(rbind, strength_wpa)))


remove(i, data, strength_epa, strength_wpa, team_strengths)



#---------------------------------------------------------------------------------------------------
#variable accounting for fans in each game is another script: nfl_fans.R
#---------------------------------------------------------------------------------------------------
#creting some variables of others
nfl_selected = nfl_selected %>% 
  mutate(home = ifelse(home_team == posteam, 1, 0)) %>% 
  mutate(home = as.factor(home))

#------------------------------------------------------------------------------------------------------
#final dataset
final = nfl_selected %>% 
  select(home, week, season_type, wins, play_type, epa, wpa, degree_epa, degree_wpa, 
         season, coach, coachexp, attendance)





#-------------------------------------------------------------------------------------------------------
#Removing used df to clean space
remove(h)
remove(nfl, fans, cumwins, coaches, wins)








































