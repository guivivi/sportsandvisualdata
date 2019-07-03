# Load packages:
library(tidyverse)
library(BAwiR)
library(forcats)
library(reshape2)

# Load data and source functions:
load("data_app_acb.RData")
source("do_recode_teams.R")

# ACB data:
df0 <- data_app_acb

# Number of threes every season:
df_pl <- df0 %>% 
  dplyr::filter(Type_season == "Regular Season") %>%
  group_by(Season) %>%
  summarise(sum_Three = sum(ThreePA), 
            num_teams = length(unique(Team)), 
            num_games = (num_teams - 1) * 2) %>%
  arrange(desc(Season))
df_pl1 <- df_pl %>%
  filter(Season %in% paste(1996:2018, 1997:2019, sep = "-"))

ggplot(df_pl1, aes(x = Season, y = sum_Three, group = 1)) +
  #geom_bar(stat = "identity", fill = "#FF9999", colour = "black") +
  geom_point() +
  geom_line() +
  geom_text(aes(label = sum_Three), vjust = -0.6, size = 1.9, color = "red") +
  labs(x = "", y = "Triples intentados") +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# Number of games with more threes than twos of every team:
df1 <- df0 %>% 
  dplyr::filter(Season == "2018-2019", Type_season == "Regular Season") %>%
  dplyr::select(GameID, Game, GameRes, Team, Player.x, TwoPA, ThreePA)

df2 <- df1 %>%
  mutate(Game = factor(Game, levels = unique(Game)),
         ID = as.integer(Game))
df3 <- df2 %>%
  group_by(ID, Game, GameRes, Team) %>%
  summarise(TwosPA = sum(TwoPA), 
            ThreesPA = sum(ThreePA)) %>%
  mutate(Home = strsplit(as.character(Game), " - ")[[1]][1], 
         Away = strsplit(as.character(Game), " - ")[[1]][2]) %>%
  separate(GameRes, c("Score_home", "Score_visitor"), remove = FALSE) %>%
  mutate(Score_home = as.numeric(Score_home),
         Score_visitor = as.numeric(Score_visitor),
         Win = ifelse(Score_home > Score_visitor, Home, Away))

df3_home <- df3 %>% ungroup() %>% select(Home) %>% rename(Team = Home)
df3_home1 <- do_recode_teams("ACB", df3_home)

df3_away <- df3 %>% ungroup() %>% select(Away) %>% rename(Team = Away)
df3_away1 <- do_recode_teams("ACB", df3_away)

df3_win <- df3 %>% ungroup() %>% select(Win) %>% rename(Team = Win)
df3_win1 <- do_recode_teams("ACB", df3_win)

df3$Home <- df3_home1$Team
df3$Away <- df3_away1$Team
df3$Win <- df3_win1$Team

df4 <- data.frame()
for (i in sort(unique(df3$Team))) {
  #print(i)
  aux <- df3 %>% 
    filter(Team == i, ThreesPA > TwosPA) %>%
    mutate(partidos = nrow(.))
  if (nrow(aux) == 0) {
    aux1 <- data.frame(Team = i, partidos = 0, ganados = 0, perdidos = 0)
  }else{
    aux$ganados <- sum(aux$Team == aux$Win)
    aux$perdidos <- sum(aux$Team != aux$Win)
    aux1 <- aux %>% 
      ungroup() %>% 
      select(Team, partidos, ganados, perdidos) %>% 
      distinct()    
  }
  
  df4 <- rbind(df4, aux1)
}
df5 <- df4 %>% 
  arrange(desc(partidos))
df6 <- melt(df5)
equipos <- unique(df6[df6$variable == "partidos", "Team"])
df6$Team <- factor(df6$Team, levels = equipos)
pos_vict <- c("15º (11)", "9º (17)", "5º (21)", "7º (18)", "4º (23)", "10º (16)", "14º (12)", 
              "1º (28)", "18º (9)", "16º (11)", "13º (13)", "12º (14)", "17º (10)", "8º (17)", 
              "2º (27)", "3º (26)", "11º (15)", "6º (18)")
equipos_clas <- paste(equipos, pos_vict)

df7 <- df6 %>%
  mutate(Team = plyr::mapvalues(Team, from = equipos, to = equipos_clas))

ggplot(data = df7, aes(x = variable, y = value)) +
  facet_wrap(~Team) +
  geom_bar(stat = "identity", color = "blue", fill = "white") +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  scale_y_continuous(limits = range(0, max(df6$value) + 10)) + 
  theme(axis.title = element_blank(), axis.text.x = element_text(size = 6)) +
  labs(title = "  ACB Temporada regular 2018-2019. \n  Número de partidos de cada equipo con más tiros de tres que de dos. \n  Victorias y derrotas en esos partidos.")

# Sebas Saiz' stats:
seb_sa <- get_stats_seasons(df0, "ACB", c("Saiz, Sebas"), 
                            c("GP", "MP", "TwoPA", "ThreePA", "ORB", "AST", "PIR"), 
                            "Regular Season", TRUE, FALSE)
seb_sa1 <- seb_sa$df_gg %>% select(-Age)
colnames(seb_sa1) <- c("Nombre", "Equipo", "Temporada", "Partidos", "Min.", 
                       "T2I", "T3I", "RebOfen.", "Asist.", "Valorac.")
seb_sa1

seb_sa_total <- df0 %>% filter(Type_season == "Regular Season", 
                               Season %in% c("2017-2018", "2018-2019"), 
                               Player.x == "Saiz, Sebas")
seb_sa_total1 <- do_add_adv_stats(seb_sa_total)
# By choosing Average instead of Total in do_stats I get seb_sa1.
seb_sa_total2 <- do_stats(seb_sa_total1, "Total", c("2017-2018", "2018-2019"), "ACB", "Regular Season") %>%
  ungroup() %>%
  select(Name, Team, Season, GP, MP, TwoPA, ThreePA, ORB, AST, PIR)
colnames(seb_sa_total2) <- c("Nombre", "Equipo", "Temporada", "Partidos", "Min.", 
                             "T2I", "T3I", "RebOfen.", "Asist.", "Valorac.")
seb_sa_total2

# Threes of teams coached by Txus Vidorreta (only seasons where Vidorreta coached 
# the entire season in the same team):
# http://www.acb.com/entrenador.php?id=AEX
df_team <- data.frame()
seasons <- paste(2004:2018, 2005:2019, sep = "-")
for (i in 1:length(seasons)) {
  #print(i)
  df_team_loop <- do_stats_teams(df0, seasons[i], "ACB", "Regular Season")
  df_team_total <- df_team_loop$df_team_total
  df_team_total$Season <- seasons[i]  
  
  df_team <- rbind(df_team, df_team_total)
}

df_team1 <- df_team %>%
  select(Team, ThreePA, Season) %>%
  filter(!Season %in% c("2009-2010", "2010-2011", "2015-2016")) %>%
  mutate(Team = ifelse(Season %in% c("2004-2005", "2005-2006", "2006-2007", 
                                     "2007-2008", "2008-2009") & Team == "Bilbao", 
                       paste(Team, " (Vidorreta)", sep = ""),
                       ifelse(Season %in% "2011-2012" & Team == "Alicante", 
                              paste(Team, " (Vidorreta)", sep = ""),
                              ifelse(Season %in% c("2012-2013", "2013-2014", "2014-2015") 
                                     & Team == "Estudiantes",
                                     paste(Team, " (Vidorreta)", sep = ""),
                                     ifelse(Season %in% c("2016-2017", "2018-2019") & Team == "Tenerife",
                                            paste(Team, " (Vidorreta)", sep = ""),
                                            ifelse(Season %in% c("2017-2018") & Team == "Valencia",
                                                   paste(Team, " (Vidorreta)", sep = ""), 
                                                   Team))))))

df_team2 <- df_team1 %>%
  group_by(Season) %>%
  arrange(Season, desc(ThreePA)) %>% 
  mutate(Rkg = ifelse(grepl("Vidorreta", Team), grep("Vidorreta", Team), Team))

df_team3 <- df_team2 %>%
  filter(grepl("Vidorreta", Team)) %>%
  arrange(desc(Season)) %>%
  select(Team, Season, ThreePA, Rkg) %>%
  rename(Clasific. = Rkg) %>%
  mutate(Team = gsub("\\(Vidorreta\\)", "", Team))
colnames(df_team3)[1:4] <- c("Equipo", "Temporada", "T3I", "Clasificación")
df_team3