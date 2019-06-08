# Load packages:
library(tidyverse)
library(lubridate)
library(plotly)

# Set working to source file location and source functions:
#setwd()
source("table_mp_pir.R")

# Load data:
load("data_app_acb.RData")

# Get ACB data:
df01 <- data_app_acb %>% 
  filter(Compet == "ACB", Type_season == "Regular Season")#, Season == "2018-2019")

# Select players of interest:
# Spanish players:
#players <- c("Abalde_Diaz_Alberto", "Alocen, Carlos", "Barreiro, J.", "Brizuela, Dario", 
#             "Diaz_Ortiz_Alberto", "Diop, Ilimane", "Fernandez_Bernabe_Jaime",
#             "Garcia, Marc", "Garcia, Sergi", "Lopez-Arostegui, X",
#             "Pauli, Oriol", "Saiz, Sebas", "Vicedo, Edgar", "Yusta, Santiago")
# Foreign players:
players <- c("Belemene, Romaric", "Birgander, Simon", "Brussino, Nico", "Burjanadze_Beqa", 
             "Cancar, Vlatko", "Cate, Emanuel", "Cvetkovic, A.", "Diagne, Moussa", 
             "Dimitrijevic, N.", "Eriksson, Marcus", "Garino, Patricio", "Hakanson, Ludde",
             "Hlinason, T.", "Niang, Mamadou", "Pangos, Kevin", "Poirier, Vincent", 
             "Radicevic, Nikola", "Radoncic, Dino", "Redivo, Lucio", "Rupnik, Luka", 
             "Sakho, Jordan", "Sedekerskis, Tadas", "Smits, Rolands", "Thomas, Matt", 
             "Tobey, Mike", "Todorovic, Dejan", "Vildoza, Luca", "Walker, David")

# Compute total and averaged minutes and PIR:
df02 <- df01 %>% 
  filter(Player.x %in% players) %>%
  group_by(Season, Player.x, CombinID) %>%
  mutate(GP = n()) %>%
  mutate(MP_oper = ifelse(all(MP == "0"), 
                          0,
                          sum(as.numeric(as.period(ms(MP), unit = "sec")), na.rm = TRUE))) %>%
  mutate(MP_oper_def = sprintf("%02d:%02d", MP_oper %/% 60, MP_oper %% 60)) %>%
  mutate(PIR_sum = sum(PIR)) %>%
  ungroup() %>%
  select(Season, Player.x, GP, MP_oper_def, PIR_sum) %>%
  rename(MP = MP_oper_def) %>%
  # To convert the minutes into numeric:
  mutate(MP = round(as.numeric(ms(MP), unit = "mins"))) %>%
  distinct() %>%
  mutate(MP_avg = round(MP / GP))  %>%
  mutate(PIR_avg = round(PIR_sum / GP, 1)) %>% 
  arrange(desc(Season), desc(GP))

tab1 <- table_mp_pir(df02, players[1])
tab1
#library(kableExtra)
#kable(tab1, align = "r")

# Split the PIR values in several intervals:
df_gg <- data.frame()
for (i in 1:length(players)) {
  print(players[i])
  df1 <- data_app_acb %>%
    filter(Compet == "ACB", Type_season == "Regular Season", Player.x == players[i]) %>%
    select(Season, Date, PIR, Position, Team, Date_birth) 
  
  df2 <- df1 %>%
    mutate(PIR_interv = ifelse(PIR >= 40, "40+", 
                               ifelse(PIR >= 30 & PIR < 40, "[30,40)",
                                      ifelse(PIR >= 20 & PIR < 30, "[20,30)",
                                             ifelse(PIR >= 10 & PIR < 20, "[10,20)",
                                                    ifelse(PIR >= 0 & PIR < 10, "[0,10)", "Negativa")))))) %>%
    mutate(PIR_interv = factor(PIR_interv, levels = c("40+", "[30,40)", "[20,30)", "[10,20)", 
                                                      "[0,10)", "Negativa"))) %>%
    mutate(Player = players[i])
  
  df_gg <- rbind(df_gg, df2)
}

# Write correct names:
players_new <- players
#players_new[c(1:3, 5, 7:11)] <- c("Abalde, Alberto", "Alocén, Carlos", "Barreiro, Jonathan", 
#                                  "Díaz, Alberto", "Fernández, Jaime", "García, Marc", 
#                                  "García, Sergi", "López-Arostegui, Xabier", "Paulí, Oriol")
players_new[c(4, 7, 9, 13)] <- c("Burjanadze, Beqa", "Cvetkovic, Aleksandar", 
                                 "Dimitrijevic, Nenad", "Hlinason, Tryggvi")

df_gg1 <- df_gg %>%
  mutate(Player = plyr::mapvalues(Player, 
                                  from = players,
                                  to = players_new))

df_gg2 <- df_gg1 %>%
  group_by(Player, Season) %>%
  add_count(PIR) %>%
  mutate(PIR1 = ifelse(n == 1, 
                       paste(PIR, " (", n, " vez)", sep = ""),
                       paste(PIR, " (", n, " veces)", sep = ""))) %>%
  rename(Equipo = Team, Temporada_regular = Season) %>%
  mutate(Player_nac = paste(Player, " (Fecha de nacimiento: ", Date_birth, ")", sep = "")) %>%
  ungroup() %>%
  mutate(Temporada_regular = as.factor(Temporada_regular), 
         Player = as.factor(Player),
         Season1 = as.numeric(Temporada_regular),
         Equipo = plyr::mapvalues(Equipo, from = "Sevilla", to = "Betis")) 
#df_gg2 %>% group_by(Player) %>% distinct(Date_birth)

df_gg3 <- df_gg2 %>%
  rename(Valorac = PIR, PIR = PIR1)

# Get plots:
#pdf("post_01.pdf", width = 10)
#pdf("post_02.pdf", width = 10)
for (i in 1:length(players_new)) {
  print(players_new[i])
  gg <- ggplot(data = df_gg3 %>% filter(Player == players_new[i]),
               aes(x = Temporada_regular, y = Valorac, color = PIR_interv, shape = Equipo)) +
    geom_point() +
    geom_text(aes(label = PIR), hjust = 0, nudge_x = 0.05, size = 2) +
    facet_wrap(~Player_nac, scales = "free") +
    scale_colour_manual(values = c("40+" = "#00BA38", "[30,40)" = "#F8766D",
                                   "[20,30)" = "#B79F00", "[10,20)" = "#00BFC4",
                                   "[0,10)" = "#619CFF", "Negativa" = "#F564E3")) +
    labs(color = "Intervalo de valoración", x = "Temporada regular", y = "Valoración") +
    theme(axis.text.x = element_text(size = 9, hjust = 0.9),
          strip.text.x = element_text(size = 12))
  print(gg)
}
#dev.off()

# https://stackoverflow.com/questions/39032791/adjusting-location-of-text-with-ggplot-and-plotly
gg
gg1 <- ggplotly(gg, tooltip = c("x", "label", "shape")) %>%
  config(displayModeBar = FALSE)
gg1
