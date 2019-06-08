# Load packages:
library(tidyverse)
library(lubridate)

# Set working to source file location and source functions:
#setwd()

# Load data:
load("data_app_acb.RData")

# Get ACB data:
df01 <- data_app_acb %>% 
  filter(Compet == "ACB", Type_season == "Regular Season", Season == "2018-2019")

# Select players of interest:
players <- c("Alocen, Carlos", "Barreiro, J.", "Cancar, Vlatko", "Cate, Emanuel",
             "Dimitrijevic, N.", "Hlinason, T.", "Lopez-Arostegui, X", "Radoncic, Dino",
             "Sakho, Jordan", "Yusta, Santiago")

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
  select(Season, Player.x, Position, Date_birth, GP, MP_oper_def, PIR_sum) %>%
  rename(MP = MP_oper_def) %>%
  # To convert the minutes into numeric:
  mutate(MP = round(as.numeric(ms(MP), unit = "mins"))) %>%
  distinct() %>%
  mutate(MP_avg = round(MP / GP))  %>%
  mutate(PIR_avg = round(PIR_sum / GP, 2)) %>% 
  arrange(desc(PIR_avg))

df03 <- df02 %>%
  select(Season, Player.x, Position, Date_birth, GP, MP_avg, PIR_avg) %>%
  rename(Temporada = Season, Jugador = Player.x, Posición = Position, 
         Fecha_nacimiento = Date_birth, Partidos = GP, 
         Minutos_por_partido = MP_avg, Valoración_por_partido = PIR_avg) %>%
  mutate(Jugador = as.character(Jugador))

df03[c(5:7,9,10),2] <- c("Dimitrijevic, Nenad", "Alocén, Carlos",
                         "López-Arostegui, Xabier", "Barreiro, Jonathan",
                         "Hlinason, Tryggvi")
df03[,3] <- c("Alero", "Alero", "Ala-pívot", "Pívot", "Base", 
              "Base", "Alero", "Ala-pívot", "Alero", "Pívot")
df03
