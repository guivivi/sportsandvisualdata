table_mp_pir <- function(data, player){
  data1 <- data %>%
    filter(Player.x == player) %>%
    select(Season, GP, MP_avg, PIR_avg) %>%
    rename(Partidos = GP, Temporada = Season, Minutos_por_partido = MP_avg, Valoración_por_partido = PIR_avg)
  data2 <- gather(data1, key,value, -Temporada) %>% spread(Temporada, value) %>% column_to_rownames("key")
  data3 <- data2[c(2,1,3),]
  # https://stackoverflow.com/questions/30949768/r-vary-by-row-the-rounding-of-digits-produced-by-knitrkable
  data3[1,] <- formatC(as.numeric(data3[1,]),format = "d")
  data3[2,] <- formatC(as.numeric(data3[2,]),format = "d")
  data3[3,] <- formatC(as.numeric(data3[3,]),format = "f", digits = 1)
  return(data3)
}