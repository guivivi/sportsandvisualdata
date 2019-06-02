plot_pir_interv <- function(data, player, text_label = 3) {
  gg <- ggplot(data = data %>% filter(Player == player),
               aes(x = Temporada_regular, y = Valorac, color = PIR_interv, shape = Equipo)) +
    geom_point() +
    geom_text(aes(label = PIR), hjust = 0, nudge_x = 0.05, size = text_label) +
    facet_wrap(~Player_nac, scales = "free") +
    scale_colour_manual(values = c("40+" = "#00BA38", "[30,40)" = "#F8766D",
                                   "[20,30)" = "#B79F00", "[10,20)" = "#00BFC4",
                                   "[0,10)" = "#619CFF", "Negativa" = "#F564E3")) +
    labs(color = "Intervalo de valoración", x = "Temporada regular", y = "Valoración") +
    theme(axis.text.x = element_text(size = 9, hjust = 0.9),
          strip.text.x = element_text(size = 12))
  return(gg)
}

