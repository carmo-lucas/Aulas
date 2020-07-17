library(tidyverse)
library(gganimate)
library(gifski)

data <- crossing(
	Cor = c("Amarelo","Azul","Vermelho"),
	Tempo = 1:10,
	Preciptado = factor(c("pouco", "medio","muito"))
)

data$Temperatura <- rpois(n = nrow(data), lambda = 50)

plot <- data %>% ggplot(
	aes(
		x = Tempo,
		y = Temperatura,
		col = Cor,
		shape = Preciptado
	)
) +
	geom_point() +
	geom_line() +
	scale_color_manual(values = c("gold","royalblue", "firebrick")) +
	theme_bw() + 
	transition_reveal(Tempo) 

anim <- animate(plot, duration = 10, # duração em segundos
								fps = 30, # frames por segundo
								renderer = gifski_renderer(width = 800, height = 600))

anim_save(filename = "jacke.gif", 
					animation = anim)

plot
