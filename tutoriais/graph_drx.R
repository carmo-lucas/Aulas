library(tidyverse)

ggplot(dados,
			 aes(x = Theta,
			 		y = I,
			 		color = nome_arquivo)) +
	geom_line()

library(ggridges)

ggplot(dados,
			 aes(
			 	x = Theta,
			 	y = nome_arquivo,
			 	color = nome_arquivo)) +
	geom_ridgeline(aes(height = I/5000), alpha = 0)
