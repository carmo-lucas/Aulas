library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)

jogar_dado <- function(n_ataque, n_defesa, n_lados = 6) {
	
	ataque <- tibble(
		Tipo = "Ataque",
		valor = sort(round(runif(n = n_ataque, min = 1, max = n_lados)), decreasing = TRUE),
		n_dado = 1:n_ataque
	)
	
	defesa <- tibble(
		Tipo = "Defesa",
		valor = sort(round(runif(n = n_defesa, min = 1, max = n_lados)), decreasing = TRUE),
		n_dado = 1:n_defesa
	)
	
	df <- rbind(ataque, defesa)
	
	ggplot(
		df,
		aes(x =n_dado,
				y = Tipo,
				fill = Tipo)
	) + 
		geom_tile(color = "white", size = 25) +
		geom_text(aes(label = valor), size = 50) +
		scale_fill_manual(values = c("firebrick", "darkgoldenrod3")) +
		coord_equal() +
		theme_void() + 
		theme(legend.position = "none")
	
}

jogar_dado(3,2)

dados <- function(n_dados, n_lados) {

	df <- tibble(
		numero_dados = 1:n_dados,
		valor = round(runif(n = n_dados, min = 1, max = n_lados))
	)
	
	ggplot(df,
				 aes(
				 	x = 1,
				 	y = 1, 
				 )) +
		geom_tile(size = 5, fill = "royalblue") +
		geom_text(aes(label = valor), size = 20) +
		facet_wrap(~numero_dados, ncol = 3) +
		theme_void() + 
		coord_equal()
}

dados(9,10)

dados(1, 6)
