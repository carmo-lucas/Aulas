library(tidyverse)

# importando dados --------------------------------------------------------

# lê os dados no arquivo DRX.txt na pasta dados
df <- read_table("dados/DRX.txt", 
# pula as 30 primeiras linhas do arquivo que contém informações não necessárias
								 skip = 30) %>% 
# seleciona colunas <2Theta> e <I> e as renomeia
	select("theta_2" = `<2Theta>`, "I" = `I   >`)

#######################################################
# Não é indicado iniciar nomes de objetos com números #
#######################################################

# método geom_smooth() ----------------------------------------------------

ggplot(df,
			 aes(
			 	x = theta_2,
			 	y = I
			 )
			) +
	geom_line() +
	geom_smooth(method = "loess", span = 0.001, se = FALSE)


# método moving average usando pacote tidyquant ---------------------------------------------------
library(tidyquant)

ggplot(df,
			 aes(
			 	x = theta_2,
			 	y = I
			 )
) +
	geom_line(alpha = 0.5) +
	# este geom introduzido pelo pacote tidyquant faz a média entre valores próximos. 
	# Um efeito colateral é a diminuição de picos estreitos.
	# Para controlar este efeito colateral deve-se ajustar o valor de n.
	# n é o números de pontos cuja média é retornada, quanto maior o n maior é o 
	# efeito de diminuição dos picos e vice-versa, ou seja, quanto maior o n, 
	# maior o efeito de smoothing.
	geom_ma(n = 5, linetype = "solid", size = 1)
