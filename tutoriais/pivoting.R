
# Pacotes -----------------------------------------------------------------

library(tidyverse)



# pivot_wider( ) ----------------------------------------------------------

dados %>% 
	pivot_wider(names_from = nome_arquivo, values_from = c(`<2Theta>`, `<I>`))
