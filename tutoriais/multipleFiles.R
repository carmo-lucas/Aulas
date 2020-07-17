library(tidyverse)

lista <- list.files(path = "dados/",
										pattern = "*.txt",
										full.names = TRUE)

lista

dados <- tibble(nome_arquivo = lista) %>% 
	mutate(conteudo_arquivo = purrr::map(nome_arquivo,
																			 ~ read_delim(., delim = ";",
																			 						 col_types = "dd"))) %>%
	unnest(conteudo_arquivo)


dados <- dados %>% 
	mutate(nome_arquivo = str_remove(string = nome_arquivo, pattern = "dados/")) %>% 
	mutate(nome_arquivo = str_remove(string = nome_arquivo, pattern = ".txt"))

dados
