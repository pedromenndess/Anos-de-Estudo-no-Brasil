# Baixando os pacotes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("sidrar")
install.packages("sf")
# Carregando os pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sidrar)
library(magrittr)
library(geobr)
library(sf)
# Importando a base de dados
dados <- get_sidra(api = "/t/7127/n3/all/v/3593/p/2023/c86/allxt/c58/2795/d/v3593%201")
glimpse(dados)
View(dados)
# Mudando o nome das variáveis
dados_selecionados <- dados %>%
select(
"anos_estudo" = Valor,
"uf" = 'Unidade da Federação (Código)',
"raca_cor" = 'Cor ou raça')
View(dados_selecionados)
# Criando o mapa
br_map_estado <- read_state(code_state = "all", year = 2020)
br_map_estado$code_state = as.character(br_map_estado$code_state)
# Juntando as bases de dadosa dados_selecionados$uf e br_map_estado$code_state
dados_juntos <- left_join(br_map_estado, dados_selecionados, by = c("code_state" = "uf"))
View(dados_juntos)
# Fazendo o mapa
ggplot() +
geom_sf(data = dados_juntos, aes(fill = anos_estudo), color = "white") +
scale_fill_gradient(low = "orange", high = "blue3", name = "média") +
geom_sf_label(data = dados_juntos, aes(label = anos_estudo),
label.padding = unit(0.5, "mm"), size = 2) +
facet_wrap(~raca_cor)+
theme_minimal() +
labs (title = "Média de anos de estudo das pessoas de 15 anos ou mais por estado e raça/cor",
subtitle = "Pessoas com 15 anos ou mais",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro",
x = "",
y="")+
theme(plot.title = element_text(face = "bold", size = 14),
plot.subtitle = element_text(face = "italic"))
# Separando por região
dados_regioes <- dados_juntos %>%
group_by(name_region, raca_cor) %>%
summarise(
anos_estudo_media = round(mean(anos_estudo, na.rm = TRUE), 1),
.groups = 'drop'
)
library(DT)
library(dplyr)
library(ggplot2)
# Preparando os dados para o gráfico
maiores_branca <- dados_juntos %>%
filter(raca_cor == "Branca") %>%
arrange(desc(anos_estudo)) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Maiores - Branca")
maiores_preta_parda <- dados_juntos %>%
filter(raca_cor == "Preta ou parda") %>%
arrange(desc(anos_estudo)) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Maiores - Preta/Parda")
menores_branca <- dados_juntos %>%
filter(raca_cor == "Branca") %>%
arrange(anos_estudo) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Menores - Branca")
menores_preta_parda <- dados_juntos %>%
filter(raca_cor == "Preta ou parda") %>%
arrange(anos_estudo) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Menores - Preta/Parda")
# Criando dataset combinado
dados_grafico <- bind_rows(
maiores_branca,
maiores_preta_parda,
menores_branca,
menores_preta_parda
)
# Criando gráfico horizontal - as 3 maiores e menores médias de anos de estudo
ggplot(dados_grafico, aes(x = reorder(name_state, anos_estudo), y = anos_estudo, fill = grupo)) +
geom_col() +
geom_text(aes(label = round(anos_estudo, 1)),
hjust = -0.1,
size = 3.5,
fontface = "bold") +
coord_flip() +
facet_wrap(~grupo, scales = "free_y") +
labs(
title = "Os 3 Estados com Menores e Maiores Médias
de Anos de Estudo por Raça/Cor",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro",
x = "",
y = "",
fill = "Categoria"
) +
theme_minimal() +
theme(
legend.position = "none",
plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
plot.subtitle = element_text(hjust = 0.5, size = 12),
strip.text = element_text(size = 11, face = "bold"),
panel.grid.major.y = element_blank(),
panel.grid.minor = element_blank()
) +
scale_fill_manual(values = c(
"Menores - Branca" = "#FF6B6B",
"Maiores - Branca" = "#4ECDC4",
"Menores - Preta/Parda" = "#FFE66D",
"Maiores - Preta/Parda" = "#95E1D3"
)) +
scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
# Gráfico com nome das regiões e média de anos de estudos por cor/raça
ggplot() +
geom_sf(data = dados_regioes, aes(fill = anos_estudo_media), color = "white", size = 0.8) +
scale_fill_gradient(low = "orange", high = "blue3", name = "Média\n(anos)") +
geom_sf_label(data = dados_regioes,
aes(label = paste0(name_region, "\n", anos_estudo_media)),
label.padding = unit(0.5, "mm"),
size = 3.5,
fontface = "bold") +
facet_wrap(~raca_cor, ncol = 2) +
theme_void() +
labs(title = "Média de anos de estudo por região do Brasil",
subtitle = "Pessoas com 15 anos ou mais, por raça/cor",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro") +
theme(
plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
plot.caption = element_text(size = 9, hjust = 0.5),
legend.position = "bottom",
strip.text = element_text(face = "bold", size = 12),
plot.margin = margin(20, 20, 20, 20)
)

# Gráfico com nome das regiões e média de anos de estudos por cor/raça
ggplot() +
geom_sf(data = dados_regioes, aes(fill = anos_estudo_media), color = "white", size = 0.8) +
scale_fill_gradient(low = "orange", high = "blue3", name = "Média\n(anos)") +
geom_sf_label(data = dados_regioes,
aes(label = paste0(name_region, "\n", anos_estudo_media)),
label.padding = unit(0.5, "mm"),
size = 3.5,
fontface = "bold") +
facet_wrap(~raca_cor, ncol = 2) +
theme_void() +
labs(title = "Média de anos de estudo por região do Brasil",
subtitle = "Pessoas com 15 anos ou mais, por raça/cor",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro") +
theme(
plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
plot.caption = element_text(size = 9, hjust = 0.5),
legend.position = "bottom",
strip.text = element_text(face = "bold", size = 12),
plot.margin = margin(20, 20, 20, 20)
)
# Carregando os pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sidrar)
library(magrittr)
library(geobr)
library(sf)
# Importando a base de dados
dados <- get_sidra(api = "/t/7127/n3/all/v/3593/p/2023/c86/allxt/c58/2795/d/v3593%201")
glimpse(dados)
View(dados)
# Mudando o nome das variáveis
dados_selecionados <- dados %>%
select(
"anos_estudo" = Valor,
"uf" = 'Unidade da Federação (Código)',
"raca_cor" = 'Cor ou raça')
View(dados_selecionados)
# Criando o mapa
br_map_estado <- read_state(code_state = "all", year = 2020)
br_map_estado$code_state = as.character(br_map_estado$code_state)
# Juntando as bases de dadosa dados_selecionados$uf e br_map_estado$code_state
dados_juntos <- left_join(br_map_estado, dados_selecionados, by = c("code_state" = "uf"))
View(dados_juntos)
# Fazendo o mapa
ggplot() +
geom_sf(data = dados_juntos, aes(fill = anos_estudo), color = "white") +
scale_fill_gradient(low = "orange", high = "blue3", name = "média") +
geom_sf_label(data = dados_juntos, aes(label = anos_estudo),
label.padding = unit(0.5, "mm"), size = 2) +
facet_wrap(~raca_cor)+
theme_minimal() +
labs (title = "Média de anos de estudo das pessoas de 15 anos ou mais por estado e raça/cor",
subtitle = "Pessoas com 15 anos ou mais",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro",
x = "",
y="")+
theme(plot.title = element_text(face = "bold", size = 14),
plot.subtitle = element_text(face = "italic"))
# Separando por região
dados_regioes <- dados_juntos %>%
group_by(name_region, raca_cor) %>%
summarise(
anos_estudo_media = round(mean(anos_estudo, na.rm = TRUE), 1),
.groups = 'drop'
)
library(DT)
library(dplyr)
library(ggplot2)
# Preparando os dados para o gráfico
maiores_branca <- dados_juntos %>%
filter(raca_cor == "Branca") %>%
arrange(desc(anos_estudo)) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Maiores - Branca")
maiores_preta_parda <- dados_juntos %>%
filter(raca_cor == "Preta ou parda") %>%
arrange(desc(anos_estudo)) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Maiores - Preta/Parda")
menores_branca <- dados_juntos %>%
filter(raca_cor == "Branca") %>%
arrange(anos_estudo) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Menores - Branca")
menores_preta_parda <- dados_juntos %>%
filter(raca_cor == "Preta ou parda") %>%
arrange(anos_estudo) %>%
slice_head(n = 3) %>%
select(name_state, anos_estudo) %>%
mutate(grupo = "Menores - Preta/Parda")
# Criando dataset combinado
dados_grafico <- bind_rows(
maiores_branca,
maiores_preta_parda,
menores_branca,
menores_preta_parda
)
# Criando gráfico horizontal - as 3 maiores e menores médias de anos de estudo
ggplot(dados_grafico, aes(x = reorder(name_state, anos_estudo), y = anos_estudo, fill = grupo)) +
geom_col() +
geom_text(aes(label = round(anos_estudo, 1)),
hjust = -0.1,
size = 3.5,
fontface = "bold") +
coord_flip() +
facet_wrap(~grupo, scales = "free_y") +
labs(
title = "Os 3 Estados com Menores e Maiores Médias
de Anos de Estudo por Raça/Cor",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro",
x = "",
y = "",
fill = "Categoria"
) +
theme_minimal() +
theme(
legend.position = "none",
plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
plot.subtitle = element_text(hjust = 0.5, size = 12),
strip.text = element_text(size = 11, face = "bold"),
panel.grid.major.y = element_blank(),
panel.grid.minor = element_blank()
) +
scale_fill_manual(values = c(
"Menores - Branca" = "#FF6B6B",
"Maiores - Branca" = "#4ECDC4",
"Menores - Preta/Parda" = "#FFE66D",
"Maiores - Preta/Parda" = "#95E1D3"
)) +
scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
# Gráfico com nome das regiões e média de anos de estudos por cor/raça
ggplot() +
geom_sf(data = dados_regioes, aes(fill = anos_estudo_media), color = "white", size = 0.8) +
scale_fill_gradient(low = "orange", high = "blue3", name = "Média\n(anos)") +
geom_sf_label(data = dados_regioes,
aes(label = paste0(name_region, "\n", anos_estudo_media)),
label.padding = unit(0.5, "mm"),
size = 3.5,
fontface = "bold") +
facet_wrap(~raca_cor, ncol = 2) +
theme_void() +
labs(title = "Média de anos de estudo por região do Brasil",
subtitle = "Pessoas com 15 anos ou mais, por raça/cor",
caption = "Dados: PNAD-Continua Anual - 2023 | Elaboração: João Pedro") +
theme(
plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
plot.caption = element_text(size = 9, hjust = 0.5),
legend.position = "bottom",
strip.text = element_text(face = "bold", size = 12),
plot.margin = margin(20, 20, 20, 20)
)