#### Script disciplina comportamento Unicamp ####

# Etograma de Sagui midas (Saguinus midas) 


#### Pacotes ####
library(tidyr)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(openxlsx)


#### Estrutura dos dados ####
# Dados de comportamentos em 50 intervalos de horários (T1, T2...)
# Horário dividido em 5 minutos de intervalo e 1 minuto de varredura
# 8 classes de comportamento
# 100 atos comportamentais 

#### Entrada de dados ####

#Organizei em estrutura de projeto no R, mas para guiar, está nesse diretório:
setwd("C:/Users/letic/OneDrive/Desktop/Doutorado/Disciplinas/NE466 - Introdução ao comportamento animal e filogenia/Zoo/Artigo Etograma/dados")

getwd()

# importando dados
dados <- readr::read_csv2("etograma.csv")

#removi colunas totalmente em branco (bug na entrada dos dados)
dados <- dados[, colSums(!is.na(dados)) > 0]

tail(names(dados)) #agora sim!

# alterando nomes das variáveis
dados <- dados %>% 
  dplyr::rename(categoria = Categoria,
                comportamento = Comportamento)

#### passando planilha para formato longo ####
dados_long <- dados %>% 
  tidyr::pivot_longer(
  cols = starts_with("T"))

#### frequencia classes comportamento ####

# Organizando dados
dados_freq <- dados_long %>% 
  dplyr::group_by(categoria) %>% 
  dplyr::summarise(freq_total = sum(value, na.rm = T)) %>% #frequencia de cada categoria de comportamentos
  dplyr::mutate(prop = round(freq_total/sum(freq_total), 4)*100) %>%#proporção dos comportamentos
  dplyr::mutate(prop_round = round(freq_total/sum(freq_total), 2)*100) #proporção arredondada

dados_freq

# Gráfico:
freq_total <- ggplot(data = dados_freq, aes(x = "", y = prop, fill = categoria)) + 
              geom_bar(stat = "identity", color = "white") +
              coord_polar("y", start = 0) +
              geom_text(aes(label = paste0(prop_round, "%")), color = "white", 
              position = position_stack(vjust = .5), size = 8) +
              scale_fill_manual(values = c(
    "#E75A4F",  
    "#F9A03F",  
    "#FFC85C",  
    "#A8C77F",  
    "#4FAB9B",  
    "#2F7D9A",  
    "#5C5E7B",  
    "#D16A86"   
  )) +
            theme_void() +
            labs(fill = "Categoria")

freq_total

dir.create('figuras')

#Salvar em .png
ggsave(filename = "figuras/figura1_freq_total.png", 
       plot = freq_total, wi = 25, he = 15, 
       un = "cm", dpi = 300)

system('open "figuras/figura1_freq_total.png"')

ggsave(filename = "figuras/figura1_freq_total.svg", 
       plot = freq_total, wi = 25, he = 15, 
       un = "cm", dpi = 300)

system('open "figuras/figura1_freq_total.svg"')


#### Dados exploratórios ####

dados_freq %>% 
  summarise(total = sum(freq_total)) # Total de atos comportamentais = 690

# Calculo de cobertura amostral
cobertura_amostral <- dados_long %>% 
  dplyr::group_by(comportamento) %>% 
  dplyr::summarise(freq_total = sum(value, na.rm = T)) %>% 
  dplyr::arrange(freq_total) %>% 
  dplyr::count(freq_total) #resume a quantidade de atos que foram feitos apenas 1x, 2x, nenhuma vez e por ai vai

# 17 atos comportamentais compilados não foram feitos nenhuma vez no segundo dia
# 16 atos foram feitos uma vez
# total de atos é 690
# Portanto, tamanho amostral é 1-(número de atos executados 1x/ total de atos)
# então:
1-(16/ 690) #0,977

# Tabela para apêndice: quantidade de atos comportamentais registrados por categoria no total:

tabela_1_apendice <- dados_long %>% 
  dplyr::group_by(comportamento, categoria) %>% 
  dplyr::summarise(freq_total = sum(value, na.rm = T)) %>% 
  dplyr::arrange(categoria) %>% 
  dplyr::rename(Categoria = categoria,
                Atos = comportamento,
                Quantidade = freq_total)
  
writexl::write_xlsx(tabela_1_apendice, "tabela_1_apendice.xlsx") #Salvando a tabela no excel

#### Hipótese 1: Comportamentos em grupo são mais frequentes que comportamentos individuais ####

dados_H1 <- dados_freq %>%
  mutate(H1 = if_else(categoria == "Interação com outros indivíduos",
                          "Interação",
                          "Solitário")) %>% 
  dplyr::group_by(H1) %>% 
  dplyr::summarise(freq_total = sum(prop, na.rm = T)) %>% 
  dplyr::mutate(prop_round = round(freq_total/sum(freq_total), 2)*100) #proporção arredondada

dados_H1

#Gráfico:

plot_H1 <- ggplot(data = dados_H1, aes(x = "", y = prop_round, fill = H1)) + 
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(prop_round, "%")), color = "white", 
            position = position_stack(vjust = .5), size = 8) +
  scale_fill_manual(values = c(
    "#5C5E7B",  
    "#B89A7A"
  )) +
  theme_void() +
  labs(fill = "Categoria")

plot_H1

#Salvar em .png
ggsave(filename = "figuras/plot_H1.png", 
       plot = plot_H1, wi = 25, he = 15, 
       un = "cm", dpi = 300)

system('open "figuras/plot_H1.png"')

ggsave(filename = "figuras/plot_H1.svg", 
       plot = plot_H1, wi = 25, he = 15, 
       un = "cm", dpi = 300)

system('open "figuras/plot_H1.svg"')

# Gráfico para artigo
library(patchwork)

H1 <- freq_total + plot_H1

H1

ggsave(filename = "figuras/H1.svg", 
       plot = H1, wi = 40, he = 15, 
       un = "cm", dpi = 300)

system('open "figuras/H1.svg"')

# Dados para analise estatistica da H1
X2_H1 <- dados_freq %>%
  mutate(H1 = if_else(categoria == "Interação com outros indivíduos",
                      "Interação",
                      "Solitário")) %>% 
  dplyr::group_by(H1) %>% 
  dplyr::summarise(freq_total_soma = sum(freq_total)) # Número total de comportamentos solitários = 603; interações = 87

# Analise estatistica: X2

X2 <- X2_H1 %>% 
  pivot_wider(
    names_from = H1,
    values_from = freq_total_soma
  ) %>% 
  rowwise() %>% 
  mutate(
    teste = list(chisq.test(c(Interação, Solitário), p = c(0.5, 0.5))),
    qui2  = teste$statistic,
    p     = teste$p.value,
    gl    = teste$parameter
  ) %>% 
  select(Interação, Solitário, qui2, gl, p) #significativo

#### Hipótese 2: Indivíduos aumentam a frequência de comportamentos ao longo do dia ####

# Criando uma coluna nova com intervalos como manha ou tarde
dados_H2 <- dados_long %>% 
  mutate(turno = if_else(readr::parse_number(name) <= 25,
                         "manhã",
                         "tarde")) %>%  #Chamando tudo que for do intervalo T1 ao T25 de "manhã" e tudo que for de T26 ao T50 de tarde
  mutate(intervalo = readr::parse_number(name)) %>%  # T1 → 1, T25 → 25 etc.
  group_by(categoria, intervalo, turno) %>% 
  summarise(soma_intervalo = sum(value, na.rm = TRUE))

# Cores em ordem para gráfico
cores_cat <- c(
  "#E75A4F",  
  "#F9A03F",  
  "#FFC85C",  
  "#A8C77F",  
  "#4FAB9B",  
  "#2F7D9A",  
  "#5C5E7B",  
  "#D16A86"
)

# Todas as categorias
cats <- unique(dados_H2$categoria)

# Garantindo a mesma ordem entre categorias e cores
cores_cat <- setNames(cores_cat, cats)

# Loop para fazer todos boxplot ao mesmo tempo
for(categoria_i in cats){
  
  # filtrar a categoria
  df_cat <- dados_H2 %>% 
    dplyr::filter(categoria == categoria_i)
  
  # cor da categoria
  cor_usada <- cores_cat[categoria_i]
  
  # criar o plot
  p <- ggplot(df_cat, aes(turno, soma_intervalo)) +
    geom_boxplot(fill = cor_usada, alpha = 0.7, color = "black") +
    geom_jitter(colour = cor_usada, alpha = 0.4, width = 0.2) +
    stat_compare_means(
      label.y = max(df_cat$soma_intervalo, na.rm = TRUE) * 1.1,
      label.x = 2 
    ) +
    labs(
      x = "Turno",
      y = "Comportamentos/varredura (nº)",
      title = categoria_i
    ) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none")
  
  print(p)
  
  nome_seguro <- gsub("/", "_", categoria_i)
  
  # salvar
  
  dir.create("figuras/h2", showWarnings = FALSE, recursive = TRUE)
  
  ggsave(
    filename = paste0("figuras/h2/", nome_seguro, ".png"),
    plot = p,
    width = 20,
    height = 15,
    units = "cm",
    dpi = 300
  )
}

# Gráfico com horário no eixo x

dados_densidade <- dados_H2 %>% 
  dplyr::mutate(intervalo = as.numeric(intervalo)) %>% 
  dplyr::select(-(turno))

plot_densidade <- ggplot(dados_densidade,
       aes(x = intervalo, y = soma_intervalo, fill = categoria)) +
  geom_col() +
  geom_vline(xintercept = 25, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ categoria, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(
    "#E75A4F", "#F9A03F", "#FFC85C", "#A8C77F",
    "#4FAB9B", "#2F7D9A", "#5C5E7B", "#D16A86"
  )) +
  labs(x = "Intervalo", y = "Frequência") +
  theme_bw(base_size = 16)+
  theme(legend.position = "none")

ggsave(
  filename = paste0("figuras/h2/densidade.png"),
  plot = plot_densidade,
  width = 25,
  height = 30,
  units = "cm",
  dpi = 300
)




