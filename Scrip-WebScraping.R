# Título: Script de Web Scraping para o Projeto PIPA/IPEA
# Autor: Wellington Santos Souza
# Data: 08 de junho de 2025
# Descrição: Este script realiza a extração de dados sobre o tipo de
#            abastecimento de água por município no Brasil, a partir
#            de uma tabela pública do IBGE.

# --------------------------------------------------------------------
# PASSO 1: INSTALAR E CARREGAR OS PACOTES NECESSÁRIOS
# --------------------------------------------------------------------

# Verifica se os pacotes estão instalados; se não, instala-os.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
if (!require("sidrar")) install.packages("sidrar")

# Carrega os pacotes para a sessão atual.
library(tidyverse)
library(janitor)
library(sidrar)

# --------------------------------------------------------------------
# PASSO 2: DEFINIR A URL E REALIZAR O WEB SCRAPING
# --------------------------------------------------------------------
# Informações sobre a tabela do IBGE:
# Tabela 1364: Número de municípios, total e os com serviço de abastecimento de água, por tipo de tratamento da água

info_sidra(1364)

# agora que temos as informações, vamos extrair os dados.
dados_saneamento_UF <- get_sidra(
  x = 1364, # Tabela do IBGE sobre abastecimento de água
  period = c("2000", "2008", "2017"),
  variable = 2598,
  geo = "State"
  
)

# --------------------------------------------------------------------
# PASSO 3: LIMPEZA E ESTRUTURAÇÃO DOS DADOS (TIDYING)
# --------------------------------------------------------------------
# Renomeia as colunas para facilitar a manipulação.
dados_saneamento_UF <- dados_saneamento_UF %>%
  select(
    uf = `Unidade da Federação`,
    ano = `Ano`,
    tipo_tratamento = `Existência e tipo de tratamento da água`,
    valor = `Valor`
  )


dados_saneamento_UF <- dados_saneamento_UF %>%
  mutate(
    regiao = case_when(
      uf %in% c("Acre", "Amapá", "Amazonas", "Rondônia", "Roraima", "Pará") ~ "Norte",
      uf %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe") ~ "Nordeste",
      uf %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul", "Tocantins") ~ "Centro-Oeste",
      uf %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
      uf %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul",
      TRUE ~ NA
    ))

# --------------------------------------------------------------------
# PASSO 4: VISUALIZAÇÃO DOS DADOS LIMPOS
# --------------------------------------------------------------------

# Exibe as primeiras linhas do dataframe para verificar a limpeza.
head(dados_saneamento_UF)

# salva o dataframe limpo em um arquivo csv
write.csv(dados_saneamento_UF, "dados_saneamento_UF.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# PASSO 5: ANÁLISE DOS DADOS
# ---------------------------------------------------------------------
# 1. Filtrar para o ano mais recente e as categorias que precisamos
dados_para_calculo <- dados_saneamento_UF %>%
  filter(
    ano == max(ano),
    tipo_tratamento %in% c("Total geral de municípios", "Total de municípios com tratamento")
  )

# 2. Pivotar os dados para ter "Total geral" e "Total com tratamento" como colunas
dados_pivotados <- dados_para_calculo %>%
  pivot_wider(
    names_from = tipo_tratamento,
    values_from = valor
  ) %>%
  clean_names() 

# 3. Calcular o percentual de municípios com tratamento
atendimento_uf_calculado <- dados_pivotados %>%
  mutate(
    percentual_municipios_tratados = (total_de_municipios_com_tratamento / total_geral_de_municipios) 
  ) %>%
  select(Regiao = regiao, UF = uf, Percentual_Municipios_Tratados = percentual_municipios_tratados) %>%
  arrange(desc(Percentual_Municipios_Tratados))

# Gráfico de barras do percentual de municípios com tratamento por UF
ggplot(atendimento_uf_calculado, aes(x = reorder(UF, Percentual_Municipios_Tratados), y = Percentual_Municipios_Tratados)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Percentual de Municípios com Tratamento de Água por UF (2017)",
    x = "Unidade da Federação (UF)",
    y = "Percentual de Municípios com Tratamento"
  ) +
 geom_text(aes(label = scales::percent(Percentual_Municipios_Tratados, accuracy = 0.01)), hjust = 3, size = 3, color = "red") +
  theme_minimal()
# Salvar gráfico
ggsave("graficos/percentual_municipios_tratamento_agua_por_uf.png", width = 10, height = 6)

# Tabela por Região
atendimento_regiao_calculado <- atendimento_uf_calculado %>%
  group_by(Regiao) %>%
  summarise(Media_Percentual_Municipios = mean(Percentual_Municipios_Tratados, na.rm = TRUE)) %>%
  arrange(desc(Media_Percentual_Municipios))

# Gráfico de barras do percentual médio de municípios com tratamento por Região
ggplot(atendimento_regiao_calculado, aes(x = reorder(Regiao, Media_Percentual_Municipios), y = Media_Percentual_Municipios)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Percentual Médio de Municípios com Tratamento de Água por Região (2017)",
    x = "Região",
    y = "Percentual Médio de Municípios com Tratamento"
  ) +
 geom_text(aes(label = scales::percent(Media_Percentual_Municipios, accuracy = 0.01)), hjust = 3, size = 5 ,color = "red") +
  theme_minimal()

# Salvar gráfico
ggsave("graficos/percentual_municipios_tratamento_agua_por_regiao.png", width = 10, height = 6)
# ---------------------------------------------------------------------

# Agora vamos analisar a evolução do percentual de municípios com tratamento de água ao longo dos anos.

# Filtrar os dados para as categorias de interesse
dados_evolucao <- dados_saneamento_UF %>%
  filter(
    tipo_tratamento %in% c("Total geral de municípios", "Total de municípios com tratamento")
  )
# Pivotar os dados para ter "Total geral" e "Total com tratamento" como colunas
dados_evolucao_pivotados <- dados_evolucao %>%
  pivot_wider(
    names_from = tipo_tratamento,
    values_from = valor
  ) %>%
  clean_names()

# Calcular o percentual de municípios com tratamento ao longo dos anos
dados_evolucao_calculado <- dados_evolucao_pivotados %>%
  mutate(
    percentual_municipios_tratados = (total_de_municipios_com_tratamento / total_geral_de_municipios)
  ) %>%
  select(ano, Regiao = regiao, UF = uf, Percentual_Municipios_Tratados = percentual_municipios_tratados) %>%
  arrange(ano, desc(Percentual_Municipios_Tratados))

# Tabela de evolução do percentual de municípios com tratamento
evolucao_tabela <- dados_evolucao_calculado %>%
  group_by(ano, Regiao) %>%
  summarise(Media_Percentual_Municipios = mean(Percentual_Municipios_Tratados, na.rm = TRUE)) %>%
  arrange(ano, desc(Media_Percentual_Municipios))

# Criar visualização da tabela 
library(gt)
evolucao_tabela_gt <- evolucao_tabela %>%
  gt() %>%
  tab_header(
    title = "Evolução do Percentual de Municípios com Tratamento de Água por Região",
    subtitle = "Dados de 2000, 2008 e 2017"
  ) %>%
  cols_label(
    ano = "Ano",
    Regiao = "Região",
    Media_Percentual_Municipios = "Percentual Médio de Municípios com Tratamento"
  ) %>%
  fmt_percent(
    columns = c(Media_Percentual_Municipios),
    scale_values = TRUE,
    decimals = 2
  )

# salvar em pdf
gtsave(evolucao_tabela_gt, "tabelas/evolucao_percentual_municipios_tratamento_agua.pdf")

# Agora por UF -----------------------------------------------------

# Tabela de evolução do percentual de municípios com tratamento por UF
evolucao_uf_tabela <- dados_evolucao_calculado %>%
  group_by(ano, Regiao, UF) %>%
  summarise(Media_Percentual_Municipios = mean(Percentual_Municipios_Tratados, na.rm = TRUE)) %>%
  arrange(ano, desc(Media_Percentual_Municipios))
# Criar visualização da tabela por UF
evolucao_uf_tabela_gt <- evolucao_uf_tabela %>%
  gt() %>%
  tab_header(
    title = "Evolução do Percentual de Municípios com Tratamento de Água por UF",
    subtitle = "Dados de 2000, 2008 e 2017"
  ) %>%
  cols_label(
    ano = "Ano",
    Regiao = "Região",
    UF = "Unidade da Federação (UF)",
    Media_Percentual_Municipios = "Percentual Médio de Municípios com Tratamento"
  ) %>%
  fmt_percent(
    columns = c(Media_Percentual_Municipios),
    scale_values = TRUE,
    decimals = 2
  )
# salvar em pdf
gtsave(evolucao_uf_tabela_gt, "tabelas/evolucao_percentual_municipios_tratamento_agua_por_uf.pdf")


# Mapa coropleth para visualização do percentual de municípios com tratamento de água por UF
if(!require(geobr)) install.packages("geobr"); library(geobr)

# Carregar os dados geográficos das unidades da federação
mapa_uf <- read_state(year = 2017)

# Juntar os dados de percentual com os dados geográficos
mapa_uf <- mapa_uf %>%
  left_join(atendimento_uf_calculado, by = c("name_state" = "UF"))
# Criar o mapa coropleth
ggplot(mapa_uf) +
  geom_sf(aes(fill = Percentual_Municipios_Tratados), color = "white") +
  scale_fill_viridis_c(option = "C", name = "Percentual de Municípios com Tratamento") +
  labs(
    title = "Mapa do Percentual de Municípios com Tratamento de Água por UF (2017)",
    subtitle = "Fonte: IBGE - Tabela 1364"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  theme(legend.position = "bottom")
# Salvar o mapa
ggsave("graficos/mapa_percentual_municipios_tratamento_agua_por_uf.png", width = 10, height = 6)
# ---------------------------------------------------------------------
# Fim do script
