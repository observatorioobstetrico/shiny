library(dplyr)
library(lubridate)

dados_real <- readr::read_csv("srag_16-22_novo.gz") %>% 
  rename(sexo = CS_SEXO, idade = NU_IDADE_N) %>% 
  filter(sexo == "F") %>%
  filter(idade > 9 & idade < 56) %>%
  mutate(ano = str_sub(DT_SIN_PRI, start = 7)) %>% 
  filter(as.Date(DT_SIN_PRI, format = "%d/%m/%Y") < as.Date("01/12/2021", format = "%d/%m/%Y")) %>% 
  mutate(classi_gesta_puerp = as.factor(case_when(
    CS_GESTANT == 1 ~ "1tri",
    CS_GESTANT == 2 ~ "2tri",
    CS_GESTANT == 3 ~ "3tri",
    CS_GESTANT == 4 ~ "IG_ig",
    CS_GESTANT == 5 & PUERPERA == 1 ~ "puerp",
    CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
    TRUE ~ "não"))
  ) %>% 
  filter(classi_gesta_puerp != "não") %>% 
  mutate(classi_gesta_puerp = droplevels(.$classi_gesta_puerp, except = c(1:4, 6))) %>% 
  mutate(classi_fin = as.factor(case_when(
    CLASSI_FIN == 1 ~ "não-covid",
    CLASSI_FIN == 2 ~ "não-covid",
    CLASSI_FIN == 3 ~ "não-covid",
    CLASSI_FIN == 5 ~ "covid-19",
    TRUE ~ NA_character_))
  ) %>% 
  filter(!is.na(classi_fin)) %>%
  mutate(raca = as.factor(case_when(
    CS_RACA == 1 ~ "branca",
    CS_RACA == 2 ~ "não branca",
    CS_RACA == 3 ~ "não branca",
    CS_RACA == 4 ~ "não branca",
    CS_RACA == 5 ~ "não branca",
    CS_RACA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(escolaridade = as.factor(case_when( # instruction
    CS_ESCOL_N == 0 ~ "até fundamental",
    CS_ESCOL_N == 1 | CS_ESCOL_N == 2 ~ "até fundamental",
    CS_ESCOL_N == 3 ~ "médio",
    CS_ESCOL_N == 4 ~ "superior",
    CS_ESCOL_N == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(vacina = as.factor(case_when(
    VACINA == 1  ~ "sim",
    VACINA == 2 ~ "não",
    VACINA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(febre = as.factor(case_when(
    FEBRE == 1 ~ "sim",
    FEBRE == 2 ~ "não",
    FEBRE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(tosse = as.factor(case_when(
    TOSSE == 1 ~ "sim",
    TOSSE == 2 ~ "não",
    TOSSE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(garganta = as.factor(case_when(
    GARGANTA == 1 ~ "sim",
    GARGANTA  == 2 ~ "não",
    GARGANTA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(dispneia = as.factor(case_when(
    DISPNEIA == 1 ~ "sim",
    DISPNEIA == 2 ~ "não",
    DISPNEIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(desc_resp = as.factor(case_when( 
    DESC_RESP == 1 ~ "sim",
    DESC_RESP == 2 ~ "não",
    DESC_RESP == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(saturacao = as.factor(case_when(
    SATURACAO == 1 ~ "sim",
    SATURACAO == 2 ~ "não",
    SATURACAO == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(diarreia = as.factor(case_when(
    DIARREIA == 1 ~ "sim",
    DIARREIA == 2 ~ "não",
    DIARREIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(dor_abd = as.factor(case_when(
    DOR_ABD == 1 ~ "sim",
    DOR_ABD == 2 ~ "não",
    DOR_ABD == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(fadiga = as.factor(case_when(
    FADIGA == 1 ~ "sim",
    FADIGA == 2 ~ "não",
    FADIGA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(perd_olft = as.factor(case_when(
    PERD_OLFT == 1 ~ "sim",
    PERD_OLFT == 2 ~ "não",
    PERD_OLFT == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(perd_pala = as.factor(case_when(
    PERD_PALA == 1 ~ "sim",
    PERD_PALA == 2 ~ "não",
    PERD_PALA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(diabetes = as.factor(case_when(
    DIABETES == 1 ~ "Sim",
    DIABETES == 2 ~ "Não",
    DIABETES == 9 ~ "Ignorado",
    TRUE ~ "Em branco"))
  ) %>% 
  mutate(cardiopatia = as.factor(case_when(
    CARDIOPATI == 1 ~ "sim",
    CARDIOPATI == 2 ~ "não",
    CARDIOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(pneumopatia = as.factor(case_when(
    PNEUMOPATI == 1 ~ "sim",
    PNEUMOPATI == 2 ~ "não", 
    PNEUMOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(renal = as.factor(case_when(
    RENAL == 1 ~ "sim",
    RENAL == 2 ~ "não", 
    RENAL == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(obesidade = as.factor(case_when(
    OBESIDADE == 1 ~ "sim",
    OBESIDADE == 2 ~ "não",
    OBESIDADE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(uti = as.factor(case_when(
    UTI == 1 ~ "Sim",
    UTI == 2 ~ "Não",
    UTI == 9 ~ "Ignorado",
    TRUE ~ "Em branco"))
  ) %>% 
  mutate(suport_ven = as.factor(case_when(
    SUPORT_VEN == 1 ~ "Sim, invasivo",
    SUPORT_VEN == 2 ~ "Sim, não invasivo",
    SUPORT_VEN == 3 ~ "Não",
    SUPORT_VEN == 9 ~ "Ignorado",
    TRUE ~ "Em branco"))
  ) %>%
  mutate(intubacao_SN = as.factor(case_when(
    SUPORT_VEN == 1 ~ "Sim",
    SUPORT_VEN == 2 ~ "Não",
    SUPORT_VEN == 3 ~ "Não",
    SUPORT_VEN == 9 ~ "Ignorado",
    TRUE ~ "Em branco"))
  ) %>% 
  mutate(evolucao = as.factor(case_when(
    EVOLUCAO == 1 ~ "Cura",
    EVOLUCAO == 2 ~ "Obito",
    EVOLUCAO == 3 ~ "Obito",
    EVOLUCAO== 9 ~ "Ignorado",
    TRUE ~ "Em branco"))
  ) %>% 
  select(DT_SIN_PRI, SG_UF, classi_gesta_puerp, raca, escolaridade, vacina,            
         febre, tosse, garganta, dispneia, desc_resp, saturacao, diarreia, 
         cardiopatia, pneumopatia, renal, obesidade, uti, suport_ven, 
         intubacao_SN, diabetes, perd_pala, fadiga, dor_abd, perd_olft, 
         evolucao, idade, classi_fin) 

saveRDS(dados_real,"dados_real.rds")


