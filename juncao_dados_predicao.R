dados_pred <- readRDS("dados_xgbost_pred.rds")
dataset <- readr::read_csv("srag_16-22_novo.gz")


dataset1 <- dataset  %>% 
  rename(sexo = CS_SEXO, idade = NU_IDADE_N) %>% 
  filter(as.Date(DT_SIN_PRI,format="%d/%m/%Y") < as.Date("01-12-2021",format="%d-%m-%Y")) %>% 
  filter(sexo == "F") %>% # 1.342.879 points
  filter(idade > 9 & idade < 56) %>% # 502.903 points
  mutate(ano = str_sub(DT_SIN_PRI, start = 7)) %>% 
  mutate(classi_gesta_puerp = as.factor(case_when(
    CS_GESTANT == 1 ~ "1tri",
    CS_GESTANT == 2 ~ "2tri",
    CS_GESTANT == 3 ~ "3tri",
    CS_GESTANT == 4 ~ "IG_ig",
    CS_GESTANT == 5 & PUERPERA == 1 ~ "puerp",
    CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
    TRUE ~ "não"))
  ) %>% 
  filter(classi_gesta_puerp != "não") %>% # 37.847 points
  mutate(classi_fin = as.factor(case_when(
    CLASSI_FIN == 1 ~ "não-covid",
    CLASSI_FIN == 2 ~ "não-covid",
    CLASSI_FIN == 3 ~ "não-covid",
    CLASSI_FIN == 4 ~ "não-especificado",
    CLASSI_FIN == 5 ~ "covid-19",
    CLASSI_FIN == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  filter(!is.na(classi_fin)) %>%  # 20.619 points
  mutate(raca = as.factor(case_when( # race
    CS_RACA == 1 ~ "branca",
    CS_RACA == 2 ~ "não branca",
    CS_RACA == 3 ~ "não branca",
    CS_RACA == 4 ~ "não branca",
    CS_RACA == 5 ~ "não branca",
    CS_RACA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(escolaridade = as.factor(case_when( # instruction
    CS_ESCOL_N == 1 | CS_ESCOL_N == 0 | CS_ESCOL_N == 2 ~ "até fundamental",
    CS_ESCOL_N == 3 ~ "médio",
    CS_ESCOL_N == 4 ~ "superior",
    CS_ESCOL_N == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(vacina = as.factor(case_when( #vacine
    VACINA == 1  ~ "sim",
    VACINA == 2 ~ "não",
    VACINA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(febre = as.factor(case_when( # fever
    FEBRE == 1 ~ "sim",
    FEBRE == 2 ~ "não",
    FEBRE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(tosse = as.factor(case_when( # cough
    TOSSE == 1 ~ "sim",
    TOSSE == 2 ~ "não",
    TOSSE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(garganta = as.factor(case_when( # throat
    GARGANTA == 1 ~ "sim",
    GARGANTA  == 2 ~ "não",
    GARGANTA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(dispneia = as.factor(case_when( # dyspnea
    DISPNEIA == 1 ~ "sim",
    DISPNEIA == 2 ~ "não",
    DISPNEIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(desc_resp = as.factor(case_when( # respiratory distress
    DESC_RESP == 1 ~ "sim",
    DESC_RESP == 2 ~ "não",
    DESC_RESP == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(saturacao = as.factor(case_when( # saturation
    SATURACAO == 1 ~ "sim",
    SATURACAO == 2 ~ "não",
    SATURACAO == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(diarreia = as.factor(case_when( # diarrhea
    DIARREIA == 1 ~ "sim",
    DIARREIA == 2 ~ "não",
    DIARREIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(cardiopatia = as.factor(case_when( # heart disease
    CARDIOPATI == 1 ~ "sim",
    CARDIOPATI == 2 ~ "não",
    CARDIOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(pneumopatia = as.factor(case_when( # lung disease
    PNEUMOPATI == 1 ~ "sim",
    PNEUMOPATI == 2 ~ "não",
    PNEUMOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(renal = as.factor(case_when( # kidney disease
    RENAL == 1 ~ "sim",
    RENAL == 2 ~ "não",
    RENAL == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(obesidade = as.factor(case_when( # obesity
    OBESIDADE == 1 ~ "sim",
    OBESIDADE == 2 ~ "não",
    OBESIDADE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(uti = case_when(UTI == 1 ~ "Sim",
                         UTI == 2 ~ "Não",
                         UTI == 9 ~ "Ignorado",
                         TRUE ~ "Em branco")) %>% 
  mutate(suport_ven = case_when(SUPORT_VEN == 1 ~ "Sim, invasivo",
                                SUPORT_VEN == 2 ~ "Sim, não invasivo",
                                SUPORT_VEN == 3 ~ "Não",
                                SUPORT_VEN == 9 ~ "Ignorado",
                                TRUE ~ "Em branco")) %>% 
  mutate(intubacao_SN = case_when(SUPORT_VEN == 1 ~ "Sim",
                                  SUPORT_VEN == 2 ~ "Não",
                                  SUPORT_VEN == 3 ~ "Não",
                                  SUPORT_VEN == 9 ~ "Ignorado",
                                  TRUE ~ "Em branco")) %>% 
  mutate(diabetes = case_when(DIABETES == 1 ~ "Sim",
                              DIABETES == 2 ~ "Não",
                              DIABETES == 9 ~ "Ignorado",
                              TRUE ~ "Em branco")) %>% 
  mutate(perd_pala = case_when(PERD_PALA == 1 ~ "sim",
                               PERD_PALA == 2 ~ "não",
                               PERD_PALA == 9 ~ "ignorado",
                               TRUE ~ "em branco")) %>% 
  mutate(fadiga = case_when(FADIGA == 1 ~ "sim",
                            FADIGA == 2 ~ "não",
                            FADIGA== 9 ~ "ignorado",
                            TRUE ~ "em branco")) %>% 
  mutate(dor_abd = case_when(DOR_ABD == 1 ~ "sim",
                             DOR_ABD == 2 ~ "não",
                             DOR_ABD == 9 ~ "ignorado",
                             TRUE ~ "em branco")) %>% 
  mutate(perd_olft = case_when(PERD_OLFT == 1 ~ "sim",
                               PERD_OLFT == 2 ~ "não",
                               PERD_OLFT == 9 ~ "ignorado",
                               TRUE ~ "em branco")) %>% 
  mutate(evolucao = case_when(EVOLUCAO == 1 ~ "Cura",
                              EVOLUCAO == 2 ~ "Obito",
                              EVOLUCAO == 3 ~ "Obito",
                              EVOLUCAO == 9 ~ "Ignorado",
                              TRUE ~ "Em branco")) 


dataset2 <- dataset1 %>% 
  filter(classi_fin == "não-especificado" | classi_fin == "em branco" | classi_fin == "ignorado")

srag_data <- dataset2 %>% # 20.629 points
  dplyr::select(!where(is.numeric), idade, -c(ano, sexo)) %>% # pneumopatia, saturacao, renal, obesidade, desc_resp, dispneia,
  # mutate_if(is.factor, as.numeric) %>% 
  as_tibble() 


### unir bases não especificado

srag_data$classi_fin <- NULL
srag_data$classi_fin_pred <- dados_pred$classi_fin_pred

srag_data1 <- srag_data %>% 
  filter(as.Date(DT_SIN_PRI,format="%d/%m/%Y") > as.Date("01-01-2020",format="%d-%m-%Y")) 

saveRDS(srag_data1,"dados_preditos_novo.rds")


