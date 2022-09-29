# XGBoost

# Libraries -----------------

library(tidymodels)
library(janitor)
library(stringr)
library(vip)
library(probably)
library(gt)

# Loading data --------------

dataset <- readr::read_csv("srag_16-22_novo.gz")

# Tidying data --------------

# filters and variable recoding

dataset <- dataset  %>% 
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
    CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp"))
  ) %>% 
  filter(!is.na(classi_gesta_puerp)) %>% # 37.847 points
  mutate(classi_fin = as.factor(case_when(
    CLASSI_FIN == 1 ~ "não-covid",
    CLASSI_FIN == 2 ~ "não-covid",
    CLASSI_FIN == 3 ~ "não-covid",
    CLASSI_FIN == 5 ~ "covid-19",
    TRUE ~ NA_character_))
  ) %>% 
  filter(!is.na(classi_fin)) %>%  # 20.619 points
  mutate(raca = as.factor(case_when( # race
    CS_RACA == 1 ~ "branca",
    CS_RACA == 2 | CS_RACA == 3 | CS_RACA == 4 |
      CS_RACA == 5 ~ "não branca",
    CS_RACA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%   
  mutate(escolaridade = as.factor(case_when( # instruction
    # ~ "sem escolaridade",
    CS_ESCOL_N == 0 | CS_ESCOL_N == 1 | CS_ESCOL_N == 2 ~ "até fundamental",
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
  ) #%>% 
# mutate(uti = as.factor(case_when(
#   UTI == 1 ~ "Sim",
#   UTI == 2 ~ "Não",
#   UTI == 9 ~ "Ignorado",
#   TRUE ~ "Em branco"))
# ) %>% 
# mutate(suport_ven = as.factor(case_when(
#   SUPORT_VEN == 1 ~ "sim, invasivo",
#   SUPORT_VEN == 2 ~ "sim, não invasivo",
#   SUPORT_VEN == 3 ~ "não",
#   SUPORT_VEN == 9 ~ "ignorado",
#   TRUE ~ "em branco"))
# ) %>% 
# mutate(evolucao = as.factor(case_when(
#   EVOLUCAO == 1 ~ "cura",
#   EVOLUCAO == 2 ~ "óbito",
#   EVOLUCAO == 3 ~ "óbito",
#   EVOLUCAO == 9 ~ "ignorado",
#   TRUE ~ "em branco"))
# )  



dataset$classi_fin <- relevel(
  dataset$classi_fin ,
  ref = "covid-19"
) 

# dataset <- cbind(dataset,data_componentes)

# Selecting data ------------

srag_data <- dataset %>% # 20.629 points
  dplyr::select(!where(is.numeric), idade, -c(ano, DT_SIN_PRI, sexo,SG_UF)) %>% # pneumopatia, saturacao, renal, obesidade, desc_resp, dispneia,
  # mutate_if(is.factor, as.numeric) %>% 
  drop_na() %>% 
  as_tibble() 

# Train/test data -----------

set.seed(123)

srag_split <- initial_split(
  srag_data, 
  prob = 0.7
)

srag_train <- training(srag_split)

saveRDS(srag_train,"Modelo/dados_treino.rds")

srag_test <- testing(srag_split)

# Recipe ---------------------

xgb_rec <- recipe(classi_fin ~ ., data = srag_train) %>% 
  step_normalize(idade) %>%
  step_dummy(all_nominal(), -classi_fin) %>% 
  themis::step_smote(classi_fin,seed = 69) #%>% 
# prep()

# sort(table(bake(xgb_rec,new_data=NULL)$classi_fin,useNA = "always"))

# Model specification -------
# mtry min_n tree_depth learn_rate loss_reduction sample_size .config              
# <int> <int>      <int>      <dbl>          <dbl>       <dbl> <chr>                
#   6    11          2     0.0582         0.0882       0.476 Preprocessor1_Model12

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Grid search --------------

xgb_grid <- grid_max_entropy(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), srag_train),
  learn_rate(),
  size = 30
)

# Workflow ----------------

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec) #%>% 
#fit(srag_train)

# Cross-validation --------

set.seed(456)

srag_folds <- vfold_cv(
  srag_train, 
  v = 10, 
  repeats = 5
)   

# Tuning ------------------

xgb_metrics <- metric_set(roc_auc, sensitivity, specificity, npv, ppv)

doParallel::registerDoParallel()

set.seed(1011)

xgb_res <- xgb_wf %>% 
  tune_grid(
    resamples = srag_folds,
    grid = xgb_grid,
    metrics = xgb_metrics,
    control = control_grid(save_pred = TRUE)
  )


saveRDS(xgb_res,"xgb_res_artigo1_16-22_final.rds") #smote sem desfecho com raça2 e escolaridade "até fundamental"
xgb_res <- readRDS("xgb_res_artigo1_16-22_final.rds")

# metrics

collect_metrics(xgb_res)

# best hyperparameters

show_best(xgb_res, "roc_auc")

# best auc

best_auc <- select_best(xgb_res, "roc_auc"); best_auc  

# Best model --------------

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

# Final fit model ---------

final_fit <- last_fit(
  final_xgb, 
  srag_split,
)

collect_metrics(final_fit)

final_xgb %>%
  fit(data = srag_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

conf_mat(
  data = final_fit$.predictions[[1]],
  truth = classi_fin,
  estimate = .new_pred_class
) %>% 
  autoplot(type = "heatmap")

preds <- final_fit %>% 
  collect_predictions() 

summary(conf_mat(preds, classi_fin, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() %>% 
  fmt_number(columns = 2, decimals = 4)

# Defining a threshold ----

thresholds <- preds %>%
  threshold_perf(classi_fin, `.pred_covid-19`, thresholds = seq(0, 1, by = 0.0025))

best_thresh <- thresholds %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold) %>% 
  max()

preds_new <- preds %>% 
  mutate(.new_pred_class = factor(ifelse(`.pred_covid-19` >= best_thresh, "covid-19", "não-covid"), 
                                  levels = c("covid-19", "não-covid")))


final_fit$.predictions[[1]]$`.new_pred_class` = preds_new$.new_pred_class

matriz_conf <- conf_mat(
  data = final_fit$.predictions[[1]],
  truth = classi_fin,
  estimate = .new_pred_class
) %>% 
  autoplot(type = "heatmap")

saveRDS(matriz_conf,"matriz_conf.rds")

# Comparing models -------

summary(conf_mat(preds, classi_fin, .pred_class)) %>%
  dplyr::select(-.estimator) %>%
  rename(old_threshold = .estimate) %>%
  bind_cols(.,
            summary(conf_mat(preds_new, classi_fin, .new_pred_class)) %>%
              dplyr::select(.estimate) %>%
              rename(new_threshold = .estimate)) %>% 
  gt() %>%
  fmt_number(columns = c(2, 3),
             decimals = 4) %>% 
  dplyr::select()


a <- summary(conf_mat(preds_new, classi_fin, .new_pred_class)) %>%
  dplyr::select(-.estimator) %>%
  gt() %>% 
  fmt_number(columns = 2, decimals = 4)

saveRDS(a,"metricas_modelo.rds")
