# dados treino

dados_treino <- readRDS("dados_treino.rds")

# modelo ajustado

xgb_rec <- recipes::recipe(classi_fin ~ ., data = dados_treino) %>% 
  recipes::step_normalize(idade) %>%
  themis::step_smotenc(classi_fin,seed=69) %>% 
  recipes::step_dummy(all_nominal(), -classi_fin) 

xgb_spec <- parsnip::boost_tree(
  trees = 1000,
  tree_depth = 12, min_n = 29, 
  loss_reduction = 0.240,                     
  sample_size = 0.743, mtry = 10,         
  learn_rate = 0.0131,                         
) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("classification")

xgb_wf <- workflows::workflow() %>%
  workflows::add_recipe(xgb_rec) %>%
  workflows::add_model(xgb_spec) %>% 
  parsnip::fit(dados_treino)
