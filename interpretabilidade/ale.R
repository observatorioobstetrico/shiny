# Accumulated Local Effects (ALE) Plot on the XGBoost model

# Libraries ----------------------------

library(janitor)
library(ggthemes)
library(iml)

# Predictor ----------------------------

# dados

X <- readRDS("dados_treino.rds")[-2]

y <- readRDS("dados_treino.rds")$classi_fin

# modelo ajustado

# xgb_wf

# funcao para prever novos dados

predict_function <- function(model, newdata){
  predict(model, new_data = newdata)$.pred_class
}

# preditor

preditor <- Predictor$new(
  model = xgb_wf, 
  data = X,
  predict.function = predict_function,
  y = y, 
  type = "prob"
)

# Interpretability ---------------------

ale <- FeatureEffect$new(
  preditor, 
  method = "ale", 
  feature = "diarreia"
)

ale$results <- filter(ale$results, as.integer(.class) == 1)

g <- ale$plot() + 
  ggtitle("ALE") + 
  scale_y_continuous("diferença para a predição média") +
  theme_bw()

g

saveRDS(g, "g_ale.rds")

ggsave(
  "result/ale_plot_xgb.png",
  width = 16,
  height = 10
)