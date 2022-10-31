# Feature Interaction on the XGBoost model

# Libraries ----------------------------

library(janitor)
library(ggthemes)
library(iml)

# Predictor ----------------------------

set.seed(25)

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

feat_int <- Interaction$new(
  preditor
)

feat_int$results <- filter(feat_int$results, as.integer(.class) == 1)

g1 <- plot(feat_int) + 
  ggtitle("Interação total das variáveis") +
  scale_x_continuous("força da interação") +
  scale_y_discrete("variável") +
  theme_bw()

g1

saveRDS(g1, "g1_featint.rds")

ggsave(
  "result/feat-int_plot_xgb.png",
  width = 16,
  height = 10
)

two_feat_int <- Interaction$new(
  preditor, 
  feature = "diarreia"
)

two_feat_int$results <- filter(two_feat_int$results, as.integer(.class) == 1)

g2 <- plot(two_feat_int) + 
  ggtitle("Interação bidimensional das variáveis") +
  scale_x_continuous("força da interação") +
  scale_y_discrete("variáveis") +
  theme_bw()

g2

saveRDS(g2, "g2_featint.rds")

ggsave(
  "result/two_feat-int_plot_xgb.png",
  width = 16,
  height = 10
)