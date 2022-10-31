# Shapley Values Plot on the XGBoost model

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

# function to predict new data

predict_function <- function(model, newdata) {
  predict(model, new_data = newdata)$.pred_class
}

# predictor 

predictor <- Predictor$new(
  model = xgb_wf, 
  data = X,
  predict.function = predict_function,
  y = y, 
  type = "prob"
)

# Interpretability ---------------------

shapley <- Shapley$new(
  predictor, 
  x.interest = X[10, ]
)

shapley$results <- filter(shapley$results, as.integer(class) == 1)

g <- shapley$plot() + 
  ggtitle("Valores Shapley") +
  labs(y = "Shapley value", x = "valores das variáveis") +
  theme_bw()

g

saveRDS(g, "g_shapley.rds")

ggsave(
  "result/shapley_plot_xgb.png",
  width = 10,
  height = 10
)