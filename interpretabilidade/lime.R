# Local Surrogate (LIME) Plot on the XGBoost model

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

# predictor 

predictor <- Predictor$new(
  model = xgb_wf, 
  data = X,
  y = y, 
  type = "prob"
)

# Interpretability ---------------------

lime <- LocalModel$new(
  predictor, 
  k = 4,
  x.interest = X[1, ]
)

lime$results$.class <- ifelse(lime$results$.class == ".pred_covid-19", "covid-19", "não-covid")

lime$results <- filter(lime$results, .class == "covid-19")

g <- plot(lime) + 
  ggtitle("LIME") +
  labs(x = "valor da variável") + 
  theme_bw()

g

saveRDS(g, "g_lime.rds")

ggsave(
  "result/lime_plot_xgb.png",
  width = 10,
  height = 10
)