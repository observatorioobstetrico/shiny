# Individual Conditional Expectation (ICE) Plot on the XGBoost model

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

# Interpretability ----------------------

ice <- FeatureEffect$new(
  preditor, 
  method = "pdp+ice", 
  feature = "diarreia"
)

levels(ice$results$.class) <- c("sim","não")

ice$results <- filter(ice$results, as.integer(.class) == 1)

g <- plot(ice) + 
  ggtitle("ICE") + 
  scale_y_continuous("probabilidade de diarreia predita") +
  theme_bw()

g

saveRDS(g, "g_ice.rds")

ggsave(
  "result/ice_plot_xgb.png",
  width = 16,
  height = 10
)