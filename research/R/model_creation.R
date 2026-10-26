# === Librerie ===
library(readxl)
library(sf)
library(dplyr)
library(stringr)
library(lme4)
library(glmmTMB)
library(ggplot2)
library(MuMIn)
library(fitdistrplus)
library(scales)
library(car)
library(nlme)
library(MuMIn)
library(MASS)
library(sjPlot)
library(readr)
library(tidyr)
library(forcats)
library(stringi)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(insight)
library(data.table)

#####
#PREPARAZIONE DATAFRAME

df_base <- fread("mixed_model_dataset.csv", encoding = "UTF-8", quote = "")
setnames(df_base, gsub('"', '', names(df_base)))

df <- df_base %>% 
  filter(!is.na(total_index_avg))
df <- df %>% 
  filter(!is.na(Anno.di.imposta))



categorical_vars <- c(
  "Codice.Istat.Comune",  
  "Sigla.Provincia",          
  "Regione"
)

df_clean <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))



#####
#TRASFORMAZIONE COVARIATE 

# === 2. Preprocessing e trasformazioni ===

# Converti tutte le colonne che iniziano con "Reddito.complessivo" a numerico
df_clean <- df_clean %>%
  mutate(across(
    matches("^Reddito\\.complessivo"),
    ~ as.numeric(.)
  ))

df_model <- df_clean %>%
  mutate(
    reddito_medio_comune = (
      coalesce(`Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro`, 0) +
        coalesce(`Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro`, 0)
    ) /
      (
        coalesce(`Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.0.a.10000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.10000.a.15000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.15000.a.26000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.26000.a.55000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.55000.a.75000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.da.75000.a.120000.euro...Frequenza`, 0) +
          coalesce(`Reddito.complessivo.oltre.120000.euro...Frequenza`, 0)
      )
  ) %>%
  rename(
    fragilita = "Indice.composito.di.fragilit.comunale....decile.",
    occupazione = "Tasso.di.occupazione..20.64.anni.",
    dipendenza_pop = "Indice.di.dipendenza.della.popolazione.aggiustato",
    incremento_pop = "Tasso.di.incremento.della.popolazione",
    aree_protette = "Aree.protette",
    densita_servizi = "Densità.delle.unità.locali.dell.industria.e.dei.servizi....ventile.",
    addetti_bassa_prod = "Addetti.in.unità.locali.a.bassa.produttività.di.settore.per.l.industria.e.i.servizi....ventile.",
    reddito = "Reddito.imponibile...Ammontare.in.euro",
    provincia = "Sigla.Provincia"
  ) %>%
  mutate(
    reddito_log = log(reddito + 1)
  ) %>%
  # Converti tutto in numerico dove necessario
  mutate(across(
    c(fragilita, occupazione, incremento_pop, aree_protette, dipendenza_pop,
      reddito_log, reddito_medio_comune, reddito),
    ~ as.numeric(as.character(.))
  )) %>%
  # Scaling (standardizzazione)
  mutate(across(
    c(occupazione, incremento_pop, aree_protette, dipendenza_pop,
      densita_servizi, addetti_bassa_prod, reddito_log, reddito_medio_comune),
    scale
  )) %>%
  # Correzione per beta regression
  mutate(
    N = n(),
    total_index_beta = (total_index_avg * (N - 1) + 0.5) / N
  )

levels(df_model$provincia) <- c(levels(df_model$provincia), "NA")
df_model$provincia[df_model$provincia == "" | is.na(df_model$provincia)] <- "NA"

# Vector of all the relevant columns
covariates_economiche_NA_0 <- c(
  "Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza",
  "Reddito.complessivo.minore.o.uguale.a.zero.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
  "Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
  "Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
  "Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
  "Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
  "Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro",
  "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
  "Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro",
  "Reddito.complessivo.oltre.120000.euro...Frequenza",
  "Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro"
)

# Replace NAs with 0 in these covariates
df_model <- df_model %>%
  mutate(across(all_of(covariates_economiche_NA_0), ~replace_na(., 0)))

df_model <- df_model %>%
  rename(
    redd_comp_0_freq         = "Reddito.complessivo.minore.o.uguale.a.zero.euro...Frequenza",
    redd_compl_0_10_freq     = "Reddito.complessivo.da.0.a.10000.euro...Frequenza",
    redd_compl_10_15_freq    = "Reddito.complessivo.da.10000.a.15000.euro...Frequenza",
    redd_compl_15_26_freq    = "Reddito.complessivo.da.15000.a.26000.euro...Frequenza",
    redd_compl_26_55_freq    = "Reddito.complessivo.da.26000.a.55000.euro...Frequenza",
    redd_compl_55_75_freq    = "Reddito.complessivo.da.55000.a.75000.euro...Frequenza",
    redd_compl_75_120_freq   = "Reddito.complessivo.da.75000.a.120000.euro...Frequenza",
    redd_compl_oltre_120_freq = "Reddito.complessivo.oltre.120000.euro...Frequenza"
  )



#redd_comp_0_freq, redd_compl_0_10_freq, redd_compl_10_15_freq, redd_compl_15_26_freq, redd_compl_26_55_freq
#redd_compl_55_75_freq, redd_compl_75_120_freq, redd_compl_oltre_120_freq
#fragilita, occupazione, incremento_pop, aree_protette,
#densita_servizi, addetti_bassa_prod, reddito_log, reddito_medio_comune


# === 4. Modelli lineari misti (per confronto) ===
mod0 <- lmer(total_index_avg ~ 1 + (1 | provincia), data = df_model)
mod1 <- lmer(total_index_avg ~ reddito_log + reddito_medio_comune + (1 | provincia), data = df_model)
mod2 <- lmer(total_index_avg ~ reddito_log + reddito_medio_comune + occupazione + (1 | provincia), data = df_model)
mod3 <- lmer(total_index_avg ~ reddito_log + reddito_medio_comune + occupazione + fragilita + (1 | provincia), data = df_model)
mod4 <- lmer(total_index_avg ~ reddito_log + reddito_medio_comune + occupazione + fragilita + incremento_pop + (1 | provincia), data = df_model)
mod5 <- lmer(total_index_avg ~ reddito_log + reddito_medio_comune + occupazione + fragilita + incremento_pop +
               aree_protette + densita_servizi + addetti_bassa_prod + (1 | provincia),
             data = df_model)

anova(mod0, mod1, mod2, mod3, mod4, mod5)


# === 5. Modello beta misto con glmmTMB ===
mod_beta <- glmmTMB(
  total_index_beta ~ reddito_log + reddito_medio_comune + occupazione + fragilita + incremento_pop +
    aree_protette + densita_servizi + addetti_bassa_prod + (1 | provincia),
  data = df_model,
  family = beta_family(link = "logit")
)
summary(mod_beta)

# Confronto AIC tra modelli lineari e beta
AIC(mod0, mod1, mod2, mod3, mod4, mod5, mod_beta)


# === 6. Selezione automatica delle variabili (dredge) ===
options(na.action = "na.fail")  # richiesto da dredge

mod_full <- glmmTMB(
  total_index_beta ~ reddito_log + reddito_medio_comune + occupazione + fragilita + 
    incremento_pop + densita_servizi + (1 | provincia),
  data = df_model,
  family = beta_family(link = "logit")
)


#####
#ADD region

# 1. Fit modello lineare semplice (senza random effects)
lm_simple <- lm(total_index_beta ~ reddito_medio_comune + fragilita + densita_servizi, data = df_model)

# 2. Stima lambda con Box-Cox
bc <- boxcox(lm_simple, lambda = seq(-2, 2, 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
cat("Lambda ottimale:", lambda_opt, "\n")

# 3. Trasforma la variabile risposta in base a lambda
if (abs(lambda_opt) < 0.1) {
  # lambda vicino a zero => trasformazione logaritmica
  df_model$bc_total_index <- log(df_model$total_index_beta + 1e-6)
} else {
  # trasformazione Box-Cox generale
  df_model$bc_total_index <- (df_model$total_index_beta^lambda_opt - 1) / lambda_opt
}

# 4. Rifit del modello misto con la variabile trasformata

ctrl <- lmeControl(opt = "optim", maxIter = 300, msMaxIter = 300)

fit <- lm( total_index_beta ~ reddito_medio_comune, data = df_model)
summary(fit)

mod_nlme <- lme(
  bc_total_index ~ reddito_medio_comune + as.factor(fragilita) + as.factor(densita_servizi) + Consumo.del.suolo + Raccolta.indifferenziata.dei.rifiuti.urbani.per.abitante,
  random = ~1 | Regione/provincia,
  weights = varIdent(form = ~1| provincia),
  data = df_model,
  control = ctrl
)

summary(mod_nlme)
VarCorr(mod_nlme)

r.squaredGLMM(mod_nlme)

vif(lm(bc_total_index ~ reddito_medio_comune + fragilita + densita_servizi, data = df_model))


# 5. Controllo residui del modello trasformato
par(mfrow=c(1,2))
plot(resid(mod_nlme), main = "Residui modello con Box-Cox")
qqnorm(resid(mod_nlme)); qqline(resid(mod_nlme))

# Istogramma dei residui
hist(resid(mod_nlme), breaks = 100, main = "Istogramma dei residui", xlab = "Residui")


par(mfrow=c(1,1))

# Residui standardizzati
std_resid <- resid(mod_nlme, type = "normalized")

# Plot dei residui standardizzati per identificare outlier
plot(std_resid, main = "Residui standardizzati")
abline(h = c(-3, 3), col = "red", lty = 2)


plot(mod_nlme, resid(., type = "pearson") ~ fitted(.), col = "red", pch = 20, cex = 0.5)

# Residui vs fitted
plot(fitted(mod_nlme), resid(mod_nlme),
     xlab = "Valori Fitted", ylab = "Residui",
     main = "Residui vs Fitted")
abline(h = 0, col = "red")

# Estrai effetti casuali a livello provincia 
ran_prov <- ranef(mod_nlme, level = 2) %>%
  tibble::rownames_to_column("provincia") %>%
  rename(effect = `(Intercept)`)

# Calcola lo standard deviation dal modello
vc <- VarCorr(mod_nlme)

sd_prov <- as.numeric(vc[2, "StdDev"])

# Costruisci intervalli ± 1.96 * sd (approssimazione 95%)
ran_prov <- ran_prov %>%
  mutate(
    lower = effect - 1.96 * sd_prov,
    upper = effect + 1.96 * sd_prov
  )

# Grafico con intervallo
ggplot(ran_prov, aes(x = reorder(provincia, effect), y = effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "Effetti Casuali per Provincia con Intervallo di Confidenza 95%",
    x = "Provincia", y = "Effetto Casuale (Intercept)"
  ) +
  theme_minimal()

intervals(mod_nlme, which = "fixed")
