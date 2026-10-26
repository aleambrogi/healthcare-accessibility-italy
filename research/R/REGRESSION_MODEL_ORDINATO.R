# === Librerie ===
library(readxl)
library(sf)
library(dplyr)
library(stringr)
library(lme4)
library(glmmTMB)
library(ggplot2)
library(fitdistrplus)
library(scales)
library(car)
library(MuMIn)
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

#####
#PREPARAZIONE DATAFRAME
library(data.table)
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
      densita_servizi, addetti_bassa_prod, reddito_log, reddito_medio_comune, reddito),
    ~ as.numeric(as.character(.))
  )) %>%
  # Scaling (standardizzazione)
  mutate(across(
    c(fragilita, occupazione, incremento_pop, aree_protette, dipendenza_pop,
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


# Fit distribuzione per vedere i parametri shape
fit_beta <- fitdist(df_clean$total_index_avg, "lnorm", method = "mle")
a <- fit_beta$estimate["shape1"]
b <- fit_beta$estimate["shape2"]

# Plot istogramma + densità + funzione beta
ggplot(df_clean, aes(x = total_index_avg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1.2) +
  stat_function(fun = dbeta, args = list(shape1 = a, shape2 = b), color = "darkgreen", linewidth = 1.2, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Distribuzione di total_index_avg con fit Beta")



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
#FITTING THE MODEL

mod_full_2 <- glmmTMB(
  total_index_beta ~ reddito_log + reddito_medio_comune + occupazione + fragilita + 
    incremento_pop + densita_servizi + (1 + reddito_medio_comune| provincia),
  data = df_model_no_out,
  family = beta_family(link = "logit")
)


#####
#ADD region

library(nlme)



ctrl <- lmeControl(opt = "optim", maxIter = 300, msMaxIter = 300)
mod_nlme <- lme(
  total_index_beta ~ reddito_medio_comune + occupazione + fragilita + 
    incremento_pop + densita_servizi,
  random = ~1 | Regione/provincia,
  weights = varIdent(form = ~1 | provincia),
  data = df_model,
  control = ctrl
)

summary(mod_nlme)
VarCorr(mod_nlme)

library(MuMIn)
r.squaredGLMM(mod_nlme)

plot(mod_nlme$residuals)




plot(mod_nlme)
conditional_effects(mod_nmle)

summary(df_model$total_index_beta)


# Add fitted values and residuals to df
df_model$fitted <- predict(mod_nmle, type = "response")
df_model$resid <- residuals(mod_nmle, type = "pearson")

plot(df_model$fitted, df_model$resid,
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)


# Check number of unique regions
n_regioni <- length(levels(df_model$Regione))
stopifnot(n_regioni == 21)  # Sanity check

unique(df_model$Regione)


# Assign region-based groups
df_model$group21 <- df_model$Regione  # Directly use Regione levels as group labels

# (Optional) Rename for consistency: "Group 1", ..., "Group 20"
levels(df_model$group21) <- paste0("Group ", seq_len(n_regioni))

range(df_model$resid, na.rm = TRUE)


ggplot(df_model, aes(x = reddito_medio_comune, y = resid, color = group21)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-1, 2.4)) +
  theme_minimal() +
  labs(title = "Residuals Colored by Region Groups",
       y = "Pearson Residuals", x = "Reddito Medio Comune") +
  scale_color_viridis_d(option = "turbo")

# Split into 5 chunks for plotting



# Unique, sorted province levels
province_levels <- sort(unique(df_model$provincia))

# Total number of provinces
n_prov <- length(province_levels)

prov_splits <- split(province_levels, ceiling(seq_along(province_levels) / ceiling(n_prov / 5)))

# First plot: fitted regression lines
for (i in seq_along(prov_splits)) {
  x11()  # Open new graphics window
  df_subset <- df_model %>% filter(provincia %in% prov_splits[[i]])
  
  p <- ggplot(df_subset, aes(x = reddito_medio_comune, y = total_index_avg)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(y = fitted, color = provincia), linewidth = 0.8) +
    facet_wrap(~ provincia, scales = "free") +
    theme_minimal() +
    labs(title = paste("Fitted Regression per Province - Tab", i),
         y = "Total Index Avg", x = "Reddito Medio Comune")
  
  print(p)
}

# Second plot: residuals per province
for (i in seq_along(prov_splits)) {
  x11()  # New graphics window
  df_subset <- df_model %>% filter(provincia %in% prov_splits[[i]])
  
  p <- ggplot(df_subset, aes(x = reddito_medio_comune, y = resid)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~ provincia, scales = "free_x") +
    theme_minimal() +
    labs(title = paste("Residuals per Province - Tab", i),
         y = "Pearson Residuals", x = "Reddito Medio Comune")
  
  print(p)
}

# Load necessary libraries
library(glmmTMB)
library(DHARMa)  # For residual diagnostics

# Simulate residuals from the model using DHARMa
sim_res <- simulateResiduals(fittedModel = mod_nmle, plot = FALSE)
plot(sim_res)


testUniformity(sim_res)
testOutliers(sim_res)


