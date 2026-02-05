rm(list = ls()) #Eliminar datos previos del Environment 

datos <- read.csv(file.choose()) # Extraer los datos del archivo Lack 

ls() # pedirle al sistema los datos cargados en environment

#Punto 1 del taller

pairs(~ BeakH + UBeakL + NUBkL, data = datos) # Gráficos

lm(BeakH ~ UBeakL, data = datos) #Regresiones lineales
lm(BeakH ~ NUBkL, data = datos)

#punto 2 del taller
aov(BeakH ~ TaxonOrig, data = datos) #ANOVA Análisis por especie e isla
aov(BeakH ~ IslandID, data = datos)

lm(BeakH ~ UBeakL * TaxonOrig, data = datos) #ANCOVA análisis por especir e isla
lm(BeakH ~ UBeakL * IslandID, data = datos)

model <- lm(BeakH ~ UBeakL, data = datos) #Análisis de Residuos
datos$shape_resid <- residuals(model)
aov(shape_resid ~ TaxonOrig, data = datos)

#punto 3 del taller
library(dplyr)

richness <- datos %>%  #Riqueza de especies por isla
  group_by(IslandID) %>%
  summarise(n_species = n_distinct(TaxonOrig))

finches <- left_join(datos, richness, by = "IslandID") #Asociación individual

finches <- finches %>% #Diferencia intraespecífica
  group_by(TaxonOrig) %>%
  mutate(dev = abs(BeakH - mean(BeakH)))

lm(dev ~ n_species, data = finches) #Análisis estadístico

#Punto 4 del taller


#Relación sugerida BeakH ~ UBeakL
set.seed(123)

n_vals <- c(10, 20, 50, 100)
power <- numeric(length(n_vals))

for (i in seq_along(n_vals)) {
  
  pvals <- rep(NA, 1000)
  
  for (j in 1:1000) {
    
    idx <- sample(seq_len(nrow(finches)), n_vals[i], replace = TRUE)
    sim_data <- finches[idx, ]
    
    fit <- lm(BeakH ~ UBeakL, data = sim_data)
    coefs <- summary(fit)$coefficients
    
    if (nrow(coefs) > 1) {
      pvals[j] <- coefs["UBeakL", "Pr(>|t|)"]
    }
  }
  
  power[i] <- mean(pvals < 0.05, na.rm = TRUE)
}

plot(n_vals, power, type = "b", #Visualización
     xlab = "Tamaño de muestra (n)",
     ylab = "Poder estadístico",
     ylim = c(0,1))