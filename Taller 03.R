.rs.restartR()
rm(list = ls()) # Limpiemos environment y reiniciemos sistema

#Librerías necesarias:
library(tidyverse)
library(lubridate)

## Cargamos los datos
core <- read.csv(file.choose(), check.names = FALSE)

core <- core %>%
  mutate(
    Site = factor(Site),
    Tree = factor(Tree),
    Year = as.integer(Year)
  )
##Aquí empieza el punto 1
#Tabla descriptiva
tabla_desc <- core %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(Site),
    n_trees = n_distinct(Tree),
    year_min = min(Year, na.rm = TRUE),
    year_max = max(Year, na.rm = TRUE)
  )
##distribución BAI 
ggplot(core, aes(x = BAI)) +
  geom_histogram(bins = 30, color = "black", fill = "grey70") +
  scale_x_continuous(trans = "log10") +
  labs(x = "BAI (escala log10)", y = "Frecuencia")
tabla_desc

##BAI en el tiempo para más de 2 sitios
sites_sel <- core %>% distinct(Site) %>% slice(1:3) %>% pull(Site)

core %>%
  filter(Site %in% sites_sel) %>%
  ggplot(aes(x = Year, y = BAI, group = Tree)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Site, scales = "free_y") +
  labs(y = "BAI", x = "Year")


## Modelo mixto _Punto 2_:
library(lme4)
library(performance)

m_base <- lmer(
  log(BAI) ~ log(Basal.Area) + (1 | Site/Tree),
  data = core
)

summary(m_base)
VarCorr(m_base)
check_model(m_base)

##Modelo espacial
m_spatial <- lmer(
  log(BAI) ~ log(Basal.Area) + bio01.norm + bio12.norm + (1 | Site/Tree),
  data = core
)

AIC(m_base, m_spatial)

###Efecto del clima:
library(ggeffects)

eff_bio01 <- ggpredict(m_spatial, terms = "bio01.norm")
plot(eff_bio01)

eff_bio12 <- ggpredict(m_spatial, terms = "bio12.norm")
plot(eff_bio12)

##Modelo temporal con pendientes aleatorias:
m_temp <- lmer(
  log(BAI) ~ log(Basal.Area) +
    mj_tmax + ja_ppt +
    (1 + mj_tmax | Site) +
    (1 | Site:Tree),
  data = core
)

AIC(m_spatial, m_temp)

## pendientes con sitio:
install.packages("broom.mixed")
library(broom.mixed)

ranef_df <- ranef(m_temp)$Site %>%
  rownames_to_column("Site")

ggplot(ranef_df, aes(x = Site, y = mj_tmax)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Pendiente aleatoria (Tmax MJ)", x = "Sitio")



