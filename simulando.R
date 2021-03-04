
library(tidyverse)
library(patchwork)

options(scipen = 999)

theme_set(theme_classic())

# población de 3 estratos desiguales con minoría aleatoriamente distribuida
# población de 3 estratos con minoría concentrada

# representación de minorías en ambos casos son aleatoriedad simple y estratificada



# POBLACIÓN DE 3 ESTRATOS DESIGUALES CON MINORÍA ALEATORIAMENTE DISTRIBUIDA
# se simula que minoría representa 10% de la población

n = 100000

set.seed(1234)
data <- data.frame(person = 1:n, 
                   strata = c(rep("strat1", n*.5),
                              rep("strat2", n*.4),
                              rep("strat3", n*.1)),
                   minoria = rbinom(n, 1, .1))


sum(data$minoria/nrow(data))*100 # % observado




# Evaluamos si en 1000 muestras de 100, que tal sale la representación
# de esta minoría
sample <- 100
set.seed(1234)
(replicate(1000, {
  temp.dat <- sample_n(data, sample)
  sum(temp.dat$minoria)/nrow(temp.dat)
}) %>%
  as.data.frame() %>%
  ggplot() +
  geom_density(aes(x=., y = ..scaled..)) +
  geom_vline(xintercept = .1)  +
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  labs(x="",y="", subtitle = "Muestreo aleatorio simple"))/
# Contrastamos con muestras estratificadas ponderadas por tamaño
(replicate(1000, {
  temp.dat_1 <- sample_n(data2[data2$strata=="strat1",], sample * .5)
  temp.dat_2 <- sample_n(data2[data2$strata=="strat2",], sample * .4)
  temp.dat_3 <- sample_n(data2[data2$strata=="strat3",], sample * .1)
  temp.dat <- bind_rows(temp.dat_1, temp.dat_2, temp.dat_3)
  sum(temp.dat$minoria)/nrow(temp.dat)
}) %>%
  as.data.frame() %>%
  ggplot() +
  geom_density(aes(x=., y = ..scaled..)) +
  geom_vline(xintercept = .1)  +
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  labs(x="",y="", subtitle = "Muestreo aleatorio estratificado")) +
  plot_annotation(caption = paste0("N= ", n, ", n= ", sample, ", rep= 1000"),
                  title = "Representación de minoría (minoría aleatoria)")



# minoría segregada o concentrada en x estrato, pensemos en una zona geográfica
set.seed(1234)
data2 <- data.frame(person = 1:n, 
                   strata = c(rep("strat1", n*.5),
                              rep("strat2", n*.4),
                              rep("strat3", n*.1)),
                   minoria = c(rbinom(n*.5, 1, .001),
                               rbinom(n*.4, 1, .1),
                               rbinom(n*.1, 1, .9)))

# vemos como queda
data2 %>%
  group_by(strata) %>%
  summarize(p= sum(minoria)/n())

# Simulamos nuevamente, pero en este escenario
(replicate(1000, {
  temp.dat <- sample_n(data2, sample)
  sum(temp.dat$minoria)/nrow(temp.dat)
}) %>%
    as.data.frame() %>%
    ggplot() +
    geom_density(aes(x=., y = ..scaled..)) +
    geom_vline(xintercept = .1)  +
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1),
                       labels = scales::percent) +
    labs(x="",y="", subtitle = "Muestreo aleatorio simple"))/
  # Contrastamos con muestras estratificadas ponderadas por tamaño
  (replicate(1000, {
    temp.dat_1 <- sample_n(data2[data2$strata=="strat1",], sample * .5)
    temp.dat_2 <- sample_n(data2[data2$strata=="strat2",], sample * .4)
    temp.dat_3 <- sample_n(data2[data2$strata=="strat3",], sample * .1)
    temp.dat <- bind_rows(temp.dat_1, temp.dat_2, temp.dat_3)
    sum(temp.dat$minoria)/nrow(temp.dat)
  }) %>%
    as.data.frame() %>%
    ggplot() +
    geom_density(aes(x=., y = ..scaled..)) +
    geom_vline(xintercept = .1)  +
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1),
                       labels = scales::percent) +
    labs(x="",y="", subtitle = "Muestreo aleatorio estratificado")) +
  plot_annotation(caption = paste0("N= ", n, ", n= ", sample, ", rep= 1000"),
                  title = "Representación de minoría (minoría concentrada)")





















