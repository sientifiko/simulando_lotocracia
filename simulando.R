
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

# ploteamos
data %>%
  group_by(strata) %>%
  summarize(p= n()/nrow(data)) %>%
  ggplot(aes(strata, p, fill = strata)) +
  geom_col() +
  guides(fill="none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="")
            

sum(data$minoria/nrow(data))*100 # % observado

# resumen por grupo
data %>%
  group_by(strata) %>%
  summarize(p= sum(minoria)/n()) %>%
  ggplot(aes(strata, p, fill = strata)) +
  geom_col() +
  guides(fill="none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="")


# Evaluamos si en 1000 muestras de 100, que tal sale la representación
# de esta minoría
sample <- 100
set.seed(1234)
replicate(1000, {
  temp.dat <- sample_n(data, sample)
  sum(temp.dat$minoria)/nrow(temp.dat)
}) -> sim1

set.seed(1234)
replicate(1000, {
  temp.dat_1 <- sample_n(data[data$strata=="strat1",], sample * .5)
  temp.dat_2 <- sample_n(data[data$strata=="strat2",], sample * .4)
  temp.dat_3 <- sample_n(data[data$strata=="strat3",], sample * .1)
  temp.dat <- bind_rows(temp.dat_1, temp.dat_2, temp.dat_3)
  sum(temp.dat$minoria)/nrow(temp.dat)
}) -> sim2

mean(sim1)
mean(sim2)

max(sim1)
max(sim2)

# graficamos caso minoría aleatoria
ggplot() +
  geom_density(aes(sim2, color= "estratificado", y=..scaled..)) +
  geom_density(aes(sim1, color= "simple", y=..scaled..)) +
  geom_vline(xintercept = .1)  +
  scale_x_continuous(limits = c(0,.5),
                     breaks = seq(0, .5, .1))+
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(x="",y="", 
       caption = paste0("N= ", n, ", n= ", sample, ", rep= 1000"),
       title = "Representación de minoría (minoría aleatoria)")



# minoría segregada o concentrada en x estrato, pensemos en una zona geográfica
set.seed(1234)
data$minoria2 <-  c(rbinom(n*.5, 1, ((n*.1)*.05)/(n*.5)),
                    rbinom(n*.4, 1, ((n*.1)*.15)/(n*.4)),
                    rbinom(n*.1, 1, ((n*.1)*.8)/(n*.1)))


sum(data$minoria2)/n # % observado

# vemos como queda
data %>%
  group_by(strata) %>%
  summarize(p= sum(minoria2)/n()) %>%
  ggplot(aes(strata, p, fill = strata)) +
  geom_col() +
  guides(fill="none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="")

# Simulamos nuevamente, pero en este escenario
set.seed(1234)
replicate(1000, {
  temp.dat <- sample_n(data, sample)
  sum(temp.dat$minoria2)/nrow(temp.dat)
}) -> sim3

set.seed(1234)
replicate(1000, {
  temp.dat_1 <- sample_n(data[data$strata=="strat1",], sample * .5)
  temp.dat_2 <- sample_n(data[data$strata=="strat2",], sample * .4)
  temp.dat_3 <- sample_n(data[data$strata=="strat3",], sample * .1)
  temp.dat <- bind_rows(temp.dat_1, temp.dat_2, temp.dat_3)
  sum(temp.dat$minoria2)/nrow(temp.dat)
}) -> sim4

ggplot() +
  geom_density(aes(sim4, color= "estratificado", y=..scaled..)) +
  geom_density(aes(sim3, color= "simple", y=..scaled..)) +
  geom_vline(xintercept = .1)  +
  scale_x_continuous(limits = c(0,.5),
                     breaks = seq(0, .5, .1))+
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(x="",y="", 
       caption = paste0("N= ", n, ", n= ", sample, ", rep= 1000"),
       title = "Representación de minoría (minoría concentrada)")





















