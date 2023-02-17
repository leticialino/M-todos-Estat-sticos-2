#O código foi  baseado na publicação "

# ---------- Sessão 1 - Atividade 2.2  --------------
#Pacotes
library(tidyverse)
library(knitr)
library(kableExtra)
library(readr)
library(dplyr)
library(statsr) #para rep_sample
library(dplyr)


# Lendo o banco de dados 
banco <- read_csv("amostra_211061037.csv")

#"extraia 50 amostras aleatórias de tamanho 20"
amostras_20 <- banco %>% 
  filter(is.na(NOTA_MT) == FALSE)%>%
  rep_sample_n(20, reps = 50, replace = TRUE) %>%
  rename(amostra=replicate)
amostras_20$amostra<- as.character(amostras_20$amostra)
str(amostras_20) 
#"50 amostras aleatórias de tamanho 200"
amostras_200 <- banco %>% 
  filter(is.na(NOTA_MT) == FALSE)%>%
  rep_sample_n(200, reps = 50, replace = TRUE) %>%
  rename(amostra=replicate)
amostras_200$amostra<- as.character(amostras_200$amostra)
str(amostras_200) 

# ---------- Sessão 2 - Atividade 3.2  --------------

#---------Questão 1 - histograma-------

amostra<-amostras_200%>%
  filter (amostra=="16")

grafico <-amostra %>%
  ggplot(aes(NOTA_MT)) +
  geom_histogram(aes(y = ..density..), bins = 26, colour = "#011e5a", fill = "white") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(amostra$NOTA_MT),
                  sd = sd(amostra$NOTA_MT)),
                colour = "#CC0000") +
  geom_vline(aes(xintercept=mean),colour = "#99CC32")+
  xlab('Notas') +
  ylab('Densidade de Frequência') +
  scale_x_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic()
ggsave("histograma_3.2.pdf", width = 140, height = 70, units = "mm")
grafico
#-----Questão 1 - quantil-quantil------------------------------
ggplot(amostra, aes(sample = NOTA_MT)) +
  xlab('Quantis') +
  ylab('Valores da Amostra, n=200') +
  scale_y_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400))+
  stat_qq() +
  stat_qq_line()+
  theme_classic()
ggsave("qq.pdf", width = 140, height = 70, units = "mm")

#----Questão 2 - teste de aderencia ------
#----port---
amostra2<-amostras_20%>%
  filter (amostra=="16")

library(goftest)
library(nortest) #ad.test e lillie.test

#----testes para mat----
lillie.test(amostra2$NOTA_MT)
ad.test(amostra2$NOTA_MT)
shapiro.test(amostra2$NOTA_MT)

#----testes para port----
lillie.test(amostra2$NOTA_LP)
ad.test(amostra2$NOTA_LP)
shapiro.test(amostra2$NOTA_LP)



grafico4 <-banco %>%
  ggplot(aes(NOTA_LP)) +
  geom_histogram(aes(y = ..density..), bins = 26, colour = "#011e5a", fill = "white") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(banco$NOTA_LP),
                  sd = sd(banco$NOTA_LP)),
                colour = "#CC0000") +
  geom_vline(aes(xintercept=mean),colour = "#99CC32")+
  xlab('Notas') +
  ylab('Densidade de Frequência')+
  scale_x_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic()
ggsave("histograma4.pdf", width = 140, height = 70, units = "mm")
grafico4


grafico5 <-banco %>%
  ggplot(aes(NOTA_MT)) +
  geom_histogram(aes(y = ..density..), bins = 26, colour = "#011e5a", fill = "white") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(banco$NOTA_MT),
                  sd = sd(banco$NOTA_MT)),
                colour = "#CC0000") +
  geom_vline(aes(xintercept=mean),colour = "#99CC32")+
  xlab('Notas') +
  ylab('Densidade de Frequência')+
  scale_x_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic()
ggsave("histograma5.pdf", width = 140, height = 70, units = "mm")
grafico5


grafico2 <-amostra2 %>%
  ggplot(aes(NOTA_MT)) +
  geom_histogram(aes(y = ..density..), bins = 26, colour = "#011e5a", fill = "white") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(amostra2$NOTA_MT),
                  sd = sd(amostra2$NOTA_MT)),
                colour = "#CC0000") +
  geom_vline(aes(xintercept=mean),colour = "#99CC32")+
  xlab('Notas') +
  ylab('Densidade de Frequência') +
  scale_x_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic()
ggsave("histograma2.pdf", width = 158, height = 93, units = "mm")
grafico2

grafico3 <-amostra2 %>%
  ggplot(aes(NOTA_LP)) +
  geom_histogram(aes(y = ..density..), bins = 26, colour = "#011e5a", fill = "white") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(amostra2$NOTA_LP),
                  sd = sd(amostra2$NOTA_LP)),
                colour = "#CC0000") +
  geom_vline(aes(xintercept=mean),colour = "#99CC32")+
  xlab('Notas') +
  ylab('Densidade de Frequência') +
  scale_x_continuous(breaks = c(0, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic()
ggsave("histograma3.pdf", width = 158, height = 93, units = "mm")
grafico3