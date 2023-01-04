# Métodos Estatísticos 2 - 2022.2
# Atividade 2.2 - Intervalos de Confiança 
# Letícia Lino - 211061037



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

#Parte 1
#-----------Coletando as amostras------------

#"extraia 50 amostras aleatórias de tamanho 20"
amostras_20 <- banco %>% 
  rep_sample_n(20, reps = 50, replace = TRUE) 

#"50 amostras aleatórias de tamanho 200"
amostras_200 <- banco %>% 
  rep_sample_n(200, reps = 50, replace = TRUE) 


#Parte 2 
#----------- Intervalos de confiança -----------
# Intervalos agrupados por parâmetro e tamanho da amostra 



#----------------- IC para Proporção de nascidos em 2001 ou antes ------------------------

#transformando chr em factor
amostras_20$ANO_NASC <- as.factor(amostras_20$ANO_NASC)
amostras_20$replicate<- as.factor(amostras_20$replicate)
str(amostras_20) # verificando

#intervalo de confiança para n=20 
#proporção pop - "suponha que o valor obtido na sua amostra (2000) para essas medidas sejam "parâmetros"

banco_2001<- banco %>% #filtrando banco original 
  filter(ANO_NASC == "1998 ou antes" | ANO_NASC == "1999" | ANO_NASC == "2000"
         | ANO_NASC == "2001")
prop_2001 <- nrow(banco_2001)/2000 #prop populacional de nascidos antes ou em 2001


#filtrando amostra
ic_2001<- amostras_20 %>%
  filter(ANO_NASC == "1998 ou antes" | ANO_NASC == "1999" | ANO_NASC == "2000"
         | ANO_NASC == "2001")


ic_2001_antes<- ic_2001 %>%
  group_by(replicate) %>%
  summarise(replicate)%>%
  count()%>%
  summarise(Amostra = replicate, n = n, prop = n/20) %>%
  mutate(ls = prop + qnorm(.975)*sqrt(prop*(1-prop)/20),
         li = prop - qnorm(.975)*sqrt(prop*(1-prop)/20),
         proporcao = prop_2001,
         pertence = ifelse(proporcao>ls | proporcao < li, 2, 1))%>%
  mutate(pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))

#para n=200
ic_2001_2<- amostras_200 %>%
  filter(ANO_NASC == "1998 ou antes" | ANO_NASC == "1999" | ANO_NASC == "2000"
         | ANO_NASC == "2001")


ic_2001_antes_2<- ic_2001_2 %>%
  group_by(replicate) %>%
  summarise(replicate)%>%
  count()%>%
  summarise(Amostra = replicate, n = n, prop = n/200) %>%
  mutate(ls = prop + qnorm(.975)*sqrt(prop*(1-prop)/200),
         li = prop - qnorm(.975)*sqrt(prop*(1-prop)/200),
         proporcao = prop_2001,
         pertence = ifelse(proporcao>ls | proporcao < li, 2, 1))%>%
  mutate(pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))


#----------------- IC para Proporção de alunas ------------------------

#filtrando banco original 
banco_fem <- banco %>%
  filter(SEXO == "Feminino")
prop_fem <- nrow(banco_fem)/2000 #prop populacional alunas

#transformando chr em factor
amostras_20$SEXO <- as.factor(amostras_20$SEXO)
amostras_20$replicate<- as.factor(amostras_20$replicate)

#intervalo de confiança para n=20 
ic_feminino <- amostras_20 %>%
  group_by(replicate, SEXO)%>%
  summarise(n = n()) %>%
  ungroup()%>%
  group_by(replicate)%>%
  summarise(Sexo = SEXO, n = n, prop = n/sum(n))%>%
  filter(Sexo == "Feminino")%>% #filtrando apenas para alunas
  ungroup()%>%
  mutate(ls = prop + qnorm(.975)*sqrt(prop*(1-prop)/20),
         li = prop - qnorm(.975)*sqrt(prop*(1-prop)/20),
         proporcao = prop_fem,
         pertence = ifelse(proporcao>ls | proporcao < li, 2, 1),
         pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))

#intervalo de confiança para n=200
amostras_20$SEXO <- as.factor(amostras_20$SEXO)
amostras_20$replicate<- as.factor(amostras_20$replicate)
  
ic_feminino_2 <- amostras_200 %>%
  group_by(replicate, SEXO)%>%
  summarise(n = n())%>%
  ungroup()%>%
  group_by(replicate)%>%
    summarise(Sexo = SEXO, n = n, prop = n/sum(n))%>%
  filter(Sexo == "Feminino")%>%
  ungroup()%>%
  mutate(ls = prop + qnorm(.975)*sqrt(prop*(1-prop)/200),
          li = prop - qnorm(.975)*sqrt(prop*(1-prop)/200),
          ls = ifelse(ls>1, 1, ls),
         proporcao = prop_fem,
          pertence = ifelse(proporcao>ls | proporcao < li, 2, 1),
          pertence = factor(pertence, levels = c(1, 2),
                             labels = c("Sim", "Não")))

#-------------IC para média das notas de língua portuguesa------------

str(amostras_20) #verificando se notas são númericas, sem necessidade de usar factor 

#para n= 20 
ic_media<- amostras_20 %>%
  filter(is.na(NOTA_LP) == FALSE)%>%
  group_by(replicate)%>%
  summarise(media = mean(NOTA_LP),
            ls = media + qnorm(.975)*sd(NOTA_LP)/sqrt(20),
            li = media - qnorm(.975)*sd(NOTA_LP)/sqrt(20)
  )%>%
  mutate(media_pop = mean(banco$NOTA_LP), #media populacional
         pertence = ifelse(media_pop>ls | media_pop < li, 2, 1),
         pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))



  
#para n= 200
  
ic_media_2<- amostras_200 %>%
  filter(is.na(NOTA_LP) == FALSE)%>%
  group_by(replicate)%>%
  summarise(media = mean(NOTA_LP),
            ls = media + qnorm(.975)*sd(NOTA_LP)/sqrt(200),
            li = media - qnorm(.975)*sd(NOTA_LP)/sqrt(200)
  )%>%
  mutate(media_pop = mean(banco$NOTA_LP),
          pertence = ifelse(media_pop>ls | media_pop < li, 2, 1),
          pertence = factor(pertence, levels = c(1, 2),
                             labels = c("Sim", "Não")))
  
  
#-------------IC para média das notas de matemática-----------

#para n= 20 
ic_media_MT<- amostras_20 %>%
  filter(is.na(NOTA_MT) == FALSE)%>%
  group_by(replicate)%>%
  summarise(media = mean(NOTA_LP),
            ls = media + qnorm(.975)*sd(NOTA_MT)/sqrt(20),
            li = media - qnorm(.975)*sd(NOTA_MT)/sqrt(20)
  )%>%
  mutate(media_pop = mean(banco$NOTA_MT),
         pertence = ifelse(media_pop>ls | media_pop < li, 2, 1),
         pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))




#para n= 200

ic_media_MT_2<- amostras_200 %>%
  filter(is.na(NOTA_MT) == FALSE)%>%
  group_by(replicate)%>%
  summarise(media = mean(NOTA_MT),
            ls = media + qnorm(.975)*sd(NOTA_MT)/sqrt(200),
            li = media - qnorm(.975)*sd(NOTA_MT)/sqrt(200)
  )%>%
  mutate(media_pop = mean(banco$NOTA_MT),
         pertence = ifelse(media_pop>ls | media_pop < li, 2, 1),
         pertence = factor(pertence, levels = c(1, 2),
                           labels = c("Sim", "Não")))


#Parte 3 
#-----------Gráficos-----------



#-----Gráfico para IC Proporção nascidos em 2001 ou antes - n=20-----
ic_2001_antes %>%
  ggplot(aes(x = prop, y = Amostra, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intervalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = proporcao), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_2001_20.pdf", width = 158, height = 93, units = "mm")


#-----Gráfico para IC Proporção nascidos em 2001 ou antes - n=200-----
ic_2001_antes_2 %>%
  ggplot(aes(x = prop, y = Amostra, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intervalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = proporcao), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_2001_200.pdf", width = 158, height = 93, units = "mm")



#-----Gráfico para IC Proporção de alunas - n=20-----
ic_feminino%>%
  ggplot(aes(x = prop, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intervalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = proporcao), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_feminino_20.pdf", width = 158, height = 93, units = "mm")

#-----Gráfico para IC Proporção de alunas - n=200-----
ic_feminino_2%>%
  ggplot(aes(x = prop, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intervalo de Confiança",
       color = "O Intervalo contém a proporção populacional?")+
  geom_vline(aes(xintercept = proporcao), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_feminino_200.pdf", width = 158, height = 93, units = "mm")


#-----Gráfico para IC Média Nota_LP, n=20-----
ic_media %>%
  ggplot(aes(x = media, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intevalo de Confiança",
       color = "O IC contém a média populacional?")+
  geom_vline(aes(xintercept = media_pop), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_LP_20.pdf", width = 158, height = 93, units = "mm")

#-----Gráfico para IC Média Nota_LP, n=200-----
ic_media_2%>%
  ggplot(aes(x = media, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intevalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = media_pop), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_LP_200.pdf", width = 158, height = 93, units = "mm")

#Gráfico para IC Média Nota_MT, n=20
ic_media_MT%>%
  ggplot(aes(x = media, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intevalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = media_pop), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_MT_20.pdf", width = 158, height = 93, units = "mm")

#Gráfico para IC Média Nota_MT, n=200
ic_media_MT_2%>%
  ggplot(aes(x = media, y = replicate, xmin = li, xmax = ls))+
  geom_errorbar(aes(color = pertence), size = .8)+
  labs(y = "Amostras", x = "Intevalo de Confiança",
       color = "O IC contém a proporção populacional?")+
  geom_vline(aes(xintercept = media_pop), color = "#575757")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_color_manual(values = c("#00A9FF", "#FF0000"))
ggsave("ic_MT_200.pdf", width = 158, height = 93, units = "mm")

