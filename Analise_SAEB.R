#tópico 2 - análise 1 
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(gapminder)
library(forcats)


banco<-read.csv("amostra_211061037.csv")

cor <- banco %>%
group_by(RACA_COR) %>%
summarise(Total = n()) %>%
filter(!is.na(RACA_COR))



raca <- cor %>%
mutate(RACA_COR = case_when(
RACA_COR %>% str_detect("Amarela") ~ "Amarela",
RACA_COR %>% str_detect("Branca") ~ "Branca",
RACA_COR %>% str_detect("IndÃ­gena") ~ "	Indígena",
RACA_COR %>% str_detect("NÃ£o quero declarar") ~ "Não quero declarar",
RACA_COR %>% str_detect("Parda") ~ "Parda",
RACA_COR %>% str_detect("Preta") ~ "Preta",
))


ggplot(raca) +
aes(x = fct_reorder(RACA_COR, Total, .desc=TRUE), y = Total) +
geom_bar(stat = "identity", fill = "darkblue") +
xlab("Número de alunos") +
xlab("Raça/cor") +
  scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900))+
  theme_classic() 



ggsave("analise1.pdf", 
       width = 200,
       height = 170,
       units = "mm")

raca <- raca %>%
  mutate(prop = round(100*(Total/sum(Total)), 2)) #porcentagen

#tabela Overleaf 
require("xtable")
data("raca")
xtable((raca))

-------------------------------------------------------------------------------

#tópico 2 - análise 2 

sexo <- banco %>%
  group_by(SEXO) %>%
  summarise(Total = n()) %>%
  filter(!is.na(SEXO)) %>%
  mutate(prop = round(100*(Total/sum(Total)), 2)) #porcentagen

library(scales)
ggplot(sexo, aes(x="", y=prop, fill=SEXO, label=prop))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme_void() +
  geom_label (position = position_stack(vjust = 0.5))


ggsave("grafico2.pdf", 
       width = 160,
       height = 100,
       units = "mm")









