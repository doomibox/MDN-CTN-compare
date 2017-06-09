library(dplyr)
library(ggplot2)

# Set source file directory

df <- data.frame(read.csv("MDRCTNCOMP.csv", stringsAsFactors = F))

df %>% filter(MDRFINAL == 'wen') %>% View()

df %>% filter(MDRTN == '1') %>% View()
df %>% filter(MDRTN == '2') %>% group_by(CTNTN) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

df %>% filter(CTNFINAL == 'aak') %>% group_by(CTNTN) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

#consonant compare
df %>% filter(MDRCSNT == 'c') %>% group_by(CTNCSNT) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

#vowel compare
df %>% filter(grepl('oe',CTNFINAL)) %>% group_by(MDRFINAL) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

df %>% filter(CTNSTP != '/') %>% View()
df %>% filter(CTNSTP == 't') %>% View()

df %>% filter(CTNNSL == 'n') %>% View()

ggplot(data = df) +
  geom_bar(mapping = aes(x = MDRCSNT, fill = as.character(CTNTN)),
           position = 'fill') +
  labs(fill = 'CTN Tone', x = 'MDR Consonant')
