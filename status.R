library(dplyr)
library(ggplot2)

# Set source file directory

df <- data.frame(read.csv("https://docs.google.com/spreadsheets/d/1VAcIz4XDN7D5NKbiIfNSpuEZ6Fmh7SWZaDQ3BCrLUAM/pub?gid=0&single=true&output=csv", stringsAsFactors = F))

fillbarchart <- function(df, xcol, fillcol, xlab, filllab) {
  p <- ggplot(data = df) +
    geom_bar(mapping = aes(x = df[,xcol], fill = as.character(df[,fillcol])),
             position = 'fill') +
    labs(fill = filllab, x = xlab)
  return(p)
}

df %>% filter(MDRFINAL == 'wen') %>% View()

df %>% filter(MDRTN == '1') %>% View()
df %>% filter(MDRTN == '2') %>% group_by(CTNTN) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

df %>% filter(CTNFINAL == 'aak') %>% group_by(CTNTN) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

#consonant compare
df %>% filter(MDRCSNT == 'c') %>% group_by(CTNCSNT) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()
fillbarchart(df, "MDRCSNT", "CTNCSNT", "MDR consonant", "CTN consonant")

#tone compare
fillbarchart(df, "MDRCSNT", "CTNTN", "MDR consonant", "CTN tone")
fillbarchart(df, "MDRTN", "CTNTN", "MDR tone", "CTN tone")
fillbarchart(df, "CTNCSNT", "MDRTN","CTN consonant","MDR tone")

fillbarchart(df, "CTNCSNT", "CTNTN","CTN consonant","CTN tone")
fillbarchart(df, "MDRCSNT", "MDRTN", "MDR consonant", "MDR tone")

#vowel compare
df %>% filter(grepl('oe',CTNFINAL)) %>% group_by(MDRFINAL) %>% summarise(n = n()) %>% mutate(percentage = paste0(round(n / sum(n) * 100, 1),'%')) %>% View()

df %>% filter(CTNSTP != '/') %>% View()
df %>% filter(CTNSTP == 't') %>% View()

df %>% filter(CTNNSL == 'n') %>% View()




