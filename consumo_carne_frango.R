setwd("~/Trabalho/R-Projetos/graficos") # Muda o dir. no meu pc. Nâo utilize.

library(sidrar)
library(tidyverse)
library(gghighlight)

# Aquisição Alimentar ---------------------------------------------------------

infos <- info_sidra(3047)                   # armazenando informações da tabela


fg.cod <- infos[[4]][[2]][                                 # Codigos de frangos
  grep('frango', infos[[4]][[2]][,2], T), 1
  ]


cn.cod <- infos[[4]][[2]][                                  # Codigos de carnes
  grep('carnes bovinas', infos[[4]][[2]][,2], T), 1
]


bd <- get_sidra(                                                # Base de dados
  x = 3047,
  period = 'all',
  geo = 'Brazil',
  classific = 'c217',
  category = list('c217' = c(fg.cod, cn.cod))
)


bd |> mutate(`Grupos, subgrupos e produtos` =   # Grafico 1: consumo per capita
               gsub('.*\\d ', '', `Grupos, subgrupos e produtos`)) |>
  ggplot(aes(x = Ano,
             y = Valor,
             group = `Grupos, subgrupos e produtos`))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  gghighlight(label_params = list(size = 2.5))

# População de cada ano -------------------------------------------------------

pop <- get_sidra(                                # Tabela da População Estimada
  x = 6579,
  period = c('2002', '2008', '2018')
)


bd$pop <- pop$Valor[match(bd$`Ano (Código)`, pop$`Ano (Código)`)] # Vetor popul.


bd |> mutate(`Grupos, subgrupos e produtos` = # Grafico 2: consumo total estim.
               gsub('.*\\d ', '', `Grupos, subgrupos e produtos`),
             Valor = (bd$Valor*bd$pop)/10^9) |>
  ggplot(aes(x = Ano,
             y = Valor,
             group = `Grupos, subgrupos e produtos`))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  gghighlight(label_params = list(size = 2.5,
                                  fill = 'transparent'))


pop |> ggplot(        # Grafico 3: População total estimada em 2002, 2008, 2018
  aes(x = Ano, y = Valor/10^6, group = `Variável (Código)`)
)+
  geom_line(size = 1)+
  geom_point(size = 5)
