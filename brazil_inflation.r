# PACKAGES
{
  library(sidrar)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(ggthemes)
}

# sidrar::info_sidra(1737)

d = get_sidra(x=1737,variable = 63, period = "198001-202401")

d$date = ym(d$`Mês (Código)`)
d$year = year(d$date)

d %>%
  # filter(year == 1994) %>% 
  ggplot(aes(x = date, y = Valor))+
  geom_line(linewidth = 0.75)+
  geom_vline(xintercept = ymd("1994-07-01"), linewidth = 0.5, colour = "blue", linetype="dashed")+
  geom_curve(
    aes(
      x=ymd("2011-01-01"),
      xend = ymd("1994-11-01"),
      y = 55,
      yend=55),
    curvature = 0,
    arrow = arrow(angle = 30, length = unit(3,"mm")),
    linewidth=0.75,
    colour = "darkblue")+
  geom_text(
    aes(
      x=ymd("2004-01-01"),
      y = 55
    ),
    label="Lançamento do Plano Real\nem 1 de julho de 1994",
    vjust=0.5
  )+
  labs(
    title = "Inflação Brasileira",
    subtitle = "IPCA, variação mensal: jan/1980 - jan/2024",
    x = "",
    y = "%",
    caption = "Fonte: IBGE | Tabela 1737, variável 63"
  )+
  theme_economist()

ggsave(
  filename = "brazil_inflation.jpeg",
  device = "jpeg",
  path = "0 Graficos/",
  scale = 1,
  width = unit(10, "cm"),
  height = unit(5, "cm"),
  dpi = 300
)
