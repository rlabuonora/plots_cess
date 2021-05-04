library(readxl)
library(tidyverse)
library(ggrepel)

source(here::here('theme_cess.R'))

theme_set(estilo_cess())

library(extrafont)
extrafont::font_import()

g10 <- readxl::read_excel("gasto_en_pensiones_sobre_relacion_dependencia_corregido.xlsx",
                         sheet = "Hoja2",
                         range="C3:E44", 
                         col_names = c("País", "Relación", "Gasto")) %>% 
  mutate(uruguay = `País` == "URU")

g10 %>% 
  ggplot(aes(`Relación`, `Gasto`)) +
  geom_point(aes(color = uruguay), size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "grey45", formula = y ~ x) +
  scale_color_manual(values = c(verde_cess, amarillo_cess)) +
  geom_text_repel(aes(`Relación`, `Gasto`, label = `País`)) +
  scale_x_continuous(limits = c(7, 37),
                     breaks = seq(7, 37, by = 5),
                     expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(limits = c(0, 16),
                     breaks = seq(0, 18, by = 2),
                     expand = expansion(mult = c(0, .01))) + 
  guides(color = F) +
  labs(x = "Relación de dependencia 65+/15-64 (2020)", 
       y = "Gasto público en pensiones como % PIB (2017*)", 
       color = "",
       title = "",
       subtitle = "",
       caption = "* Para Uruguay e Israel datos de 2019 Para Canadá, Hungría y Francia datos de 2018.\nFuentes: OECD Social Expenditure Database.\nA. Arenas de Mesa, “Los sistemas de pensiones en América Latina: institucionalidad, gasto público y sostenibilidad financiera en tiempos del COVID-19”, serie\nMacroeconomía del Desarrollo, N° 212 (LC/TS.2020/99), Santiago, Comisión Económica para América Latina y el Caribe (CEPAL), 2020. United Nations,\nDepartment of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.") + 
  theme_minimal(base_family = "Gotham-Book",
                base_size = 16) +
  theme(axis.line = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), 
        panel.background = ggplot2::element_blank(), 
        plot.title = element_text(hjust = .5, 
                                  family = "Gotham-Bold"),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(family = "Gotham-Light", hjust = 1, 
                                    margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "bottom")

+
  theme(plot.caption = element_text(size=12, color="gray20")) +        
  ggsave("graf10.png", 
         dpi = 300, width = 15, height = 8.5)
