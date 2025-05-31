#Diret?rio
setwd("C:/Users/lucas/Downloads")
#setwd("C:/Users/usuario/Downloads")
#setwd("C:/Users/laca/Downloads")

#Pacotes
library(readxl)
library(quantreg)
library(AER)
library(plm)
library(lqmm)
library(car)
library(ggplot2)
library(broom)
library(psych)
library(knitr)
library(stargazer)
library(sf)
library(rnaturalearth)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#Banco de dados
dados <- read_excel("2004-2023.xlsx")
View(dados)
str(dados)
as.factor(dados$ano)
as.factor(dados$regiao)
dados$ano <- as.factor(dados$ano)
dados$regiao <- as.factor(dados$regiao)
dados$estado < as.factor(dados$estado)

#Descritiva variaveis numericas
descmort <- describeBy(dados$IDSS13.1, dados$regiao)
descmort
writexl::write_xlsx(descmort, "C:/Users/lucas/Downloads/descmortregcomp.xlsx")
descmortest <- describeBy(dados$IDSS13.1, dados$estado)
descmortest 
writexl::write_xlsx(descmortest, "C:/Users/lucas/Downloads/descmortestcomp.xlsx")
hist(dados$IDSS13.1)

descinsan <- describeBy(dados$insan, dados$regiao)
descinsan
writexl::write_xlsx(descmort, "C:/Users/lucas/Downloads/descinsanregcomp.xlsx")
descinsanest <- describeBy(dados$insan, dados$estado)
descinsanest
writexl::write_xlsx(descinsanest, "C:/Users/lucas/Downloads/descinsanestcomp.xlsx")
hist(dados$insan)

descmort2 <- describeBy(dados$IDSS13.1, dados$ano)
descmort2
writexl::write_xlsx(descmort2, "C:/Users/lucas/Downloads/descmortanocomp.xlsx")

descinsan2 <- describeBy(dados$insan, dados$ano)
descinsan2
writexl::write_xlsx(descinsan2, "C:/Users/lucas/Downloads/descinsananocomp.xlsx")

desccomp1 <- describeBy(dados$COMP1, dados$estado)
desccomp1
writexl::write_xlsx(desccomp1, "C:/Users/lucas/Downloads/desccomp1.xlsx")

desccomp2 <- describeBy(dados$COMP2, dados$estado)
desccomp2
writexl::write_xlsx(desccomp2, "C:/Users/lucas/Downloads/desccomp2.xlsx")

desccomp2_INFL <- describeBy(dados$COMP2_INFL, dados$estado)
desccomp2_INFL
writexl::write_xlsx(desccomp2_INFL, "C:/Users/lucas/Downloads/desccomp2INFL.xlsx")

#Criando vari?vel identificadora
dados$id <- paste(dados$sigla_uf, dados$ano, sep = "_")

#Quantis da vari?vel dependente (mortalidade infantil) e independentes (Ano e InSan)
quantile(dados$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(dados$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#Dados por regiao
amostra_sul <- subset(dados, regiao == "SUL")
amostra_sud <- subset(dados, regiao == "SUDESTE")
amostra_co <- subset(dados, regiao == "CENTROOESTE")
amostra_nor <- subset(dados, regiao == "NORTE")
amostra_ne <- subset(dados, regiao == "NORDESTE")

#Quantis por regiao
quantile(amostra_sul$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(amostra_sul$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quantile(amostra_sud$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(amostra_sud$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quantile(amostra_co$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(amostra_co$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quantile(amostra_nor$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(amostra_nor$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quantile(amostra_ne$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(amostra_ne$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

quantis_df <- data.frame(
  amostra = c("sul", "sud", "co", "nor", "ne"),
  IDSS13.1_quantis = c(
    quantile(amostra_sul$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_sud$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_co$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_nor$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_ne$IDSS13.1, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  ),
  insan_quantis = c(
    quantile(amostra_sul$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_sud$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_co$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_nor$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
    quantile(amostra_ne$insan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  )
)

print(quantis_df)
writexl::write_xlsx(quantis_df, "C:/Users/lucas/Downloads/quantisregiaocomp.xlsx")

# Padronizando as variavsigla_uf# Padronizando as variaveis numericas
#dados$insanz <- scale(dados$insan)
#dados$insanGz <- scale(dados$insanG)
#dados$IDSS11z <- scale(dados$IDSS11)
#dados$IDSS13.1z <- scale(dados$IDSS13.1)
#dados$IDSS14z <- scale(dados$IDSS14)
#dados$IDSS16z <- scale(dados$IDSS16)
#dados$IDSA5.1.2z <- scale(dados$IDSA5.1.2)
#dados$IDSA5.2.2z <- scale(dados$IDSA5.2.2)
#dados$IDSA6.1.2z <- scale(dados$IDSA6.1.2)
#dados$IDSA6.2.2z <- scale(dados$IDSA6.2.2)

#Teste Mann Kendall
library(Kendall)
MannKendall(dados$insan)
MannKendall(dados$IDSS13.1)

#Avaliando a multicolinearidade (VIF)
modelovif <- lm(IDSS13.1 ~  insan + regiao + COMP1 + COMP2_INFL, data = dados)
summary(modelovif)

vif(modelovif)
durbinWatsonTest(modelovif)
shapiro.test(residuals(modelovif))

#Categoria de referencia das variaveis categoricas
levels(dados$regiao)
dados$regiao <- relevel(dados$regiao, ref = "SUL")

levels(dados$ano)
dados$ano <- relevel(dados$ano, ref = "2022")

#Modelo de regressao quantilica longitudinal
modelotot <- lqmm(IDSS13.1 ~  insan + regiao + COMP1 + COMP2_INFL, random = ~1, group = id, data = dados, tau = c(0.1, 0.25, 0.50, 0.75, 0.9))
summary(modelotot)
coef(modelotot)
exp(coef(modelotot))

logLik(modelotot)

##Histograma e QQ-plot
hist(residuals(modelotot))
qqnorm(residuals(modelotot))
qqline(residuals(modelotot))

##Grafico de dispersao dos valores ajustados versus os residuos
plot(predict(modelotot), resid(modelotot))
abline(h = 0, col = "red")

##Obtendo mapas da distribuição espacial das variáveis

#Criando banco com as medianas das variaveis (insan e IDSS13.1)
media_estados <- dados %>%
  group_by(estado) %>%
  summarize(medinsan = median(insan), medmort = median(IDSS13.1), region = regiao)
View(media_estados)
writexl::write_xlsx(media_estados, "C:/Users/lucas/Downloads/medianas_estados.xlsx")

#Calculando os quantis da tx. mort. infant.
quantis <- quantile(media_estados$medmort, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#Calculando os quantis da inseguranca alimentar
quantis2 <- quantile(media_estados$medinsan, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#Carregando os dados espaciais do Brasil
shapefile <- st_read("C:/Users/lucas/Downloads/BR_UF_2022/BR_UF_2022.shp")

#Criando uma nova coluna no shapefile para cada quantil de cada variavel
shapefile$quantil1 <- cut(media_estados$medmort,
                         breaks = c(min(media_estados$medmort), quantis, max(media_estados$medmort)),
                         labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"),
                         include.lowest = TRUE)

shapefile$quantil2 <- cut(media_estados$medinsan,
                          breaks = c(min(media_estados$medinsan), quantis2, max(media_estados$medinsan)),
                          labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"),
                          include.lowest = TRUE)

View(shapefile)

#Plot do mapa
grafico1 <- ggplot() +
  geom_sf(data = shapefile, aes(fill = quantil1), color = "black") +
  scale_fill_brewer(palette = "Greens", name = "Quantiles", labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%")) +
  theme_minimal()
grafico1

grafico2 <- ggplot() +
  geom_sf(data = shapefile, aes(fill = quantil2), color = "black") +
  scale_fill_brewer(palette = "Reds", name = "Quantiles", labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%")) +
  theme_minimal()
grafico2

#########Dados Bolsa Família############
dadosPBF <- read_excel("visdata3-download-13-02-2025 083209.xlsx", 
                    sheet = "visdata3-download-13-02-2025 08")
View(dadosPBF)

#Filtrando os anos desejados
anos_desejados <- c(2004, 2009, 2013, 2018, 2023)

dados_filtrados <- dadosPBF %>%
  filter(ano %in% anos_desejados)

#Calcular a média do número de famílias por UF para esses anos
fam_ben_UF <- dados_filtrados %>%
  group_by(ano, Unidade_Territorial) %>%
  summarise(
    Media_Familias = mean(Famílias_PBF, na.rm = TRUE),
    Media_beneficio = mean(valor_ben_med, na.rm = TRUE)
  ) %>%
  arrange(desc(Media_Familias))
fam_ben_UF
writexl::write_xlsx(fam_ben_UF, "C:/Users/lucas/Downloads/fam_ben_UF.xlsx")

#Grafico efeitos insan

quantis <- c(0.1, 0.25, 0.5, 0.75, 0.9)
coef_insan <- c(-0.36, -0.26, -0.14, 0.04, 0.20)
erro_insan <- c(0.077, 0.058, 0.053, 0.080, 0.075)

library(ggplot2)

df <- data.frame(
  quantil = quantis,
  coef = coef_insan,
  erro_inf = coef_insan - 1.96 * erro_insan,
  erro_sup = coef_insan + 1.96 * erro_insan
)

ggplot(df, aes(x = quantil, y = coef)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = erro_inf, ymax = erro_sup), alpha = 0.2) +
  labs(x = "Quantil", y = "Coeficiente estimado") +
  theme_minimal()

#Grafico efeitos Insan e Bolsa Família

library(ggplot2)
library(patchwork)

#Data frame separado por variável
quantis <- c(0.1, 0.25, 0.5, 0.75, 0.9)

#Insegurança Alimentar
df_insan <- data.frame(
  quantil = quantis,
  coef = c(-0.29, -0.25, -0.14, 0.06, 0.20),
  erro = c(0.08, 0.07, 0.06, 0.07, 0.1)
)
df_insan$erro_inf <- df_insan$coef - 1.96 * df_insan$erro
df_insan$erro_sup <- df_insan$coef + 1.96 * df_insan$erro

#Bolsa Família
df_bf <- data.frame(
  quantil = quantis,
  coef = c(-0.03, -0.02, -0.02, -0.02, -0.02),
  erro = c(0.01, 0.006, 0.006, 0.004, 0.02)
)
df_bf$erro_inf <- df_bf$coef - 1.96 * df_bf$erro
df_bf$erro_sup <- df_bf$coef + 1.96 * df_bf$erro

#Gráfico 1 – Insegurança Alimentar
g1 <- ggplot(df_insan, aes(x = quantil, y = coef)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = erro_inf, ymax = erro_sup), alpha = 0.2) +
  labs(
    title = "A",
    x = NULL, y = "Coeficiente"
  ) +
  theme_minimal()

#Gráfico 2 – Bolsa Família
g2 <- ggplot(df_bf, aes(x = quantil, y = coef)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = erro_inf, ymax = erro_sup), alpha = 0.2) +
  labs(
    title = "B",
    x = "Quantil", y = "Coeficiente"
  ) +
  theme_minimal()

#Combinar os dois gráficos (um acima do outro)
g1 / g2


#Gráfico medianas Insegurança alimentar e Taxa de mortalidade
dados_med <- read_excel("medianas_estados.xlsx")

#Heatmap
dados_long <- dados_med %>%
  pivot_longer(cols = c(`Insegurança alimentar`, `Taxa de Mortalidade`),
               names_to = "variavel", values_to = "valor")

ggplot(dados_long, aes(x = variavel, y = reorder(estado, valor), fill = valor)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Variável", y = "Estado", fill = "Legenda") +
  theme_minimal()

#Gráfico de linhas por estado
ggplot(dados_long, aes(x = estado, y = valor, color = variavel, group = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Medianas por Estado: Insegurança Alimentar e Mortalidade",
       x = "Estado", y = "Valor", color = "Variável") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Mapas de distribuição espacial (Figura suplementar)
library(viridis)
library(cowplot)
library(geobr)
library(ggspatial) 

estados_sf <- read_state(year = 2020)

dados_filtrados <- dados %>%
  filter(ano %in% c(2004, 2023)) %>%
  rename(abbrev_state = sigla_uf)

map_data <- estados_sf %>%
  left_join(dados_filtrados, by = "abbrev_state")

nomes_legenda <- list(
  "insan" = "Insegurança alimentar (%)",
  "IDSS13.1" = "Taxa de mortalidade (por 100.000 hab.)",
  "COMP1" = "Nº de leitos (por 1000 hab.)",
  "COMP2" = "Valor médio do Bolsa Família (R$)"
)

plot_map <- function(estados_sf, dados, var, year, nomes_legenda) {
  dados_ano <- dados %>% filter(ano == year)
  
  mapa <- estados_sf %>%
    left_join(dados_ano, by = "abbrev_state")
  
  ggplot(mapa) +
    geom_sf(aes(fill = .data[[var]]), color = "white", size = 0.2) +
    scale_fill_viridis(option = "viridis", direction = -1, na.value = "grey80") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.3) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      style = ggspatial::north_arrow_fancy_orienteering) +
    theme_minimal() +
    labs(title = paste("Ano:", year),
         fill = nomes_legenda[[var]]) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, hjust = 0.5)
    )
}



variaveis <- c("insan", "IDSS13.1", "COMP1", "COMP2")

mapas <- lapply(variaveis, function(v) {
  list(
    plot_map(estados_sf, dados_filtrados, v, 2004, nomes_legenda),
    plot_map(estados_sf, dados_filtrados, v, 2023, nomes_legenda)
  )
})

linhas <- lapply(mapas, function(mapa_par) {
  plot_grid(plotlist = mapa_par, ncol = 2, rel_widths = c(1, 1))
})


figura_final <- plot_grid(plotlist = linhas, ncol = 1, rel_heights = rep(1, length(variaveis)), align = 'v')

grid::grid.newpage()
print(figura_final)

ggsave("figura_mapas.jpg", plot = figura_final, width = 16, height = 14, dpi = 600)