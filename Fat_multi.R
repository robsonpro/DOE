# Exemplo fatorial multi-niveis
# Torneamento aco ABNT 1045

# CB: Chip-breaker
# f: feed [mm/rev]
# vc: cutting velocity [m/min]

# DOI 10.1007/s00170-013-5126-3
###***************************************************************************************************###
## Planejamento

library(DoE.base)

# Planejamento decodificado via pacote DoE.base
design <- fac.design(factor.names=list(CB = c("PF", "PM", "QM", "KR"), 
                                       f  = c(0.16, 0.24, 0.32), 
                                       vc = c(310, 380)),
                     replications = 3, 
                     randomize = FALSE)
design

# Planejamento codificado
design2 <- fac.design(factor.names=list(CB = c(-1, -0.33, 0.33, 1), 
                                        f  = c(-1, 0, 1), 
                                        vc = c(-1, 1)),
                      replications = 3, 
                      randomize = FALSE)

# Forca de corte - Fc
Fc <- c(902.30, 877.31, 845.72, 991.03, 1287.40, 1198.23, 1166.36, 
        1399.52, 1724.08, 1544.10, 1510.12, 1712.83, 870.88, 888.42, 
        857.49, 955.14, 1280.49, 1200.65, 1161.42, 1342.05, 1674.23, 
        1522.24, 1508.66, 1687.77, 912.67, 882.97, 835.37, 974.67, 
        1309.27, 1205.55, 1194.54, 1370.80, 1721.67, 1528.56, 1545.90, 
        1727.58, 894.77, 880.27, 846.80, 959.81, 1280.55, 1227.36, 
        1169.19, 1330.01, 1676.26, 1501.24, 1509.74, 1650.20, 916.21, 
        880.05, 850.29, 1008.54, 1311.10, 1209.18, 1200.13, 1356.21, 
        1697.23, 1566.89, 1525.19, 1683.15, 885.12, 875.53, 838.02, 
        979.96, 1286.59, 1193.36, 1164.99, 1350.14, 1686.84, 1503.87, 
        1536.91, 1690.23)
design$Fc <- Fc
design2$Fc <- Fc

###***************************************************************************************************###
## Analise

# ANOVA para Fc
res.Fc <- aov(Fc ~ CB*f*vc, data = design)
summary(res.Fc)

lm.Fc <- lm(Fc ~ CB*f*vc, data = design2)
summary(lm.Fc)

###***************************************************************************************************###
## Pressuposicoes

# Normalidade
shapiro.test(res.Fc$residuals)

par(mfrow=c(2,2))
plot(res.Fc)

# Homocedasticidade
library(car)
leveneTest(Fc ~ CB*f*vc, data = design)

###***************************************************************************************************###
## Teste de comparacoes multiplas

library(emmeans)
tukey.Fc <- emmeans(res.Fc,   # Tukey
                    ~ CB|f)
tukey.Fc
plot(tukey.Fc)

library(ScottKnott)
sk1 <- with(design,
            SK(x = res.Fc,    # Skott-Knott
               y = Fc,
               model = 'Fc ~ CB*f',
               which = 'f:CB',
               fl1=1))

sk2 <- with(design,
            SK(x = res.Fc,
                    y = Fc,
                    model = 'Fc ~ CB*f',
                    which = 'f:CB',
                    fl1=2))
sk3 <- with(design,
            SK(x = res.Fc,
                    y = Fc,
                    model = 'Fc ~ CB*f',
                    which = 'f:CB',
                    fl1=3))

summary(sk1)
summary(sk2)
summary(sk3)

par(mfrow=c(1,3))
plot(sk1)
plot(sk2)
plot(sk3)

###***************************************************************************************************###
## Graficos de efeitos

library(phia)
IM <- interactionMeans(res.Fc)
IM
plot(IM)

library(ggpubr)

p1 <- ggline(data = design,
             x = "CB",
             y = "Fc",
             # add = c("mean", "point"),
             add = c("mean"),
             ylim = c(900,1600),
             color = "blue") + theme_bw()

p2 <- ggline(data = design,
             x = "f",
             y = "Fc",
             # add = c("mean", "point"),
             add = c("mean"),
             ylim = c(900,1600),
             color = "red") + theme_bw()

p3 <- ggline(data = design,
             x = "vc",
             y = "Fc",
             # add = c("mean", "point"),
             add = c("mean"),
             ylim = c(900,1600),
             color = "green3") + theme_bw()

ggarrange(p1,p2,p3)

p12 <- ggline(data = design,
              x = "f",
              y = "Fc",
              # add = c("mean", "point"),
              add = c("mean"),
              ylim = c(800,1700),
              color = "CB") + theme_bw()

p13 <- ggline(data = design,
              x = "vc",
              y = "Fc",
              # add = c("mean", "point"),
              add = c("mean"),
              ylim = c(800,1700),
              color = "CB") + theme_bw()

p23 <- ggline(data = design,
              x = "f",
              y = "Fc",
              # add = c("mean", "point"),
              add = c("mean"),
              ylim = c(800,1700),
              color = "vc") + theme_bw()

ggarrange(p12,p13,p23)

library(dplyr)

ggplot(design |>
         group_by(f,vc,CB) |>
         summarise(Fc=mean(Fc)), (aes(x=f,
                                      y=Fc,
                                      group=vc,
                                      col = vc,
                                      pch=vc, 
                                      lty=vc))) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(CB)) +
  theme_bw()

vc.labs <- c("vc = 310 m/min", "vc = 380 m/min")
names(vc.labs) <- c("310", "380")

ggplot(design |>
         group_by(f,vc,CB) |>
         summarise(Fc=mean(Fc)), (aes(x=f,
                                      y=Fc,
                                      group=CB,
                                      col = CB,
                                      pch=CB, 
                                      lty=CB))) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(vc), 
             labeller = labeller(vc = vc.labs)) +
  theme_bw()
