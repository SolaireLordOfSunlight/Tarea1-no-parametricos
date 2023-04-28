data <- read.csv("./datos_t1.csv")
head(data)
attach(data)

# Origen
summary(as.factor(Origen))
(bp_vars <- summary(as.factor(Origen)) / dim(data)[1])

smry_df_org <- data.frame(
    clase = c("extranjera", "nacional"),
    p = c(0.59, 0.41),
    n = c(59, 41)
)

library(ggplot2)

png(filename = "bar1.png",
    width = 600, height = 800)
ggplot(data = smry_df_org, aes(x = clase, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = p), vjust = 1.6, color = "white", size = 5) +
    ggtitle("Barplot Origen de Empresa") +
    theme_minimal() +
    theme(plot.title = element_text(size = 30))
dev.off()

# Tipo
summary(as.factor(Tipo))
summary(as.factor(Tipo)) / dim(data)[1]

smry_df_type <- data.frame(
    tipo = c("1", "2", "3", "4"),
    p = c(0.3, 0.25, 0.27, 0.18),
    n = c(30, 25, 27, 18)
)

png(filename = "bar2.png",
    width = 600, height = 800)
ggplot(data = smry_df_type, aes(x = clase, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = p), vjust = 1.6, color = "white", size = 5) +
    theme_minimal() +
    ggtitle("Barplot Tipo de Empresa") +
    theme(plot.title = element_text(size = 30))
dev.off()

# Tiempo y Capitalizacion
library(dplyr)
data_tl <- data %>% select(Tiempo, Capitalizacion)

library(moments)
custom_smry <- function(x) {
    c(
        mean = mean(x),
        sd = sd(x),
        min = min(x),
        max = max(x),
        asimetria = skewness(x),
        krts = kurtosis(x))
}

apply(data_tl, 2, custom_smry)

# Test de Normalidad
hist(Tiempo)
png(filename = "hist1.png", width = 800, height = 450)
ggplot(data_tl, aes(x = Tiempo)) +
    geom_histogram(color = "white", fill = "#4682b4", bins = 10) +
    ggtitle("Histograma Tiempo") +
    theme(plot.title = element_text(size = 30))
dev.off()


hist(Capitalizacion)
png(filename = "hist2.png", width = 800, height = 450)
ggplot(data_tl, aes(x = Capitalizacion)) +
    geom_histogram(color = "white", fill = "#4682b4", bins = 10) +
    ggtitle("Histograma Capitalizacion") +
    theme(plot.title = element_text(size = 30))
dev.off()

# qqplotttt
library(ggpubr)
a <- ggqqplot(data_tl, x = "Capitalizacion",
    ggtheme = theme_minimal(),
    color = "#4682b4",
    title = "QQPlot")

png(filename = "qqplot.png", width = 800, height = 450)
ggpar(a, font.main = 25)
dev.off()

ks.test(Tiempo, "pnorm", mean(Tiempo), sd(Tiempo))
ks.test(Capitalizacion, "pnorm", mean(Capitalizacion), sd(Capitalizacion))

# test de signos

data  <- data %>% filter(Origen == "nacional")
dim(data)

dkmd <- 10.5
sum(data$Capitalizacion == dkmd) # no hay X_i = m

r <- max(sum(data$Capitalizacion > dkmd), sum(data$Capitalizacion < dkmd))

pvalue <- 2 * (1 - pbinom(r - 1, p = 0.5, size = 41))
#mismo que el pvalue de aca

library(BSDA)
BSDA::SIGN.test(data$Capitalizacion, md = 10.5)

###############################################
sample1 <- Capitalizacion
sample2<-rnorm(100, mean(Capitalizacion), sd = sd(Capitalizacion))

group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 

minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 


ggplot(dat, aes(x = KSD, group = group, colour = group, linetype=group))+
  stat_ecdf(size=1) +
  xlab("mm") +
  ylab("Cumulitive Distibution") +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]), linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", linewidth=1) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", linewidth=1) +
  ggtitle("K-S Test: Sample 1 / Sample 2")


  ######### ks test a mano
  n <- 100
  srt_cpt <- sort(Capitalizacion)
  z_cpt <- (srt_cpt - mean(srt_cpt)) / sd(srt_cpt)
  empirica <- c(1:n) / n
  empirica2 <- empirica - 1 / n
  teorica <- pnorm(z_cpt)
  hist(srt_cpt)
  plot(pnorm, from=-5, to=5, col="red", lwd=3)
  ECDF <- ecdf(z_cpt)
  plot(ECDF, add = T)
  qqnorm(srt_cpt)

  Dms <- abs(empirica-teorica)
  Dmenos <- abs(empirica2-teorica)
  D <- max(Dms, Dmenos)
  D #same

  alpha <- 0.05
  Ca <- 0.895
  Kn <- sqrt(n) - 0.01 + (0.85/sqrt(n))
  Kn
  Da <- Ca/Kn
  Da


