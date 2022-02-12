# Pacotes
library(xtable)
library(betareg)
library(tidyverse)
library(gridExtra)

# Lendo os dados
data("GasolineYield", package = "betareg")
head(GasolineYield)

# Modelo de Regressão Beta com função de ligação logito
ajuste <- betareg(yield ~ temp + batch, link = "logit",
                   data = GasolineYield,x=T)
summary(ajuste)

# Modelos com funções logito e log-log para criar o gráfico
ajuste1g <- betareg(yield ~ temp, link = "logit",
                    data = GasolineYield, subset = batch == 6)
ajuste2g <- betareg(yield ~ temp, link = "loglog",
                    data = GasolineYield, subset = batch == 6)

ggplot(GasolineYield, aes(x = temp, y = yield)) +
  geom_point(size = 4, aes(fill = batch), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(ajuste2g, GasolineYield),
                colour = "log-log", linetype = "log-log")) +
  geom_line(aes(y = predict(ajuste1g, GasolineYield), 
                colour = "logit", linetype = "logit")) +
  scale_colour_manual("", values = c("red", "blue")) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  labs(y="Proporção de oléo convertido para gasolina",
       x="Temperatura (F°) em que a gasolina evaporou",
       fill="Condição do lote",
       title="Comparação entre as funções de ligação logito e log-log no ajuste")+
  theme_light()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

# Função para criar os gráficos de diagnóstico
graficos_diagnostico <- function(ajuste_f,a){
  
  set.seed(123)
  p <- ncol(model.matrix(ajuste_f))
  n <- nrow(model.matrix(ajuste_f))
  cutoff <- qf(0.5, p, n - p)
  cooks <- cooks.distance(ajuste_f) # distancia de Cook
  index <- as.factor(1:length(ajuste_f$y))
  
  p1 <- ggplot()+
    geom_point(aes(y=residuals(ajuste_f,type = "pearson"),x=1:length(ajuste_f$y)),
               size = 2)+
    geom_hline(yintercept = 0, linetype = 2)+
    geom_hline(yintercept = c(-3,3), linetype = 3, color="blue")+
    labs(y="Residuos de Pearson",
         x="Índices das observações")+
    theme_light()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size=8))
  
  p2 <- ggplot()+
    geom_segment(aes(yend = cooks, y = 0,x=index,xend=index))+
    geom_hline(aes(yintercept = cutoff),linetype=2,color="blue")+
    theme_light()+
    labs(x="",y="Distância de Cook")+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size=8))
  
  p3 <- ggplot()+
    geom_point(aes(y=gleverage(ajuste_f),x=ajuste_f$fitted.values),
               size = 2)+
    labs(x="Valores preditos",
         y="Valores de alavanca generalizados")+
    theme_light()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size=8))
  
  p4 <- ggplot()+
    geom_point(aes(y=residuals(ajuste_f,type = "pearson"),
                   x=predict(ajuste_f)),
               size = 2)+
    geom_hline(yintercept = 0, linetype = 2)+
    labs(x="Resíduos de Pearson",
         y="Preditores linear")+
    theme_light()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size=8))
  
  p5 <- ggplot()+
    geom_point(aes(y=predict(ajuste_f),
                   x=ajuste_f$y),
               size = 2)+
    geom_abline(xintercept = 0, yintercept = 1,linetype = 2)+
    labs(x="Valores preditos",
         y="Valores observados")+
    theme_light()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size=8))
  
  p6 <- as.ggplot(function() plot(ajuste_f,which=5,type="deviance",sub.caption = " ",pch=19))
  
  grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2,top = paste0("Gráficos de diagnóstico para a regressão beta com função de ligação logito",a))
  
}

# Função para criar os códigos de significância
signif <- function(ajuste1){
  case_when(
    summary(ajuste_f)$coef[[1]][,4] > 0.1 ~ " ",
    summary(ajuste1)$coef[[1]][,4] <= 0.1 & summary(ajuste1)$coef[[1]][,4] > 0.05 ~ ".",
    summary(ajuste1)$coef[[1]][,4] <= 0.05 & summary(ajuste1)$coef[[1]][,4] > 0.01 ~ "*",
    summary(ajuste1)$coef[[1]][,4] <= 0.01 & summary(ajuste1)$coef[[1]][,4] > 0.001 ~ "**",
    summary(ajuste1)$coef[[1]][,4] <= 0.001 ~ "***",
  )
}

# Gráficos de diagnóstico  (Figura 2)
graficos_diagnostico(ajuste,a=NULL)

# Modelo de Regressão Beta com função de ligação logito
ajuste_f <- betareg(yield ~ temp + batch, link = "logit",
                  data = GasolineYield[-4,],x=T)
summary(ajuste_f)

# Código para gerar a Tabela 1
xtable(cbind(round(summary(ajuste)$coef[[1]][,-4],2),
             c("<2e-16","<2e-16","<2e-16","<2e-16","<2e-16","<2e-16",
               "<2e-16","<2e-16","6.29e-07","5.3e-06","0.00114"),
             signif(ajuste)),
       caption = "Coeficientes da regressão Beta com a função de ligação logito")

# Gráficos de diagnóstico (Figura 3)
graficos_diagnostico(ajuste_f,a=" (sem a 4° observação)")

# Código para gerar a Tabela 2
xtable(cbind(round(summary(ajuste_f)$coef[[1]][,-4],2),
             c("<2e-16","<2e-16","<2e-16","<2e-16","<2e-16","<2e-16",
               "<2e-16","<2e-16","3.39e-09","5.25e-07","0.0002"),
             signif(ajuste_f)),
       caption = "Coeficientes da regressão Beta com a função de ligação logito sem a 4° observação")

# Códigos para gerar a Figura 1
mu <- c(0.1,0.25,0.5,0.75,0.9,0.1,0.25,0.5,0.75,0.9)
phi <- c(5,5,5,5,5,100,100,100,100,100)

func <- dbeta
argu <- list(NULL)

for (i in 1:length(mu)) {
  argu[[i]] <- list(shape1 = mu[i]*phi[i], shape2 = (1-mu[i])*phi[i])
}

cond <- seq(0,1,length.out = 1000)

cols <- c("0.1"="#FF7F00",
          "0.25"="#7FFF00",
          "0.5"="#00FF7F",
          "0.75"="#007FFF",
          "0.9"="#7F00FF")

p1 <- qplot(cond,geom = 'blank') +   
  stat_function(aes(color = "0.1"),fun = func, n=length(cond),
                args = argu[[1]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.25"),fun = func, n=length(cond),
                args = argu[[2]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.5"),fun = func, n=length(cond),
                args = argu[[3]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.75"),fun = func, n=length(cond),
                args = argu[[4]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.9"),fun = func, n=length(cond),
                args = argu[[5]],
                inherit.aes = FALSE)+
  theme_light()+
  ylim(0,15)+
  labs(y = "Função densidade", x = expression(y),
       title = expression(paste(phi, "= 5")))+
  scale_colour_manual(name=expression(mu),values=cols)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

p2 <- qplot(cond,geom = 'blank') +   
  stat_function(aes(color = "0.1"),fun = func, n=length(cond),
                args = argu[[6]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.25"),fun = func, n=length(cond),
                args = argu[[7]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.5"),fun = func, n=length(cond),
                args = argu[[8]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.75"),fun = func, n=length(cond),
                args = argu[[9]],
                inherit.aes = FALSE)+
  stat_function(aes(color = "0.9"),fun = func, n=length(cond),
                args = argu[[10]],
                inherit.aes = FALSE)+
  theme_light()+
  ylim(0,15)+
  labs(y = "Função densidade", x = expression(y),
       title = expression(paste(phi, "= 100")))+
  scale_colour_manual(name=expression(mu),values=cols)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")
