data <- read.csv('test.csv', sep=',')

install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("devtools")
library(devtools)
devtools::install_github('cttobin/ggthemr')
devtools::uninstall('ggthemr')
library(ggthemr)
ggthemr("dust")
ggthemr_reset()
warnings()
remove.packages("devtools")
install.packages("qcc")
library(qcc)
##PROBLEMA _ COMO DIMINUIR A ANSIEDADE DOS USUARIOS DE REDES SOCIAIS

##Primeiro -> descobrir onde está os piores casos (qual plataforma e genêro)?
##Segundo -> qual os maiores motivos dessa ansiedade?


glimpse(data)

sapply(data, function (x) sum(is.na(x)))
sapply(data, function (x) sum(is.nan(x)))

data <- data %>% filter(Age == as.numeric(data$Age))
data$Age <- as.numeric(data$Age)
data <- data %>% relocate(Age, .before = Dominant_Emotion)

boxplot(data[ ,4:9])

summary(data[ ,4:9])



data_means <- anx %>%
  group_by(Gender) %>%
  summarise(mean_usage = mean(Daily_Usage_Time..minutes.))
data_means



pareto <- anx %>% select(Platform, Daily_Usage_Time..minutes.) %>% 
  group_by(Platform) %>% summarise(tempo = sum(Daily_Usage_Time..minutes.)) %>%
  mutate(perc = tempo / sum(tempo) * 100) %>% arrange(desc(tempo)) %>%
  mutate(acumulado = cumsum(perc))



names(pareto$tempo) <- pareto$Platform
pareto.chart(pareto$tempo, cumperc = seq(0, 100, by=5), main='pareto',
             col = rainbow(length(pareto$tempo)))




anx <- data %>% filter(Dominant_Emotion == "Anxiety")



boxplot(anx[, 4:8])



ggplot(anx, aes(x = Gender, y=Daily_Usage_Time..minutes., col=Gender))+
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 3)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.3)+
  geom_label(data = data_means, 
             aes(label = round(mean_usage, 1), y = mean_usage + 4),
             vjust = -0.5,
             size=2)+
  labs(
    title = "Médias e IC 95% para Tempo de uso por Gênero",
    subtitle = "Instagram se destaca pelo maior tempo gasto",
    x = "Plataforma",
    y = "Média de uso por dia (em minutos)"
  )+
  theme_pubr(border = FALSE,
             margin = TRUE,
             x.text.angle = 45
  ) + scale_fill_cosmic() + theme(
    legend.position = c(0.95, 1),
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    legend.title = element_blank(),
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    plot.subtitle = element_text(size = 8, hjust = 0),
    axis.title = element_text(face = "bold"))






matriz_corr <- cor(data[4:9], method = "pearson")
ggcorrplot(matriz_corr,
           hc.order = TRUE,
           type = 'lower',
           legend.title = 'Coeficientes de Correlação',
           lab = TRUE,
           title = 'Correlação entre Variáveis',
           lab_size = 3,
           method = 'circle',
           colors = c('tomato','purple','lightblue'),
           ggtheme = theme_foundation())




ggplot(data, aes(x = Dominant_Emotion, y =Posts_Per_Day))+
  geom_bar(stat = "identity", fill = "tomato",  alpha = 0.7)


hist(anx$Age)
density(anx$Age)
ggplot(anx, aes(x = Age, y = ..density..))+
  geom_histogram(bins=7, aes(col='black', fill='tomato'), show.legend = F)+
  geom_density(alpha = 0.1, color='red', position = 'stack', show.legend = F)
##Probabilidade de pessoas ansiosas terem menos de 30 anos?
media <- mean(anx$Age)
desvio <- sd(anx$Age)
pnorm(29, mean = media, sd = desvio)



ggplot(anx, aes(y=Age))+
  geom_errorbar(stat='boxplot', width=0.01, col='black')+
  geom_boxplot(
               width = 0.02, outlier.shape = 1, outlier.size = 2,
               show.legend = F) + scale_fill_jama() + theme_classic() + scale_color_economist()+
  labs(title='Análise Variância e Médias por Emoção Dominante',
       y='Tempo médio de uso de aplicativos por dia - em Minutos',
       x = NULL)+ 
  theme(
         text = element_text(family = "serif", size = 15),
         plot.title = element_text(face = "bold", size = 12, hjust = 0),
         plot.subtitle = element_text(size = 8, hjust = 0),
         axis.title = element_text(face = "bold"))






anx_above_60 <- data %>% filter(Daily_Usage_Time..minutes. >= 100 & Dominant_Emotion == "Anxiety" )
anx_before_60 <- data %>% filter(Daily_Usage_Time..minutes. < 100 & Dominant_Emotion == "Anxiety" )
nrow(anx_above_60) # menor que 30 elementos 
nrow(anx_before_60) # menor que 35 elementos
shapiro.test(anx_before_60$Daily_Usage_Time..minutes.) # p < 0.05
shapiro.test(anx_above_60$Daily_Usage_Time..minutes.) # p < 0.05
# Dados com menos de 30 elementos e não normais, a ideia será usar um teste não paramétrico para as medianas

#TESTE MANN-WHITNEY (MEUS GRUPOS SÃO INDEPENDENTES)
#Ha : Usuários com ansiedade costumam usar redes sociais mais de 60 minutos por dia 
wilcox.test(anx_above_60$Daily_Usage_Time..minutes., anx_before_60$Daily_Usage_Time..minutes., 
            alternative = "greater", conf.int = T, conf.level = 0.9) 


#HÁ PROVAS QUE PESSOAS QUE MEXEM MAIS DE 60 MINUTOS NAS REDES SOCIAIS SÃO ANSIOSAS - P_VALOR DE MANN-WHITNEY < 0.05

#QUAL DISTRIBUIÇÃO MEUS DADOS SE ENQUADRA?
anx <- anx %>% filter(Age < 30)
hist(anx$Daily_Usage_Time..minutes.)
shapiro.test(anx$Daily_Usage_Time..minutes.)
#NÃO É NORMAL P < 0.05
gama <- fitdist(anx$Daily_Usage_Time..minutes., "gamma")
exponencial <- fitdist(anx$Daily_Usage_Time..minutes., "exp")
normal <- fitdist(anx$Daily_Usage_Time..minutes., "norm")

ks.test(anx$Daily_Usage_Time..minutes., "pgamma", shape = 9.9482040, rate = 0.1062555) #TESTANDO GAMMA
# P > 0.05
ks.test(anx$Daily_Usage_Time..minutes., "pexp", rate = 0.01068249) #TESTANDO EXP
# P < 0.05
ks.test(anx$Daily_Usage_Time..minutes., "pnorm", mean = 93.61, sd = 32.77) #TESTANDO NORMAL
# P < 0.05

media <- mean(anx$Daily_Usage_Time..minutes.)
media
desvio <- sd(anx$Daily_Usage_Time..minutes.)
desvio
shap <- (media^2) / (desvio^2)
shap
rat <- media / (desvio^2)
rat

#Qual a probabilidade do usuário estar nas redes após 60 minutos?
probabiliade_1h <- 1 - pgamma(60, shape = 7.774004623683532, rate = 0.08793218597482659)
# ~85%
probabiliade_1h





dados <- NULL
resultados <- NULL
lista_rep <- NULL

#demonstrando resultados
lista_rep <- seq(1:100)
resultados <- c()

for (x in seq_along(lista_rep)) {
  proba <- rgamma(lista_rep[x], shape = shap, rate = rat)
  resultados <- append(resultados, proba)
}
resultados

dados <- data.frame(resultados)


tiff('1.png', h=1800, w=2400, res='300')
ggplot(dados, aes(x = resultados)) +
  geom_histogram(aes(y = ..density..),binwidth = 1, color = "black", alpha = 0.5) +
  labs(title = "Distribuição de Probabilidades Gamma",
       subtitle = 'Probabilidade de cada tempo de uso das redes sociais por jovens de 25 a 28 anos com condição emocional "Ansioso"',
       x = "Tempo de Uso (minutos)",
       y = "Frequência")+
  geom_density(col='#2F4858', lwd=0.8, linetype=4)+
  geom_vline(xintercept = 60, lwd=0.5, col='#2F4858', linetype=3)+
  theme_classic()+
  scale_fill_gdocs()+
  scale_color_gdocs()+
  annotate('text', x = 150, y = 0.012, label=paste('Probabilidade de uso maior que 1 hora: ', round(probabiliade_1h*100,2),'%' ),
           size= 3.5, color='black')+
  theme(
    text = element_text(family = "serif", size = 15),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    axis.title = element_text(face = "bold")
  )

ggsave('plot_gamma.png', dpi = 300)



############teste chi-quadrado

tabela <- table(anx$Gender, anx$Platform)

View(tabela)
tabela2 <- prop.table(tabela)
tabela2

teste_chi <- chisq.test(tabela)
teste_chi$expected
teste_chi$observed
#ao que tudo indica o genero non-binary tem a maior frequência de casos de ansiedade





## SHIFT PLOT, INDICA DIFERENÇA DE QUANTIS ENTRE DOIS GRUPOS, CADA PONTO ACIMA 
## REPRESENTA QUE O GRUPO EXPERIMENTAL TEM DADOS MAIORES

set.seed(123)
before_treatment <- rnorm(100, mean = 50, sd = 10)
after_treatment <- rnorm(100, mean = 55, sd = 10)

# Calcular quantis
quantis_before <- quantile(anx_before_60$Likes_Received_Per_Day, probs = seq(0, 1, 0.01))
quantis_after <- quantile(anx_above_60$Likes_Received_Per_Day, probs = seq(0, 1, 0.01))

# Criar dataframe com os quantis
df_quantis <- data.frame(
  Quantis = seq(0, 1, 0.01),
  Before = quantis_before,
  After = quantis_after
)

# Calcular as diferenças dos quantis
df_quantis <- df_quantis %>%
  mutate(Difference = After - Before)

# Criar shift plot usando ggplot2
shift <- ggplot(df_quantis, aes(x = Before, y = Difference)) +
  geom_point(color = "tomato") +
  geom_line(aes(group = 1), color = "tomato4") +
  geom_hline(yintercept = 0, color = "lightblue", linetype = "dashed") +
  labs(
    title = "Shift Plot - Tempo Gastos > 60 min, < 60",
    x = "Quantis da distribuição de referência",
    y = "Diferença dos quantis"
  ) +
  theme_bw()


#################REGRESSÃO

anx$Daily_Usage_Time..minutes. <- log(anx$Daily_Usage_Time..minutes.)

model <- lm(anx$Daily_Usage_Time..minutes. ~ anx$Messages_Sent_Per_Day)

durbinWatsonTest(model)

bptest(model, studentize = TRUE)

shapiro.test(model$residuals)

anova(model)

summary(model)

confint.lm(model)

p <- ggplot(data = anx,aes(x = Messages_Sent_Per_Day, y = Daily_Usage_Time..minutes.)) +
  geom_point(size=2.5,, pch= 21, col="brown4", fill="coral") +
  geom_smooth(method = "lm", col = "cornflowerblue", se=FALSE) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")),
                        label.x = 20, label.y =1.4) +
  labs(title = 'Regressão Linear', 
       subtitle = 'para cada 1 mensagem enviada se gasta ~ um minuto e meio na rede social ',
       y='Tempo gasto nas Redes (minuto',
       x = 'Quantidade de Mensagens Enviadas')+theme_bw()


grid.arrange(shift, p)

install.packages("pROC")
install.packages("caret")
install.packages("performance")
install.packages("lindia")
install.packages("DAAG")
library(pROC)
library(caret)
library(performance)
library(lindia)
library(DAAG)


check_model(model)

gg_diagnose(model)




teste <- cv.lm(anx, form.lm = (Daily_Usage_Time..minutes. ~ Messages_Sent_Per_Day), m = 3,
               plotit = c("Observed","Residual"), dots = T, printit = T)



