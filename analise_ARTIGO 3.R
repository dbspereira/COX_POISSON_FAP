#------------------------ Artigo 3 - Regressão de COX E POISSON -------------------#
############################################################################
##################################### pacotes diversos
library(tidyverse)
library(ggplot2)
library(foreign)
library(tidyverse)
library(anchors)
library(readxl)
library(openxlsx)
library(dplyr)
library(mice)
library(VIM)
library(lattice)
library(ggplot2)
library(hrbrthemes)
library(Rcpp)
library(roxygen2)
library(devtools)
library(digest)
library(lme4)
library(sjPlot)
library(reshape2)
library(glmmTMB)
library(nlme)
library(rlist)
library(summarytools)
library(glue)
library(stats)
library(readxl)
library(readr)
library(shiny)
library(AGD)
library(gamlss)
library(gamlss.data)
library(gamlss.dist)
library(MASS)
library(nlme)
library(parallel)
library(devtools)
library(xlsx)
library(survey)
library(haven)
library(rJava)
library(openxlsx)
library(writexl)
library(lbreg)
library(Hmisc)
library(epitools)
library(psych)
library(mnormt)
library(pROC)
library(ResourceSelection)
library(visreg)
library(tidyverse)
library(magrittr)
library(skimr)
library(mlbench)
library(naniar)
library(data.table)
library(psych)
require(AER)
library(survival)   # faz análise de sobrevivência
library(survminer)  # faz gráficos de sobrevida no ggplot2
library(car)   
library(ggExtra)
library(ggfortify)
library(readr)
require(stats)
library(udunits2)
library(units)
library(tidyr)
library(twoxtwo)
library(epiR)
library(rio)
library(jtools)
library(broom)
library(ggstance)
library(broom.mixed)
library(eha)
library(gtsummary)
library(dplyr)
library(Hmisc)
library(srvyr)
library(lubridate)
library(ggsurvfit)
library(tidycmprsk)
library(condSURV)
library(rms)
################################################################################


####################### Demográficos ###########################################
#-----------------------------------------------------------------------------##

setwd("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS")

demo99_00 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo99-00.dta")
demo01_02 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo01-02.dta")
demo03_04 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo03-04.dta")
demo05_06 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo05-06.dta")
demo07_08 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo07-08.dta")
demo09_10 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo09-10.dta")
demo11_12 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo11-12.dta")
demo13_14 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo13-14.dta")
demo15_16 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/DEMO_I.XPT")
demo17_18 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/DEMO_J.XPT")
demo17_20 <- read_dta("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/demo1720.dta")

# -------------------- Separando variaveis de interesse -------------------#


# SEQN     - Respondent sequence number
# SDDSRVYR - Data release cycle
# RIDSTATR - Interview/Examination status
# WTINT2YR - Full sample 2 year interview weight
# WTMEC2YR - Full sample 2 year MEC exam weight
# SDMVPSU - pseudo-PSU de variação mascarada
#SDMVSTRA - Pseudoestrato de variância mascarada


###############################################################################                Variáveis de pesos amostrais selecionadas
##############################################################################

pesos0708 <- subset(demo07_08, select = c('seqn', 'sddsrvyr', 'ridstatr', 'wtint2yr',
                                       'wtmec2yr', 'sdmvpsu','sdmvstra'))
pesos0910 <- subset(demo09_10, select = c('seqn', 'sddsrvyr', 'ridstatr', 'wtint2yr',
                                       'wtmec2yr','sdmvpsu','sdmvstra'))
pesos1112 <- subset(demo11_12, select = c('seqn', 'sddsrvyr', 'ridstatr', 'wtint2yr',
                                       'wtmec2yr','sdmvpsu','sdmvstra'))
pesos1314 <- subset(demo13_14, select = c('seqn', 'sddsrvyr', 'ridstatr', 'wtint2yr',
                                       'wtmec2yr','sdmvpsu','sdmvstra'))
pesos1516 <- subset(demo15_16, select = c('SEQN', 'SDDSRVYR', 'RIDSTATR', 'WTINT2YR',
                                       'WTMEC2YR','SDMVPSU','SDMVSTRA'))


pesos1516 <- rename(pesos1516, seqn = SEQN, 
                    sddsrvyr = SDDSRVYR,
                    ridstatr = RIDSTATR,
                    wtint2yr = WTINT2YR,
                    wtmec2yr = WTMEC2YR,
                    sdmvpsu = SDMVPSU,
                    sdmvstra = SDMVSTRA)


pesos1718 <- subset(demo17_18,select = c('SEQN', 'SDDSRVYR', 'RIDSTATR', 'WTINT2YR',
                                         'WTMEC2YR','SDMVPSU','SDMVSTRA'))


pesos1718 <- rename(pesos1718, seqn = SEQN, 
                    sddsrvyr = SDDSRVYR,
                    ridstatr = RIDSTATR,
                    wtint2yr = WTINT2YR,
                    wtmec2yr = WTMEC2YR,
                    sdmvpsu = SDMVPSU,
                    sdmvstra = SDMVSTRA)

pesos0718 <- rbind(pesos0708, pesos0910, pesos1112, pesos1314, pesos1516, pesos1718)
 

table(pesos0718$sddsrvyr)

#write.xlsx(pesos0718, file = "pesos0718.xlsx", sheetName = "pesos0718", append = FALSE)

pesos0718 <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/pesos0718.xlsx")

View(pesos0718)

###############################################################################                 Banco imputado com sociodemográficos e história de peso
##############################################################################

bancoimputado <- read.csv("imp_pmm_final_MA.csv")


bancoimputado$X <- NULL
bancoimputado$RIDEXMON <- NULL

bancoimputado <- rename(bancoimputado, seqn = SEQN)
bancoimputado <- rename(bancoimputado, sexo = RIAGENDR)
bancoimputado <- rename(bancoimputado, idade = RIDAGEYR)
bancoimputado <- rename(bancoimputado, etnia = RIDRETH1)
bancoimputado <- rename(bancoimputado, escolaridade = DMDEDUC2)
bancoimputado <- rename(bancoimputado, estcivil = DMDMARTL)
bancoimputado <- rename(bancoimputado, indpobreza = INDFMPIR)
bancoimputado <- rename(bancoimputado, gravidez = RIDEXPRG)
bancoimputado <- rename(bancoimputado, ciclo = SDDSRVYR)


#RECORTE ETÁRIO DESEJADO

bancoimputado1 <- filter(bancoimputado, idade <= 60)

bancoimputado <- bancoimputado1

############### Banco SOCIODEMOGRÁFICOS limpo!!!!! ###########################

######################### ESTILO DE VIDA #####################################
#____________________________________________________________________________#


# ---------------------------------------------------------------------------#
#                                ÁLCOOL
#                                -----------------------------------------------------------------------------#

setwd("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL")

#Selecionando variáveis de interesse

#SEQN     - Respondent sequence number
#ALQ101 - Had at least 12 alcohol drinks/1 yr?
#ALQ130 - Avg # alcoholic drinks/day -past 12 mos
#nunca bebeu, pelo menos 12 no último ano 
#0 = nunca bebeu, 1 = até uma vez por mês, 2=15 ou mais

alc07_08 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_E.XPT")
alc09_10 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_F.XPT")
alc11_12 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_G.XPT")
alc13_14 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_H.XPT")
alc15_16 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_I.XPT")
alc17_18 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/ALQ_J.XPT")



alc0708 <- subset(alc07_08, select = c('SEQN', 'ALQ101', 'ALQ130'))
alc0910 <- subset(alc09_10, select = c('SEQN', 'ALQ101', 'ALQ130'))
alc1112 <- subset(alc11_12, select = c('SEQN', 'ALQ101', 'ALQ130'))
alc1314 <- subset(alc13_14, select = c('SEQN', 'ALQ101', 'ALQ130'))
alc1516 <- subset(alc15_16, select = c('SEQN', 'ALQ101', 'ALQ130'))
alc1718 <- subset(alc17_18, select = c('SEQN', 'ALQ111', 'ALQ130'))


#excluir uma coluna 1 variável 111
view(alc1718)

alc1718$ALQ111 =  alc1718$ALQ101
alc1718 <- alc1718[,-c(1)]

#write.xlsx(alc1718, file = "alc1718.xlsx", sheetName = "alc1718", append = FALSE)

alc1718 <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/ALCOOL/alc1718.xlsx")


alctot <- rbind(alc0708, alc0910, alc1112, alc1314, alc1516, alc1718)

#limpeza do banco de álcool

#VARIÁVEL 101 tomou bebida alcóolica nos últimos 12 meses:
#1 = SIM /2 = NÃO / 7= RECUSAS / 9 = NÃO SABE / . missing

alctot$ALQ101[alctot$ALQ101 >= 7] <- NA

table(alctot$ALQ101)

alctot$consumoalcool <- factor(alctot$ALQ101,
                      levels = c(2,1),
                      labels = c("0","1"))


table(alctot$consumoalcool)
str(alctot$consumoalcool)

summary(alctot$ALQ101)

#recodifiquei para:: 0 = não 1 = sim

#variável ALQ130  Nº médio de bebidas alcoólicas/dia - últimos 12 meses
#Nos últimos 12 meses, nos dias em que {você/SP} consumiu bebidas alcoólicas, em média, quantos #drinques {você/ele/ela} tomou?
#SE MENOS DE 1 BEBIDA, DIGITE '1'. SE 95 #BEBIDAS OU MAIS, DIGITE '95'. INSIRA # DE BEBIDAS
# 77/777 = RECUSOU / 99/999 = NÃO SABE / . = MISSING / 

table(alctot$ALQ130)

alctot$ALQ130[alctot$ALQ130 >= 777] <- NA

alctot$ALQ130 <- as.numeric(alctot$ALQ130)
alctot$ALQ130 < alctot$ALQ130/12


alctot$consumomedio <- alctot$ALQ130/12
alctot$consumomedio <- round(alctot$consumomedio, 2)

summary(alctot$consumomedio)


str(alctot)

consalcool <-subset(alctot, select = c(SEQN, consumomedio, consumoalcool))

################################################# banco álcool está limpo!!!!


############################################################################# 
#                        ATIVIDADE FÍSICA                                   #
# --------------------------------------------------------------------------#

setwd("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA")

af07_08 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_E.XPT")

af09_10 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_F.XPT")

af11_12 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_G.XPT")

af13_14 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_H.XPT")

af15_16 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_I.XPT")

af17_18 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\PAQ_J.XPT")

#Selecionando as variáveis de interesse
# SEQN - Respondent sequence number
#paq610 - Days vigorous work
#pad615 - Minutes vigorous-intensity work
#paq625 - Number of days moderate work
#pad630 - Minutes moderate-intensity work
#paq640 - Number of days walk or bicycle
#pad645 - Minutes walk/bicycle for transportation
#paq655 - Days vigorous recreational activities
#pad660 - Minutes vigorous recreati
#paq670 - Days moderate recreational activities
#pad675 - Minutes moderate recreational activities
#pad680 - Minutes sedentary activity

#ANOS 1999 A 2006
#SEQN - Respondent sequence number
#PAQ050Q - # times walked or bicycled
#PAQ050U - Unit of measure (day/week/month)
#PAD080 - How long per day (minutes)
#PAQ180 - Avg level of physical activity each day
#PAD200 - Vigorous activity over past 30 days
#PAD320 - Moderate activity over past 30 days
#PAQ560 - # time/week you play or exercise hard

#Variáveis de interesse

#ANOS 2007 A 2018
af0708 <- subset(af07_08, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))


af09010 <- subset(af09_10, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))

af1112 <- subset(af11_12, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))


af1314 <- subset(af13_14, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))


af1516 <- subset(af15_16, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))


af1718 <- subset(af17_18, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))

#af1720 <- subset(af17_20, select = c('SEQN', 'PAQ610', 'PAD615', 'PAQ625', 'PAD630', 'PAQ640', 'PAD645', 'PAQ655', 'PAD660', 'PAQ670', 'PAD675', 'PAD680'))

AF0720 <- rbind(af0708, af09010, af1112, af1314, af1516, af1718)

#PAQ610 - Dias de trabalho vigoroso
#PAD615 - Minutos de trabalho de intensidade vigorosa
#PAQ625 - Número de dias de trabalho moderado
#PAD630 - Minutos de trabalho de intensidade moderada
#PAQ640 - Número de dias a pé ou de bicicleta
#PAD645 - Minutos a pé/bicicleta para transporte
#PAQ655 - Dias de atividades recreativas vigorosas
#PAD660 - Minutos de atividades recreativas vigorosas
#PAD675 - Minutos de atividades recreativas moderadas
#PAD680 - Minutos de atividade sedentária

#removendo missings
#99 não sabe, 7777 recusas,	 9999	não sabe
AF0720$PAQ610[AF0720$PAQ610 == 99] <- NA
AF0720$PAD615[AF0720$PAD615 == 9999] <- NA
AF0720$PAQ625[AF0720$PAQ625 >= 77] <- NA
AF0720$PAD630[AF0720$PAD630 == 9999] <- NA
AF0720$PAQ640[AF0720$PAQ640 == 99] <- NA
AF0720$PAD645[AF0720$PAD645 == 9999] <- NA
AF0720$PAQ655[AF0720$PAQ655 == 99] <- NA
AF0720$PAD660[AF0720$PAD660 == 9999] <- NA
AF0720$PAQ670[AF0720$PAQ670 == 99] <- NA
AF0720$PAD675[AF0720$PAD675 == 9999] <- NA
AF0720$PAD680[AF0720$PAD680 >= 7777] <- NA

summary(AF0720)

#TRATAMENTO DO BANCO DE AF

AF0720$atmod1 <- AF0720$PAQ610*AF0720$PAD615
AF0720$atvig1 <- AF0720$PAQ625*AF0720$PAD630
AF0720$atleve1 <- AF0720$PAQ640*AF0720$PAD645
AF0720$atvig2 <- AF0720$PAQ655*AF0720$PAD660
AF0720$atmod2 <- AF0720$PAQ670*AF0720$PAD675


AF0720$atlevmodtot <- AF0720$atmod1 + AF0720$atmod2 + AF0720$atleve1 
AF0720$atvigtot <- AF0720$atvig1 + AF0720$atvig2
AF0720$mod <- cut(AF0720$atlevmodtot, breaks=c(-Inf,1, 150, Inf), labels=c("0", "1", "3"), right=FALSE)
AF0720$vig <- cut(AF0720$atvigtot, breaks=c(-Inf,1, 75, Inf), labels=c("0", "1", "3"), right=FALSE)

write.csv(AF0720, "C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\AF0720.csv")

table(AF0720$vig)

#Feito no excel
#bancof3$atv = bancof3$mod+bancof3$vig

AF0720 <- read.csv("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\ATIVIDADE FÍSICA\\AF0720.csv", sep=";")

table(AF0720$atv)


AF0720$catv1_teste <- factor(AF0720$atv,
                   levels = c(0,1,2,3,4,6),
                   labels = c("0","1","2", "2","2","2"))
table(AF0720$catv1_teste)

#0=sedentário, 1=insuficiente e 2=ativo

AF0720$catv <- cut(AF0720$atv, breaks=c(-Inf,1, 3, Inf), labels=c("0", "1", "2"), right=FALSE)

#2=sedentário e insuf 1=ativo


AF0720$catv2_teste <- factor(AF0720$catv1_teste,
                             levels = c(0,1,2),
                             labels = c("sedentario/insuf","sedentario/insuf","ativo"))

table(AF0720$catv2_teste)

#2=sedentário e insuf 1=ativo
AF0720$catv2 <- cut(AF0720$atv, breaks=c(-Inf, 3, Inf), labels=c("2", "1"), right=FALSE)

#banco atividade física ajustado!!!!

ativ0720 <- subset(AF0720, select = c('SEQN', "catv1_teste", "catv2_teste"))

######################### ESTADO NUTRICIONAL ################################
# ---------------------------------------------------------------------------#

setwd("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/HISTÓRIA DE PESO")

#já está no banco imputado

############################ DOENÇAS CRÔNICAS ############################# 
# ------------------------------------------------------------------------#

# ----------------------------- Diabetes ---------------------------------#
###########################################################################

#SEQN
#DIQ010 - SE TEM DM OU NÃO
#DID40G - IDADE DO DIAGNÓSTICO DA DOENÇA

setwd("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES")


DIA07_08 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_E.XPT")
DIA09_10 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_F.XPT")
DIA11_12 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_G.XPT")
DIA13_14 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_H.XPT")
DIA15_16 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_I.XPT")
DIA17_18 <- read.xport("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DIABETES/DIQ_J.XPT")

DM0708 <- subset(DIA07_08, select = c('SEQN', 'DIQ010','DID040'))
DM0910 <- subset(DIA09_10, select = c('SEQN', 'DIQ010','DID040'))
DM1112<- subset(DIA11_12, select = c('SEQN', 'DIQ010','DID040'))
DM1314 <- subset(DIA13_14, select = c('SEQN', 'DIQ010','DID040'))
DM1516 <- subset(DIA15_16, select = c('SEQN', 'DIQ010','DID040'))
DM1718 <- subset(DIA17_18, select = c('SEQN', 'DIQ010','DID040'))


dmtot <- rbind(DM0708, DM0910, DM1112, DM1314 , DM1516, DM1718)

#DIQ010 = 1 = SIM / 2 = NÃO / 3 = nível de açúcar no limite(NO SANGUE ESTÃO SUPERIORES DO QUE O NORMAL, MAS NÃO SUFICIENTEMENTE ALTOS PARA SER DIABETES.)
#/ 7 = RECUSA / 9 = NÃO SABE / . = missing

table(dmtot$DIQ010)

dmtot$DM <- factor(dmtot$DIQ010,
                        levels = c(1,2,3,7,9),
                        labels = c("1","0","0", "Recusa", "Não sabe"))
table(dmtot$DM)
#ver depois 
#dmtot$DM[dmtot$DM == "Recusa"] <- NA
#dmtot$DM[dmtot$DM == "Não sabe"] <- NA

#DID 040 IDADE DO DIAGNÓSTICO
# 1 A 79 ANOS = INSERÇÃO NORMAL
#80 = 80 ANOS OU MAIS
#666 = MENOS DE 1 ANO
#999 = NÃO SABE

table(dmtot$DID040)
dmtot$DID040[dmtot$DID040 == 999] <- NA
dmtot$DID040[dmtot$DID040 == 666] <- 0

dmtot$idadeDM <- dmtot$DID040

dm <- subset(dmtot, select = c(SEQN,idadeDM, DM))

###############################################################################                                   Hipertensão
#----------------------------------------------------------------------------#

#entre os anos 1999 a 2006 não possui a variável da idade do diagnóstico da doença,
#somente a partir do ano de 2007


setwd("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2")


HAS0708 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_E.XPT")
HAS0910 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_F.XPT")
HAS1112 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_G.XPT")
HAS1314 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_H.XPT")
HAS1516 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_I.XPT")
HAS1718 <- read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\PRESSÃO ARTERIAL 2\\BPQ_J.XPT")


###############################################################################                    SELECIONANDO AS VARIÁVEIS DE INTERESSE
##############################################################################

has0708 <- subset(HAS0708, select = c('SEQN', 'BPQ020', 'BPD035'))
has0910 <- subset(HAS0910, select = c('SEQN', 'BPQ020', 'BPD035'))
has1112 <- subset(HAS1112, select = c('SEQN', 'BPQ020', 'BPD035'))
has1314 <- subset(HAS1314, select = c('SEQN', 'BPQ020', 'BPD035'))
has1516 <- subset(HAS1516, select = c('SEQN', 'BPQ020', 'BPD035'))
has1718 <- subset(HAS1718, select = c('SEQN', 'BPQ020', 'BPD035'))

HAS0718 <- rbind(has0708, has0910, has1112, has1314, has1516, has1718)

#BPQ020 - Já lhe disseram que tinha pressão alta
#1 = SIM / 2 = NÃO / 7 = RECUSA / 9 NÃO SABE

HAS0718$HAS <- factor(HAS0718$BPQ020,
                       levels = c(1,2,9),
                       labels = c("1","0","Não sabe"))
table(HAS0718$HAS)
#VER DEPOIS
HAS0718$HAS[HAS0718$HAS == "Não sabe"] <- NA
table(HAS0718$HAS)

#BPD 035 = IDADE DO DIAGNÓSTICO
#4 A 79 = FAIXA DE VALORES
#80 = 80 ANOS OU MAIS
# 777 = RECUSA 999=NÃO SABE 

table(HAS0718$BPD035)

HAS0718$BPD035[HAS0718$BPD035 == 777] <- NA
HAS0718$BPD035[HAS0718$BPD035 == 999] <- NA

HAS0718$idadeHAS <- HAS0718$BPD035

hipertensao <- subset(HAS0718, select = c(SEQN,HAS, idadeHAS))


###################### Unindo DM com HAS ###################################

dmhas <- merge(dm, hipertensao, by="SEQN")
str(dmhas)

#Criando variável com idade mínima para has e dm
#PARA NÃO ESCOLHER O NA COMO MÍNIMO

dmhas[is.na(dmhas)] = 998

dmhas1 <- transform(dmhas, idademin_DM_HAS = pmin(idadeHAS, idadeDM))

dmhas1[dmhas1== 998] <- NA



################### DOENÇAS CARDIOVASCULARES E CÂNCER ########################

setwd <-("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/CONDIÇÕES MÉDICAS")


dcvca0708 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_E.XPT")

dcvca0910 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_F.XPT")

dcvca1112 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_G.XPT")

dcvca1314 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_H.XPT")

dcvca1516 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_I.XPT")

dcvca1718 <-  read_xpt("C:\\Users\\Débora\\Google Drive\\TESE\\Projeto TESE\\BANCOS NHANES\\CONDIÇÕES MÉDICAS\\MCQ_J.XPT")

#Selecionando variáveis de interesse

#2017-2020

#SEQN - Respondent sequence number
#MCQ160b - Ever told had congestive heart failure
#MCD180b - Age when told you had heart failure
#MCQ160c - Ever told you had coronary heart disease
#MCD180c - Age when told had coronary heart disease
#MCQ160d - Ever told you had angina/angina pectoris
#MCD180d - Age when told you had angina pectoris
#MCQ160e - Ever told you had heart attack
#MCD180e - Age when told you had heart attack
#MCQ160f - Ever told you had a stroke
#MCD180F - Age when told you had a stroke
#MCQ220 - Ever told you had cancer or malignancy
#MCQ230a - 1st cancer - what kind was it? # se o primeiro não tem haver com o cancer associado ao #peso corporal 
#MCQ230b - 2nd cancer - what kind was it?
#MCQ230c - 3rd cancer - what kind was it?
#MCQ230d - More than 3 kinds of cancer #DESCONSIDERAR

#última onda recente a idade do cancer não contém!
#somente de 2007 a 2018

#filtrar as idades 

#bancos de 2007 a 2008/ 2009-2010 /2011-2012/ 2013-2014/ 2015-2016/  com idades de cancer separados, já a partir dos anos 2017-2018, já pergunta a idade do primeiro tipo de cancer diagnosticado, 2017-2020 não contém a idade.

#MCQ240H - Câncer de esôfago com idade diagnosticado pela primeira vez
#MCQ240G - Idade em que o câncer de cólon foi diagnosticado pela primeira vez
#MCQ240V - Idade em que o câncer retal foi diagnosticado pela primeira vez
#MCQ240M - Idade em que o câncer de fígado foi diagnosticado pela primeira vez
#MCQ240I - Câncer de vesícula biliar com idade diagnosticado pela primeira vez
##MCQ240T - Câncer de pâncreas de idade diagnosticado pela primeira vez
#MCQ240E - Idade em que o câncer de mama foi diagnosticado pela primeira vez
#MCQ240CC - Idade em que o câncer uterino foi diagnosticado pela primeira vez
#MCQ240S - Idade em que o câncer de ovário foi diagnosticado pela primeira vez
#MCQ240J - Idade em que o câncer de rim foi diagnosticado pela primeira vez
#MCQ240BB - Idade em que o câncer de tireoide foi diagnosticado pela primeira vez
#anos 2017-2018 --> #MCQ240dk - idade do primeiro cancer diagnosticado

mcq0708 <-subset(dcvca0708, select = c('SEQN', 'MCQ160B', 'MCQ180B',
                                       'MCQ160C', 'MCQ180C', 'MCQ160D',
                                       'MCQ180D', 'MCQ160E', 'MCQ180E',
                                       'MCQ180F','MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B','MCQ240H',
                                       'MCQ240G','MCQ240V',
                                       'MCQ240M','MCQ240I','MCQ240T','MCQ240E',
                                       'MCQ240CC','MCQ240S','MCQ240J','MCQ240BB'))
                                    

mcq0910 <-subset(dcvca0910, select = c('SEQN', 'MCQ160B', 'MCQ180B',
                                       'MCQ160C', 'MCQ180C', 'MCQ160D',
                                       'MCQ180D', 'MCQ160E', 'MCQ180E',
                                       'MCQ180F','MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B','MCQ240H',
                                       'MCQ240G','MCQ240V',
                                       'MCQ240M','MCQ240I','MCQ240T','MCQ240E',
                                       'MCQ240CC','MCQ240S','MCQ240J','MCQ240BB'))
                                    

mcq1112 <-subset(dcvca1112, select = c('SEQN', 'MCQ160B', 'MCQ180B',
                                       'MCQ160C', 'MCQ180C', 'MCQ160D',
                                       'MCQ180D', 'MCQ160E', 'MCQ180E',
                                       'MCQ180F','MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B','MCQ240H',
                                       'MCQ240G','MCQ240V',
                                       'MCQ240M','MCQ240I','MCQ240T','MCQ240E',
                                       'MCQ240CC','MCQ240S','MCQ240J','MCQ240BB'))
                                       
mcq1314 <-subset(dcvca1314, select = c('SEQN', 'MCQ160B', 'MCQ180B',
                                       'MCQ160C', 'MCQ180C', 'MCQ160D',
                                       'MCQ180D', 'MCQ160E', 'MCQ180E',
                                       'MCQ180F','MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B','MCQ240H',
                                       'MCQ240G','MCQ240V',
                                       'MCQ240M','MCQ240I','MCQ240T','MCQ240E',
                                       'MCQ240CC','MCQ240S','MCQ240J','MCQ240BB'))
                                       

mcq1516 <-subset(dcvca1516, select = c('SEQN', 'MCQ160B', 'MCQ180B',
                                       'MCQ160C', 'MCQ180C', 'MCQ160D',
                                       'MCQ180D', 'MCQ160E', 'MCQ180E',
                                       'MCQ180F','MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B','MCQ240H',
                                       'MCQ240G','MCQ240V',
                                       'MCQ240M','MCQ240I','MCQ240T','MCQ240E',
                                       'MCQ240CC','MCQ240S','MCQ240J','MCQ240BB'))


mcqcor1 <- rbind(mcq0708, mcq0910, mcq1112, mcq1314, mcq1516)

##### MANIPULANDO O BANCO OS ANOS DE 2007 A 2016
#vendo os bancos como estão
str(mcqcor1)
skim(mcqcor1)

#renomeando as colunas
mcqcor1 <- rename(mcqcor1, insuf_cardiacacong = MCQ160B)
mcqcor1 <- rename(mcqcor1, idade_insufcardiacacong = MCQ180B)
mcqcor1 <- rename(mcqcor1, doenca_coronaria = MCQ160C)
mcqcor1 <- rename(mcqcor1, idade_doenca_coronaria = MCQ180C)
mcqcor1 <- rename(mcqcor1, angina_prectoris = MCQ160D)
mcqcor1 <- rename(mcqcor1, idade_angina_prectoris = MCQ180D)
mcqcor1 <- rename(mcqcor1, ataque_cardiaco = MCQ160E)
mcqcor1 <- rename(mcqcor1, idade_ataque_cardiaco = MCQ180E)
mcqcor1 <- rename(mcqcor1, idade_derrame = MCQ180F)
mcqcor1 <- rename(mcqcor1, derrame = MCQ160F)
mcqcor1 <- rename(mcqcor1, cancer = MCQ220)
mcqcor1 <- rename(mcqcor1, tipo1_cancer = MCQ230A)
mcqcor1 <- rename(mcqcor1, tipo2_cancer = MCQ230B)
mcqcor1 <- rename(mcqcor1, idade_ca_esofago = MCQ240H)
mcqcor1 <- rename(mcqcor1, idade_ca_colon = MCQ240G)
mcqcor1 <- rename(mcqcor1, idade_ca_reto = MCQ240V)
mcqcor1 <- rename(mcqcor1, idade_ca_figado = MCQ240M)
mcqcor1 <- rename(mcqcor1, idade_ca_vesicula = MCQ240I)
mcqcor1 <- rename(mcqcor1, idade_ca_pancreas = MCQ240T)
mcqcor1 <- rename(mcqcor1, idade_ca_mama = MCQ240E)
mcqcor1 <- rename(mcqcor1, idade_ca_uterino = MCQ240CC)
mcqcor1 <- rename(mcqcor1, idade_ca_ovario = MCQ240S)
mcqcor1 <- rename(mcqcor1, idade_ca_rim = MCQ240J)
mcqcor1 <- rename(mcqcor1, idade_ca_tireoide = MCQ240BB)

################# removendo missings

mcqcor1$insuf_cardiacacong[mcqcor1$insuf_cardiacacong == 7] <- NA
mcqcor1$insuf_cardiacacong[mcqcor1$insuf_cardiacacong == 9] <- NA


mcqcor1$idade_insufcardiacacong[mcqcor1$idade_insufcardiacacong == 77777] <- NA
mcqcor1$idade_insufcardiacacong[mcqcor1$idade_insufcardiacacong == 99999] <- NA

mcqcor1$doenca_coronaria[mcqcor1$doenca_coronaria == 7] <- NA
mcqcor1$doenca_coronaria[mcqcor1$doenca_coronaria == 9] <- NA

mcqcor1$idade_doenca_coronaria[mcqcor1$idade_doenca_coronaria == 77777] <- NA
mcqcor1$idade_doenca_coronaria[mcqcor1$idade_doenca_coronaria == 99999] <- NA

mcqcor1$angina_prectoris[mcqcor1$angina_prectoris == 7] <- NA
mcqcor1$angina_prectoris[mcqcor1$angina_prectoris == 9] <- NA

mcqcor1$idade_angina_prectoris[mcqcor1$idade_angina_prectoris == 77777] <- NA
mcqcor1$idade_angina_prectoris[mcqcor1$idade_angina_prectoris == 99999] <- NA


mcqcor1$ataque_cardiaco[mcqcor1$ataque_cardiaco == 7] <- NA
mcqcor1$ataque_cardiaco[mcqcor1$ataque_cardiaco == 9] <- NA


mcqcor1$idade_ataque_cardiaco[mcqcor1$idade_ataque_cardiaco == 77777] <- NA
mcqcor1$idade_ataque_cardiaco[mcqcor1$idade_ataque_cardiaco == 99999] <- NA

mcqcor1$derrame[mcqcor1$derrame == 7] <- NA
mcqcor1$derrame[mcqcor1$derrame == 9] <- NA


mcqcor1$idade_derrame[mcqcor1$idade_derrame == 77777] <- NA
mcqcor1$idade_derrame[mcqcor1$idade_derrame == 99999] <- NA

mcqcor1$cancer[mcqcor1$cancer == 7] <- NA
mcqcor1$cancer[mcqcor1$cancer == 9] <- NA


mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 77] <- NA
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 99] <- NA

mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 77] <- NA
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 99] <- NA

mcqcor1$idade_ca_mama[mcqcor1$idade_ca_mama == 99999] <- NA
mcqcor1$idade_ca_uterino[mcqcor1$idade_ca_uterino == 99999] <- NA
mcqcor1$idade_ca_tireoide[mcqcor1$idade_ca_tireoide == 99999] <- NA




############## primeiro tipo de cancer
table(mcqcor1$tipo1_cancer)

mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 14] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 16] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 17] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 18] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 19] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 22] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 28] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 29] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 31] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 37] <- 1
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 38] <- 1

table(mcqcor1$tipo1_cancer)

#segundo tipo de cancer

mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 14] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 16] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 17] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 18] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 19] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 22] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 28] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 29] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 31] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 37] <- 1
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 38] <- 1


#para quem não tem esses tipos de câncer, coloco 0

mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 10] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 11] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 12] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 13] <- 0
#14 É
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 15] <- 0
#16, 17, 18, 19
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 20] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 21] <- 0
#22 é
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 23] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 24] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 25] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 26] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 27] <- 0
#28 e 29
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 30] <- 0
#31 é
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 32] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 33] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 34] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 35] <- 0
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 36] <- 0
#37 e 38 é
mcqcor1$tipo1_cancer[mcqcor1$tipo1_cancer == 39] <- 0

#SEGUNDO TIPO DE CANCER

mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 10] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 11] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 12] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 13] <- 0
#14 É
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 15] <- 0
#16, 17, 18, 19
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 20] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 21] <- 0
#22 é
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 23] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 24] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 25] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 26] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 27] <- 0
#28 e 29
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 30] <- 0
#31 é
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 32] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 33] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 34] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 35] <- 0
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 36] <- 0
#37 e 38 é
mcqcor1$tipo2_cancer[mcqcor1$tipo2_cancer == 39] <- 0

#idade única para todas as doenças

mcqcor1[is.na(mcqcor1)] = 998

mcqcor1 <- transform(mcqcor1, idade_min_DCV_CA = pmin(idade_insufcardiacacong ,idade_doenca_coronaria, idade_angina_prectoris, idade_ataque_cardiaco, idade_derrame, idade_ca_esofago, idade_ca_colon, idade_ca_reto, idade_ca_figado,idade_ca_vesicula, idade_ca_pancreas, idade_ca_mama, idade_ca_uterino, idade_ca_ovario, idade_ca_rim, idade_ca_tireoide ))

mcqcor1[mcqcor1 == 998] <- NA

table(mcqcor1$idade_min_DCV_CA)

#manipulando o banco mcq1718
                                       
mcq1718 <-subset(dcvca1718, select = c('SEQN', 'MCQ160B', 'MCD180B',
                                       'MCQ160C', 'MCD180C', 'MCQ160D',
                                       'MCD180D', 'MCQ160E', 'MCD180E','MCD180F',
                                       'MCQ160F', 'MCQ220',
                                       'MCQ230A', 'MCQ230B',
                                       'MCD240A','MCD240B','MCD240C'))


#RECODIFICANDO AS COLUNAS DA BASE MCQ 1718


mcq1718 <- rename(mcq1718, c("MCQ180B"="MCD180B"))
mcq1718 <- rename(mcq1718, c("MCQ180C"="MCD180C"))
mcq1718 <- rename(mcq1718, c("MCQ180D"="MCD180D"))
mcq1718 <- rename(mcq1718, c("MCQ180E"="MCD180E"))
mcq1718 <- rename(mcq1718, c("MCQ180F"="MCD180F"))

skim(mcq1718)
View(mcq1718)
mcq1718 <- rename(mcq1718, insuf_cardiacacong = MCQ160B)
mcq1718 <- rename(mcq1718, idade_insufcardiacacong = MCQ180B)
mcq1718 <- rename(mcq1718, doenca_coronaria = MCQ160C)
mcq1718 <- rename(mcq1718, idade_doenca_coronaria = MCQ180C)
mcq1718 <- rename(mcq1718, angina_prectoris = MCQ160D)
mcq1718 <- rename(mcq1718, idade_angina_prectoris = MCQ180D)
mcq1718 <- rename(mcq1718, ataque_cardiaco = MCQ160E)
mcq1718 <- rename(mcq1718, idade_ataque_cardiaco = MCQ180E)
mcq1718 <- rename(mcq1718, idade_derrame = MCQ180F)
mcq1718 <- rename(mcq1718, derrame = MCQ160F)
mcq1718 <- rename(mcq1718, cancer = MCQ220)
mcq1718 <- rename(mcq1718, tipo1_cancer = MCQ230A)
mcq1718 <- rename(mcq1718, tipo2_cancer = MCQ230B)
mcq1718 <- rename(mcq1718, idade_tipo1_cancer = MCD240A)
mcq1718 <- rename(mcq1718, idade_tipo2_cancer = MCD240B)

mcq1718$MCD240C <- NULL

mcq1718$insuf_cardiacacong[mcq1718$insuf_cardiacacong == 9] <- NA

mcq1718$idade_insufcardiacacong[mcq1718$idade_insufcardiacacong == 99999] <- NA

mcq1718$doenca_coronaria[mcq1718$doenca_coronaria == 9] <- NA
mcq1718$idade_doenca_coronaria[mcq1718$idade_doenca_coronaria == 99999] <- NA
mcq1718$angina_prectoris[mcq1718$angina_prectoris == 9] <- NA
mcq1718$idade_angina_prectoris[mcq1718$idade_angina_prectoris == 99999] <- NA
mcq1718$ataque_cardiaco[mcq1718$ataque_cardiaco == 9] <- NA
mcq1718$idade_ataque_cardiaco[mcq1718$idade_ataque_cardiaco == 99999] <- NA
mcq1718$idade_derrame[mcq1718$idade_derrame == 99999] <- NA
mcq1718$cancer[mcq1718$cancer == 9] <- NA
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 99] <- NA
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 99] <- NA
mcq1718$idade_tipo1_cancer[mcq1718$idade_tipo1_cancer == 99999] <- NA
mcq1718$idade_tipo2_cancer[mcq1718$idade_tipo2_cancer == 99999] <- NA


### recodificando o tipo de cancer de interesse
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 14] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 16] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 17] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 18] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 19] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 22] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 28] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 29] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 31] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 37] <- 1
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 38] <- 1

table(mcq1718$tipo1_cancer)



#segundo tipo de cancer

mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 14] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 16] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 17] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 18] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 19] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 22] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 28] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 29] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 31] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 37] <- 1
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 38] <- 1


#para quem não tem esses tipos de câncer, coloco 0

mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 10] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 11] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 12] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 13] <- 0
#14 É
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 15] <- 0
#16, 17, 18, 19
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 20] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 21] <- 0
#22 é
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 23] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 24] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 25] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 26] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 27] <- 0
#28 e 29
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 30] <- 0
#31 é
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 32] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 33] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 34] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 35] <- 0
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 36] <- 0
#37 e 38 é
mcq1718$tipo1_cancer[mcq1718$tipo1_cancer == 39] <- 0

#SEGUNDO TIPO DE CANCER

mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 10] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 11] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 12] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 13] <- 0
#14 É
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 15] <- 0
#16, 17, 18, 19
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 20] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 21] <- 0
#22 é
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 23] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 24] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 25] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 26] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 27] <- 0
#28 e 29
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 30] <- 0
#31 é
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 32] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 33] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 34] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 35] <- 0
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 36] <- 0
#37 e 38 é
mcq1718$tipo2_cancer[mcq1718$tipo2_cancer == 39] <- 0


#idade única dos canceres de interesse


mcq1718$idade_unica_cancer <- ifelse(mcq1718$tipo1_cancer == "1", mcq1718$idade_tipo1_cancer, 0)

mcq1718$idade_unica_cancer2 <- ifelse(mcq1718$tipo2_cancer == "1", mcq1718$idade_tipo2_cancer, 0)



mcq1718[is.na(mcq1718)] = 998

mcq1718 <- transform(mcq1718, idade_todos_canceres = pmin(idade_unica_cancer, idade_unica_cancer2))

mcq1718[mcq1718 == 998] <- NA


table(mcq1718$idade_todos_canceres)

#incluindo as DCV

mcq1718[is.na(mcq1718)] = 998

doencas_DCV_CA2 <- transform(mcq1718, idade_min_DCV_CA = pmin(idade_insufcardiacacong ,idade_doenca_coronaria, idade_angina_prectoris, idade_ataque_cardiaco, idade_derrame, idade_unica_cancer))

doencas_DCV_CA2 [doencas_DCV_CA2 == 998] <- NA


###### unindo os bancos finais de doenças após a manipulação de cada um 

doencas_DCVCA_1_1 <-  subset(mcqcor1, select = c(1:14,26))
doencas_DCVCA_2_2 <-  subset(doencas_DCV_CA2, select = c(1:14,20))


doencas_dcvca_3 <- rbind(doencas_DCVCA_1_1,doencas_DCVCA_2_2)

table(doencas_dcvca_3$idade_min_DCV_CA)
table(doencas_dcvca_3$tipo1_cancer)
table(doencas_dcvca_3$tipo2_cancer)

doencas_dcvca_3$derrame[doencas_dcvca_3$derrame == 9] <- NA

#criando variável se tem DCV


#1 = sim e 2/0= não

doencas_dcvca_3$insuf_cardiacacong[doencas_dcvca_3$insuf_cardiacacong == 2] <- 0
doencas_dcvca_3$doenca_coronaria[doencas_dcvca_3$doenca_coronaria == 2] <- 0
doencas_dcvca_3$angina_prectoris[doencas_dcvca_3$angina_prectoris == 2] <- 0
doencas_dcvca_3$ataque_cardiaco[doencas_dcvca_3$ataque_cardiaco == 2] <- 0
doencas_dcvca_3$derrame[doencas_dcvca_3$derrame == 2] <- 0
doencas_dcvca_3$cancer[doencas_dcvca_3$cancer == 2] <- 0

table(doencas_dcvca_3$insuf_cardiacacong)
table(doencas_dcvca_3$doenca_coronaria)
table(doencas_dcvca_3$angina_prectoris)
table(doencas_dcvca_3$ataque_cardiaco)
table(doencas_dcvca_3$derrame)
table(doencas_dcvca_3$tipo1_cancer)
table(doencas_dcvca_3$tipo2_cancer)


str(doencas_dcvca_3$insuf_cardiacacong)
str(doencas_dcvca_3$doenca_coronaria)
str(doencas_dcvca_3$angina_prectoris)
str(doencas_dcvca_3$ataque_cardiaco)
str(doencas_dcvca_3$derrame)


###############################################################################

doencas_dcvca_3$DCV <- doencas_dcvca_3$insuf_cardiacacong+doencas_dcvca_3$doenca_coronaria+doencas_dcvca_3$angina_prectoris+doencas_dcvca_3$ataque_cardiaco+doencas_dcvca_3$derrame


doencas_dcvca_3$DCVteste <- rowSums(doencas_dcvca_3[, c("insuf_cardiacacong","doenca_coronaria",
                                                        "angina_prectoris","ataque_cardiaco", "derrame")], na.rm = TRUE)

table(doencas_dcvca_3$DCVteste)
skim(doencas_dcvca_3$DCV)

doencas_dcvca_3$DCVclass <- factor(doencas_dcvca_3$DCV,
                      levels = c(0,1,2,3,4,5),
                      labels = c("0","1", "1", "1", "1", "1"))
table(doencas_dcvca_3$DCVteste)

table(doencas_dcvca_3$cancer)



################################################################################3

#convertendo para numerico para poder somar
doencas_dcvca_3$DCVclass <- as.numeric(doencas_dcvca_3$DCVclass)
doencas_dcvca_3$DCVclass[doencas_dcvca_3$DCVclass == 1] <- 0
doencas_dcvca_3$DCVclass[doencas_dcvca_3$DCVclass == 2] <- 1

doencas_dcvca_3$cancer <- as.numeric(doencas_dcvca_3$cancer)
table(doencas_dcvca_3$cancer)

table(doencas_dcvca_3$DCVclass)

doencas_dcvca_3$dcv_cancer <- doencas_dcvca_3$DCVclass + doencas_dcvca_3$cancer

doencas_dcvca_3$dcv_cancer <- factor(doencas_dcvca_3$dcv_cancer,
                                   levels = c(0,1,2),
                                   labels = c("0","1", "1"))


table(doencas_dcvca_3$dcv_cancer)

#0 é = ausência e 1=presença das doenças (cancer ou qualquer tipo de dcv)

#tipos de câncer que quero selecionar
#tem cancer?

doencas_dcvca_3$cancer[doencas_dcvca_3$cancer == 2] <- 0
doencas_dcvca_3$cancer <- factor(doencas_dcvca_3$MCQ220,
                           levels = c(0,1),
                           labels = c("0","1"))

table(doencas_dcvca_3$cancer)

#esôfago 17, cólon 16, reto 31, fígado 22, vesícula biliar 18, pancreas 29, mama 14,
# utero 38, ovario 28, rins 19, tireoide 37, cardia gástrica e mieloma múltiplo pela IARC, mas a variável não possui

#UNIFICANDO TODAS AS BASES QUE CONTÉM DOENÇAS

cargadoencas <- merge(doencas_dcvca_3, dmhas, by="SEQN")

#CRIANDO IDADE MÍNICA DE TODAS AS DOENÇAS
cargadoencas[is.na(cargadoencas)] = 998

cargadoencas <- transform(cargadoencas, idade_DCNT = pmin(idadeHAS, idadeDM, idade_min_DCV_CA))

cargadoencas[cargadoencas == 998] <- NA

table(cargadoencas$idade_DCNT)

#dm
str(cargadoencas$DM)
table(cargadoencas$DM)
cargadoencas$DM <- as.numeric(cargadoencas$DM)
table(cargadoencas$DM)
cargadoencas$DM [cargadoencas$DM == "3"] <- NA
cargadoencas$DM [cargadoencas$DM == "4"] <- NA
table(cargadoencas$DM)
cargadoencas$DM [cargadoencas$DM == "2"] <- 0
table(cargadoencas$DM)

#has
str(cargadoencas$HAS)
table(cargadoencas$HAS)
cargadoencas$HAS <- as.numeric(cargadoencas$HAS)
table(cargadoencas$HAS)
cargadoencas$HAS [cargadoencas$HAS == "2"] <- "0"
table(cargadoencas$HAS)

str(cargadoencas)
skim(cargadoencas)

#cargadoencas <- write.xlsx(cargadoencas, file = "cargadoencas.xlsx", sheetName = "cargadoencas", append = FALSE)


cargadoencas <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/PRESSÃO ARTERIAL 2/cargadoencas.xlsx")
View(cargadoencas)


###### LOOP para contagem de doenças nas variáveis #####################

value <- 0
for (row in 1:nrow(cargadoencas)) {
  doenca_coronaria <- cargadoencas[row, "doenca_coronaria"]
  insuf_cardiacacong <- cargadoencas[row, "insuf_cardiacacong"]
  angina_prectoris <- cargadoencas[row, "angina_prectoris"]
  ataque_cardiaco <- cargadoencas[row, "ataque_cardiaco"]
  derrame <- cargadoencas[row, "derrame"]
  tipo1_cancer <- cargadoencas[row, "tipo1_cancer"]
  tipo2_cancer <- cargadoencas[row, "tipo2_cancer"]
  DM <- cargadoencas [row, "DM"]
  HAS <- cargadoencas [row, "HAS"]
  
  if (!is.na(doenca_coronaria) & doenca_coronaria == "1") {
    value <- value + 1
  }
  if (!is.na(insuf_cardiacacong) & insuf_cardiacacong == "1") {
    value <- value + 1
  }
  
  if (!is.na(angina_prectoris) &  angina_prectoris  == "1") {
    value <- value + 1
  }
  if (!is.na(ataque_cardiaco) &  ataque_cardiaco  == "1") {
    value <- value + 1
  }
  if (!is.na(derrame) &  derrame  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo1_cancer) &  tipo1_cancer  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo2_cancer) &  tipo2_cancer  == "1") {
    value <- value + 1
  }
  if (!is.na(DM) &  DM  == "1") {
    value <- value + 1
  }
  if (!is.na(HAS) &  HAS  == "1") {
    value <- value + 1
  }  
} 

print(value)


############################3# LOOP PARA CRIAR VARIÁVEL DE CARGA DE DCNT

value <- 0
list <- 0

for (row in 1:nrow(cargadoencas)) {
  doenca_coronaria <- cargadoencas[row, "doenca_coronaria"]
  insuf_cardiacacong <- cargadoencas[row, "insuf_cardiacacong"]
  angina_prectoris <- cargadoencas[row, "angina_prectoris"]
  ataque_cardiaco <- cargadoencas[row, "ataque_cardiaco"]
  derrame <- cargadoencas[row, "derrame"]
  DM <- cargadoencas[row, "DM"]
  HAS <- cargadoencas[row, "HAS"]
  tipo1_cancer <- cargadoencas[row, "tipo1_cancer"]
  tipo2_cancer <- cargadoencas[row, "tipo2_cancer"]
  
  if (!is.na(doenca_coronaria) & doenca_coronaria == "1") {
    value <- value + 1
  }
  if (!is.na(insuf_cardiacacong) & insuf_cardiacacong == "1") {
    value <- value + 1
  }
  
  if (!is.na( angina_prectoris) &  angina_prectoris  == "1") {
    value <- value + 1
  }
  if (!is.na(ataque_cardiaco) &  ataque_cardiaco  == "1") {
    value <- value + 1
  }
  if (!is.na(derrame) &  derrame  == "1") {
    value <- value + 1
  }
  if (!is.na(DM) &  DM  == "1") {
    value <- value + 1
  }
  if (!is.na(HAS) &  HAS  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo1_cancer) &  tipo1_cancer  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo2_cancer) &  tipo2_cancer  == "1") {
    value <- value + 1
  }
  list <- c(list,value)
  value <- 0
} 

lista <- list[-1]
cargadoencas$DCNTcarga <- lista
cargadoencas$DCNT_carga <- ifelse(cargadoencas$DCNTcarga >= 1, 1, 0)
table(cargadoencas$DCNT_carga)



###### ------------------ BANCO TABAGISMO -------------------- ######

setwd <- ("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO")


tabag0708 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_E.XPT")

tabag0910 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_F.XPT")

tabag1112 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_G.XPT")

tabag1314 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_H.XPT")

tabag1516 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_I.XPT")

tabag1718 <- read_xpt("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/TABAGISMO/SMQ_J.XPT")

#variáveis de interesse:

#SEQN - Respondent sequence number
#SMQ020 - Smoked at least 100 cigarettes in life #nunca fumou e fumante e fumante atual
#SMQ040 - Do you now smoke cigarettes? 
#SMQ050Q - How long since quit smoking cigarettes
#SMD630 (idade que começou fumar)

tab0708 <- subset(tabag0708, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))
tab0910 <- subset(tabag0910, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))
tab1112 <- subset(tabag1112, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))
tab1314 <- subset(tabag1314, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))
tab1516 <- subset(tabag1516, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))
tab1718 <- subset(tabag1718, select = c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q','SMD630'))

tabagismotot <- rbind(tab0708,tab0910, tab1112, tab1314, tab1516, tab1718)
str(tabagismotot)
describe(tabagismotot)

tabagismotot$SMQ050Q[tabagismotot$SMQ050Q >= 77777] <- NA
tabagismotot$SMQ050Q[tabagismotot$SMQ050Q == 66666] <- 50
tabagismotot$SMQ050Q[tabagismotot$SMQ050Q == 193] <- NA

table(tabagismotot$SMQ050Q)

#9 - não sabe
tabagismotot$SMQ020[tabagismotot$SMQ020 >= 7] <- NA

table(tabagismotot$SMQ020)

#7- recusa 9- não sabe
tabagismotot$SMQ040[tabagismotot$SMQ040 == 7] <- NA
table(tabagismotot$SMQ040)


#renomeando as colunas das variáveis de tabagismo
tabagismotot <- rename(tabagismotot, fumou100_cigarros = SMQ020)
tabagismotot <- rename(tabagismotot, fumante_atual = SMQ040)
tabagismotot <- rename(tabagismotot, tempo_parou_de_fumar = SMQ050Q)
tabagismotot <- rename(tabagismotot, idade_comecou_fumar = SMD630)

str(tabagismotot)
summary(tabagismotot)

######################## TRANSFORMAÇÃO DAS VARIÁVEIS ##########################


#HISTÓRIA DE PESO
#peso atual
bancoimputado$WHD020 = as.numeric(bancoimputado$WHD020)
bancoimputado$WHD020 <- bancoimputado$WHD020*0.453592
bancoimputado$WHD020 <- round(bancoimputado$WHD020, digits = 2)


#peso 1 ano atrás
bancoimputado$WHD050 = as.numeric(bancoimputado$WHD050)
bancoimputado$WHD050 <- bancoimputado$WHD050*0.453592
bancoimputado$WHD050 <- round(bancoimputado$WHD050, digits = 2)


#peso 10 anos atrás
bancoimputado$WHD110 = as.numeric(bancoimputado$WHD110)
bancoimputado$WHD110 <- bancoimputado$WHD110*0.453592
bancoimputado$WHD110 <- round(bancoimputado$WHD110, digits = 2)


#peso aos 25 anos
bancoimputado$WHD120 = as.numeric(bancoimputado$WHD120)
bancoimputado$WHD120 <- bancoimputado$WHD120*0.453592
bancoimputado$WHD120 <- round(bancoimputado$WHD120, digits = 2)


#maior peso obtido na vida 
bancoimputado$WHD140 = as.numeric(bancoimputado$WHD140)
bancoimputado$WHD140 <- bancoimputado$WHD140*0.453592
bancoimputado$WHD140 <- round(bancoimputado$WHD140, digits = 2)



#altura atual

bancoimputado$WHD010 = as.numeric(bancoimputado$WHD010)
bancoimputado$WHD010 <- bancoimputado$WHD010*0.0254
bancoimputado$WHD010 <- round(bancoimputado$WHD010, digits = 2)


#altura aos 25 anos

bancoimputado$WHD130 = as.numeric(bancoimputado$WHD130)
bancoimputado$WHD130 <- bancoimputado$WHD130*0.0254
bancoimputado$WHD130 <- round(bancoimputado$WHD130, digits = 2)


#-----
#PARA TODOS OS CALCULOS DE IMC, CONSIDEREI A ALTURA ATUAL!

#imc atual
bancoimputado$imcatual <- (bancoimputado$WHD020/(bancoimputado$WHD010*bancoimputado$WHD010))
bancoimputado$imcatual <- round(bancoimputado$imcatual, digits = 2)

#-
#imc um ano
bancoimputado$imc1ano <- (bancoimputado$WHD050/(bancoimputado$WHD010*bancoimputado$WHD010))
bancoimputado$imc1ano <- round(bancoimputado$imc1ano, digits = 2)


#imc 10 anos

bancoimputado$imc10ano <- (bancoimputado$WHD110/(bancoimputado$WHD010*bancoimputado$WHD010))
bancoimputado$imc10ano <- round(bancoimputado$imc10ano, digits = 2)


#imc 25 anos
bancoimputado$imc25ano <- (bancoimputado$WHD120/(bancoimputado$WHD010*bancoimputado$WHD010))
bancoimputado$imc25ano <- round(bancoimputado$imc25ano, digits = 2)

#maior IMC obtido

bancoimputado$imcmaior <- (bancoimputado$WHD140/(bancoimputado$WHD010*bancoimputado$WHD010))
bancoimputado$imcmaior <- round(bancoimputado$imcmaior, digits = 2)

#--criando colunas DO IMC

bancoimputado$cimcatual = cut(bancoimputado$imcatual, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
bancoimputado$cimc1ano = cut(bancoimputado$imc1ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
bancoimputado$cimc10ano = cut(bancoimputado$imc10ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
bancoimputado$cimc25ano = cut(bancoimputado$imc25ano, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)
bancoimputado$cimcmaior = cut(bancoimputado$imcmaior, breaks=c(0,18.5, 25.0, 30.0, Inf), labels=c("baixo peso", "peso normal","sobrepeso", "obesidade"), right=FALSE)

View(bancoimputado)

n_occur <- data.frame(table(bancoimputado$SEQN))
table(n_occur$Freq)
describe(bancoimputado)
hist(bancoimputado$imcatual)

#Cálculos do limite para as diferenças

#par(new=TRUE)


bancoimputado$difimc1 = bancoimputado$imcatual-bancoimputado$imc1ano
qqnorm(bancoimputado$difimc1)
qqline(bancoimputado$difimc1)
hist(bancoimputado$difimc1)
hist(rnorm(bancoimputado$difimc1))
plot(density(bancoimputado$difimc1))
###

bancoimputado$difimc10 = bancoimputado$imcatual-bancoimputado$imc10ano
qqnorm(bancoimputado$difimc10)
qqline(bancoimputado$difimc10)
hist(bancoimputado$difimc10)
hist(rnorm(bancoimputado$difimc10))
plot(density(bancoimputado$difimc10))


#####
bancoimputado$difimc25 = bancoimputado$imcatual-bancoimputado$imc25ano

qqnorm(bancoimputado$difimc25)
qqline(bancoimputado$difimc25)
hist(bancoimputado$difimc25)
hist(rnorm(bancoimputado$difimc25))
plot(density(bancoimputado$difimc25))

############ diferenças solicitadas pelo wolney



par(mfrow=c(3,3))

bancoimputado$difimc25ximc10 = bancoimputado$imc25ano-bancoimputado$imc10ano
qqnorm(bancoimputado$difimc25ximc10)
qqline(bancoimputado$difimc25ximc10)
hist(bancoimputado$difimc25ximc10)
plot(density(bancoimputado$difimc25ximc10))



########
bancoimputado$difimc10ximc1 = bancoimputado$imc10ano-bancoimputado$imc1ano
qqnorm(bancoimputado$difimc10ximc1)
qqline(bancoimputado$difimc10ximc1)
hist(bancoimputado$difimc10ximc1)
plot(density(bancoimputado$difimc10ximc1))

############

bancoimputado$difimc1ximcatual = bancoimputado$imc1ano-bancoimputado$imcatual
qqnorm(bancoimputado$difimc1ximcatual)
qqline(bancoimputado$difimc1ximcatual)
hist(bancoimputado$difimc1ximcatual)
plot(density(bancoimputado$difimc1ximcatual))


dev.off()


#------------------------
bancoimputado$limimca = bancoimputado$imcatual*0.05
bancoimputado$limimc1 = bancoimputado$imc1ano*0.05
bancoimputado$limimc10 = bancoimputado$imc10ano*0.05
bancoimputado$limimc25 = bancoimputado$imc25ano*0.05

#----

bancoimputado$cdif1ano<- ifelse (bancoimputado$difimc1 <= -bancoimputado$limimc1, "perdeu", ifelse(bancoimputado$difimc1 >= bancoimputado$limimc1, "ganhou", "manteve"))
table(bancoimputado$cdif1ano)


bancoimputado$cdif10ano<- ifelse (bancoimputado$difimc10 <= -bancoimputado$limimc10, "perdeu", ifelse(bancoimputado$difimc10 >= bancoimputado$limimc10, "ganhou", "manteve"))
table(bancoimputado$cdif10ano)


bancoimputado$cdif25ano<- ifelse (bancoimputado$difimc25 <= -bancoimputado$limimc25, "perdeu", ifelse(bancoimputado$difimc25 >= bancoimputado$limimc25, "ganhou", "manteve"))
table(bancoimputado$cdif25ano)


################## UNIÃO FINAL DOS BANCOS #####################################
nhanes0718 <- merge(bancoimputado, pesos0718, by = "seqn")
nhanes0718 <- rename(nhanes0718, SEQN = seqn)
banco_nhanes1 <- merge(nhanes0718, cargadoencas, by="SEQN")
banco_nhanes2 <- merge(banco_nhanes1, ativ0720, by="SEQN")
banco_nhanes3 <- merge(banco_nhanes2, consalcool, by="SEQN")
banco_nhanes4 <- merge(banco_nhanes3, tabagismotot, by="SEQN")

table(banco_nhanes4$ciclo)

n_occur <- data.frame(table(banco_nhanes4$SEQN))
table(n_occur$Freq)

banco_analise <- banco_nhanes4


#banco_analise <- write.xlsx(banco_analise, file = "bancoanalise.xlsx", sheetName = "bancoanalise", append = FALSE)

banco_analise <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/bancoanalise.xlsx")

View(banco_analise)
colnames(banco_analise)

banco_analise_tot <- banco_analise
summary(banco_analise)





################################################################################


class(banco_analise)
str(banco_analise)
#CRIANDO  VARIÁVEL DE DCNT

#EXEMPLO : (f <- f %>% 
#mutate(dcv = ifelse(a =="1" | b =="1" | c =="1" | d =="1" | e =="1", "1", "0")))


#total : 12.381
#total somente da variável: 8292

(banco_analise <- banco_analise %>% 
    mutate(DCNT_carga_teste = ifelse(DM =="1" | 
                           HAS =="1" | 
                           insuf_cardiacacong  =="1" | 
                           doenca_coronaria  =="1" | 
                           ataque_cardiaco  =="1" |
                           derrame  =="1" |
                           tipo1_cancer  =="1" |
                           tipo2_cancer  =="1" |
                           angina_prectoris =="1", "1", "0")))

table(banco_analise$DCNT_carga_teste)

#testando outra forma:

banco_analise $DCNTteste1 <- rowSums(banco_analise[, c("DM","HAS","insuf_cardiacacong",
                                                  "angina_prectoris","ataque_cardiaco", 
                                                  "derrame",
                                                  "doenca_coronaria","tipo1_cancer", 
                                                  "tipo2_cancer" )], na.rm = TRUE)

table(banco_analise$DCNTteste1)
#8292


#vendo por loop

value <- 0
list <- 0
SEQN <- 0
for (row in 1:nrow(banco_analise)) {
  doenca_coronaria <- banco_analise[row, "doenca_coronaria"]
  insuf_cardiacacong <- banco_analise[row, "insuf_cardiacacong"]
  angina_prectoris <- banco_analise[row, "angina_prectoris"]
  ataque_cardiaco <- banco_analise[row, "ataque_cardiaco"]
  derrame <- banco_analise[row, "derrame"]
  DM <- banco_analise[row, "DM"]
  HAS <- banco_analise[row, "HAS"]
  tipo1_cancer <- banco_analise[row, "tipo1_cancer"]
  tipo2_cancer <- banco_analise[row, "tipo2_cancer"]
  
  if (!is.na(doenca_coronaria) & doenca_coronaria == "1") {
    value <- value + 1
  }
  if (!is.na(insuf_cardiacacong) & insuf_cardiacacong == "1") {
    value <- value + 1
  }
  
  if (!is.na( angina_prectoris) &  angina_prectoris  == "1") {
    value <- value + 1
  }
  if (!is.na(ataque_cardiaco) &  ataque_cardiaco  == "1") {
    value <- value + 1
  }
  if (!is.na(derrame) &  derrame  == "1") {
    value <- value + 1
  }
  if (!is.na(DM) &  DM  == "1") {
    value <- value + 1
  }
  if (!is.na(HAS) &  HAS  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo1_cancer) &  tipo1_cancer  == "1") {
    value <- value + 1
  }
  if (!is.na(tipo2_cancer) &  tipo2_cancer  == "1") {
    value <- value + 1
  }
  list <- c(list,value)
  SEQN <- c(SEQN, banco_analise$SEQN)
  value <- 0
} 

print(list)

banco_analise$DCNT_CARGA <- ifelse(list >= 1, 1, 0) 

###############################################################################
####              Deixando somente as variáveis de interesse no banco

###############################################################################
colnames(banco_analise_tot)

banco_analise <- subset(banco_analise_tot, select = 
                          c('SEQN', 'ciclo', 'sexo', 
                            'etnia','idade','estcivil','gravidez'
                            ,'escolaridade',                              
                            'imcatual', 'imc1ano', 
                            'imc25ano','imc10ano','cimcatual', 
                            'cimc1ano', 'cimcmaior',
                            'cimc10ano', 'cimc25ano', 
                            'sddsrvyr', 'ridstatr', 
                            'wtint2yr', 'wtmec2yr', 'sdmvpsu', 'sdmvstra', 
                            'idade_DCNT', 'DCNT_carga', 'consumoalcool', 
                            'fumou100_cigarros', 'fumante_atual', 
                             'consumomedio', 'indpobreza','catv1_teste', 
                            'catv2_teste'))

colnames(banco_analise)


#avaliando os percentuais de imputação

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(banco_analise, function(x) sum(is.na(x)))
sapply(banco_analise, function(x) sum(is.nan(x)))


#padrão de valores missings
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(banco_analise,2,pMiss)
apply(banco_analise,1,pMiss)
pMiss(banco_analise)

md.pattern(banco_analise) #padrão de dados ausentes
p <- md.pairs(banco_analise)
p

mice_plot <- aggr(banco_analise, col=c('purple','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(banco_analise), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


gg_miss_var(banco_analise, show_pct = TRUE) #mostrando % de missings por variável


n_var_miss(banco_analise) #número de variáveis contendo missings


colnames(banco_analise)
summary(banco_analise$idade_DCNT)



##############################################################################  #                        
#                           Imputação do banco ##############################################################################

banco_analise$idade_DCNT_2 <- ifelse(banco_analise$DCNT_carga == 1, banco_analise$idade_DCNT, banco_analise$idade)


table(banco_analise$fumante_atual)
table(banco_analise$consumoalcool)
table(banco_analise$consumomedio)
table(banco_analise$escolaridade)
table(banco_analise$estcivil)
table(banco_analise$fumou100_cigarros)
table(banco_analise$idade_DCNT)
str(banco_analise$idade_DCNT_2)
mean(banco_analise$idade_DCNT_2)
hist(banco_analise$idade_DCNT_2)
hist(banco_analise$idade_DCNT)


banco_analise$fumante_atual <- as.factor(banco_analise$fumante_atual)
banco_analise$consumoalcool <- as.factor(banco_analise$consumoalcool)
banco_analise$consumomedio <- as.numeric(banco_analise$consumomedio)
banco_analise$escolaridade <- as.factor(banco_analise$escolaridade)
banco_analise$estcivil <- as.factor(banco_analise$estcivil)
banco_analise$fumou100_cigarros <- as.factor(banco_analise$fumou100_cigarros)
banco_analise$idade_DCNT <- as.numeric(banco_analise$idade_DCNT)
banco_analise$idade_DCNT_2 <- as.numeric(banco_analise$idade_DCNT_2)

imputbase <- mice(banco_analise, m = 5, maxit = 10, method = 'pmm', seed = 
                    12345)

#verificando as variáveis que foram imputadas
head(imputbase)

head(imputbase$imp$idade_DCNT)
head(imputbase$imp$idade_DCNT_2)
imputbase$imp$fumou100_cigarros
imputbase$imp$estcivil
imputbase$imp$escolaridade
imputbase$imp$consumomedio
imputbase$imp$consumoalcool
imputbase$imp$fumante_atual

print(imputbase)
plot(imputbase)
summary(imputbase)


fitm <- with(imputbase, lm(idade_DCNT_2 ~ consumoalcool + fumante_atual+estcivil+consumomedio+escolaridade+ estcivil+fumou100_cigarros))

summary(fitm)

est <- pool(fitm)
summary(est)


summary(pool(fitm))
coef(fitm$analyses[[1]])
coef(fitm$analyses[[2]])
coef(fitm$analyses[[3]])
coef(fitm$analyses[[4]])
coef(fitm$analyses[[5]])

imp_tot2 <- complete(imputbase, "long", inc = TRUE)


banco_analise_ofc <- (complete(imputbase, 4))

sapply(banco_analise_ofc, function(x) sum(is.na(x)))
sapply(banco_analise_ofc, function(x) sum(is.nan(x)))

#banco_analise_ofc <- write.xlsx(banco_analise_ofc, file = "banco_analise_ofc.xlsx", sheetName = "banco_analise_ofc", append = FALSE)

banco_analise_ofc <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/banco_analise_ofc.xlsx")


freq(banco_analise$fumante_atual)
freq(base_imputada$fumante_atual)

freq(banco_analise$fumou100_cigarros)
freq(base_imputada$fumou100_cigarros)

freq(banco_analise$estcivil)
freq(base_imputada$estcivil)


freq(banco_analise$escolaridade)
freq(base_imputada$escolaridade)


freq(banco_analise$consumoalcool)
freq(base_imputada$consumoalcool)


freq(banco_analise$consumoalcool)
freq(base_imputada$consumoalcool)

hist(banco_analise$idade_DCNT_2)
hist(base_imputada$idade_DCNT_2)


hist(banco_analise$consumomedio)
hist(base_imputada$consumomedio)


banco_analise = base_imputada
banco_analise

sapply(banco_analise, function(x) sum(is.na(x)))
sapply(banco_analise, function(x) sum(is.nan(x)))


###############################################################################                            Análises preliminares
###############################################################################


########################## Verificação de ausentes #############################

#verificando valores ausentes no banco
sapply(banco_analise_cox, function(x) sum(is.na(x)))
sapply(banco_analise_cox, function(x) sum(is.nan(x)))

sapply(banco_analise_ofc, function(x) sum(is.na(x)))
sapply(banco_analise_ofc, function(x) sum(is.nan(x)))

################################################################################
#             
#             RECONFIGURANDO A IDADE DA DOENÇA PARA O MODELO DE COX
#
################################################################################

banco_analise_ofc$idade_DCNT_2 <- ifelse(banco_analise_ofc$DCNT_carga == 1, banco_analise_ofc$idade_DCNT, banco_analise_ofc$idade)

banco_analise_cox <- banco_analise_ofc


#banco_analise_cox <- write.xlsx(banco_analise_cox, file = "banco_analise_cox.xlsx", sheetName = "banco_analise_cox", append = FALSE)

banco_analise_cox <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/banco_analise_cox.xlsx")

str(banco_analise_cox)

# tratamento das variáveis de IMC

banco_analise_cox$cimc25 <- factor(banco_analise_cox$cimc25ano,
                               levels = c("baixo peso", "peso normal",
                                          "sobrepeso", "obesidade"),
                               labels = c("0", "0", "1", "2"))
banco_analise_cox$DCNT_carga <- as.numeric(banco_analise_cox$DCNT_carga)


table(banco_analise_cox$cimc25)
table(banco_analise_cox$DCNT_carga)
str(banco_analise_cox$DCNT_carga)
summary(banco_analise_cox$idade_DCNT_2)
str(banco_analise_cox$cimc25)

############################################################################
#                             Modelo de cox
############################################################################
table(banco_analise_cox$ciclo)
#ponderação

banco_analise_cox$mecteste <- ifelse(banco_analise_cox$ciclo <= 10, banco_analise_cox$wtmec2yr*0.2, 0)

sum(banco_analise_cox$mecteste)
str(banco_analise_cox$mecteste)
sum(banco_analise_cox$mec)

#ciclos de 6 anos

banco_analise_cox$mec <- banco_analise_cox$wtmec2yr/6
sum(banco_analise_cox$mec)

banco_analise_ponderado_cox <- svydesign(data=banco_analise_cox, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)

str(banco_analise_cox)

#testando o banco todo

#modulo survey
#Estimates the survival function using a weighted Kaplan-Meier estimator.
modelo.cox25.survey <- svykm(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, data=banco_analise_cox, design = banco_analise_ponderado_cox)

summary(modelo.cox25.survey)
modelo.cox25.survey

#gráfico 1
plot(modelo.cox25.survey,xlab="time",ylab="Proportion surviving",
     ylim=c(0,1),ci=TRUE, lwd=3,
     pars=list(lty=c(1,2,3),col=c("grey","darkgrey", "black")))

#gráfico 2

plot(modelo.cox25.survey, cumhaz=TRUE, xlab="Age (years)", ylab="Cumulative hazard")

#teste de diferença das curvas
survdiff(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, data = banco_analise_cox)

############# sem modulo survey

modelo.cox25.full <- survfit(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, data=banco_analise_cox)
summary(modelo.cox25.full)
surv_summary(modelo.cox25.full)
#    Gráficos

autoplot(modelo.cox25.full,conf.int = F)+
  labs(x="Anos",y="Probabilidade de Sobrevivência",
       title="Kaplan-Meier DCNT_CARGA",colour="IMC_25 ANOS")+theme_bw()
summary(modelo.cox25.full)

#gráfico 2
ggsurvplot(modelo.cox25.full, data=banco_analise_cox,
           title = "",
           xlab = "Age (Years)", 
           ylab = "Cumulative survival function",
           xlim = c(0, 60),
           censor.size = 2.5,
           linetype = 1,
           font.main = c(12, "bold", "black"),
           font.x = c(10, "bold.italic", "black"),
           font.y = c(10, "bold.italic", "black"),
           font.tickslab = c(10, "plain", "black"),
           surv.median.line = "hv",
           risk.table = FALSE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           legend.title = "BMI at 25 years",
           legend.labs = c("Normal wieght", "Overweight", "Obesity"),
           surv.scale = ("default"),
           pval = T,
           pval.method = T,
           conf.int = F,
           palette = c("gray", "gray62", "black"),
           ggtheme = theme_survminer()) # Change ggplot2 theme

#gráfico 3
ggsurvplot(modelo.cox25.full,
           conf.int = F,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_classic(), # Change ggplot2 theme
           palette = c("gray", "gray62", "black"),
           fun = "event")

################################################################################


surv_summary(modelo.cox25.survey, banco_analise_cox) # tempo de seguimento, número de indivídos sob risco de obito, número de eventos, número de censuras, função de sobrevida, erro padrão e IC 

surv_summary(modelo.cox25.full, banco_analise_cox) # tempo de seguimento, número de indivídos sob risco de obito, número de eventos, número de censuras, função de sobrevida, erro padrão e IC 



# TESTE DE LOG-RANK (DISTRIBUIÇÃO QUI-QUADRADO)
# H0: As curvas são iguais 
# HA: As curvas são diferentes
# Observar o p-valor (p<0,05 é signficativo)

print(survdiff(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, data = banco_analise_cox), 10) # diferença entre as curvas

# CONCLUSÃO: Como p-valor=< 2.22045e-16 , p-valor é menor que 0,05, portanto rejeito H0 e concluo que as curvas não são iguais.

# AVALIANDO AS MÉTRICAS DO MODELO

modelo.cox25_full_2 <- svycoxph(Surv(idade_DCNT_2, DCNT_carga) ~ 
                                factor(cimc25), 
                                method='breslow', 
                                data = banco_analise_cox, 
                                design = banco_analise_ponderado_cox)
testeph <- cox.zph(modelo.cox25_full_2)
ggcoxzph(testeph)


s<- predict(modelo.cox25_full_2, banco_analise_cox, se=FALSE,
        type=c("lp", "risk", "expected", "terms","curve"))



tbl_regression(modelo.cox25_full_2, exponentiate = TRUE)

summary(modelo.cox25_full_2)

  

plot_summs(modelo.cox25_full_2, scale = TRUE, exp = TRUE)

# teste de logrank, podemos testar se houve diferença no tempo de sobrevivência de acordo com o imc
svylogrank(Surv(idade_DCNT_2, DCNT_carga)~factor(cimc25),design=banco_analise_ponderado_cox)

#p<0.0001, ouve diferença significativa na sobrevida global de acordo com o imc


#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox25_full_2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox25_full_2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#testando não linearidade
#log não é permitido para fatores

ggcoxfunctional(Surv(idade_DCNT_2, DCNT_carga) ~ factor(cimc25) 
                + log(cimc25) + sqrt(cimc25), data = banco_analise_cox,                      design = banco_analise_ponderado_cox)


s <- predict(modelo.cox25_full_2, banco_analise_cox, se=FALSE,
        type=c("lp", "risk", "expected", "terms","curve"))
s

# GRÁFICO ln(-ln(S(t))) = ln(H(t))
# ESTE GRÁFICO MOSTRA A PROPORCIONALIDADE DOS RISCOS AO LONGO DO TEMPO. QUANDO AS CURVAS SE CRUZAM, MOSTRA QUE OS RISCOS NÃO SÃO PROPORCIONAIS AO LONGO DO TEMPO.
# SE OS RISCOS NÃO SÃO PROPORCIONAIS, NÃO SE PODE USAR O MODELO DE RISCOS PROPORCIONAIS DE COX.
st <- surv_summary(modelo.cox25.full, banco_analise_cox)$surv
t <- surv_summary(modelo.cox25.full, banco_analise_cox)$time
BMI <- surv_summary(modelo.cox25.full, banco_analise_cox)$cimc25
sdata <- data.frame(st, t, BMI)
sdata

ggplot(sdata,aes(y=log(-log(st)),x=t, colour=BMI))+
  geom_line()+
  xlab("Tempo (anos)")


# GRÁFICO log(-log(S(t)))
# Esse gráfico deve ser observado quando o KM + log-rank apontarem que existe diferença entre as curvas.

km_loglog <- survfit(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, data = banco_analise_cox)
ggsurvplot(km_loglog)


km_loglog <- svylogrank(Surv(idade_DCNT_2, DCNT_carga) ~ cimc25, design = banco_analise_ponderado_cox)
summary(km_loglog)

plot(km_loglog[3:1],lty=3:1, lwd=2, fun='cloglog',
     xlab="Age (years old)", ylab="Survival",
     main = "Kaplan-Meier curve")




# GRÁFICO DOS RESÍDUOS DE SCHOENFELD
# Teste de hipóteses:
# H0: Os riscos não mudam no tempo, ou seja, os riscos são proporcionais
# HA: Os riscos mudam no tempo, ou seja, os riscos não são proporcionais
# Observar o valor p. Esse valor de p se refere ao gráfico log(-log(S(t))). 
# Observar se os pontos estão dentro das linhas pontilhadas e próximos da linha central.
ggcoxzph(cox.zph(modelo.cox25_full_2))
ggcoxzph(cox.zph(modelo.cox25.survey))

cox.zph(modelo.cox25_full_2);teste.ph
ggcoxzph(cox.zph(modelo.cox25.survey))



#Segunda forma de gráfico:
# Create separate data frames for "Normal" and "Low BP"
data.normal <- subset(banco_analise_cox, cimc25==0)
data.ow <- subset(banco_analise_cox, cimc25==1)
data.ob <- subset(banco_analise_cox, cimc25==2)

# Get Kaplan Meier Curves
normal.KM <- survfit(Surv(idade_DCNT_2,DCNT_carga)~1,data=data.normal)
ow.KM <- survfit(Surv(idade_DCNT_2,DCNT_carga)~1,data=data.ow)
ob.km <- survfit(Surv(idade_DCNT_2,DCNT_carga)~1,data=data.ob)

# For plotting, get Y = ln [ -ln (S[t])]
normalY <- log(-log(summary(normal.KM)$surv))
lowY <- log(-log(summary(ow.KM)$surv))
lobY <- log(-log(summary(ob.km)$surv))
# For plotting, get X = ln [ t ]
normalX <- log(summary(normal.KM)$time)
lowX <- log(summary(low.KM)$time)
lobX <- log(summary(ob.km)$time)

# Put together the Y and X for plotting into dataframes
normaldf <- data.frame(normalY, normalX)
lowdf1 <- data.frame(lowY, lowX)
lowdf2 <- data.frame(lobY, lobX)
# Plot (admittedly the code is complicated)
plot(lowdf$lowX, y=lowdf$lowY,lwd=2,col="blue",xlab="Time",
     ylab="ln(-ln(S(t)))",
     main="Check of PH Assumption")
points(normaldf$normalX, y=normaldf$normalY, lwd=2,col="red")
lines(normaldf$normalX, y=normaldf$normalY, lwd=2,col="red")
lines(lowdf$lowX, y=lowdf$lowY, lwd=2,col="blue")
legend("topright", legend=c("Normal", "Low"),
       col=c("red", "blue"), lty=1:2, cex=0.8, box.lty=0)
#_______________________________________________________________________________
# CONCLUSÃO:
# Não REJEITO H0, OS RISCOS SÃO PROPORCIONAIS
# A VARIÁVEL SERÁ SELECIONADA PARA O MODELO MÚLTIPLO
#_______________________________________________________________________________

########### Modelo 1
modelo.cox25_1 <- svycoxph(Surv(idade_DCNT_2, DCNT_carga)
                           ~factor(cimc25),
                           method='breslow',
                           data = banco_analise_cox,
                           design = banco_analise_ponderado_cox)

tbl_regression(modelo.cox25_1, exponentiate = TRUE)

#teste log rank
svylogrank(Surv(idade_DCNT_2, DCNT_carga)~factor(cimc25),
                            design = banco_analise_ponderado_cox)


#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox25_1))


########## Modelo 2
modelo.cox25_2 <- svycoxph(Surv(idade_DCNT_2, DCNT_carga)
                               ~factor(cimc25)+
                                 factor(sexoCat)+
                                 factor(etniaCat),
                               method='breslow',
                               data = banco_analise_cox,
                               design = banco_analise_ponderado_cox)

tbl_regression(modelo.cox25_2, exponentiate = TRUE)


svylogrank(Surv(idade_DCNT_2, DCNT_carga)~factor(cimc25)+
             factor(sexoCat)+
             factor(etniaCat),
           design = banco_analise_ponderado_cox)

ggcoxzph(cox.zph(modelo.cox25_2))


##### Modelo 3

modelo.cox25_3 <- svycoxph(Surv(idade_DCNT_2, DCNT_carga) ~ factor(cimc25)+
                             factor(sexoCat)+
                             factor(etniaCat)+
                             factor(estcivilCat)+
                             factor(escolaridadeCat)+
                             factor(indpobrezaCat),
                           method='breslow',
                           data = banco_analise_cox,
                           design = banco_analise_ponderado_cox)

tbl_regression(modelo.cox25_3, exponentiate = TRUE)

svylogrank(Surv(idade_DCNT_2, DCNT_carga)~factor(cimc25)+
             factor(sexoCat)+
             factor(etniaCat)+
             factor(estcivilCat)+
             factor(escolaridadeCat)+
             factor(indpobrezaCat),
           design = banco_analise_ponderado_cox)

ggcoxzph(cox.zph(modelo.cox25_3))


####### Modelo 4 
modelo.cox25_4 <- svycoxph(Surv(idade_DCNT_2, DCNT_carga) ~ factor(cimc25)+
                             factor(sexoCat)+
                             factor(etniaCat)+
                             factor(estcivilCat)+
                             factor(escolaridadeCat)+
                             factor(indpobrezaCat)+
                             factor(catv2_teste)+
                             factor(fumante_atual)+
                             factor(consumoalcool),
                           method='breslow',
                           data = banco_analise_cox,
                           design = banco_analise_ponderado_cox)

tbl_regression(modelo.cox25_4, exponentiate = TRUE)

svylogrank(Surv(idade_DCNT_2, DCNT_carga)~factor(cimc25)+
             factor(sexoCat)+
             factor(etniaCat)+
             factor(estcivilCat)+
             factor(escolaridadeCat)+
             factor(indpobrezaCat)+
             factor(catv2_teste)+
             factor(fumante_atual)+
             factor(consumoalcool),
           design = banco_analise_ponderado_cox)

ggcoxzph(cox.zph(modelo.cox25_4))

#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox25_multi, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox25_multi, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox25_multi))

#-------------------------------------------------------------------------------#    Refazendo os modelos para gerar os resíduos de Schoenfeld sem módulo
#     survey
#-------------------------------------------------------------------------------

#sem modulo survey

modelo.cox.teste <- coxph(Surv(idade_DCNT_2, DCNT_carga) ~ 
                            factor(cimc25), 
                          method='breslow', 
                          data = banco_analise_cox)
summary(modelo.cox.teste)
tbl_regression(modelo.cox.teste, exponentiate = TRUE)

#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox.teste, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox.teste, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox.teste))


# Modelo 2 

modelo.cox.teste2 <- coxph(Surv(idade_DCNT_2, DCNT_carga) ~ 
                            factor(cimc25)+
                            factor(sexoCat)+
                            factor(etniaCat), 
                          method='breslow', 
                          data = banco_analise_cox)
summary(modelo.cox.teste2)

tbl_regression(modelo.cox.teste2, exponentiate = TRUE)

#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox.teste2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox.teste2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox.teste2))



# Modelo 3


modelo.cox.teste3 <- coxph(Surv(idade_DCNT_2, DCNT_carga) ~ 
                             factor(cimc25)+
                             factor(sexoCat)+
                             factor(etniaCat)+
                             factor(estcivilCat)+
                             factor(escolaridadeCat)+
                             factor(indpobrezaCat), 
                           method='breslow', 
                           data = banco_analise_cox)
summary(modelo.cox.teste3)

tbl_regression(modelo.cox.teste3, exponentiate = TRUE)

#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox.teste3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox.teste3, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox.teste3))


# Modelo 4


modelo.cox.teste4 <- coxph(Surv(idade_DCNT_2, DCNT_carga) ~ 
                             factor(cimc25)+
                             factor(sexoCat)+
                             factor(etniaCat)+
                             factor(estcivilCat)+
                             factor(escolaridadeCat)+
                             factor(indpobrezaCat)+
                             factor(catv2_teste)+
                             factor(fumante_atual)+
                             factor(consumoalcool), 
                           method='breslow', 
                           data = banco_analise_cox)
summary(modelo.cox.teste4)

tbl_regression(modelo.cox.teste4, exponentiate = TRUE)

#6.2.3 Testing Influential Observations
ggcoxdiagnostics(modelo.cox.teste4, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#deviance
ggcoxdiagnostics(modelo.cox.teste4, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#residuos de shoenfield
ggcoxzph(cox.zph(modelo.cox.teste4))


anova(modelo.cox.teste, modelo.cox.teste2, modelo.cox.teste3, modelo.cox.teste4, test = "Chisq")

test.ph <- cox.zph(modelo.cox.teste);test.ph
test.ph1 <- cox.zph(modelo.cox.teste2);test.ph1
test.ph2 <- cox.zph(modelo.cox.teste3);test.ph2
test.ph3 <- cox.zph(modelo.cox.teste4);test.ph3


#----------------------  FIM DAS ANÁLISES DE COX -----------------------------#


#################################################################################  
#                
#                        REGRESSÃO LOGÍSTICA - POISSON
# 
################################################################################
head(banco_analise)

# criando novo banco excluindo indivíduos com a doença

banco_analise_semdoenca <- subset(banco_analise_ofc, DCNT_carga == 0)
sapply(banco_analise_semdoenca, function(x) sum(is.na(x)))
sapply(banco_analise_semdoenca, function(x) sum(is.nan(x)))

#banco_analise_semdoenca <- write.xlsx(banco_analise_semdoenca, file = "banco_analise_semdoenca.xlsx", sheetName = "banco_analise_semdoenca", append = FALSE)

banco_analise_semdoenca <- read_excel("C:/Users/Débora/Google Drive/TESE/Projeto TESE/BANCOS NHANES/DEMOGRAFICOS/banco_analise_semdoenca.xlsx")



# CATEGORIZANDO AS VARIÁVEIS QUE ENTRARÃO NO MODELO DE POISSON

# sexo
# 1 = homem, 2 = mulher
table(banco_analise_semdoenca$sexo)
str(banco_analise_semdoenca$sexo)
banco_analise_semdoenca$sexo <- as.factor(banco_analise_semdoenca$sexo)
freq(banco_analise_semdoenca$sexo)

banco_analise_semdoenca$sexoCat <- factor(banco_analise_semdoenca$sexo,
                                levels = c("2", "1"),
                                labels = c("Female", "Male"))
table(banco_analise_semdoenca$sexoCat)


# idade categórica

banco_analise_semdoenca$idadeCat = cut(banco_analise_semdoenca$idade,
                             breaks = c(0, 39, 59, Inf),
                             labels = c('28-39','40-59','60+')) 

table(banco_analise_semdoenca$idadeCat)
str(banco_analise_semdoenca$idadeCat)


# etnia

table(banco_analise_semdoenca$etnia)

banco_analise_semdoenca$etniaCat <- factor(banco_analise_semdoenca$etnia,
                                 levels = c("1", "2", "3", "4", "5"),
                                 labels = c("Mexican American",
                                            "Other Hispanic",
                                            "Non-Hispanic White",
                                            "Non-Hispanic Black",
                                            "Other Race - Including Multi-Racial"))

banco_analise_semdoenca$etniaCat <- relevel(banco_analise_semdoenca$etniaCat, ref = "Non-Hispanic White")

table(banco_analise_semdoenca$etniaCat)
freq(banco_analise_semdoenca$etniaCat)


# índice de pobreza

table(banco_analise_semdoenca$indpobreza)

banco_analise_semdoenca$indpobrezaCat = cut(banco_analise_semdoenca$indpobreza,
                                  breaks = c(-Inf, 1.25, 3.50, Inf),
                                  labels = c('<1.25','1.25-3.49','>=3.50')) 


levels(banco_analise_semdoenca$indpobrezaCat)
banco_analise_semdoenca$indpobrezaCat <- relevel(banco_analise_semdoenca$indpobrezaCat, ref = ">=3.50")

table(banco_analise_semdoenca$indpobrezaCat)

# escolaridade contínua

banco_analise_semdoenca$escolaridadeCat2[banco_analise_semdoenca$escolaridade  == 1] <- 7
banco_analise_semdoenca$escolaridadeCat2[banco_analise_semdoenca$escolaridade  == 2] <- 10
banco_analise_semdoenca$escolaridadeCat2[banco_analise_semdoenca$escolaridade  == 3] <- 11
banco_analise_semdoenca$escolaridadeCat2[banco_analise_semdoenca$escolaridade  == 4] <- 12
banco_analise_semdoenca$escolaridadeCat2[banco_analise_semdoenca$escolaridade  == 5] <- 14


table(banco_analise_semdoenca$escolaridadeCat2)
str(banco_analise_semdoenca$escolaridadeCat2)

# escolaridade categórica

table(banco_analise_semdoenca$escolaridade)

banco_analise_semdoenca$escolaridadeCat <- factor(banco_analise_semdoenca$escolaridade,
                                        levels = c("1", "2", "3", "4", "5"),
                                        labels = c("Incomplete High School/Elementary School",
                                                   "Incomplete High School/Elementary School",
                                                   "Complete High School",
                                                   "Complete Undergraduate or Graduate Education",
                                                   "Complete Undergraduate or Graduate Education"))


table(banco_analise_semdoenca$escolaridadeCat)

banco_analise_semdoenca$escolaridadeCat <- relevel(banco_analise_semdoenca$escolaridadeCat, ref = "Complete High School")

# atividade física

table(banco_analise_semdoenca$catv1_teste)

#0=sedentário, 1=insuficiente e 2=ativo

banco_analise_semdoenca$atv1Cat <- factor(banco_analise_semdoenca$catv1_teste,
                                levels = c("0", "1", "2"),
                                labels = c("Inactive",
                                           "Insufficient assets",
                                           "Physically active"))
table(banco_analise_semdoenca$atv1Cat)

banco_analise_semdoenca$atv1Cat <- relevel(banco_analise_semdoenca$atv1Cat, ref = "Physically active")

#segunda classificação
table(banco_analise_semdoenca$catv2_teste)

#2=sedentário e insuf 1=ativo

# estado civil

table(banco_analise_semdoenca$estcivil)

banco_analise_semdoenca$estcivil[banco_analise_semdoenca$estcivil >= 7] <- NA


banco_analise_semdoenca$estcivilCat <- factor(banco_analise_semdoenca$estcivil,
                                    levels = c("1", "2", "3","4","5","6"),
                                    labels = c("Married/Living with partner	",
                                               "Widowed/Divorced/Separated",
                                               "Widowed/Divorced/Separated",
                                               "Widowed/Divorced/Separated",
                                               "Never married",
                                               "Married/Living with partner	"))

table(banco_analise_semdoenca$estcivilCat)
freq(banco_analise_semdoenca$estcivilCat)


#Consumo de álcool nos últimos 12 meses

# 0 é não e 1 é sim.
table(banco_analise_semdoenca$consumoalcool)
str(banco_analise_semdoenca$consumoalcool)
banco_analise_semdoenca$consumoalcool <- as.factor(banco_analise_semdoenca$consumoalcool)

#tabagismo atualmente

table(banco_analise_semdoenca$fumante_atual)
#1 fuma todos os dias, 2 alguns dias, 3 de jeito nenhum

banco_analise_semdoenca$fuma_atualCat <- factor(banco_analise_semdoenca$fumante_atual,
                                      levels = c("1", "2", "3"),
                                      labels = c("Every day",
                                                 "Some days",
                                                 "Not at all"))
table(banco_analise_semdoenca$fuma_atualCat)
banco_analise_semdoenca$fuma_atualCat <- relevel(banco_analise_semdoenca$fuma_atualCat, ref = "Not at all")

# fumou 100 cigarros na vida : 0 não, 1 sim
table(banco_analise_semdoenca$fumou100_cigarros)

banco_analise_semdoenca$fumou100_cigarrosCat <- factor(banco_analise_semdoenca$fumou100_cigarros,
                                             levels = c("1", "2"),
                                             labels = c("sim","não"))

table(banco_analise_semdoenca$fumou100_cigarrosCat)

levels(banco_analise_semdoenca$fumou100_cigarrosCat)

banco_analise_semdoenca$fumou100_cigarrosCat <- relevel(banco_analise_semdoenca$fumou100_cigarrosCat, ref = "não")



# ----------------------- Organizando a variável de IMC atual --------------------- #

table(banco_analise_semdoenca$cimcatual)
banco_analise_semdoenca$cimcatual.poisson1 <- factor(banco_analise_semdoenca$cimcatual,
                                      levels = c("baixo peso", "peso normal",
                                                 "sobrepeso", "obesidade"),
                                      labels = c("0", "0", "1", "1"))

table(banco_analise_semdoenca$cimcatual.poisson1)

#convertendo para numérico para entrar no modelo de poisson

banco_analise_semdoenca$cimcatual.poisson1 <- as.numeric(banco_analise_semdoenca$cimcatual.poisson1)

#quando converte numérico, o 0 vira 1
table(banco_analise_semdoenca$cimcatual.poisson1)
str(banco_analise_semdoenca$cimcatual.poisson1)

banco_analise_semdoenca$cimcatual.poisson1 <- banco_analise_semdoenca$cimcatual.poisson1-1


#  verificando se a conversão ficou correta

table(banco_analise_semdoenca$cimcatual.poisson1)

# variável independente precisa ser quantitativa
# variável dependente, qualitativa

# Categorizando a variável de imc aos 25 anos

table(banco_analise_semdoenca$cimc25ano)
banco_analise_semdoenca$cimc25.poisson1 <- factor(banco_analise_semdoenca$cimc25ano,
                                           levels = c("baixo peso", "peso normal",
                                                      "sobrepeso", "obesidade"),
                                           labels = c("0", "0", "1", "2"))

table(banco_analise_semdoenca$cimc25.poisson1)

#testando modelo de regressão de poisson com o módulo survey
#peso amostral
banco_analise_semdoenca$mec <- banco_analise_semdoenca$wtmec2yr/5

banco_analise_semdoenca_ponderado <- svydesign(data=banco_analise_semdoenca, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)

m1 = svyglm(cimcatual.poisson1~cimc25.poisson1+
              sexoCat+
              consumoalcool+
              fuma_atualCat+
              catv2_teste+
              idadeCat+
              indpobrezaCat+
              escolaridadeCat+
              estcivilCat+
              etniaCat,
            family = poisson(link = 'log'), 
            data = banco_analise_semdoenca,
            design = banco_analise_semdoenca_ponderado)
summary(m1)

tbl_regression(m1, exponentiate = TRUE)
predict(m1, type = "response")
exp(coef(m1))

#Teste de dispersão do modelo:
#dispersiontest(regpoisson.imcatual, alternative = c("two.sided"))
dispersiontest(m1)

#        ANÁLISE DE VARIÂNCIA
#
analise_variancia_modelo1 <- anova(m1, test="Chisq")
analise_variancia_modelo1

#valores preditos do modelo
print=data.frame(banco_analise_semdoenca, pred=(m1$fitted.values))
head(print)

##### sem usar o modo survey 


regpoisson.imcatual =glm(cimcatual.poisson1~cimc25.poisson1+
                           fuma+
                           idadeCat+
                           etniaCat+
                           escolaridadeCat+
                           indpobreza+
                           consumoalcool+
                           sexoCat+
                           catv2_teste,
                       family= poisson(link = "log"), 
                        data=banco_analise_semdoenca)

summary(regpoisson.imcatual)

exp(cbind(regpoisson.imcatual$coefficients, confint(regpoisson.imcatual)))

tab_model(regpoisson.imcatual)

#Teste de dispersão do modelo:
#dispersiontest(regpoisson.imcatual, alternative = c("two.sided"))
dispersiontest(regpoisson.imcatual)

#O parâmetro de super-dispersão para esta equação não é igual a 1. Sempre que for muito superior a 1 há vestígios de super-disposição no modelo.


#        ANÁLISE DE VARIÂNCIA
#
anova(regpoisson.imcatual, test="Chisq")

# Abaixo são incluídos os valores preditos regpoisson$fitted.values juntamente com os dados originais.

print=data.frame(banco_analise_semdoenca, pred=(m1$fitted.values))
head(print)


#################################################################################     
#
#         FAZENDO A MESMA ANÁLISE, ALTERANDO A EXPOSIÇÃO DO IMC ATUAL 
#
################################################################################

banco_analise_semdoenca$cimcatual.poisson2<- factor(banco_analise_semdoenca$cimcatual,
                                          levels = c("baixo peso", "peso normal",
                                                     "sobrepeso", "obesidade"),
                                          labels = c("1", "1", "0", "0"))
table(banco_analise_semdoenca$cimcatual.poisson2)
levels(banco_analise_semdoenca$cimcatual.poisson2)

#convertendo para numérico para entrar no modelo de poisson

banco_analise_semdoenca$cimcatual.poisson2 <- as.numeric(banco_analise_semdoenca$cimcatual.poisson2)

#quando converte numérico, o 0 vira 1
table(banco_analise_semdoenca$cimcatual.poisson2)
str(banco_analise$cimcatual.poisson2)

banco_analise_semdoenca$cimcatual.poisson2[banco_analise_semdoenca$cimcatual.poisson2 == 2] <- 0

banco_analise_semdoenca_ponderado <- svydesign(data=banco_analise_semdoenca, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)

regpoisson.imcatual_inverso = svyglm(cimcatual.poisson2~cimc25.poisson1+
              sexoCat+
              consumoalcool+
              estcivilCat+
              fuma_atualCat+
              catv2_teste+
              idadeCat+
              indpobrezaCat+
              escolaridadeCat+
              etniaCat,
            family = poisson(link = 'log'), 
            data = banco_analise_semdoenca,
            design = banco_analise_semdoenca_ponderado)

summary(regpoisson.imcatual_inverso)

tbl_regression(regpoisson.imcatual_inverso, exponentiate = TRUE)


#  Teste de dispersão do modelo:

dispersiontest(regpoisson.imcatual_inverso)

#O parâmetro de super-dispersão para esta equação não é igual a 1. Sempre que for muito superior a 1 há vestígios de super-disposição no modelo.

#        ANÁLISE DE VARIÂNCIA
#
anova(regpoisson.imcatual_inverso, test="Chisq")

# Abaixo são incluídos os valores preditos regpoisson$fitted.values juntamente com os dados originais.

print2=data.frame(banco_analise, pred=(regpoisson.imcatual_inverso$fitted.values))
head(print2)


################################################################################# 
#
#            FAZENDO A MESMA ANÁLISE, SOMENTE OBESIDADE COMO EXPOSIÇÃO
#
#################################################################################

banco_analise_semdoenca$cimcatual.poisson3<- factor(banco_analise_semdoenca$cimcatual,
                                          levels = c("baixo peso", "peso normal",
                                                     "sobrepeso", "obesidade"),
                                          labels = c("0", "0", "0", "1"))

table(banco_analise_semdoenca$cimcatual.poisson3)
str(banco_analise_semdoenca$cimcatual.poisson3)

# CONVERTENDO A VARIÁVEL DE IMC PARA NUMÉRICA

banco_analise_semdoenca$cimcatual.poisson3 <- as.numeric(banco_analise_semdoenca$cimcatual.poisson3)

table(banco_analise_semdoenca$cimcatual.poisson3)

banco_analise_semdoenca$cimcatual.poisson3 <- banco_analise_semdoenca$cimcatual.poisson3-1

table(banco_analise_semdoenca$cimc25.poisson3)

banco_analise_semdoenca_ponderado <- svydesign(data=banco_analise_semdoenca, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)


# MODELO DE POISSON 

# com o módulo survey 

m2 <- svyglm(cimcatual.poisson3~cimc25.poisson1+
               sexoCat+
               fuma_atualCat+
               consumoalcool+
               estcivilCat+
               catv2_teste+
               idadeCat+
               indpobrezaCat+
               escolaridadeCat+
               etniaCat,
             family = poisson(link = 'log'), 
             data = banco_analise_semdoenca,
             design = banco_analise_semdoenca_ponderado)

summary(m2)

tbl_regression(m2, exponentiate = TRUE)



dispersiontest(m2)

anova(m2, test="Chisq")



#sem módulo survey


regpoisson.imcatual_normal =glm(cimcatual.poisson3~cimc25.poisson1+
                                  fumou100_cigarrosCat+
                                  idadeCat+
                                  etniaCat+
                                  escolaridadeCat+
                                  indpobreza+
                                  consumoalcool+
                                  sexoCat+
                                  catv2_teste,
                                 family= poisson(link = "log"), 
                                 data=banco_analise_semdoenca)

summary(regpoisson.imcatual_normal)
tab_model(regpoisson.imcatual_normal)

# MEDIDA DE ASSOCIAÇÃO + INTERVALOS DE CONFIANÇA

exp(cbind(regpoisson.imcatual_normal$coefficients, confint(regpoisson.imcatual_normal)))

tab_model(regpoisson.imcatual_normal)

#  Teste de dispersão do modelo:

dispersiontest(regpoisson.imcatual_normal)

#O parâmetro de super-dispersão para esta equação não é igual a 1. Sempre que for muito superior a 1 há vestígios de super-disposição no modelo.

anova(m2, test="Chisq")



#        ANÁLISE DE VARIÂNCIA
#
anova(regpoisson.imcatual_normal, test="Chisq")

# Abaixo são incluídos os valores preditos regpoisson$fitted.values juntamente com os dados originais.

print5=data.frame(banco_analise, pred=(regpoisson.imcatual_normal$fitted.values))
head(print5)


################################################################################# 
#
#            FAZENDO A MESMA ANÁLISE, SOMENTE SOBREPESO COMO EXPOSIÇÃO
#
#################################################################################

banco_analise_semdoenca$cimcatual.poisson4<-factor(banco_analise_semdoenca$cimcatual,
                                                    levels = c("baixo peso", "peso normal",
                                                               "sobrepeso", "obesidade"),
                                                    labels = c("0", "0", "1", "0"))

table(banco_analise_semdoenca$cimcatual.poisson4)
str(banco_analise_semdoenca$cimcatual.poisson4)

# CONVERTENDO A VARIÁVEL DE IMC PARA NUMÉRICA

banco_analise_semdoenca$cimcatual.poisson4 <- as.numeric(banco_analise_semdoenca$cimcatual.poisson4)

table(banco_analise_semdoenca$cimcatual.poisson4)

banco_analise_semdoenca$cimcatual.poisson4 <- banco_analise_semdoenca$cimcatual.poisson4-1

table(banco_analise_semdoenca$cimc25.poisson4)

banco_analise_semdoenca_ponderado <- svydesign(data=banco_analise_semdoenca, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)

# MODELO DE POISSON 

# com o módulo survey 

m3 <- svyglm(cimcatual.poisson4~cimc25.poisson1+
               sexoCat+
               fuma_atualCat+
               consumoalcool+
               estcivilCat+
               catv2_teste+
               idadeCat+
               indpobrezaCat+
               escolaridadeCat+
               etniaCat,
             family = poisson(link = 'log'), 
             data = banco_analise_semdoenca,
             design = banco_analise_semdoenca_ponderado)

summary(m3)
coefci(m3)
exp(coef(m3))

tbl_regression(m3, exponentiate = TRUE)
anova(m3, test="Chisq")



logLik(m3)
AIC(m3,k=2, null_has_intercept=TRUE)
BIC(m3,maximal=m3)
AIC(m3)
BIC(m3, modelM = maximal)



anova(m3, test="Chisq")
anova(m1,method="Wald")
anova(m2,method="Wald")
anova(m3,method="Wald")
anova(m1, m3, method="Wald")
anova(m1, m2, method="Wald")

############# gráficos das regressões
plot_summs(m1,
  ci_level = 0.95,
  model.names = NULL,
  coefs = NULL,
  omit.coefs = "(Intercept)",
  inner_ci_level = NULL,
  colors = "CUD Bright",
  plot.distributions = FALSE,
  rescale.distributions = FALSE,
  exp = FALSE,
  point.shape = TRUE,
  point.size = 3,
  legend.title = "Model",
  groups = NULL,
  facet.rows = NULL,
  facet.cols = NULL,
  facet.label.pos = "top",
  resp = NULL,
  dpar = NULL)


plot_summs(m1, scale = TRUE, exp = TRUE, ci_level = 0.95, point.size = 3, omit.coefs = "(Intercept)", inner_ci_level = NULL, colors = "CUD Bright", point.shape = TRUE)

plot_summs(m2, scale = TRUE, exp = TRUE)
plot_summs(m3, scale = TRUE, exp = TRUE)
plot_summs(regpoisson.imcatual_inverso, scale = TRUE, exp = TRUE)

## plot regression coefficients for poisson.model2 and poisson.model
plot_summs(m1, m2, scale = TRUE, exp = TRUE)
require(ResourceSelection)
library(pROC)
roc_fit = roc(m1$y, m1$fitted.values)
points(roc_fit, col="red")
plot(roc_fit, col= "red")
auc(roc_fit)
coords(roc_fit, "best")
hoslem.test(m1$y, m1$fitted.values)



roc1=plot.roc(banco_analise_semdoenca$cimcatual.poisson1,fitted(m1))
plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

roc_fit_2 = roc(m2$y, m2$fitted.values)
points(roc_fit_2, col="red")
plot(roc_fit_2, col= "red")
auc(roc_fit_2)
coords(roc_fit_2, "best")

hoslem.test(m2$y, m2$fitted.values)

roc_fit_3 = roc(regpoisson.imcatual_inverso$y, regpoisson.imcatual_inverso$fitted.values)
points(roc_fit_3, col="red")
plot(roc_fit_3, col= "red")
auc(roc_fit_3)
coords(roc_fit_3, "best")
hoslem.test(regpoisson.imcatual_inverso$y, regpoisson.imcatual_inverso$fitted.values)

plot(roc_fit, roc_fit_2, roc_fit_3, col= c("red", "blue", "black"))

#################################################################################               
#                 Cálculo da fração atribuível e populacional
#
################################################################################

#configurando a variável de exposição!
table(banco_analise_ofc$cimc25ano)

banco_analise_ofc$cimc25.arp<- factor(banco_analise_ofc$cimc25ano,
                                  levels = c("baixo peso", "peso normal",
                                             "sobrepeso", "obesidade"),
                                  labels = c("0", "0", "1", "1"))

# criando tabela 2x2 no pacote paf
dat <- banco_analise %>%
  twoxtwo(., exposure = cimc25.arp, outcome = DCNT_carga)

#ARP 

banco_analise %>%
  arp(exposure = cimc25.arp, outcome = DCNT_carga, percent = TRUE)



str(banco_analise$DCNT_carga)

# populacional

banco_analise %>%
  parp(exposure = cimc25.arp, outcome = DCNT_carga, percent = TRUE)


# número de impacto de exposição

banco_analise %>%
  ein(exposure = cimc25.arp, outcome = DCNT_carga)


#Case Impact Number (CIN)

banco_analise %>%
  cin(exposure = cimc25.arp, outcome = DCNT_carga)



#Exposed Cases Impact Number (ECIN)

banco_analise %>%
  ecin(exposure = cimc25.arp, outcome = DCNT_carga)

# usando o pacote epiR

table(banco_analise_ofc$cimc25.arp, banco_analise_ofc$DCNT_carga)
table(banco_analise_ofc$cimc25.arp)
table(banco_analise_ofc$etnia)
dat = table(banco_analise_ofc$cimc25.arp, banco_analise_ofc$DCNT_carga)
print(dat)


epi1<- epi.2by2(dat, method = "cross.sectional", 
         conf.level = 0.95, units = 100, outcome = "as.columns")
print(epi1)
print(epi1$conf.level)
print(epi1$massoc.detail$PAFRisk.strata.wald)


barplot(dat, beside = T, legend = T)

str(banco_analise_ofc)
    dat2 = table(banco_analise_ofc$cimc25.arp, banco_analise_ofc$DCNT_carga,banco_analise_ofc$sexoCat,
             dnn = c("imc","dcnt", "sexo"))
print(dat2)

epi2<- epi.2by2(dat2, method = "cross.sectional", 
                conf.level = 0.95, units = 100, outcome = "as.columns")

print(epi2)
# a partir de um glm 

banco_analise_ofc$DCNT_carga <- as.numeric(banco_analise_ofc$DCNT_carga) 
banco_analise_ofc$DCNT_carga <- banco_analise_ofc$DCNT_carga-1
table(banco_analise_ofc$DCNT_carga)
banco_analise_ofc$cimc25.arp <- as.numeric(banco_analise_ofc$cimc25.arp)
table(banco_analise_ofc$cimc25.arp)
banco_analise_ofc$cimc25.arp <- banco_analise_ofc$cimc25.arp-1
banco_analise_ofc$sexo <- as.numeric(banco_analise_ofc$sexo)
table(banco_analise_ofc$sexo)
banco_analise_ofc$sexo <- banco_analise_ofc$sexo-1

fit_af <- glm(DCNT_carga ~ cimc25.arp + sexo + etnia +idade,
              family = binomial,
              data = banco_analise_ofc)

tbl_regression(fit_af)

AFest <- AFglm(object = fit_af , 
               data=banco_analise_ofc, 
               exposure = "cimc25.arp")
summary(AFest)


#fracao atribuivel por cox

modelo.cox25_FA<- coxph(Surv(idade_DCNT_2, DCNT_carga) ~ factor(cimc25.arp)+factor(sexo)+factor(etnia), ties='breslow', data = banco_analise_ofc)
tbl_regression(modelo.cox25_FA, exponentiate = TRUE)

AFest <- AFcoxph(modelo.cox25_FA,
                 data = banco_analise_ofc,
                 exposure = "cimc25.arp",
                 times = idade_DCNT_2)


# com o pacote RPAF
devtools::install_github("https://github.com/inpowell/rpaf", force = TRUE)
call(rpaf)
bmi_data <- gen.data(
  indata = banco_analise_ofc, 
  id_var = "SEQN", 
  ft_breaks = seq(0,60,10),
  disease_ind = "DCNT_carga", disease_time = "idade_DCNT_2",
  variables = c("sexoCat", "cimc25.arp", "fuma_atualCat"),
  period_factor = "F_PERIOD", time_var = "FT_END")




######################## prevalências da tabela descritiva

banco_analise_ofc$mec <- banco_analise_ofc$wtmec2yr/6

banco_analise_ponderado <- svydesign(data=banco_analise_ofc, id=~sdmvpsu, strata = ~sdmvstra, weights = ~mec, nest = TRUE)

summary(banco_analise_ponderado)


#variavel contínua
svymean(~idade, banco_analise_ponderado)

#variavel categorica
svymean(~factor(sexo), banco_analise_ponderado)

#para estimar estatísticas em subconjuntos definidos porum fator, usa-se o comando "svyby"

svyby(~idadeCat, banco_analise_ponderado, svymean)

#teste t para comparação de médias

svyttest(idade~factor(sexo), banco_analise_ponderado)

#teste qui quadrado para comparação de proporções: 

svychisq(~cimc25ano+sexo, banco_analise_ponderado)

svyratio(~sexoCat, banco_analise_ponderado)

wpct(sexoCat, weight=mec, data= banco_analise_ofc)

svytotal(~DCNT_carga, design = banco_analise_ponderado, deff=TRUE)


banco_analise_ofc %>% 
  as_survey_design(., weights=mec) -> banco_analise_ponderado

######### 


# sexo
# 1 = homem, 2 = mulher
table(banco_analise_ofc$sexo)
str(banco_analise_ofc$sexo)
banco_analise_ofc$sexo <- as.factor(banco_analise_ofc$sexo)
freq(banco_analise_ofc$sexo)

banco_analise_ofc$sexoCat <- factor(banco_analise_ofc$sexo,
                                          levels = c("2", "1"),
                                          labels = c("Female", "Male"))
table(banco_analise_ofc$sexoCat)




banco_analise_ponderado %>% group_by(sexoCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

a <- svymean(~sexoCat, design = banco_analise_ponderado)
confint(a, level = 0.95)
# idade categórica

banco_analise_ofc$idadeCat = cut(banco_analise_ofc$idade,
                                       breaks = c(0, 39, 59, Inf),
                                       labels = c('28-39','40-59','60+')) 

table(banco_analise_ofc$idadeCat)
str(banco_analise_ofc$idadeCat)

banco_analise_ponderado %>% group_by(idadeCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

b <- svymean(~idadeCat, design = banco_analise_ponderado)
confint(b, level = 0.95)

# etnia

table(banco_analise_ofc$etnia)

banco_analise_ofc$etniaCat <- factor(banco_analise_ofc$etnia,
                                           levels = c("1", "2", "3", "4", "5"),
                                           labels = c("Mexican American",
                                                      "Other Hispanic",
                                                      "Non-Hispanic White",
                                                      "Non-Hispanic Black",
                                                      "Other Race - Including Multi-Racial"))


banco_analise_ponderado %>% group_by(etniaCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

c <- svymean(~etniaCat, design = banco_analise_ponderado)
confint(c, level = 0.95)

# índice de pobreza

table(banco_analise_ofc$indpobreza)

banco_analise_ofc$indpobrezaCat = cut(banco_analise_ofc$indpobreza,
                                            breaks = c(-Inf, 1.25, 3.50, Inf),
                                            labels = c('<1.25','1.25-3.49','>=3.50')) 


levels(banco_analise_ofc$indpobrezaCat)

table(banco_analise_ofc$indpobrezaCat)

banco_analise_ponderado %>% group_by(indpobrezaCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

d <- svymean(~indpobrezaCat, design = banco_analise_ponderado)
confint(d, level = 0.95)

# escolaridade contínua

banco_analise_ofc$escolaridadeCat2[banco_analise_ofc$escolaridade  == 1] <- 7
banco_analise_ofc$escolaridadeCat2[banco_analise_ofc$escolaridade  == 2] <- 10
banco_analise_ofc$escolaridadeCat2[banco_analise_ofc$escolaridade  == 3] <- 11
banco_analise_ofc$escolaridadeCat2[banco_analise_ofc$escolaridade  == 4] <- 12
banco_analise_ofc$escolaridadeCat2[banco_analise_ofc$escolaridade  == 5] <- 14


table(banco_analise_ofc$escolaridadeCat2)
str(banco_analise_ofc$escolaridadeCat2)

# escolaridade categórica

table(banco_analise_ofc$escolaridade)
banco_analise_ofc$escolaridadeCat <- factor(banco_analise_ofc$escolaridade,
                                                  levels = c("1", "2", "3", "4", "5"),
                                                  labels = c("Incomplete High School/Elementary School",
                                                             "Incomplete High School/Elementary School",
                                                             "Complete High School",
                                                             "Complete Undergraduate or Graduate Education",
                                                             "Complete Undergraduate or Graduate Education"))


table(banco_analise_ofc$escolaridadeCat)

banco_analise_ponderado %>% group_by(escolaridadeCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

e <- svymean(~escolaridadeCat, design = banco_analise_ponderado)
confint(e, level = 0.95)


# atividade física

table(banco_analise_ofc$catv1_teste)

#0=sedentário, 1=insuficiente e 2=ativo

banco_analise_ofc$atv1Cat <- factor(banco_analise_ofc$catv1_teste,
                                          levels = c("0", "1", "2"),
                                          labels = c("Inactive",
                                                     "Insufficient assets",
                                                     "Physically active"))
table(banco_analise_ofc$atv1Cat)




#segunda classificação
table(banco_analise_ofc$catv2_teste)

banco_analise_ponderado %>% group_by(catv2_teste) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

f <- svymean(~catv2_teste, design = banco_analise_ponderado)
confint(f, level = 0.95)


#2=sedentário e insuf 1=ativo

# estado civil

table(banco_analise_ofc$estcivil)

banco_analise_ofc$estcivilCat <- factor(banco_analise_ofc$estcivil,
                                              levels = c("1", "2", "3","4","5","6"),
                                              labels = c("Married/Living with partner	",
                                                         "Widowed/Divorced/Separated",
                                                         "Widowed/Divorced/Separated",
                                                         "Widowed/Divorced/Separated",
                                                         "Never married",
                                                         "Married/Living with partner	"))

banco_analise_ponderado %>% group_by(estcivilCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

f <- svymean(~estcivilCat, design = banco_analise_ponderado)
confint(f, level = 0.95)


#Consumo de álcool nos últimos 12 meses

# 0 é não e 1 é sim.
table(banco_analise_ofc$consumoalcool)
str(banco_analise_ofc$consumoalcool)
banco_analise_ofc$consumoalcool <- as.factor(banco_analise_ofc$consumoalcool)

banco_analise_ponderado %>% group_by(consumoalcool) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

g <- svymean(~consumoalcool, design = banco_analise_ponderado)
confint(g, level = 0.95)


#tabagismo atualmente

table(banco_analise_ofc$fumante_atual)
#1 fuma todos os dias, 2 alguns dias, 3 de jeito nenhum

banco_analise_ofc$fuma_atualCat <- factor(banco_analise_ofc$fumante_atual,
                                                levels = c("1", "2", "3"),
                                                labels = c("Every day",
                                                           "Some days",
                                                           "Not at all"))
table(banco_analise_ofc$fuma_atualCat)


banco_analise_ponderado %>% group_by(fuma_atualCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

l <- svymean(~fuma_atualCat, design = banco_analise_ponderado)
confint(l, level = 0.95)


# fumou 100 cigarros na vida : 2 não, 1 sim
table(banco_analise_ofc$fumou100_cigarros)

banco_analise_ofc$fumou100_cigarrosCat <- factor(banco_analise_ofc$fumou100_cigarros,
                                                       levels = c("1", "2"),
                                                       labels = c("sim","não"))

table(banco_analise_ofc$fumou100_cigarrosCat)

banco_analise_ponderado %>% group_by(fumou100_cigarrosCat) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

h <- svymean(~fumou100_cigarrosCat, design = banco_analise_ponderado)
confint(h, level = 0.95)


#carga de DCNT
table(banco_analise_ofc$DCNT_carga)
banco_analise_ofc$DCNT_carga <- as.factor(banco_analise_ofc$DCNT_carga)

banco_analise_ponderado %>% group_by(DCNT_carga) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

i <- svymean(~DCNT_carga, design = banco_analise_ponderado)
confint(i, level = 0.95)

#imc aos 25 anos de idade
banco_analise_ofc$cimc25ano.desc<- factor(banco_analise_ofc$cimc25ano,
                                         levels = c("baixo peso", "peso normal",
                                                    "sobrepeso", "obesidade"),
                                         labels = c("0", "0", "1", "2"))

table(banco_analise_ofc$cimc25ano.desc)

banco_analise_ponderado %>% group_by(cimc25ano.desc) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

j <- svymean(~cimc25ano.desc, design = banco_analise_ponderado)
confint(j, level = 0.95)


#imc atual
table(banco_analise_ofc$cimcatual)

banco_analise_ofc$cimcatual.desc<- factor(banco_analise_ofc$cimcatual,
                                          levels = c("baixo peso", "peso normal",
                                                     "sobrepeso", "obesidade"),
                                          labels = c("0", "0", "1", "2"))

banco_analise_ponderado %>% group_by(cimcatual.desc) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))

k <- svymean(~cimcatual.desc, design = banco_analise_ponderado)
confint(k, level = 0.95)

counts <- table(banco_analise_ofc$cimcatual.desc)
counts



#medias de imc por ciclo
table(banco_analise_ofc$imcatual)
svyby(~cimcatual, by=~ciclo, design = banco_analise_ponderado, FUN = svymean)

xx <- banco_analise_ponderado %>% group_by(cimcatual, ciclo) %>% summarise(n=survey_total(vartype="ci"))  %>% 
  mutate(pct=(n/sum(n)*100))
print(xx, n=28)

svyby(~imcatual, by=~ciclo==5, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc1ano, by=~ciclo==5, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc10ano, by=~ciclo, design = banco_analise_ponderado, FUN = svymean)
svyby(~imc25ano, by=~ciclo, design = banco_analise_ponderado, FUN = svymean)

svyby(~imcatual, by=~idade, design = banco_analise_ponderado, FUN = svymean)

library(hrbrthemes)
library(viridis)

prevalencias_IMC <- data.frame(
  Years = c(07, 09, 11, 13, 15, 17),
 Underweight = c(1.40, 1.40, 0.76, 0.83, 1.48, 1.56),
 Normal_Weight = c(30.51, 31.26, 30.03, 28.11, 29.91, 26.32),
 Overweight = c(36.08, 34.15, 36.74, 35.37, 33.37, 32.45),
 Obesity = c(31.98, 33.17, 32.45, 35.70, 36.24, 39.66),
 stringsAsFactors = FALSE)
str(prevalencias_IMC)

prevalencias_IMC$Years<- as.numeric(prevalencias_IMC$Years)


prevalencias_IMC %>% group_by(Years, Underweight,Normal_Weight,Overweight,Obesity) %>% 
  summarize(media_delay = mean(Years, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x = month, y = media_delay, group = origin, col = origin)) + 
  scale_x_continuous(breaks = 1:100)




svyby(~DCNT_carga, by=~sexoCat, design = banco_analise_ponderado, FUN = svymean)
svyby(~DCNT_carga, by=~etniaCat, design = banco_analise_ponderado, FUN = svymean)
svyby(~DCNT_carga, by=~idadeCat, design = banco_analise_ponderado, FUN = svymean)
svyby(~DCNT_carga, by=~escolaridadeCat, design = banco_analise_ponderado, FUN = svymean)
svyby(~DCNT_carga, by=~indpobrezaCat, design = banco_analise_ponderado, FUN = svymean)
svyby(~DCNT_carga, by=~catv2_teste, design = banco_analise_ponderado, FUN = svymean)



###############################################################################
AFcoxph(object = modelo.cox25, data = banco_analise, exposure = cimc25.arp2)

banco_analise_25anos$cimc25.arp<- factor(banco_analise_25anos$cimc25ano,
                                  levels = c("baixo peso", "peso normal",
                                             "sobrepeso", "obesidade"),
                                  labels = c("0", "0", "1", "1"))

AFcoxph_est <- AFcoxph(modelo.cox25_2, data=banco_analise_25anos, exposure ="cimc25.arp")
summary(AFcoxph_est)





