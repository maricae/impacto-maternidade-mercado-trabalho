load("C:/Users/Cliente/OneDrive/Estudos/Mono/painelid_0709_1.Rdata")

###################### PREPARANDO O MODELO #####################
mono <- painelid_0709_1[,c(3,4,8,20,23,24,31,36,41,46,48,49,50,53)]
mono$rem_med_rs <- as.numeric(sub(",", ".", mono$rem_med_rs, fixed = TRUE))
mono$temp_empr <- as.numeric(sub(",", ".", mono$temp_empr, fixed = TRUE))
mono$licença <- ifelse (mono$`CAUS AFAST 1`==50 | mono$`CAUS AFAST 2`==50 | mono$`CAUS AFAST 3`==50, 1, 0)
mono$afastamento_licença <- ifelse (mono$licença == 1, mono$`QT DIAS AFAS`, 0)
mono$ln_salario <- ifelse(mono$rem_med_rs == 0, 0, log(mono$rem_med_rs))
mono$idade2 <- (mono$idade)^2
mono$afastamento_licença2 <- (mono$afastamento_licença)^2
mono$sexo <- factor(mono$sexo, label = c("F", "M"), levels = c(0, 1))
mono$licença <- factor(mono$licença, label = c("Sem Licença", "Com Licença"), levels = c(0, 1))

rm(painelid_0709_1)

mono_2007 <- subset(mono, 
                    mono$ano == 2007 & 
                    mono$idade >= 25 & 
                    mono$idade <= 35 & 
                    mono$tp_vinculo != 30 &
                    mono$tp_vinculo != 31 &
                    mono$tp_vinculo != 35 &
                    mono$grau_instr >= 7 &
                    mono$rem_med_rs > 380 &
                    mono$rem_med_rs <= 3800 &
                    mono$horas_contr >= 20
                    )

mono_2008 <- subset(mono, 
                    mono$ano == 2008 & 
                    mono$idade >= 25 & 
                    mono$idade <= 36 & 
                    mono$tp_vinculo != 30 &
                    mono$tp_vinculo != 31 &
                    mono$tp_vinculo != 35 &
                    mono$grau_instr >= 7 &
                    mono$rem_med_rs > 415 &
                    mono$rem_med_rs <= 4150 &
                    mono$horas_contr >= 20
                    )

mono_2009 <- subset(mono, 
                    mono$ano == 2009 & 
                    mono$idade >= 25 & 
                    mono$idade <= 37 & 
                    mono$tp_vinculo != 30 &
                    mono$tp_vinculo != 31 &
                    mono$tp_vinculo != 35 &
                    mono$grau_instr >= 7 &
                    mono$rem_med_rs > 465 &
                    mono$rem_med_rs <= 4650 &
                    mono$horas_contr >= 20
                    )

rm(mono)

mono1 <- rbind(mono_2007, mono_2008)
mono1$distinct <- ifelse (duplicated(mono1$chave, fromLast = TRUE) | duplicated(mono1$chave), 1, 0)
mono1 <- subset(mono1, mono1$distinct == 1)
mono1 <- mono1[,-c(20)]

mono1[["dup_chave"]]<- duplicated(mono1[['chave']]) | duplicated(mono1[['chave']], fromLast=TRUE)
mono1$dup_chave <- ifelse( mono1$dup_chave==TRUE, 1, 0)
r <- subset(mono1,mono1$dup_chave == 1)
r$sexopis <- paste(r$chave, r$sexo, sep="")
r[["dup_sexo"]]<- duplicated(r[['sexopis']]) | duplicated(r[['sexopis']], fromLast=TRUE)
r$dup_sexo <- ifelse(r$dup_sexo==TRUE, 1,0)
r1 <- subset(r,r$dup_sexo == 0)
r2 <- r1
r3 <- merge(r1, r2, by.x='chave', by.y='chave')
r4 <- subset(r3, r3$`sexo.x`!= r3$`sexo.y` )
r5<- r4$chave
r6 <- mono1[!mono1$chave %in% r5, ]

mono1<-r6
mono1 <- mono1[,-c(20)]
mono1.1 <- subset(mono1, mono1$ano==2008)
mono2 <- rbind(mono1.1, mono_2009)

mono2[["dup_chave"]]<- duplicated(mono2[['chave']]) | duplicated(mono2[['chave']], fromLast=TRUE)
mono2$dup_chave <- ifelse( mono2$dup_chave==TRUE, 1, 0)
r <- subset(mono2,mono2$dup_chave == 1)
r$sexopis <- paste(r$chave, r$sexo, sep="")
r[["dup_sexo"]]<- duplicated(r[['sexopis']]) | duplicated(r[['sexopis']], fromLast=TRUE)
r$dup_sexo <- ifelse(r$dup_sexo==TRUE, 1,0)
r1 <- subset(r,r$dup_sexo == 0)
r2 <- r1
r3 <- merge(r1, r2, by.x='chave', by.y='chave')
r4 <- subset(r3, r3$`sexo.x`!= r3$`sexo.y` )
r5<- r4$chave
r6 <- mono2[!mono2$chave %in% r5, ]
rm(r5, r4, r3, r2, r1)

mono2.2 <- r6
mono2009 <- subset(mono2.2, mono2.2$ano==2009)
rm(mono2)
mono2009 <- mono2009[, -c(20)]
mono2 <- rbind(mono1, mono2009)

freq <- as.data.frame(table(mono2$chave))
freq <- subset(freq, freq$Freq == 3)
library(dplyr)
names(freq)[names(freq)=="Var1"] <- "chave"
mono3 <- mono2 %>% inner_join(freq)

mono3$grau_instr <- factor(mono3$grau_instr, label = c("Médio Completo", 
                                                      "Superior Incompleto",
                                                      "Superior Completo",
                                                      "Mestrado",
                                                      "Doutorado"), 
                          levels = 7:11, order = T)

rm(freq, mono_2007, mono_2008, mono_2009, mono1, mono1.1, mono2, mono2.2, mono2009, r, r6)

###################### MODELO #####################
library(plm)
painel <- pdata.frame(mono3, index = c('chave','ano'))
rm(mono3)
summary(painel)

## Modelo Pooled
reg.pooled <- plm(ln_salario ~ sexo + licença + sexo:licença + afastamento_licença + afastamento_licença2 + sexo:QT.DIAS.AFAS + grau_instr + temp_empr + idade + idade2 , data = painel, model='pooling')
summary(reg.pooled)

## Modelo Efeitos Fixos
reg.ef <- plm(ln_salario ~ sexo + licença + sexo:licença + afastamento_licença + afastamento_licença2 + sexo:QT.DIAS.AFAS + grau_instr + temp_empr + idade + idade2 , data = painel, model='within')
summary(reg.ef)

## Modelo de Efeitos Aleatórios
reg.ea=plm(ln_salario ~ sexo + licença + sexo:licença + afastamento_licença + afastamento_licença2 + sexo:QT.DIAS.AFAS + grau_instr + temp_empr + idade + idade2 , data = painel, model="random", random.method = "walhus")
summary(reg.ea)

## Comparação de modelos
pFtest(reg.ef,reg.pooled)  # EF x Pooled
plmtest(reg.pooled, type="bp")  # EA x Pooled
phtest(reg.ef,reg.ea)  # EF x EA

#################### DESCRITIVA ########################
table(painel$sexo[painel$ano == 2007])
table(painel$sexo[painel$ano == 2008])
table(painel$sexo[painel$ano == 2009])

summary(painel$rem_med_rs[painel$ano == 2007 & painel$sexo == 1])
summary(painel$rem_med_rs[painel$ano == 2008 & painel$sexo == 1])
summary(painel$rem_med_rs[painel$ano == 2009 & painel$sexo == 1])
summary(painel$rem_med_rs[painel$ano == 2007 & painel$sexo == 0])
summary(painel$rem_med_rs[painel$ano == 2008 & painel$sexo == 0])
summary(painel$rem_med_rs[painel$ano == 2009 & painel$sexo == 0])

sd(painel$rem_med_rs[painel$ano == 2007 & painel$sexo == 1])
sd(painel$rem_med_rs[painel$ano == 2008 & painel$sexo == 1])
sd(painel$rem_med_rs[painel$ano == 2009 & painel$sexo == 1])
sd(painel$rem_med_rs[painel$ano == 2007 & painel$sexo == 0])
sd(painel$rem_med_rs[painel$ano == 2008 & painel$sexo == 0])
sd(painel$rem_med_rs[painel$ano == 2009 & painel$sexo == 0])

summary(painel$horas_contr[painel$ano == 2007 & painel$sexo == 1])
summary(painel$horas_contr[painel$ano == 2008 & painel$sexo == 1])
summary(painel$horas_contr[painel$ano == 2009 & painel$sexo == 1])
summary(painel$horas_contr[painel$ano == 2007 & painel$sexo == 0])
summary(painel$horas_contr[painel$ano == 2008 & painel$sexo == 0])
summary(painel$horas_contr[painel$ano == 2009 & painel$sexo == 0])

sd(painel$horas_contr[painel$ano == 2007 & painel$sexo == 1])
sd(painel$horas_contr[painel$ano == 2008 & painel$sexo == 1])
sd(painel$horas_contr[painel$ano == 2009 & painel$sexo == 1])
sd(painel$horas_contr[painel$ano == 2007 & painel$sexo == 0])
sd(painel$horas_contr[painel$ano == 2008 & painel$sexo == 0])
sd(painel$horas_contr[painel$ano == 2009 & painel$sexo == 0])

table(painel$idade[painel$ano == 2007 & painel$sexo == 1])
table(painel$idade[painel$ano == 2008 & painel$sexo == 1])
table(painel$idade[painel$ano == 2009 & painel$sexo == 1])
table(painel$idade[painel$ano == 2007 & painel$sexo == 0])
table(painel$idade[painel$ano == 2008 & painel$sexo == 0])
table(painel$idade[painel$ano == 2009 & painel$sexo == 0])

summary(painel$temp_empr[painel$ano == 2007 & painel$sexo == 1])
summary(painel$temp_empr[painel$ano == 2008 & painel$sexo == 1])
summary(painel$temp_empr[painel$ano == 2009 & painel$sexo == 1])
summary(painel$temp_empr[painel$ano == 2007 & painel$sexo == 0])
summary(painel$temp_empr[painel$ano == 2008 & painel$sexo == 0])
summary(painel$temp_empr[painel$ano == 2009 & painel$sexo == 0])

sd(painel$temp_empr[painel$ano == 2007 & painel$sexo == 1])
sd(painel$temp_empr[painel$ano == 2008 & painel$sexo == 1])
sd(painel$temp_empr[painel$ano == 2009 & painel$sexo == 1])
sd(painel$temp_empr[painel$ano == 2007 & painel$sexo == 0])
sd(painel$temp_empr[painel$ano == 2008 & painel$sexo == 0])
sd(painel$temp_empr[painel$ano == 2009 & painel$sexo == 0])

table(painel$licença[painel$ano == 2007 & painel$sexo == 1])
table(painel$licença[painel$ano == 2008 & painel$sexo == 1])
table(painel$licença[painel$ano == 2009 & painel$sexo == 1])
table(painel$licença[painel$ano == 2007 & painel$sexo == 0])
table(painel$licença[painel$ano == 2008 & painel$sexo == 0])
table(painel$licença[painel$ano == 2009 & painel$sexo == 0])

summary(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 1 & painel$afastamento_licença!=0])
summary(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 1 & painel$afastamento_licença!=0])
summary(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 1 & painel$afastamento_licença!=0])
summary(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 0 & painel$afastamento_licença!=0])
summary(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 0 & painel$afastamento_licença!=0])
summary(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 0 & painel$afastamento_licença!=0])

sd(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 1 & painel$afastamento_licença!=0])
sd(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 1 & painel$afastamento_licença!=0])
sd(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 1 & painel$afastamento_licença!=0])
sd(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 0 & painel$afastamento_licença!=0])
sd(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 0 & painel$afastamento_licença!=0])
sd(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 0 & painel$afastamento_licença!=0])


