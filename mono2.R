load("diretorio")

###################### PREPARANDO O MODELO #####################

mono <- painelid_0709_1[, c(3,4,8,20,23,24,31,36,41,46,48,49,50,53)]

mono$rem_med_rs <- as.numeric(sub(",", ".", mono$rem_med_rs, fixed = TRUE))
mono$temp_empr   <- as.numeric(sub(",", ".", mono$temp_empr, fixed = TRUE))

mono$licença <- ifelse(
  mono$`CAUS AFAST 1` == 50 |
  mono$`CAUS AFAST 2` == 50 |
  mono$`CAUS AFAST 3` == 50, 1, 0
)

mono$afastamento_licença <- ifelse(mono$licença == 1, mono$`QT DIAS AFAS`, 0)
mono$ln_salario          <- ifelse(mono$rem_med_rs == 0, 0, log(mono$rem_med_rs))
mono$idade2              <- mono$idade^2
mono$afastamento_licença2 <- mono$afastamento_licença^2

mono$sexo    <- factor(mono$sexo,    levels = c(0,1), labels = c("F","M"))
mono$licença <- factor(mono$licença, levels = c(0,1), labels = c("Sem Licença","Com Licença"))

rm(painelid_0709_1)

###################### FILTRAGENS POR ANO #####################

mono_2007 <- subset(mono,
  ano == 2007 &
  idade >= 25 & idade <= 35 &
  tp_vinculo != 30 & tp_vinculo != 31 & tp_vinculo != 35 &
  grau_instr >= 7 &
  rem_med_rs > 380 & rem_med_rs <= 3800 &
  horas_contr >= 20
)

mono_2008 <- subset(mono,
  ano == 2008 &
  idade >= 25 & idade <= 36 &
  tp_vinculo != 30 & tp_vinculo != 31 & tp_vinculo != 35 &
  grau_instr >= 7 &
  rem_med_rs > 415 & rem_med_rs <= 4150 &
  horas_contr >= 20
)

mono_2009 <- subset(mono,
  ano == 2009 &
  idade >= 25 & idade <= 37 &
  tp_vinculo != 30 & tp_vinculo != 31 & tp_vinculo != 35 &
  grau_instr >= 7 &
  rem_med_rs > 465 & rem_med_rs <= 4650 &
  horas_contr >= 20
)

rm(mono)

###################### CONSTRUÇÃO DO PAINEL #####################

mono1 <- rbind(mono_2007, mono_2008)

mono1$distinct <- ifelse(
  duplicated(mono1$chave, fromLast = TRUE) | duplicated(mono1$chave),
  1, 0
)

mono1 <- subset(mono1, distinct == 1)
mono1 <- mono1[, -c(20)]

mono1$dup_chave <- ifelse(
  duplicated(mono1$chave) | duplicated(mono1$chave, fromLast = TRUE),
  1, 0
)

r <- subset(mono1, dup_chave == 1)
r$sexopis <- paste(r$chave, r$sexo, sep = "")

r$dup_sexo <- ifelse(
  duplicated(r$sexopis) | duplicated(r$sexopis, fromLast = TRUE),
  1, 0
)

r1 <- subset(r, dup_sexo == 0)
r3 <- merge(r1, r1, by = "chave")
r4 <- subset(r3, sexo.x != sexo.y)

r6 <- mono1[!mono1$chave %in% r4$chave, ]
mono1 <- r6[, -c(20)]
mono1.1 <- subset(mono1, ano == 2008)

mono2 <- rbind(mono1.1, mono_2009)
mono2$dup_chave <- ifelse(
  duplicated(mono2$chave) | duplicated(mono2$chave, fromLast = TRUE),
  1, 0
)

r <- subset(mono2, dup_chave == 1)
r$sexopis <- paste(r$chave, r$sexo, sep = "")

r$dup_sexo <- ifelse(
  duplicated(r$sexopis) | duplicated(r$sexopis, fromLast = TRUE),
  1, 0
)

r1 <- subset(r, dup_sexo == 0)
r3 <- merge(r1, r1, by = "chave")
r4 <- subset(r3, sexo.x != sexo.y)

r6 <- mono2[!mono2$chave %in% r4$chave, ]
mono2.2 <- r6

mono2009 <- subset(mono2.2, ano == 2009)
mono2009 <- mono2009[, -c(20)]

mono2 <- rbind(mono1, mono2009)

freq <- as.data.frame(table(mono2$chave))
freq <- subset(freq, Freq == 3)

library(dplyr)
names(freq)[1] <- "chave"

mono3 <- mono2 %>% inner_join(freq)

mono3$grau_instr <- factor(
  mono3$grau_instr,
  levels = 7:11,
  labels = c("Médio Completo", "Superior Incompleto",
             "Superior Completo", "Mestrado", "Doutorado")
)

rm(freq, mono_2007, mono_2008, mono_2009, mono1, mono1.1, mono2, mono2.2, mono2009, r, r6)

###################### MODELOS #####################

library(plm)

painel <- pdata.frame(mono3, index = c("chave","ano"))
rm(mono3)

summary(painel)

reg.pooled <- plm(
  ln_salario ~ sexo + licença + sexo:licença + afastamento_licença +
    afastamento_licença2 + grau_instr + temp_empr + idade + idade2,
  data = painel, model = "pooling"
)

summary(reg.pooled)

reg.ef <- plm(
  ln_salario ~ sexo + licença + sexo:licença + afastamento_licença +
    afastamento_licença2 + grau_instr + temp_empr + idade + idade2,
  data = painel, model = "within"
)

summary(reg.ef)

reg.ea <- plm(
  ln_salario ~ sexo + licença + sexo:licença + afastamento_licença +
    afastamento_licença2 + grau_instr + temp_empr + idade + idade2,
  data = painel, model = "random", random.method = "walhus"
)

summary(reg.ea)

pFtest(reg.ef, reg.pooled)
plmtest(reg.pooled, type = "bp")
phtest(reg.ef, reg.ea)

###################### DESCRITIVA #####################

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

summary(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 1 & painel$afastamento_licença != 0])
summary(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 1 & painel$afastamento_licença != 0])
summary(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 1 & painel$afastamento_licença != 0])

summary(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 0 & painel$afastamento_licença != 0])
summary(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 0 & painel$afastamento_licença != 0])
summary(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 0 & painel$afastamento_licença != 0])

sd(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 1 & painel$afastamento_licença != 0])
sd(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 1 & painel$afastamento_licença != 0])
sd(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 1 & painel$afastamento_licença != 0])

sd(painel$afastamento_licença[painel$ano == 2007 & painel$sexo == 0 & painel$afastamento_licença != 0])
sd(painel$afastamento_licença[painel$ano == 2008 & painel$sexo == 0 & painel$afastamento_licença != 0])
sd(painel$afastamento_licença[painel$ano == 2009 & painel$sexo == 0 & painel$afastamento_licença != 0])






