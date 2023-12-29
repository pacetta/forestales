# Contrastes
options(contrasts = c('contr.sum', 'contr.poly'))

# paquetes requeridos
pacman::p_load(rio, tidyverse, car, emmeans, writexl)

datos <- import("./oct 22/Datos y Analisis Infostat La Lehmann San Agustín Joni 24-8-22.xlsx")

datos <- datos %>% 
  mutate(bloque = as.factor(bloque), 
         variedad = as.factor(variedad),
         tratamiento = as.factor(tratamiento),
         parcela  = as.factor(paste(bloque , variedad , sep= "-")))

# medias
# FALTA TASA DE SOBREVIVENCIA 

medias_datos <- datos %>% 
  group_by(bloque,variedad,tratamiento)  %>% 
  filter(DAP > 0) %>% 
  summarise(media_DAP = mean(DAP, na.rm = TRUE),
            Min_DAP = min(DAP, na.rm = TRUE),
            Max_DAP = max(DAP, na.rm = TRUE),
            desvio_DAP = sd(DAP, na.rm = TRUE),
            media_ALT = mean(ALT, na.rm = TRUE),
            Min_ALT = min(ALT, na.rm = TRUE),
            Max_ALT = max(ALT, na.rm = TRUE),
            desvio_ALT = sd(ALT, na.rm = TRUE))

medias_datos

# tabla de medias
write_xlsx(medias_datos, "medias_datos.xlsx")

# grafico de medias 
medias2 <- medias_datos  %>% 
  mutate(parcela  = as.factor(paste(bloque , variedad , sep= "-")))

medias2 %>% 
  ggplot(.)+
  aes(x = variedad, y = media_DAP, color = tratamiento) +
  geom_point() +
  geom_line(aes(group = tratamiento), stat = "summary", fun.y = mean)

medias2 %>% 
  ggplot(.)+
  aes(x = variedad, y = media_ALT, color = tratamiento) +
  geom_point() +
  geom_line(aes(group = tratamiento), stat = "summary", fun.y = mean)

## solo bloque 3
medias_bloque3 <- datos %>% 
  filter(bloque == 3)  %>% 
  group_by(variedad,tratamiento)  %>% 
  summarise(DAPm = mean(DAP, na.rm = TRUE),
            ALTm = mean(ALT, na.rm = TRUE)) 

writexl::write_xlsx(medias_bloque3, "medias_datos_bloque3.xlsx")


medias_bloque3 %>% 
  ggplot(.)+
  aes(x = variedad, y = DAPm, color = tratamiento) +
  geom_point() +
  geom_line(aes(group = tratamiento), stat = "summary", fun.y = mean)

## solo bloque 4
medias_bloque4 <- datos %>% 
  filter(bloque == 4)  %>% 
  group_by(variedad,tratamiento)  %>% 
  summarise(DAPm = mean(DAP, na.rm = TRUE),
            ALTm = mean(ALT, na.rm = TRUE)) 

writexl::write_xlsx(medias_bloque4, "medias_datos_bloque4.xlsx")


medias_bloque4 %>% 
  ggplot(.)+
  aes(x = variedad, y = DAPm, color = tratamiento) +
  geom_point() +
  geom_line(aes(group = tratamiento), stat = "summary", fun.y = mean)

####

# modelo DAP
# supuestos
mtDAP <- aov(media_DAP ~ variedad*tratamiento, medias_datos)
shapiro.test(resid(mtDAP))
leveneTest(mtDAP)

# anova
mDAP <- aov(media_DAP ~ bloque+ variedad*tratamiento + Error(parcela), medias2)
summary(mDAP)


# modelo ALT
# supuestos
mtALT <- aov(ALTm ~ variedad*tratamiento, medias_datos)
shapiro.test(resid(mtALT))
leveneTest(mtALT)

# anova
mALT <- aov(ALTm ~ bloque+ variedad*tratamiento + Error(parcela), medias_datos)
summary(mALT)

# graficos
ggplot(medias_datos) +
  aes(x=variedad , y = DAPm , color= tratamiento) +
  geom_line(aes(group = tratamiento), stat = 'summary')

ggplot(medias_datos) +
  aes(x=variedad , y = ALTm , color= tratamiento) +
  geom_line(aes(group = tratamiento), stat = 'summary')

# medias DAP
lsm_i <- emmeans(mDAP, ~ tratamiento*variedad)
lsm_v <- emmeans(mDAP, ~ variedad)
lsm_t <- emmeans(mDAP, ~ tratamiento)

contrast(lsm_t, method = "pairwise")

contrast(lsm_i, method = "pairwise", by = "variedad") ## diferencia en CG08  (C vs T)



# medias ALT
lsm_ai <- emmeans(mALT, ~ tratamiento*variedad)
lsm_av <- emmeans(mALT, ~ variedad)
lsm_at <- emmeans(mALT, ~ tratamiento)

contrast(lsm_at, method = "pairwise")

contrast(lsm_ai, method = "pairwise", by = "variedad")

## agregar analisis de supervivencia

## agregar diferencia en bloques (diferencias fisicas y quimicas)