# ============================================================================
# PREDICCIÓN DE CUSTOMER CHURN - QWE INC.
# VERSIÓN FINAL CON GRÁFICOS INTEGRADOS
# ============================================================================

# --- CONFIGURACIÓN INICIAL ---------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(12345)

cat("\n========================================\n")
cat("INSTALANDO Y CARGANDO PAQUETES...\n")
cat("========================================\n\n")

# Paquetes (instala si falta)
pkgs <- c("tidyverse","readr","broom","modelsummary","margins","pROC","caret","janitor","gt")
for(p in pkgs) if(!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(tidyverse); library(readr); library(broom); library(modelsummary)
library(margins); library(pROC); library(caret); library(janitor); library(gt)

# Crear carpeta de salida
outdir <- "outputs"
if(!dir.exists(outdir)) dir.create(outdir)

# Crear carpeta de salida
dir_salida <- "resultados_churn"
if (!dir.exists(dir_salida)) dir.create(dir_salida)

cat("========================================\n")
cat("ANÁLISIS DE CUSTOMER CHURN - QWE INC.\n")
cat("========================================\n\n")

# --- CARGAR DATOS DESDE EXCEL ------------------------------------------------
cat("1. Cargando datos desde Excel...\n")

archivo_datos <- "C:/Users/angel/OneDrive - Pontificia Universidad Javeriana/Github- Analitica de los Negocios/Caso Harvard Final - Predicting Costumer/DATA.xlsx"

if (!file.exists(archivo_datos)) {
  cat("⚠ ADVERTENCIA: No se encontró el archivo\n")
  archivo_datos <- file.choose()
}

datos <- read_excel(archivo_datos, sheet = "Case Data") %>%
  clean_names()

cat("   ✓ Datos cargados:", nrow(datos), "observaciones,", ncol(datos), "variables\n\n")

# --- PREPARACIÓN DE DATOS ----------------------------------------------------
cat("2. Preparando datos...\n")

nombre_churn <- names(datos)[grepl("churn", names(datos), ignore.case = TRUE)]

if (length(nombre_churn) == 0) {
  stop("ERROR: No se encontró la variable 'churn'")
}

if (nombre_churn[1] != "churn") {
  datos <- datos %>% rename(churn = !!sym(nombre_churn[1]))
}

datos <- datos %>%
  mutate(churn = case_when(
    is.na(churn) ~ NA_real_,
    churn == 0 | churn == "-" ~ 0,
    TRUE ~ 1
  )) %>%
  filter(!is.na(churn))

# Distribución
tabla_churn <- data.frame(
  Churn = c("No (0)", "Sí (1)", "TOTAL"),
  Frecuencia = c(sum(datos$churn == 0), 
                 sum(datos$churn == 1), 
                 nrow(datos)),
  Porcentaje = c(
    paste0(round(100 * sum(datos$churn == 0) / nrow(datos), 1), "%"),
    paste0(round(100 * sum(datos$churn == 1) / nrow(datos), 1), "%"),
    "100.0%"
  )
)

cat("\n")
print(kable(tabla_churn, format = "simple", align = c("l", "r", "r")))
cat("\n")

# GRÁFICO 0: Distribución de Churn
cat("📊 Generando gráfico de distribución de churn...\n")

png(file.path(dir_salida, "grafico_0_distribucion_churn.png"), 
    width = 800, height = 600)
barplot(table(datos$churn),
        main = "Gráfico 0: Distribución de la Variable Churn",
        xlab = "Churn",
        ylab = "Frecuencia",
        col = c("#06D6A0", "#EF476F"),
        names.arg = c("No (0)", "Sí (1)"))
text(x = c(0.7, 1.9), 
     y = table(datos$churn) + max(table(datos$churn)) * 0.05,
     labels = paste0(round(100 * table(datos$churn) / nrow(datos), 1), "%"),
     cex = 1.2, font = 2)
dev.off()

# Mostrar
barplot(table(datos$churn),
        main = "Gráfico 0: Distribución de la Variable Churn",
        xlab = "Churn",
        ylab = "Frecuencia",
        col = c("#06D6A0", "#EF476F"),
        names.arg = c("No (0)", "Sí (1)"))
text(x = c(0.7, 1.9), 
     y = table(datos$churn) + max(table(datos$churn)) * 0.05,
     labels = paste0(round(100 * table(datos$churn) / nrow(datos), 1), "%"),
     cex = 1.2, font = 2)

cat("   ✓ Gráfico guardado\n\n")

# --- TABLA DESCRIPTIVA -------------------------------------------------------
cat("3. Generando tabla descriptiva...\n\n")

columnas_numericas <- sapply(datos, is.numeric)
nombres_numericos <- names(datos)[columnas_numericas]
vars_numericas <- nombres_numericos[!nombres_numericos %in% c("churn", "id")]

desc_list <- list()

for (var in vars_numericas) {
  desc_list[[var]] <- data.frame(
    Variable = var,
    N = sum(!is.na(datos[[var]])),
    Media = round(mean(datos[[var]], na.rm = TRUE), 2),
    Desv_Std = round(sd(datos[[var]], na.rm = TRUE), 2),
    Mínimo = round(min(datos[[var]], na.rm = TRUE), 2),
    Mediana = round(median(datos[[var]], na.rm = TRUE), 2),
    Máximo = round(max(datos[[var]], na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
}

tabla_desc_general <- do.call(rbind, desc_list)
rownames(tabla_desc_general) <- NULL

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 1: ESTADÍSTICAS DESCRIPTIVAS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(tabla_desc_general, format = "simple", align = "lrrrrrrr"))
cat("\n")

write_csv(tabla_desc_general, 
          file.path(dir_salida, "tabla_1_descriptivos_generales.csv"))

cat("   ✓ Tabla guardada\n\n")

# --- PARTICIÓN TRAIN/TEST ----------------------------------------------------
cat("4. Dividiendo datos (70% train, 30% test)...\n")

set.seed(12345)
indices_train <- createDataPartition(datos$churn, p = 0.70, list = FALSE)
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

particion_tabla <- data.frame(
  Conjunto = c("Train", "Test", "Total"),
  Observaciones = c(nrow(datos_train), nrow(datos_test), nrow(datos)),
  Porcentaje = c("70%", "30%", "100%")
)

cat("\n")
print(kable(particion_tabla, format = "simple", align = c("l", "r", "r")))
cat("\n")

# --- ESTIMAR MODELOS ---------------------------------------------------------
cat("5. Estimando modelos de probabilidad...\n")

vars_predictoras <- vars_numericas
formula_modelo <- as.formula(paste("churn ~", paste(vars_predictoras, collapse = " + ")))

# MODELO LOGIT
modelo_logit <- glm(formula_modelo, 
                    data = datos_train, 
                    family = binomial(link = "logit"))

cat("   ✓ Modelo Logit estimado\n")

# MODELO PROBIT
modelo_probit <- glm(formula_modelo, 
                     data = datos_train, 
                     family = binomial(link = "probit"))

cat("   ✓ Modelo Probit estimado\n\n")

# --- TABLA DE REGRESIÓN ------------------------------------------------------
cat("6. Tabla de regresión...\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 2: MODELOS DE PREDICCIÓN DE CUSTOMER CHURN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

stargazer(modelo_logit, modelo_probit,
          type = "text",
          title = "",
          dep.var.labels = "Churn (1 = Sí, 0 = No)",
          column.labels = c("Logit", "Probit"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"))

stargazer(modelo_logit, modelo_probit,
          type = "text",
          out = file.path(dir_salida, "tabla_2_regresion.txt"))

modelsummary(
  list("Logit" = modelo_logit, "Probit" = modelo_probit),
  output = file.path(dir_salida, "tabla_2_regresion.html"),
  stars = c('*' = .05, '**' = .01, '***' = .001)
)

cat("\n   ✓ Tabla exportada\n\n")

# GRÁFICO 1: Coeficientes del modelo (NUEVO)
cat("📊 Generando gráfico de coeficientes de regresión...\n")

coefs_plot <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term))

png(file.path(dir_salida, "grafico_1_coeficientes_regresion.png"), 
    width = 1000, height = 800)
par(mar = c(5, 10, 4, 2))

# Crear gráfico de puntos con intervalos de confianza
plot(coefs_plot$estimate, 1:nrow(coefs_plot),
     xlim = range(c(coefs_plot$conf.low, coefs_plot$conf.high)),
     yaxt = "n",
     ylab = "",
     xlab = "Coeficiente (Log-Odds)",
     main = "Gráfico 1: Coeficientes del Modelo Logit\ncon Intervalos de Confianza 95%",
     pch = 19,
     col = ifelse(coefs_plot$p.value < 0.05, "#EF476F", "#2E86AB"),
     cex = 1.5)

# Agregar intervalos de confianza
segments(coefs_plot$conf.low, 1:nrow(coefs_plot),
         coefs_plot$conf.high, 1:nrow(coefs_plot),
         col = ifelse(coefs_plot$p.value < 0.05, "#EF476F", "#2E86AB"),
         lwd = 2)

# Línea de referencia en cero
abline(v = 0, lty = 2, col = "gray50", lwd = 2)

# Etiquetas del eje Y
axis(2, at = 1:nrow(coefs_plot), labels = coefs_plot$term, las = 1, cex.axis = 0.9)

# Leyenda
legend("topright", 
       legend = c("Significativo (p < 0.05)", "No significativo"),
       col = c("#EF476F", "#2E86AB"),
       pch = 19, pt.cex = 1.5)

grid()
dev.off()

# Mostrar
par(mar = c(5, 10, 4, 2))
plot(coefs_plot$estimate, 1:nrow(coefs_plot),
     xlim = range(c(coefs_plot$conf.low, coefs_plot$conf.high)),
     yaxt = "n",
     ylab = "",
     xlab = "Coeficiente (Log-Odds)",
     main = "Gráfico 1: Coeficientes del Modelo Logit\ncon Intervalos de Confianza 95%",
     pch = 19,
     col = ifelse(coefs_plot$p.value < 0.05, "#EF476F", "#2E86AB"),
     cex = 1.5)
segments(coefs_plot$conf.low, 1:nrow(coefs_plot),
         coefs_plot$conf.high, 1:nrow(coefs_plot),
         col = ifelse(coefs_plot$p.value < 0.05, "#EF476F", "#2E86AB"),
         lwd = 2)
abline(v = 0, lty = 2, col = "gray50", lwd = 2)
axis(2, at = 1:nrow(coefs_plot), labels = coefs_plot$term, las = 1, cex.axis = 0.9)
legend("topright", 
       legend = c("Significativo (p < 0.05)", "No significativo"),
       col = c("#EF476F", "#2E86AB"),
       pch = 19, pt.cex = 1.5)
grid()

cat("   ✓ Gráfico de coeficientes guardado\n\n")

# --- MÉTRICAS DE BONDAD DE AJUSTE --------------------------------------------
cat("7. Métricas de bondad de ajuste...\n\n")

ll_completo <- as.numeric(logLik(modelo_logit))
ll_nulo <- as.numeric(logLik(update(modelo_logit, . ~ 1)))
pseudo_r2 <- 1 - (ll_completo / ll_nulo)

metricas_modelo <- data.frame(
  Métrica = c("Log-Likelihood Completo", "Log-Likelihood Nulo", 
              "Pseudo R² (McFadden)", "AIC", "BIC", "Observaciones"),
  Valor = round(c(ll_completo, ll_nulo, pseudo_r2, 
                  AIC(modelo_logit), BIC(modelo_logit), nobs(modelo_logit)), 4)
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 3: MÉTRICAS DEL MODELO LOGIT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(metricas_modelo, format = "simple", align = c("l", "r")))
cat("\n")

write_csv(metricas_modelo, 
          file.path(dir_salida, "tabla_3_metricas_modelo.csv"))

# --- INTERPRETACIÓN DE COEFICIENTES ------------------------------------------
cat("8. Interpretación de coeficientes...\n\n")

coefs <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    cambio_porcentual = (odds_ratio - 1) * 100,
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(p.value)

top_coefs <- head(coefs, 2)

interpretaciones <- data.frame(
  Variable = top_coefs$term,
  Coeficiente = round(top_coefs$estimate, 4),
  Odds_Ratio = round(top_coefs$odds_ratio, 4),
  `Cambio_%` = paste0(round(top_coefs$cambio_porcentual, 1), "%"),
  P_valor = format.pval(top_coefs$p.value, digits = 3),
  Significancia = top_coefs$significancia,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 4: INTERPRETACIÓN DE LOS 2 COEFICIENTES MÁS SIGNIFICATIVOS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(interpretaciones, format = "simple", align = "lrrrrr"))
cat("\n")

write_csv(interpretaciones, 
          file.path(dir_salida, "tabla_4_interpretacion_coeficientes.csv"))
write_csv(coefs, file.path(dir_salida, "tabla_5_todos_coeficientes.csv"))

# --- PREDICCIONES EN TEST ----------------------------------------------------
cat("9. Generando predicciones...\n")

datos_test$prob_churn <- predict(modelo_logit, 
                                  newdata = datos_test, 
                                  type = "response")

datos_test$pred_churn <- ifelse(datos_test$prob_churn >= 0.5, 1, 0)

cat("   ✓ Predicciones completadas\n\n")

# --- TOP 100 CLIENTES EN RIESGO ---------------------------------------------
cat("10. Identificando top 100 clientes en riesgo...\n\n")

if (!"id" %in% names(datos_test)) {
  datos_test$id <- seq_len(nrow(datos_test))
}

datos_test_ordenado <- datos_test %>%
  arrange(desc(prob_churn)) %>%
  mutate(ranking = row_number())

columnas_disponibles <- names(datos_test_ordenado)
columnas_deseadas <- c("ranking", "id", "prob_churn", "churn")
vars_adicionales <- intersect(vars_numericas, columnas_disponibles)[1:3]
vars_adicionales <- vars_adicionales[!is.na(vars_adicionales)]
columnas_finales <- c(columnas_deseadas, vars_adicionales)
columnas_finales <- columnas_finales[columnas_finales %in% columnas_disponibles]

top_100_riesgo <- datos_test_ordenado[1:min(100, nrow(datos_test_ordenado)), 
                                       columnas_finales]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 6: TOP 10 CLIENTES CON MAYOR PROBABILIDAD DE CHURN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print(kable(head(top_100_riesgo, 10), format = "simple", 
            digits = 4, align = "r"))
cat("\n(Lista completa guardada en CSV)\n\n")

write_csv(top_100_riesgo, 
          file.path(dir_salida, "tabla_6_top_100_clientes_riesgo.csv"))

# --- MATRIZ DE CONFUSIÓN -----------------------------------------------------
cat("11. Evaluación del modelo...\n\n")

conf_matrix <- confusionMatrix(
  factor(datos_test$pred_churn, levels = c(0, 1)),
  factor(datos_test$churn, levels = c(0, 1)),
  positive = "1"
)

metricas_eval <- data.frame(
  Métrica = c("Exactitud", "Sensibilidad", "Especificidad", 
              "Precisión", "F1-Score", "Kappa"),
  Valor = round(c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["F1"],
    conf_matrix$overall["Kappa"]
  ), 4)
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 7: MÉTRICAS DE EVALUACIÓN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(metricas_eval, format = "simple", align = c("l", "r")))
cat("\n")

write_csv(metricas_eval, 
          file.path(dir_salida, "tabla_7_metricas_evaluacion.csv"))

# --- CURVA ROC ---------------------------------------------------------------
cat("12. Generando curva ROC...\n")

roc_obj <- roc(datos_test$churn, datos_test$prob_churn, quiet = TRUE)
auc_valor <- auc(roc_obj)

png(file.path(dir_salida, "grafico_2_curva_roc.png"), 
    width = 800, height = 600)
plot(roc_obj, 
     main = paste0("Gráfico 2: Curva ROC\nAUC = ", round(auc_valor, 3)),
     col = "#2E86AB", lwd = 3)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
legend("bottomright", 
       legend = c(paste("AUC =", round(auc_valor, 3)), "Clasificador aleatorio"),
       col = c("#2E86AB", "gray50"), 
       lty = c(1, 2), lwd = c(3, 2))
dev.off()

plot(roc_obj, 
     main = paste0("Gráfico 2: Curva ROC\nAUC = ", round(auc_valor, 3)),
     col = "#2E86AB", lwd = 3)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
legend("bottomright", 
       legend = c(paste("AUC =", round(auc_valor, 3)), "Clasificador aleatorio"),
       col = c("#2E86AB", "gray50"), 
       lty = c(1, 2), lwd = c(3, 2))

cat("   ✓ ROC guardada (AUC =", round(auc_valor, 3), ")\n\n")

# --- GRÁFICO DE ERRORES ------------------------------------------------------
cat("13. Generando gráfico de errores...\n")

datos_test$error <- datos_test$churn - datos_test$prob_churn

png(file.path(dir_salida, "grafico_3_errores.png"), 
    width = 1200, height = 600)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

plot(datos_test$prob_churn, datos_test$error,
     main = "Gráfico 3A: Errores de Predicción",
     xlab = "Probabilidad Predicha",
     ylab = "Error (Real - Predicho)",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.4), cex = 1.2)
abline(h = 0, col = "red", lwd = 3, lty = 2)
grid(col = "gray90")

hist(datos_test$error,
     main = "Gráfico 3B: Distribución de Errores",
     xlab = "Error",
     ylab = "Frecuencia",
     col = "#06D6A0",
     border = "white",
     breaks = 30)
abline(v = 0, col = "red", lwd = 3, lty = 2)
dev.off()

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
plot(datos_test$prob_churn, datos_test$error,
     main = "Gráfico 3A: Errores de Predicción",
     xlab = "Probabilidad Predicha",
     ylab = "Error (Real - Predicho)",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.4), cex = 1.2)
abline(h = 0, col = "red", lwd = 3, lty = 2)
grid(col = "gray90")

hist(datos_test$error,
     main = "Gráfico 3B: Distribución de Errores",
     xlab = "Error",
     ylab = "Frecuencia",
     col = "#06D6A0",
     border = "white",
     breaks = 30)
abline(v = 0, col = "red", lwd = 3, lty = 2)

cat("   ✓ Gráfico de errores guardado\n\n")

# --- GRÁFICO PREDICHOS VS REALES ---------------------------------------------
cat("14. Generando gráfico predichos vs reales...\n")

p <- ggplot(datos_test, aes(x = prob_churn, fill = factor(churn))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(
    values = c("0" = "#06D6A0", "1" = "#EF476F"),
    labels = c("No Churn (0)", "Churn (1)")
  ) +
  labs(
    title = "Gráfico 4: Distribución de Probabilidades Predichas",
    subtitle = paste0("Modelo Logit - N = ", nrow(datos_test)),
    x = "Probabilidad Predicha de Churn",
    y = "Frecuencia",
    fill = "Clase Real"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top"
  )

ggsave(file.path(dir_salida, "grafico_4_predichos_vs_reales.png"), 
       plot = p, width = 10, height = 6, dpi = 300)

print(p)

cat("   ✓ Gráfico guardado\n\n")

# --- CALIBRACIÓN -------------------------------------------------------------
cat("15. Análisis de calibración...\n\n")

calibracion <- datos_test %>%
  mutate(decil = ntile(prob_churn, 10)) %>%
  group_by(decil) %>%
  summarise(
    n = n(),
    prob_media = round(mean(prob_churn), 4),
    tasa_observada = round(mean(churn), 4),
    .groups = "drop"
  )

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 8: CALIBRACIÓN POR DECILES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(calibracion, format = "simple", align = "rrrr",
            col.names = c("Decil", "N", "Prob. Predicha", "Tasa Observada")))
cat("\n")

write_csv(calibracion, file.path(dir_salida, "tabla_8_calibracion.csv"))

png(file.path(dir_salida, "grafico_5_calibracion.png"), 
    width = 800, height = 600)
plot(calibracion$prob_media, calibracion$tasa_observada,
     main = "Gráfico 5: Curva de Calibración",
     xlab = "Probabilidad Predicha (promedio por decil)",
     ylab = "Tasa de Churn Observada",
     pch = 19, col = "#2E86AB", cex = 2,
     xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lwd = 3, lty = 2)
grid(col = "gray90")
legend("topleft", 
       legend = c("Predicciones", "Calibración perfecta"),
       col = c("#2E86AB", "red"), 
       pch = c(19, NA), lty = c(NA, 2), lwd = c(NA, 3), pt.cex = 2)
dev.off()

plot(calibracion$prob_media, calibracion$tasa_observada,
     main = "Gráfico 5: Curva de Calibración",
     xlab = "Probabilidad Predicha (promedio por decil)",
     ylab = "Tasa de Churn Observada",
     pch = 19, col = "#2E86AB", cex = 2,
     xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lwd = 3, lty = 2)
grid(col = "gray90")
legend("topleft", 
       legend = c("Predicciones", "Calibración perfecta"),
       col = c("#2E86AB", "red"), 
       pch = c(19, NA), lty = c(NA, 2), lwd = c(NA, 3), pt.cex = 2)

cat("   ✓ Calibración completada\n\n")

# --- GUARDAR MODELOS ---------------------------------------------------------
cat("16. Guardando modelos...\n")

saveRDS(modelo_logit, file.path(dir_salida, "modelo_logit.rds"))
saveRDS(modelo_probit, file.path(dir_salida, "modelo_probit.rds"))

cat("   ✓ Modelos guardados\n\n")

# --- RESUMEN FINAL -----------------------------------------------------------
cat("\n████████████████████████████████████████████████████████████████████\n")
cat("█         ✓ ANÁLISIS COMPLETADO EXITOSAMENTE ✓                    █\n")
cat("████████████████████████████████████████████████████████████████████\n\n")

resumen_final <- data.frame(
  Métrica = c("Pseudo R² (McFadden)", "AUC", "Exactitud", 
              "Sensibilidad", "F1-Score", "Observaciones Test"),
  Valor = c(
    round(pseudo_r2, 4),
    round(auc_valor, 4),
    round(conf_matrix$overall["Accuracy"], 4),
    round(conf_matrix$byClass["Sensitivity"], 4),
    round(conf_matrix$byClass["F1"], 4),
    nrow(datos_test)
  )
)

print(kable(resumen_final, format = "simple", align = c("l", "r")))

cat("\n\n📁 Archivos en:", normalizePath(dir_salida), "\n")
cat("📊 Total archivos:", length(list.files(dir_salida)), "\n\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  🎉 ¡LISTO PARA ENTREGAR!\n")
cat("  ⏰ Entrega: HOY 25 de octubre, 11:59 PM\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
