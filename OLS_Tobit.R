library(FactoMineR)
library(factoextra)
library(sandwich)
library(corrplot)
library(flexclust)
library(cobalt)
library(MatchIt)
library(caret)
library(ggplot2)
library(dplyr)
library(readr)
library(stargazer)
library(psych)
library(gt)
library(skimr)
library(tibble)
library(MASS)
library(car)
library(caret)
library(glmnet)
library(censReg)
library(lmtest)

#### Чтение данных.  ##############################################################################################

library(data.table)
Data <- fread("~/Desktop/Научные страдания/Данные/Data_full.csv", sep=";")

Data <- Data %>% dplyr::select(-'Revenue',-'Revenue potential', -'Revenue average', -'Median price',
                               -'Final price', -'Max price', -'Min price', -'L_mean',
                               -'SKU', -'SPP Price', -'Basic Sale Price', -'Price with WB wallet', 
                               -'Sales Per Day Average', -'Days with sales', -'Days in stock', 
                               -'Average if in stock', -'Base price', -'Days in website', 
                               -'Turnover days', -'Item combination ID', -'Size count',
                               -'Warehouses count', -'Top sells', -'Ext advertising', 
                               -'Latest negative comments percent', -'Frozen stocks', -'Subject ID', -'Purchase',
                               -'Purchase After Return', -'Person_yolo', -'Search organic position avg',
                               -'Percentage of revenue of top products in the subject', -'Balance', 
                               -'Lost profit percent', - 'Turnover',
                               -'S_mean', -'RoT_horiz_yolov8x', -'RoT_vert_yolov8x')

# Оставляем только числовые столбцы
Data_numeric <- Data %>% select_if(is.numeric)
# Добавляем category_prod
Data_numeric$category_prod <- Data$category_prod
Data <- Data_numeric

str(Data)
skim(Data)

#### Чистка 1.0 ############################################################################################

table(Data$category_prod)

# Шаг 1: Подготовка данных
data_numeric <- Data %>% select_if(is.numeric)
data_scaled <- scale(data_numeric)
# Шаг 2: PCA
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)  
pca_data <- as.data.frame(pca$x[, 1:2]) 

# Шаг 3: Визуализация PCA без выбросов
library(ggplot2)

ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue") +
  ggtitle("PCA: Первые две главные компоненты") +
  theme_minimal()

# Шаг 4: Вычисление расстояний от центра
pca_data$distance <- sqrt(pca_data$PC1^2 + pca_data$PC2^2)
threshold <- quantile(pca_data$distance, 0.99)  # 95-й перцентиль

# Шаг 5: Отметка выбросов
pca_data$outlier <- pca_data$distance > threshold

# Визуализация выбросов
ggplot(pca_data, aes(x = PC1, y = PC2, color = outlier)) +
  geom_point() +
  ggtitle("PCA: Выбросы по 99-му перцентилю") +
  theme_minimal()

# Шаг 6: Удаление выбросов
cleaned_data <- pca_data[!pca_data$outlier, ]

# Шаг 7: Повторная визуализация без выбросов
ggplot(cleaned_data, aes(x = PC1, y = PC2)) +
  geom_point(color = "green") +
  ggtitle("PCA: Без выбросов") +
  theme_minimal()

remaining_indices <- as.numeric(rownames(cleaned_data))
Data <- Data[remaining_indices, ]

table(Data$category_prod)

#Первые 2 компоненты 
library(ggplot2)
library(reshape2)

pcs <- pca$x[, 1:2]
cor_matrix <- cor(data_scaled, pcs)
cor_df <- melt(cor_matrix)
colnames(cor_df) <- c("Variable", "PrincipalComponent", "Correlation")

ggplot(cor_df, aes(x = PrincipalComponent, y = Variable, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 4) +  # подписи значений
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Корреляция") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Корреляции переменных и главных компонент",
       x = "Главные компоненты", y = "Исходные переменные")

#### Корреляции ############################################################################
df <- Data

#индексы переменных для удаления
spisok <- subset(df, select = c(-category_prod))
cor_matrix <- cor(spisok, use = "pairwise.complete.obs")
to_remove <- findCorrelation(cor_matrix, cutoff = 0.7)
# Получаем имена этих переменных
columns_to_remove <- colnames(spisok)[to_remove]
columns_to_remove

high_cor_indices <- which(abs(cor_matrix) >= 0.7 & abs(cor_matrix) < 0.99, arr.ind = TRUE)
# Извлекаем уникальные имена переменных из этих пар
vars_in_high_cor_pairs <- unique(c(
  rownames(cor_matrix)[high_cor_indices[, 1]],
  colnames(cor_matrix)[high_cor_indices[, 2]]
))
high_cor_df <- subset(df, select = which(names(df) %in% vars_in_high_cor_pairs))
cor_matrix <- cor(high_cor_df, use = "pairwise.complete.obs")
# Альтернативная визуализация с использованием ggcorrplot
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, tl.cex = 0.7)

df_cleaned <- subset(df, select = -which(names(df) %in% columns_to_remove))

skimr::skim(df_cleaned) 

#############################################################################################################################################################################
# Если есть нулевые значения
df_cleaned$log_Sales <- log(df_cleaned$Sales + 1)
df_cleaned$log_Comments <- log(df_cleaned$Comments + 1)
df_cleaned$log_Average_price <- log(df_cleaned$`Average price` + 1)
df_cleaned$log_Frozen_stocks_cost <- log(df_cleaned$`Frozen stocks cost` + 1)

# Удаляем исходные столбцы
df_cleaned <- subset(df_cleaned, select = -which(names(df_cleaned) %in%   c('Sales', 'Comments', 'Average price', 'Frozen stocks cost')))
log_Comments <- df_cleaned$log_Comments

colnames(df_cleaned)
#### Отбор переменных #####################################################################################################################################################################
#### Акаике #######

category_prod <- df$category_prod
df_final <- df_cleaned %>% dplyr::select(-'log_Comments', -'category_prod', -'Size count in stock')

# Список переменных, для которых нужно добавить квадраты
vars_to_square <- c("Brightness", "Contrast", "Entropy_value", "char_count_sum", 
                    "Area_ratio_yolov8x", "rot_v", "rot_h")
for (var in vars_to_square) {
  new_col_name <- paste0(var, "_2")
  df_final[[new_col_name]] <- df_final[[var]]^2
}
  
AIC_0 <- lm(log_Sales ~ ., data = df_final)
AIC <- stepAIC(AIC_0)
summary(AIC)
formula_AIC <- formula(AIC)
formula_AIC
vif(AIC)

#### Базовая модель ######################################################################################################
check <- lm(log_Sales ~ Brightness + Contrast + Person_probability + Temp_cool + Entropy_value +
              + has_text + char_count_sum + Area_ratio_yolov8x + rot_v 
            + rot_h, data = df_cleaned)
summary(check)

resettest(check)
resettest(check, power = 2)
resettest(check, power = 3)

# Вывод таблицы результатов
stargazer(check, 
          type = "html",
          title = "Graphic OLS",
          align = TRUE,
          no.space = TRUE, 
          dep.var.labels = c("Логарифм продаж"),
          keep = c('S_mean', 'Contrast', 'Person_probability', 'Temp_cool', 'Entropy_value',
                   'has_text','char_count_sum', 'Area_ratio_yolov8x', 'RoT_horiz_yolov8x', 'RoT_vert_yolov8x'),
          covariate.labels = c('Яркость', 'Контраст', 'Наличие человека', 'Цветовая гамма (холодная)',
                               'Энтропия', 'Наличие текста', 'Сумма символов', 
                               'Площадь объекта', 'Правило третей: Горизонталь', 'Правило третей: Вертикаль'),
          out = '~/Desktop/Научные страдания/Данные/Only_graph.html')

#### На подвыборки #########################################################################################

df_final$category_prod <- category_prod

clothes <- subset(df_final, category_prod == 'clothes', select = -category_prod)
boots <- subset(df_final, category_prod == 'boots', select = -category_prod)
home <- subset(df_final, category_prod == 'home', select = -category_prod)
electr <- subset(df_final, category_prod == 'electronics', select = -category_prod)
food <- subset(df_final, category_prod == 'food', select = -category_prod)
cosm <- subset(df_final, category_prod == 'cosmetics', select = -category_prod)

skim(clothes)
skim(boots)
skim(home)
skim(electr)

clothes <- na.omit(clothes)
boots <- na.omit(boots)
home <- na.omit(home)
electr <- na.omit(electr)
food <- na.omit(food)
cosm <- na.omit(cosm)

#### МНК ПРОДАЖИ #############################################################################

#Одежда
mod1 <- lm(formula_AIC, data = clothes)
summary(mod1)

#Обувь
mod2 <- lm(formula_AIC, data = boots)
mod21 <- update(mod2, .~. -`Package length` -`Package height`)
formula_boots <- formula(mod21)
summary(mod21)

#Дом
mod3 <- lm(formula_AIC, data = home)
summary(mod3)

#Электроника
mod4 <- lm(formula_AIC, data = electr)
summary(mod4)

#Еда
mod5 <- lm(formula_AIC, data = food)
summary(mod5)

#Косметика
mod6 <- lm(formula_AIC, data = cosm)
summary(mod6)

# Вывод таблицы результатов
stargazer(mod6,
          type = "text")
          

library(AER)
# Построение цензурированной регрессии
tobit_1 <- tobit(formula_AIC, left = 0, data = clothes)
tobit_2 <- tobit(formula_boots, left = 0, data = boots)
tobit_3 <- tobit(formula_AIC, left = 0, data = home)
tobit_4 <- tobit(formula_AIC, left = 0, data = electr)
tobit_5 <- tobit(formula_AIC, left = 0, data = food)
tobit_6 <- tobit(formula_AIC, left = 0, data = cosm)

# Вывод таблицы результатов
stargazer(tobit_6,
          type = "text")


#### МНК КОММЕНТАРИИ #############################################################################

df_cleaned1 <- subset(df_final, select = -log_Sales)
df_cleaned1$log_Comments <- log_Comments
#### На подвыборки #########################################################################################

clothes1 <- subset(df_cleaned1, category_prod == 'clothes', select = -category_prod)
boots1 <- subset(df_cleaned1, category_prod == 'boots', select = -category_prod)
home1 <- subset(df_cleaned1, category_prod == 'home', select = -category_prod)
electr1 <- subset(df_cleaned1, category_prod == 'electronics', select = -category_prod)
food1 <- subset(df_cleaned1, category_prod == 'food', select = -category_prod)
cosm1 <- subset(df_cleaned1, category_prod == 'cosmetics', select = -category_prod)

clothes1 <- na.omit(clothes1)
boots1 <- na.omit(boots1)
home1 <- na.omit(home1)
electr1 <- na.omit(electr1)
food1 <- na.omit(food1)
cosm1 <- na.omit(cosm1)

#Одежда
mod10 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = clothes1)
summary(mod10)
Tobit_new <- formula(mod10)

#Обувь
mod20 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = boots1)
mod201 <- update(mod20, .~. -`Package length` -`Package height`)
summary(mod201)
Tobit_boots <- formula(mod201)

#Дом
mod30 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = home1)
summary(mod30)

#Электроника
mod40 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = electr1)
summary(mod40)

#Еда
mod50 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = food1)
summary(mod50)

#Косметика
mod60 <- lm(log_Comments ~ `Basic Sale` + `Category Position` + `Pics Count` + 
              `Search position avg` + `Search words in ads` + `Category visibility` + 
              `Category promo count` + `Latest rating avg` + `Related products count` + 
              `Search cpm avg` + `Search ads position avg` + `Description length` + 
              `Name length` + `Package length` + `Package height` + Contrast + 
              Person_probability + Entropy_value + Temp_cool + has_text + 
              char_count_sum + Area_ratio_yolov8x + Brightness + log_Average_price + 
              log_Frozen_stocks_cost + Brightness_2 + Contrast_2 + Entropy_value_2 + 
              char_count_sum_2 + Area_ratio_yolov8x_2 + rot_v_2 + rot_h_2, data = cosm1)
summary(mod60)

# Вывод таблицы результатов
stargazer(mod201,
          type = "text")

library(AER)
# Построение цензурированной регрессии
tobit_10 <- tobit(Tobit_new, left = 0, data = clothes1)
tobit_20 <- tobit(Tobit_boots, left = 0, data = boots1)
tobit_30 <- tobit(Tobit_new, left = 0, data = home1)
tobit_40 <- tobit(Tobit_new, left = 0, data = electr1)
tobit_50 <- tobit(Tobit_new, left = 0, data = food1)
tobit_60 <- tobit(Tobit_new, left = 0, data = cosm1)

# Вывод таблицы результатов
stargazer(tobit_60,
          type = "text")


##### ЭКСТРЕМУМЫ ПРОДАЖИ ############

# Список моделей мнк
models <- list(mod1, mod21, mod3, mod4, mod5, mod6)
model_names <- c('mod1', 'mod21', 'mod3', 'mod4', 'mod5', 'mod6')

# Список переменных с квадратичными термами
quad_vars <- c("Brightness", "Contrast", "Entropy_value", "char_count_sum", 
               "Area_ratio_yolov8x", "rot_h","rot_v")

# Создаем пустой data.frame для хранения результатов
extrema_results_ols_sales <- data.frame(Model = character(),
                              Variable = character(),
                              X_star = numeric(),
                              Type = character(),
                              stringsAsFactors = FALSE)

# Цикл по моделям
for (i in seq_along(models)) {
  mod <- models[[i]]
  coef_mod <- coef(mod)  # Извлекаем коэффициенты модели
  model_name <- model_names[i]
  
  # Цикл по переменным с квадратами
  for (var in quad_vars) {
    linear_term <- coef_mod[var]
    quadratic_term <- coef_mod[paste0(var, "_2")]
    
    if (!is.na(linear_term) & !is.na(quadratic_term) & quadratic_term != 0) {
      X_star <- -linear_term / (2 * quadratic_term)
      extrema_type <- ifelse(quadratic_term > 0, "Минимум", "Максимум")
      
      extrema_results_ols_sales <- rbind(extrema_results_ols_sales, 
                               data.frame(Model = model_name, 
                                          Variable = var, 
                                          X_star = X_star, 
                                          Type = extrema_type,
                                          stringsAsFactors = FALSE))
    }
  }
}

print(extrema_results_ols_sales)


# Список моделей тобит
models <- list(tobit_1, tobit_2, tobit_3, tobit_4, tobit_5, tobit_6)
model_names <- c('tobit_1', 'tobit_2', 'tobit_3', 'tobit_4', 'tobit_5', 'tobit_6')

# Создаем пустой data.frame для хранения результатов
extrema_results_tobit_sales <- data.frame(Model = character(),
                                        Variable = character(),
                                        X_star = numeric(),
                                        Type = character(),
                                        stringsAsFactors = FALSE)

# Цикл по моделям
for (i in seq_along(models)) {
  mod <- models[[i]]
  coef_mod <- coef(mod)  # Извлекаем коэффициенты модели
  model_name <- model_names[i]
  
  # Цикл по переменным с квадратами
  for (var in quad_vars) {
    linear_term <- coef_mod[var]
    quadratic_term <- coef_mod[paste0(var, "_2")]
    
    if (!is.na(linear_term) & !is.na(quadratic_term) & quadratic_term != 0) {
      X_star <- -linear_term / (2 * quadratic_term)
      extrema_type <- ifelse(quadratic_term > 0, "Минимум", "Максимум")
      
      extrema_results_tobit_sales <- rbind(extrema_results_tobit_sales, 
                                         data.frame(Model = model_name, 
                                                    Variable = var, 
                                                    X_star = X_star, 
                                                    Type = extrema_type,
                                                    stringsAsFactors = FALSE))
    }
  }
}

print(extrema_results_tobit_sales)


##### ЭКСТРЕМУМЫ КОММЕНТАРИИ ############

# Список моделей мнк
models <- list(mod10, mod201, mod30, mod40, mod50, mod60)
model_names <- c('mod10', 'mod201', 'mod30', 'mod40', 'mod50', 'mod60')

# Список переменных с квадратичными термами
quad_vars <- c("Brightness", "Contrast", "Entropy_value", "char_count_sum", 
               "Area_ratio_yolov8x", "rot_h","rot_v")

# Создаем пустой data.frame для хранения результатов
extrema_results_ols_comm <- data.frame(Model = character(),
                                        Variable = character(),
                                        X_star = numeric(),
                                        Type = character(),
                                        stringsAsFactors = FALSE)

# Цикл по моделям
for (i in seq_along(models)) {
  mod <- models[[i]]
  coef_mod <- coef(mod)  # Извлекаем коэффициенты модели
  model_name <- model_names[i]
  
  # Цикл по переменным с квадратами
  for (var in quad_vars) {
    linear_term <- coef_mod[var]
    quadratic_term <- coef_mod[paste0(var, "_2")]
    
    if (!is.na(linear_term) & !is.na(quadratic_term) & quadratic_term != 0) {
      X_star <- -linear_term / (2 * quadratic_term)
      extrema_type <- ifelse(quadratic_term > 0, "Минимум", "Максимум")
      
      extrema_results_ols_comm <- rbind(extrema_results_ols_comm, 
                                         data.frame(Model = model_name, 
                                                    Variable = var, 
                                                    X_star = X_star, 
                                                    Type = extrema_type,
                                                    stringsAsFactors = FALSE))
    }
  }
}

print(extrema_results_ols_comm)


# Список моделей тобит
models <- list(tobit_10, tobit_20, tobit_30, tobit_40, tobit_50, tobit_60)
model_names <- c('tobit_10', 'tobit_20', 'tobit_30', 'tobit_40', 'tobit_50', 'tobit_60')

# Создаем пустой data.frame для хранения результатов
extrema_results_tobit_comm <- data.frame(Model = character(),
                                          Variable = character(),
                                          X_star = numeric(),
                                          Type = character(),
                                          stringsAsFactors = FALSE)

# Цикл по моделям
for (i in seq_along(models)) {
  mod <- models[[i]]
  coef_mod <- coef(mod)  # Извлекаем коэффициенты модели
  model_name <- model_names[i]
  
  # Цикл по переменным с квадратами
  for (var in quad_vars) {
    linear_term <- coef_mod[var]
    quadratic_term <- coef_mod[paste0(var, "_2")]
    
    if (!is.na(linear_term) & !is.na(quadratic_term) & quadratic_term != 0) {
      X_star <- -linear_term / (2 * quadratic_term)
      extrema_type <- ifelse(quadratic_term > 0, "Минимум", "Максимум")
      
      extrema_results_tobit_comm <- rbind(extrema_results_tobit_comm, 
                                           data.frame(Model = model_name, 
                                                      Variable = var, 
                                                      X_star = X_star, 
                                                      Type = extrema_type,
                                                      stringsAsFactors = FALSE))
    }
  }
}

print(extrema_results_tobit_comm)


#сохраним файл
write.csv(df_cleaned, "df_cleaned2.csv", row.names = FALSE)






