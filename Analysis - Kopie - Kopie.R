##### data #####
silage <- read.csv2("Silage.csv")
background_a <- read.csv2("Background_agri.csv")
background_a <- background_a[, -1]
background_p <- read.csv2("Background_past.csv")
background_p <- background_p[, -1]

#dealing with NAs
silage[is.na(silage)] <- 0
background_a[is.na(background_a)] <- 0
background_p[is.na(background_p)] <- 0

#factorizing, cleaning, dropping dist_localgovs and getting dataframes ready
silage <- silage[,c(2:15)]
silage$type <- as.factor(silage$type)
silage$crop <- as.factor(silage$crop)
silage$fl <- as.factor(silage$fl)
time_levels <- c("Dec18", "Jan19", "Feb19", "Mar19", "Apr19", "May19", "Jun19", "Jul19", "Aug19", "Sep19", "Oct19", "Nov19")
silage$time <- factor(silage$time, levels = time_levels, ordered = TRUE)

silage$crop <- ifelse(silage$crop %in% c(0, 3), 1,
                      ifelse(silage$crop %in% c(1, 2), 2,
                             ifelse(silage$crop %in% c(4, 5, 6, 11, 12, 13, 14), 3,
                                    ifelse(silage$crop == 7, 4,
                                           ifelse(silage$crop %in% c(8, 9, 10, 15), 0, silage$crop)))))
silage$crop <- as.factor(silage$crop)

background_a$crop <- ifelse(background_a$crop %in% c(0, 3), 1,
                      ifelse(background_a$crop %in% c(1, 2), 2,
                             ifelse(background_a$crop %in% c(4, 5, 6, 11, 12, 13, 14), 3,
                                    ifelse(background_a$crop == 7, 4,
                                           ifelse(background_a$crop %in% c(8, 9, 10, 15), 0, background_a$crop)))))
background_a$crop <- as.factor(background_a$crop)


background_a <- background_a[,c(2:10)]
background_p <- background_p[,c(2:11)]
background_a$crop <- as.factor(background_a$crop)
background_a$fl <- as.factor(background_a$fl)
background_p$fl <- as.factor(background_p$fl)
background_a <- background_a[, -5]
background_p <- background_p[, -c(4, 9)]
background_a$silage <- F
background_p$silage <- F

##### Load required libraries #####
install.packages("dplyr", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("GGally", dependencies = TRUE)
install.packages("FactoMineR", dependencies = TRUE)
install.packages("factoextra", dependencies = TRUE) 
install.packages("caret", dependencies = TRUE)
install.packages("gbm", dependencies = TRUE)
install.packages("plotmo", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("tree", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)

library(dplyr)
library(psych)
library(reshape2)
library(ggplot2)
library(GGally)
library(FactoMineR)
library(factoextra)
library(caret)
library(gbm)
library(plotmo)
library(rpart.plot)
library(tree)
library(randomForest)

##### Calculate summary statistics using base R #####
summary(silage)

#area
describe(silage$area)
ggplot(data = silage, aes(x = area)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of area",
       x = "area",
       y = "Frequency")
ggplot(data = silage, aes(y = area)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of area",
       y = "dist_paved")
#dist_paved
describe(silage$dist_paved)
ggplot(data = silage, aes(x = dist_paved)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_paved",
       x = "dist_paved",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_paved)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_paved",
       y = "dist_paved")
#dist_unpaved 
describe(silage$dist_unpaved)
ggplot(data = silage, aes(x = dist_unpaved)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_unpaved",
       x = "dist_unpaved",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_unpaved)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_unpaved",
       y = "dist_unpaved")
#dist_silos
describe(silage$dist_silos)
ggplot(data = silage, aes(x = dist_silos)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_silos",
       x = "dist_silos",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_silos)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_silos",
       y = "dist_silos")
#dist_fridge
describe(silage$dist_fridge)
ggplot(data = silage, aes(x = dist_fridge)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_fridge",
       x = "dist_fridge",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_fridge)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_fridge",
       y = "dist_fridge")
#dist_protected
describe(silage$dist_prot)
ggplot(data = silage, aes(x = dist_prot)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_prot",
       x = "dist_protected",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_prot)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_prot",
       y = "dist_protected")
#dist_settlement
describe(silage$dist_settlement)
ggplot(data = silage, aes(x = dist_settlement)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_settlement",
       x = "dist_settlement",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_settlement)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_settlement",
       y = "dist_settlement")
#dist_rail
describe(silage$dist_rail)
ggplot(data = silage, aes(x = dist_rail)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of dist_rail",
       x = "dist_rail",
       y = "Frequency")
ggplot(data = silage, aes(y = dist_rail)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of dist_rail",
       y = "dist_rail")
#pop_mean
describe(silage$pop_mean)
ggplot(data = silage, aes(x = pop_mean)) +
  geom_histogram(binwidth = 3, color = "black", fill = "lightblue") +
  labs(title = "Histogram of pop_mean",
       x = "pop_mean",
       y = "Frequency")
ggplot(data = silage, aes(y = pop_mean)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of pop_mean",
       y = "pop_mean")
#type
rm(data)
data <- data.frame(
  type = as.character(silage$type)
)
data$type <- factor(data$lc, levels = c("a", "p"))
plot_type <- ggplot(data, aes(x = type)) +
  geom_bar() +
  labs(title = "Type",
       x = "Type",
       y = "Count")
plot_type + theme_minimal()

summary_data <- data %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_type_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: Type") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_type_pie)
summary_data
#crop
rm(data)
data <- data.frame(
  crop = as.character(silage$crop)
)
data[is.na(data)] <- 0
data$crop <- factor(data$crop, levels = c("0", "1", "2", "3", "4", "7","8","9","11","12","13","14","15"))
plot_crop <- ggplot(data, aes(x = crop)) +
  geom_bar() +
  labs(title = "Crop type",
       x = "Categories",
       y = "Count")
plot_crop + theme_minimal()

summary_data <- data %>%
  group_by(crop) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_crop_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = crop)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: crop") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_crop_pie)
summary_data
#fl
rm(data)
data <- data.frame(
  fl = as.character(silage_area_p$fl)
)
data$fl <- factor(data$fl, levels = c("0", "1", "2", "3"))
plot_fl <- ggplot(data, aes(x = fl)) +
  geom_bar() +
  labs(title = "Forest law",
       x = "Categories",
       y = "Count")
plot_fl + theme_minimal()

summary_data <- data %>%
  group_by(fl) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_fl_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = fl)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: fl") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_fl_pie)
summary_data

#time
rm(data)
data <- data.frame(
  time = as.character(silage_area_a$time)
)
data$time <- factor(data$time, levels = c("Dec18", "Jan19", "Feb19", "Mar19", "Apr19", "May19", "Jun19", "Jul19", "Aug19", "Sep19", "Oct19", "Nov19"))
plot_time <- ggplot(data, aes(x = time)) +
  geom_bar() +
  labs(title = "Time",
       x = "Month",
       y = "Count")
plot_time + theme_minimal()

summary_data <- data %>%
  group_by(time) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_time_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = time)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: fl") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_time_pie)
summary_data
### background (big maybe)
summary(background)

##### Variable interactions #####
# Calculate the correlation matrix

silage_cor_matrix <- cor(silage[, c(1:2, 4:11)])
background_cor_matrix <- cor(background)

# Print the correlation matrix
print(silage_cor_matrix)
print(background_cor_matrix)

# Melt the correlation matrix for visualization
silage_cor_matrix_melted <- melt(silage_cor_matrix)
background_cor_matrix_melted <- melt(background_cor_matrix)

# Create a heatmap of the correlation matrix
ggplot(silage_cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()
ggplot(background_cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

#panels
dev.new()
pairs.panels(silage)
dev.new()
pairs.panels(background)



# Factor Analysis of mixed data
famd_result <- FAMD(silage_all, graph = T)  # Set graph to TRUE if you want visualization (requires 'factoextra' package)
famd_result

# Visualization of the variance explained
fviz_eig(famd_result, addlabels = TRUE, ylim = c(0, 50))

# Perform PCA on the dataframe
pca_result <- PCA(silage_all[1:9], graph = T)

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Print the summary of PCA results
summary(pca_result)

##### BRTs for area A########

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
silage_area <- silage[, 2:14]
silage_area_a <- silage_area[silage_area$type == "a", -c(2,7)]

silage_area_a$corn <- ifelse(silage_area_a$crop == 1, 1, 0)
silage_area_a$corn <- as.factor(silage_area_a$corn)
silage_area_a$soy <- ifelse(silage_area_a$crop == 2, 1, 0)
silage_area_a$soy <- as.factor(silage_area_a$soy)
silage_area_a <- silage_area_a[, -9]

train_index <- sample(nrow(silage_area_a), 0.7 * nrow(silage_area_a))
train_data <- silage_area_a[train_index, ]
test_data <- silage_area_a[-train_index, ]

# Train your model without the target variable 'area'
model_area <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area <- predict.gbm(model_area,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area)

plotres(model_area)

#second run without soy, corn, fl, time
silage_area_a2 <- silage_area_a[, -c(4, 5, 7)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_area_a2), 0.7 * nrow(silage_area_a2))
train_data <- silage_area_a2[train_index, ]
test_data <- silage_area_a2[-train_index, ]

# Train your model without the target variable 'area'
model_area2 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area2 <- predict.gbm(model_area2,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area2, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area2)

plotres(model_area2)

#third run without important infrastructure variables silos, settlement, pop_mean, dist_rail
silage_area_a3 <- silage_area_a[, -c(2:5, 7, 8, 10:12)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_area_a3), 0.7 * nrow(silage_area_a3))
train_data <- silage_area_a3[train_index, ]
test_data <- silage_area_a3[-train_index, ]

# Train your model without the target variable 'area'
model_area3 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area3 <- predict.gbm(model_area3,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area3, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area3)

plotres(model_area3)

#fourth run splitting up time
silage_area_a4 <- silage_area_a[, c(1,10)]

silage_area_a4$rain <- ifelse(silage_area_a4$time %in% c("Dec18", "Jan19", "Feb19", "Nov19"), 1, 0)
silage_area_a4$rain <- as.factor(silage_area_a4$rain)
silage_area_a4$spring <- ifelse(silage_area_a4$time %in% c("Oct19", "Nov19"), 1, 0)
silage_area_a4$spring <- as.factor(silage_area_a4$spring)
silage_area_a4$rest <- ifelse(!silage_area_a4$rain & !silage_area_a4$spring, 1, 0)
silage_area_a4$rest <- as.factor(silage_area_a4$rest)
silage_area_a4 <- silage_area_a4[, -c(2,6)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_area_a4), 0.7 * nrow(silage_area_a4))
train_data <- silage_area_a4[train_index, ]
test_data <- silage_area_a4[-train_index, ]

# Train your model without the target variable 'area'
model_area4 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area4 <- predict.gbm(model_area4,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area4, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area4, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area4)

plotres(model_area4)

##### BRTs for area P########

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
silage_area <- silage[, 2:14]
silage_area_p <- silage_area[silage_area$type == "p", -c(2,6,11)]
silage_area_p <- silage_area_p[, -8]

train_index <- sample(nrow(silage_area_p), 0.7 * nrow(silage_area_p))
train_data <- silage_area_p[train_index, ]
test_data <- silage_area_p[-train_index, ]

# Train your model without the target variable 'area'
model_area <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area <- predict.gbm(model_area,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area)

plotres(model_area)

#second run without fl and time
silage_area_p2 <- silage_area_p[, -c(4,7)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_area_p2), 0.7 * nrow(silage_area_p2))
train_data <- silage_area_p2[train_index, ]
test_data <- silage_area_p2[-train_index, ]

# Train your model without the target variable 'area'
model_area2 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area2 <- predict.gbm(model_area2,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area2, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area2)

plotres(model_area2)

#third run without dist_settlement
silage_area_p3 <- silage_area_p[, -c(2:6,7)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_area_p3), 0.7 * nrow(silage_area_p3))
train_data <- silage_area_p3[train_index, ]
test_data <- silage_area_p3[-train_index, ]

# Train your model without the target variable 'area'
model_area3 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area3 <- predict.gbm(model_area3,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area3, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area3)

plotres(model_area3)

##### BRT for type #####

set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
silage_type$type <- as.numeric(ifelse(silage_type$type == "a", 1, 0))
train_index <- sample(nrow(silage_type), 0.7 * nrow(silage_type))
train_data <- silage_type[train_index, ]
test_data <- silage_type[-train_index, ]

# Train your model without the target variable 'type'
model_type <- gbm(type ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_type <- predict.gbm(model_type,newdata = test_data, n.trees = 100)
pred_type <- ifelse(pred_type < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_type, test_data$type, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_type, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_type)

plotres(model_type)

#second model leaving out lc and crop

# Split the data into train and test sets (70% train, 30% test)
silage_type$type <- as.numeric(ifelse(silage_type$type == "a", 1, 0))
silage_type <- silage_type[, c(1:8,11:12)]
train_index <- sample(nrow(silage_type), 0.7 * nrow(silage_type))
train_data <- silage_type[train_index, ]
test_data <- silage_type[-train_index, ]

# Train your model without the target variable 'type'
model_type2 <- gbm(type ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_type2 <- predict.gbm(model_type2,newdata = test_data, n.trees = 100)
pred_type2 <- ifelse(pred_type2 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_type2, test_data$type, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_type2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_type2)

plotres(model_area2)
##### BRT for area including type as predictor #####

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_all), 0.7 * nrow(silage_all))
train_data <- silage_all[train_index, ]
test_data <- silage_all[-train_index, ]

# Train your model without the target variable 'area'
model_area_t <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area_t <- predict.gbm(model_area_t,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area_t, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area_t, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area_t)

plotres(model_area_t)

#second run without lc and crop

silage_all2 <- silage_all[, c(1:9, 12:13)]

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_all2), 0.7 * nrow(silage_all2))
train_data <- silage_all2[train_index, ]
test_data <- silage_all2[-train_index, ]

# Train your model without the target variable 'area'
model_area_t2 <- gbm(area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_area_t2 <- predict.gbm(model_area_t2,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_area_t2, test_data$area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_area_t2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_area_t2)

plotres(model_area_t)
##### BRT for presence A #####

silage_presence_a <- silage[silage$type == "a", -c(1:3, 8, 14)]
silage_presence_a$silage <- T
background_a <- background_a %>%
  rename(dist_prot = dist_protected)
silage_presence_a <- rbind(silage_presence_a, background_a)

silage_presence_a$corn <- ifelse(silage_presence_a$crop == 1, 1, 0)
silage_presence_a$soy <- ifelse(silage_presence_a$crop == 2, 1, 0)
silage_presence_a$corn <- as.factor(silage_presence_a$corn)
silage_presence_a$soy <- as.factor(silage_presence_a$soy)
silage_presence_a <- silage_presence_a[, -8]

set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_a), 0.7 * nrow(silage_presence_a))
train_data <- silage_presence_a[train_index, ]
test_data <- silage_presence_a[-train_index, ]

# Train your model without the target variable 'silage'
model_presence <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence <- predict.gbm(model_presence,newdata = test_data, n.trees = 100)
pred_presence <- ifelse(pred_presence < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence, pmethod = "partdep", all1 = F, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence)

plotres(model_presence)

#presence2 only silos and pop_mean

silage_presence_a2 <- silage_presence_a[, c(4, 7, 9)]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_a2), 0.7 * nrow(silage_presence_a2))
train_data <- silage_presence_a2[train_index, ]
test_data <- silage_presence_a2[-train_index, ]

# Train your model without the target variable 'silage'
model_presence2 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence2 <- predict.gbm(model_presence2,newdata = test_data, n.trees = 100)
pred_presence2 <- ifelse(pred_presence2 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence2, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence2)

plotres(model_presence2)

#presence3 without silos

silage_presence_a3 <- silage_presence_a[, -4]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_a3), 0.7 * nrow(silage_presence_a3))
train_data <- silage_presence_a3[train_index, ]
test_data <- silage_presence_a3[-train_index, ]

# Train your model without the target variable 'silage'
model_presence3 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence3 <- predict.gbm(model_presence3,newdata = test_data, n.trees = 100)
pred_presence3 <- ifelse(pred_presence3 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence3, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence3)

plotres(model_presence3)

#presence3 without pop_mean

silage_presence_a3 <- silage_presence_a[, -7]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_a3), 0.7 * nrow(silage_presence_a3))
train_data <- silage_presence_a3[train_index, ]
test_data <- silage_presence_a3[-train_index, ]

# Train your model without the target variable 'silage'
model_presence3 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence3 <- predict.gbm(model_presence3,newdata = test_data, n.trees = 100)
pred_presence3 <- ifelse(pred_presence3 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence3, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence3)

plotres(model_presence3)

#presence3 without pop_mean and silos

silage_presence_a3 <- silage_presence_a[, -c(4,7)]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_a3), 0.7 * nrow(silage_presence_a3))
train_data <- silage_presence_a3[train_index, ]
test_data <- silage_presence_a3[-train_index, ]

# Train your model without the target variable 'silage'
model_presence3 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence3 <- predict.gbm(model_presence3,newdata = test_data, n.trees = 100)
pred_presence3 <- ifelse(pred_presence3 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence3, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence3)

plotres(model_presence3)
##### BRT for presence P #####

silage_presence_p <- silage[silage$type == "p", -c(1:3, 7, 12, 14)]
silage_presence_p$silage <- T
background_p <- background_p %>%
  rename(dist_prot = dist_protected)
silage_presence_p <- rbind(silage_presence_p, background_p)

set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_p), 0.7 * nrow(silage_presence_p))
train_data <- silage_presence_p[train_index, ]
test_data <- silage_presence_p[-train_index, ]

# Train your model without the target variable 'silage'
model_presence <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence <- predict.gbm(model_presence,newdata = test_data, n.trees = 100)
pred_presence <- ifelse(pred_presence < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence, pmethod = "partdep", all1 = F, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence)

plotres(model_presence)

#presence2 without unpaved, paved, settlement, fl

silage_presence_p2 <- silage_presence_p[, -c(1, 2, 6, 8)]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_p2), 0.7 * nrow(silage_presence_p2))
train_data <- silage_presence_p2[train_index, ]
test_data <- silage_presence_p2[-train_index, ]

# Train your model without the target variable 'silage'
model_presence2 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence2 <- predict.gbm(model_presence2,newdata = test_data, n.trees = 100)
pred_presence2 <- ifelse(pred_presence2 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence2, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence2)

plotres(model_presence2)

#presence3 without pop_mean

silage_presence_p3 <- silage_presence_p[, -c(6,7)]
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(silage_presence_p3), 0.7 * nrow(silage_presence_p3))
train_data <- silage_presence_p3[train_index, ]
test_data <- silage_presence_p3[-train_index, ]

# Train your model without the target variable 'silage'
model_presence3 <- gbm(silage ~ ., data = train_data, n.trees = 100, interaction.depth = 3)

pred_presence3 <- predict.gbm(model_presence3,newdata = test_data, n.trees = 100)
pred_presence3 <- ifelse(pred_presence3 < 0.5, 0, 1)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_presence3, test_data$silage, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")
# Dependent variable was transformed into continuous and then rounded back for the model.

x11()
plotmo(model_presence3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies",)
x11()
summary(model_presence3)

plotres(model_presence3)
######fancy illustrative graphs 
# for illustrating the most important variable(s) after brt / split continuous variables into quartiles -> categories
ggplot(data = silage) + aes(x = area, fill = type) + geom_density(alpha = 0.3)
ggplot(data = silage_all) + aes(x = dist_protected, fill = fl) + geom_density(alpha = 0.3)
silage_all$dist_rail_q <- cut(silage_all$dist_rail, breaks = 4)
ggplot(data = silage_all) + aes(x = area, fill = dist_rail_q) + geom_density(alpha = 0.3) + xlim(c(0, 20000))
ggplot(data = silage) + aes(x = area, fill = type) + geom_density(alpha = 0.3) + xlim(c(0, 10000)) 
silage_all$dist_settlement_q <- cut(silage_all$dist_settlement, breaks = 4)
ggplot(data = silage_all) + aes(x = area, fill = dist_settlement_q) + geom_density(alpha = 0.3) + xlim(c(0, 20000))
silage_presence_a$dist_silos_q <- cut(silage_presence_a$dist_silos, breaks = 4)
ggplot(data = silage_presence_a) + aes(x = dist_silos, fill = silage) + geom_density(alpha = 0.3)
ggplot(data = silage_presence_a) + aes(x = dist_settlement, fill = silage) + geom_density(alpha = 0.3)
ggplot(data = silage_presence_a) + aes(x = dist_rail, fill = silage) + geom_density(alpha = 0.3)

##### Part 2: Silage intensity######

##### data #####

Grid10 <- read.csv2("Grid10.csv")
Grid10 <- Grid10[, -1]
Grid10[is.na(Grid10)] <- 0
Grid10$fl <- as.factor(Grid10$fl)
Grid10$crop <- ifelse(Grid10$crop %in% c(0, 3), 1,
                      ifelse(Grid10$crop %in% c(1, 2), 2,
                             ifelse(Grid10$crop %in% c(4, 5, 6, 11, 12, 13, 14), 3,
                                    ifelse(Grid10$crop == 7, 4,
                                           ifelse(Grid10$crop %in% c(8, 9, 10, 15), 0, Grid10$crop)))))
Grid10$crop <- as.factor(Grid10$crop)
Grid10$lc <- ifelse(Grid10$lc %in% c(3, 4, 6, 45), 1,
                      ifelse(Grid10$lc == 9, 2,
                             ifelse(Grid10$lc %in% c(11, 42, 43), 3,
                                    ifelse(Grid10$lc == 15, 4,
                                           ifelse(Grid10$lc %in% c(57, 58), 5,
                                                  ifelse(Grid10$lc %in% c(0, 22, 26), 0, Grid10$lc))))))
Grid10$lc <- as.factor(Grid10$lc)

Grid10 <- Grid10[, -c(12, 13)]
##### Calculate summary statistics #####
summary(Grid10_2)

#unpaved
describe(Grid10$unpaved)
ggplot(data = Grid10, aes(x = unpaved)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of unpaved",
       x = "unpaved",
       y = "Frequency")
ggplot(data = Grid10, aes(y = unpaved)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of unpaved",
       y = "unpaved")

#paved
describe(Grid10$paved)
ggplot(data = Grid10, aes(x = paved)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of paved",
       x = "paved",
       y = "Frequency")
ggplot(data = Grid10, aes(y = paved)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of paved",
       y = "paved")

#rail
describe(Grid10$rail)
ggplot(data = Grid10, aes(x = rail)) +
  geom_histogram(binwidth = 10000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of rail",
       x = "rail",
       y = "Frequency")
ggplot(data = Grid10, aes(y = rail)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of rail",
       y = "rail")

#silos
describe(Grid10$silos)
ggplot(data = Grid10, aes(x = silos)) +
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue") +
  labs(title = "Histogram of silos",
       x = "silos",
       y = "Frequency")
ggplot(data = Grid10, aes(y = silos)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of silos",
       y = "silos")

#fridges
describe(Grid10$fridges)
ggplot(data = Grid10, aes(x = fridges)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histogram of fridges",
       x = "fridges",
       y = "Frequency")
ggplot(data = Grid10, aes(y = fridges)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of fridges",
       y = "fridges")

#settlements
describe(Grid10$settlements)
ggplot(data = Grid10, aes(x = settlements)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histogram of settlements",
       x = "settlements",
       y = "Frequency")
ggplot(data = Grid10, aes(y = settlements)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of settlements",
       y = "settlements")

#protected_area
describe(Grid10$protected_area)
ggplot(data = Grid10, aes(x = protected_area)) +
  geom_histogram(binwidth = 10000000, color = "black", fill = "lightblue") +
  labs(title = "Histogram of protected_area",
       x = "protected_area",
       y = "Frequency")
ggplot(data = Grid10, aes(y = protected_area)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of protected_area",
       y = "protected_area")

#fl
rm(data)
data <- data.frame(
  fl = as.character(Grid10$fl)
)
data$fl <- factor(data$fl, levels = c("0", "1", "2", "3"))
plot_fl <- ggplot(data, aes(x = fl)) +
  geom_bar() +
  labs(title = "Forest law",
       x = "Categories",
       y = "Count")
plot_fl + theme_minimal()

summary_data <- data %>%
  group_by(fl) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_fl_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = fl)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: fl") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_fl_pie)
summary_data
#crop
rm(data)
data <- data.frame(
  crop = as.character(Grid10$crop)
)
data$crop <- factor(data$crop, levels = c("0", "1", "2", "3", "7","8","12","13","15"))
plot_crop <- ggplot(data, aes(x = crop)) +
  geom_bar() +
  labs(title = "Crop type",
       x = "Categories",
       y = "Count")
plot_crop + theme_minimal()

summary_data <- data %>%
  group_by(crop) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_crop_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = crop)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: crop") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_crop_pie)
summary_data

#lc
rm(data)
data <- data.frame(
  lc = as.character(Grid10$lc)
)
data$lc <- factor(data$lc, levels = c("0", "3", "4", "6", "9", "11","15","22","26","42","43","45","57","58"))
plot_lc <- ggplot(data, aes(x = lc)) +
  geom_bar() +
  labs(title = "Land cover",
       x = "Categories",
       y = "Count")
plot_lc + theme_minimal()

summary_data <- data %>%
  group_by(lc) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

plot_lc_pie <- ggplot(summary_data, aes(x = "", y = prop, fill = lc)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Proportions of Categories: lc") +
  theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(), # Remove axis titles
        panel.grid = element_blank(), # Remove grid lines
        plot.margin = unit(rep(1, 4), "cm"))  # Adjust plot margins for better appearance
print(plot_lc_pie)

#pop_mean
describe(Grid10$pop_mean)
ggplot(data = Grid10, aes(x = pop_mean)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histogram of pop_mean",
       x = "pop_mean",
       y = "Frequency")
ggplot(data = Grid10, aes(y = pop_mean)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of pop_mean",
       y = "pop_mean")

#agri_count
describe(Grid10$agri_count)
ggplot(data = Grid10, aes(x = agri_count)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of agri_count",
       x = "agri_count",
       y = "Frequency")
ggplot(data = Grid10, aes(y = agri_count)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of agri_count",
       y = "agri_count")

#past_count
describe(Grid10$past_count)
ggplot(data = Grid10, aes(x = past_count)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of past_count",
       x = "past_count",
       y = "Frequency")
ggplot(data = Grid10, aes(y = past_count)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of past_count",
       y = "past_count")

#total_count
describe(Grid10$total_count)
ggplot(data = Grid10, aes(x = total_count)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  labs(title = "Histogram of total_count",
       x = "total_count",
       y = "Frequency")
ggplot(data = Grid10, aes(y = total_count)) +
  geom_boxplot(color = "darkblue", fill = "lightyellow") +
  labs(title = "Box Plot of total_count",
       y = "total_count")

##### Variable interactions #####

# Calculate the correlation matrix
Grid10_cor_matrix <- cor(Grid10[,c(1:8)])

# Print the correlation matrix
print(Grid10_cor_matrix)

# Melt the correlation matrix for visualization
Grid10_cor_matrix_melted <- melt(Grid10_cor_matrix)

# Create a heatmap of the correlation matrix

ggplot(Grid10_cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

#panels
dev.new()
pairs.panels(Grid10)

# Factor Analysis of mixed data
famd_result <- FAMD(Grid10, graph = T)  # Set graph to TRUE if you want visualization (requires 'factoextra' package)
famd_result

# Visualization of the variance explained
fviz_eig(famd_result, addlabels = TRUE, ylim = c(0, 50))

# Perform PCA on the dataframe
pca_result <- PCA(Grid10[,c(1:9, 13:17)], graph = T)

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Print the summary of PCA results
summary(pca_result)

##### BRT Intensity by area #####
Grid10_1 <- Grid10[, c(1:6, 9:13, 16)]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_1), 0.7 * nrow(Grid10_1))
train_data <- Grid10_1[train_index, ]
test_data <- Grid10_1[-train_index, ]

# Train your model without the target variable 'area'
model_int1 <- gbm(silage_area_abs ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int1 <- predict.gbm(model_int1,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int1, test_data$silage_area_abs, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int1, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int1)

plotres(model_int1)
##### BRT Intensity by count #####
Grid10_2 <- Grid10[, -c(9, 10)]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_2), 0.7 * nrow(Grid10_2))
train_data <- Grid10_2[train_index, ]
test_data <- Grid10_2[-train_index, ]

# Train your model without the target variable 'total_count'
model_int2 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int2 <- predict.gbm(model_int2,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int2, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int2, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int2)

plotres(model_int2)

#model 2 without silos
Grid10_3 <- Grid10_2[, -4]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_3), 0.7 * nrow(Grid10_3))
train_data <- Grid10_3[train_index, ]
test_data <- Grid10_3[-train_index, ]

# Train your model without the target variable 'total_count'
model_int3 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int3 <- predict.gbm(model_int3,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int3, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int3)

plotres(model_int3)

#model 3 without silos and crop
Grid10_4 <- Grid10_3[, -1]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_4), 0.7 * nrow(Grid10_4))
train_data <- Grid10_4[train_index, ]
test_data <- Grid10_4[-train_index, ]

# Train your model without the target variable 'total_count'
model_int4 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int4 <- predict.gbm(model_int4,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int4, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int4, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int4)

plotres(model_int4)

#model 4 without silos and crop and unpaved
Grid10_5 <- Grid10_4[, -6]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_5), 0.7 * nrow(Grid10_5))
train_data <- Grid10_5[train_index, ]
test_data <- Grid10_5[-train_index, ]

# Train your model without the target variable 'total_count'
model_int5 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int5 <- predict.gbm(model_int5,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int5, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int5, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int5)

plotres(model_int5)

#model 5 without silos and crop and unpaved and lc
Grid10_6 <- Grid10_5[, -2]
#Grid10_6$total_count <- Grid10_6$total_count * -1

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_6), 0.7 * nrow(Grid10_6))
train_data <- Grid10_6[train_index, ]
test_data <- Grid10_6[-train_index, ]

# Train your model without the target variable 'total_count'
model_int6 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int6 <- predict.gbm(model_int6,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int6, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int6, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int6)

plotres(model_int6)

#model 6 focus on infrastructure
Grid10_6 <- Grid10_2[, -c(5, 6, 7, 8, 9)]
#Grid10_6$total_count <- Grid10_6$total_count * -1

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_6), 0.7 * nrow(Grid10_6))
train_data <- Grid10_6[train_index, ]
test_data <- Grid10_6[-train_index, ]

# Train your model without the target variable 'total_count'
model_int6 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int6 <- predict.gbm(model_int6,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int6, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int6, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int6)

plotres(model_int6)

#model 6 focus on infrastructure
Grid10_6 <- Grid10_2[, c(6, 8, 10)]
#Grid10_6$total_count <- Grid10_6$total_count * -1

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_6), 0.7 * nrow(Grid10_6))
train_data <- Grid10_6[train_index, ]
test_data <- Grid10_6[-train_index, ]

# Train your model without the target variable 'total_count'
model_int6 <- gbm(total_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int6 <- predict.gbm(model_int6,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int6, test_data$total_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int6, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int6)

plotres(model_int6)
##### BRT	Relative agri / past by area #####
Grid10_3 <- Grid10[, c(1:6, 9:14)]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_3), 0.7 * nrow(Grid10_3))
train_data <- Grid10_3[train_index, ]
test_data <- Grid10_3[-train_index, ]

# Train your model without the target variable 'area'
model_int3 <- gbm(agri.past_area ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int3 <- predict.gbm(model_int3,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int3, test_data$agri.past_area, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int3, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int3)

plotres(model_int3)
##### BRT	Relative agri / past by count #####
Grid10_4 <- Grid10[, c(1:6, 9:13, 15)]

# Set a seed for reproducibility
set.seed(123)

# Split the data into train and test sets (70% train, 30% test)
train_index <- sample(nrow(Grid10_4), 0.7 * nrow(Grid10_4))
train_data <- Grid10_4[train_index, ]
test_data <- Grid10_4[-train_index, ]

# Train your model without the target variable 'area'
model_int4 <- gbm(agri.past_count ~ ., data = train_data, n.trees = 100, interaction.depth = 3)
pred_int4 <- predict.gbm(model_int4,newdata = test_data, n.trees = 100)

# Calculate the Spearman correlation
spearman_corr <- cor(pred_int4, test_data$agri.past_count, method = "spearman", use = "all.obs")
cat("Spearman Correlation:", spearman_corr, "\n")

x11()
plotmo(model_int4, pmethod = "partdep", all1 = T, degree2 = F, caption = "Partial dependencies", )
x11()
summary(model_int4)

plotres(model_int3)