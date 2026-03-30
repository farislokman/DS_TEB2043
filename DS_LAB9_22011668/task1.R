# --- 1.LOAD ---
  
  library(corrplot)
  library(RColorBrewer)
  library(lattice)
  library(reshape2)
  library(ggplot2)

# --- 2. DATA PREPARATION ---
data <- ToothGrowth
data$supp <- as.numeric(as.factor(data$supp))

# --- 3. CORRELATION ANALYSIS ---
dose_corr <- cor(data$dose, data$len, method="pearson")
M <- cor(data)

# --- 4. VISUALIZATION ---
# Fancy corrplot
corrplot(M, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdYlBu"))

# Heatmap Construction
corr_mat <- round(cor(data), 2)
melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label = value), color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), name="Pearson") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

# --- 5. OBSERVATIONS ---
cat("\n1. Relationship Strength:", round(dose_corr, 2), 
    "\n2. Image generated")