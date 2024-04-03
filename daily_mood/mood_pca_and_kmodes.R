
library(tidyverse)

dat<-use("C:\\Users\\Andrew.Mccartney2\\Desktop\\daily_mood\\dailybean.xlsx") %>% 
  mutate(day = ymd(day)) %>% 
  rename(feeling_pit_a_pat = `feeling_pit-a-pat`)%>%
  filter(across(everything(), ~ !is.na(.x)))


dates<-use("C:\\Users\\Andrew.Mccartney2\\Desktop\\daily_mood\\dailybean.xlsx") %>% 
  mutate(day = ymd(day)) %>% 
  filter(across(everything(), ~ !is.na(.x)))
dat <- dat %>% 
  select(rating, everything()) %>%
  select(where(is.numeric))

pca_result <- prcomp(dat[, -1], scale. = TRUE)  # Exclude the target variable (rating) from the analysis
pca_result
summary(pca_result)

# Loadings
loadings(pca_result)
loadings <- pca_result$rotation
loadings

plot(pca_result, type = "l")



biplot(pca_result, scale = 0)


library(ggplot2)

# Assuming your PCA result object is stored in pca_result
eigenvalues <- pca_result$sdev^2

# Create a data frame for plotting
scree_data <- data.frame(
  PC = 1:length(eigenvalues),
  Eigenvalue = eigenvalues
)

# Plot the scree plot
ggplot(scree_data, aes(x = PC, y = Eigenvalue)) +
  geom_point() +
  geom_line() +
  labs(x = "Principal Component", y = "Eigenvalue") +
  ggtitle("Scree Plot")


# Extract loadings
loadings <- as.data.frame(pca_result$rotation[, 1:4])
loadings$Variable <- rownames(loadings)
loadings
# Reshape data for ggplot
loadings_long <- tidyr::pivot_longer(loadings, cols = c(PC1, PC2, PC3, PC4),
                                     names_to = "Principal_Component", values_to = "Loading")


loadings_long
# Plot

pc1<-loadings_long %>% 
  filter(Principal_Component=="PC1") %>% 
  mutate(posneg = if_else(Loading > 0, 1, 0)) %>% 
 ggplot( aes(y = fct_reorder(Variable,abs(Loading)), 
            x = (Loading), 
            fill = factor(posneg))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Absolute Loading", fill = "") +
  ggtitle("PC 1: isolated and emotionally turbulent") + 
  guides(fill = "none")+
  theme_tufte()


pc2<-loadings_long %>% 
  filter(Principal_Component=="PC2") %>% 
  mutate(posneg = if_else(Loading > 0, 1, 0)) %>% 
  ggplot( aes(y = fct_reorder(Variable,abs(Loading)), 
              x = (Loading), 
              fill = factor(posneg))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Absolute Loading", fill = "") +
  ggtitle("PC2: Tense Restlessness") + 
  guides(fill = "none")+
  theme_tufte()

pc3<-loadings_long %>% 
  filter(Principal_Component=="PC3") %>% 
  mutate(posneg = if_else(Loading > 0, 1, 0)) %>% 
  ggplot( aes(y = fct_reorder(Variable,abs(Loading)), 
              x = (Loading), 
              fill = factor(posneg))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Absolute Loading", fill = "") +
  ggtitle("PC3: Gloomy Yet Social") + 
  guides(fill = "none")+
  theme_tufte()
pc4<-loadings_long %>% 
  filter(Principal_Component=="PC4") %>% 
  mutate(posneg = if_else(Loading > 0, 1, 0)) %>% 
  ggplot( aes(y = fct_reorder(Variable,abs(Loading)), 
              x = (Loading), 
              fill = factor(posneg))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Absolute Loading", fill = "") +
  ggtitle("PC4: day off excitement") + 
  guides(fill = "none")+
  theme_tufte()

library(patchwork) 
(pc1 + pc2) / (pc3 + pc4)





# custom biplot -----------------------------------------------------------

pc_scores <- as.data.frame(pca_result$x[, 1:4])
pc_scores
# Create a dataframe combining PC1 and PC2 scores with observation names
data_plot <- cbind(pc_scores, Observation = rownames(pc_scores))
data_plot


# Plot the biplot
ggplot(data_plot, aes(x = PC1, y = PC2, label = Observation)) +
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.1) +  # Adjust the position of the text labels
  labs(x = "Isolated and Turbulent", y = "Tense and Restless") +
  ggtitle("Biplot of PC1 and PC2: Isolated and Emotionally Turbulent vs Tense and Restless")+ 
  theme_tufte()



# kmodes ------------------------------------------------------------------

install.packages("klaR")
library(klaR)
dat
cluster.results <-kmodes(dat[,2:23], 3, iter.max = 10, weighted = FALSE )
cluster.results

cluster.output <- cbind(dat,cluster.results$cluster)
cluster.output

# re-join data ------------------------------------------------------------
dates <- dates %>% select(day)
learned_data <- cluster.output %>% 
  as_tibble() %>% 
  rename(cluster = `cluster.results$cluster`)  %>% 
  bind_cols(dates) %>% 
  bind_cols(pc_scores) %>% 
  select(day, rating, cluster, PC1, PC2, PC3, PC4, everything()) 
learned_data  %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(cluster), label = day))+
  geom_point() +
  labs(
    title = "Day clusters by principle component", 
    x = "Isolated and Turbulent", y = "Tense and Restless", 
    fill = "Kmodes Cluster"
  )+
  theme_tufte()


learned_data %>% 
  group_by(cluster) %>% 
  summarize(mu = mean(rating),
            sigma = sd(rating))

# kmodes2 ------------------------------------------------------------------

library(klaR)
dat
cluster.results <-kmodes(dat[,1:23], 2, iter.max = 10, weighted = FALSE )
cluster.output <- cbind(dat,cluster.results$cluster)

