
library(neuralnet)
library(NeuralNetTools) # via 

library(tidyverse)

dat<-use("C:\\Users\\Andrew.Mccartney2\\Desktop\\daily_mood\\dailybean.xlsx") %>% 
  mutate(day = ymd(day)) %>% 
  rename(feeling_pit_a_pat = `feeling_pit-a-pat`)
dat<-dat %>%
  filter(across(everything(), ~ !is.na(.x)))
dat


# prepare the data ------------------------------------------
# 
# (we need to scale the variables, recode the DV, and split into test/train)


# Create Vector of Column Max and Min Values
maxs <- apply(dat[,3:24], 2, max)
mins <- apply(dat[,3:24], 2, min)
maxs # first argument is data, second is 1 (rows) or 2 (columns) and third is function to use. 
mins # essentially makes a list of all the data elements by max value and min value for each 
# variable

# Use scale() and convert the resulting matrix to a data frame
scaled_data <- dat %>% 
  select(-day, -rating) %>% 
  scale(center = mins, scale = maxs - mins) %>% 
  as_tibble()

# Check out results
scaled_data

ratings <- dat %>% 
  select(day, rating)

# rejoin private with the scaled data. 
df <- cbind(ratings, scaled_data) %>% 
  as_tibble() 
# Machine Learning Experts in the audience - what is the relative
# benefit of scaling? 

# Now that we have centered the data
set.seed(123) # your number here. 

# Split the data into 70/30 proportions at random:
training_data <- df %>% 
  sample_frac(0.70, replace = FALSE)
test_data <- df %>% 
  anti_join(training_data, by = 'day')




# Define all features
features <- c("social_acquaintaince", "social_friend", "social_family", 
              "social_none", "weekday", "holiday", "feeling_excited", 
              "feeling_relaxed", "feeling_proud", "feeling_hopeful", 
              "feeling_happy", "feeling_enthusiastic", "feeling_pit_a_pat", 
              "feeling_refreshed", "feeling_gloomy", "feeling_lonely", 
              "feeling_anxious", "feeling_sad", "feeling_angry", 
              "feeling_burdensome", "feeling_annoyed", "feeling_tired")

formula <- as.formula(paste("rating ~", paste(features, collapse = " + ")))

# Print the formula
print(formula)
architecture <- c(10)
nn <- neuralnet(formula, training_data, architecture, linear.output=TRUE)
# nn <- neuralnet(f, training_data, hidden=12, linear.output=FALSE) # one hidden layer



# You can use base R plotting to see the net
plot(nn) 

# Compute Predictions off Test Set
predicted_nn <- neuralnet::compute(nn,test_data[3:24])

# Check out net.result
head(predicted_nn$net.result)

predicted_nn$net.result <- sapply(predicted_nn$net.result,round,digits=0)
table(test_data$rating, predicted_nn$net.result)


plotnet(nn, alpha.val=0.8)
#
garson(nn) # this algorithm apparently only works with 1 H layer
class(nn)
lekprofile(nn) # same here  
