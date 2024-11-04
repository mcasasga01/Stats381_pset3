### Libraries 
library(ggplot2)
library(cowplot) 
library(tidyverse)
library(dplyr)

### Uploading data 
gss <- read_csv("/Users/michellecasas/Documents/R/Stats381_pset3/gss.csv")


############################## Cleaning ########################################
################################################################################

# Changing variable names to lowercase 
colnames(gss) <- tolower(colnames(gss))


# Creating subset of data and cleaning key variables
gss_clean <- gss %>% 
  mutate(peduc = pmax(padeg, madeg, na.rm = TRUE), # creating var called "peduc" of highest parental education attainment
         peduc = ifelse(peduc %in% c(-100, -99, -98, -97), NA, peduc), # replacing neg values w/ NA for peduc
         degree = ifelse(degree %in% c(-99, -98, -97), NA, degree), # replacing neg values w NA for degree
         birthyear = (year - age), # getting the respondents birthyear
         decade = floor(birthyear / 10) * 10) %>% #Creating a variable for each decade
         filter(born == 1, age >= 30, decade > 1930, decade < 1990) # filtering to those born in the US and within ranges specified

# Calculate conditional probabilities by decade
conditional_probs <- gss_clean %>%
  filter(!is.na(degree) & !is.na(peduc)) %>%
  group_by(decade, peduc) %>%
  count(degree) %>% 
  mutate(probability = n / sum(n)) %>%  # Calculate conditional probability
  ungroup()


conditional_probs$degree <- factor(conditional_probs$degree, 
                                   levels = c(0, 1, 2, 3, 4), 
                                   labels = c("<HS", "HS", "AA", "BA", "MA+"))
conditional_probs$peduc <- factor(conditional_probs$peduc, 
                                  levels = c(4, 3, 2, 1, 0), 
                                  labels = c("MA+", "BA", "AA", "HS", "<HS"))

# Calculate Rho by decade
rho <- gss_clean %>%
  group_by(decade) %>%
  summarize(rho = cor(as.numeric(degree), as.numeric(peduc), method = "spearman", use = "complete.obs")) %>%
  ungroup()

overall_rho <- cor(as.numeric(rho$degree), as.numeric(rho$peduc), method = "spearman", use = "pairwise.complete.obs")

############################## Plotting ########################################
################################################################################
### main plot: conditional probabilities 
plot_main <- ggplot(conditional_probs, aes(x = degree, y = peduc, fill = probability)) +
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  labs(
    x = "Offspring Education (Destination)", 
    y = "Parent Education (Origin)", 
    fill = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()  
  ) +
  scale_x_discrete(position = "top") 

### smaller plot: overall rho
plot_overall <- ggplot(conditional_probs, aes(x = degree, y = peduc, fill = probability)) +
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  labs(
    title = "Observed Overall", 
    x = "(Rho = .42)", 
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none", 
    axis.text.y = element_blank(),  # Remove y-axis text labels
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    plot.title = element_text(hjust = 0.5)
  )

### Max Equality 
# Creating fake dataset to plot 

decade <- c(1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980)

degree <- c("MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS")

peduc <- c("<HS", "<HS", "<HS", "<HS", "<HS", 
           "HS", "HS", "HS", "HS", "HS",
           "AA", "AA", "AA", "AA", "AA",
           "BA", "BA", "BA", "BA", "BA",
           "MA+", "MA+", "MA+", "MA+", "MA+") 

n <- c(100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100)

probability <- c(.2, .9, .4, .5, .4, 
                 .2, .9, .4, .5, .4,
                 .2, .9, .4, .5, .4,
                 .2, .9, .4, .5, .4,
                 .2, .9, .4, .5, .4)

df <- data.frame(decade, peduc, degree, n, probability)
df$peduc <- factor(df$peduc)

plot_equality <- ggplot(df, aes(x = degree, y = peduc, fill = probability)) +
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  scale_y_discrete(limits = rev(levels(df$peduc))) +
  labs(
    title = "Max Equality", 
    x = "(Rho = 0)", 
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none", 
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    plot.title = element_text(hjust = 0.5)
  )

### Max Inequality 
# I don't know what I'm supposed to be doing for this one, tbh 
# Creating fake dataset to plot

decade <- c(1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980,
            1940, 1950, 1960, 1970, 1980)

degree <- c("MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS",
            "MA+", "BA", "AA", "HS", "<HS")
  
peduc <- c("<HS", "<HS", "<HS", "<HS", "<HS", 
           "HS", "HS", "HS", "HS", "HS",
           "AA", "AA", "AA", "AA", "AA",
           "BA", "BA", "BA", "BA", "BA",
           "MA+", "MA+", "MA+", "MA+", "MA+") 

n <- c(100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100,
       100, 100, 100, 100, 100)

probability <- c(0, 0, 0, 0, 1, 
                 0, 0, 0, 1, 0, 
                 0, 0, 1, 0, 0,
                 0, 1, 0, 0, 0,
                 1, 0, 0, 0, 0)

df <- data.frame(decade, peduc, degree, n, probability)
df$peduc <- factor(df$peduc)
  
plot_inequality <- ggplot(df, aes(x = degree, y = peduc, fill = probability)) +
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  scale_y_discrete(limits = rev(levels(df$peduc))) +
  labs(
    title = "Max Inequality",
    x = "(Rho = 1)", 
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    plot.title = element_text(hjust = 0.5)
  )


# Arrange the plots
combined_plot <- plot_grid(
  plot_main,
  plot_grid(plot_equality, plot_overall, plot_inequality, ncol = 3),
  nrow = 2, rel_heights = c(2, 1)
)


