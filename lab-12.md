Lab 13 - Colonizing Mars
================
Benjamin Egan
Start date 1-29

Link to
assignment:<https://datascience4psych.github.io/DataScience4Psych/lab13.html>

Link to archived
assignment:<https://web.archive.org/web/20250414135229/https://datascience4psych.github.io/DataScience4Psych/lab13.html#exercise-4-preparing-for-the-unexpected>

### Exercise 1 - Simulating colonists

``` r
set.seed(123)

age <- rnorm(100, mean = 30, sd = 5)

df_colonists <- data.frame(id = 1:100, age)
```

``` r
data.frame(df_colonists) %>%
ggplot(
 aes(x=age)
)+
  theme_bw()+
  geom_histogram(binwidth=2,color="black", fill="blue") +
  labs(title = "Histogram of Colonists Ages by Seed",
       x = "Age", y = "Frequency")
```

![](lab-12_files/figure-gfm/1.1%20-%20visualizing%20base%20colonists-1.png)<!-- -->

This is an age distribution graph. It created a roughly normal
distribution that is centered at 30. It looks like changing the seed
makes the distribution more normal. The 100 seed looks normally
distributed and the 1000 seed even more so. Looks like it’s due to
random chance, as 999 seed doesn’t have the nice bellcurve shape.

``` r
role <- rep(c("engineer", "scientist", "medic"), 
             each = 34, 
             length.out = 100)

df_colonists <- df_colonists %>%
  mutate(role)

print(role)
```

    ##   [1] "engineer"  "engineer"  "engineer"  "engineer"  "engineer"  "engineer" 
    ##   [7] "engineer"  "engineer"  "engineer"  "engineer"  "engineer"  "engineer" 
    ##  [13] "engineer"  "engineer"  "engineer"  "engineer"  "engineer"  "engineer" 
    ##  [19] "engineer"  "engineer"  "engineer"  "engineer"  "engineer"  "engineer" 
    ##  [25] "engineer"  "engineer"  "engineer"  "engineer"  "engineer"  "engineer" 
    ##  [31] "engineer"  "engineer"  "engineer"  "engineer"  "scientist" "scientist"
    ##  [37] "scientist" "scientist" "scientist" "scientist" "scientist" "scientist"
    ##  [43] "scientist" "scientist" "scientist" "scientist" "scientist" "scientist"
    ##  [49] "scientist" "scientist" "scientist" "scientist" "scientist" "scientist"
    ##  [55] "scientist" "scientist" "scientist" "scientist" "scientist" "scientist"
    ##  [61] "scientist" "scientist" "scientist" "scientist" "scientist" "scientist"
    ##  [67] "scientist" "scientist" "medic"     "medic"     "medic"     "medic"    
    ##  [73] "medic"     "medic"     "medic"     "medic"     "medic"     "medic"    
    ##  [79] "medic"     "medic"     "medic"     "medic"     "medic"     "medic"    
    ##  [85] "medic"     "medic"     "medic"     "medic"     "medic"     "medic"    
    ##  [91] "medic"     "medic"     "medic"     "medic"     "medic"     "medic"    
    ##  [97] "medic"     "medic"     "medic"     "medic"

Here I assigned each of them roles. I chose this method mostly out of
pure randomness. But I like that it listed out each role together.

#### Health can be measured using the MARSGAR

``` r
set.seed(123)

df_colonists$marsgar <- runif(100, min = 40, max = 100)

df_colonists %>%
ggplot( aes(
    x = age,
    y = marsgar
  ) )+
  theme_bw()+
  geom_point()+
  geom_smooth(method=lm, color = "black",se = FALSE)+
  labs(
      x = "Age",
      y = "MARSGAR",
      title = "Scatterplot of Age and Health"
  )
```

![](lab-12_files/figure-gfm/1.4%20heathy-1.png)<!-- -->

### Exercise 2 - Growing the colonists’ skills

``` r
set.seed(1235)

df_colonists$technical_skills <- 2 * df_colonists$age + rnorm(100, mean = 0, sd = 1)


df_colonists %>%
lm(technical_skills = 2 * age + noise)
```

    ## 
    ## Call:
    ## lm(formula = ., technical_skills = 2 * age + noise)
    ## 
    ## Coefficients:
    ##      (Intercept)               age         rolemedic     rolescientist  
    ##         11.42824           0.93744          67.73639          35.05438  
    ##          marsgar  technical_skills  
    ##          0.08384          -0.47441

``` r
df_colonists %>%
  ggplot(aes(
    x = age,
    y = technical_skills
  ))+
  geom_point(alpha = .6)+
  geom_line(stat = "smooth", method="lm", alpha = .6, color = "blue", se = FALSE)+
  labs(
    x = "Age",
    y = "Technical Skill",
    title = "Scatterplot of Colonist Age and Technical Skills"
  )
```

![](lab-12_files/figure-gfm/2.1%20technical%20skills%20and%20visual-1.png)<!-- -->

Here we can see that age is about evenly distributed among the roles

``` r
df_colonists %>%
  ggplot(aes(
    x = age,
    y = technical_skills,
    color = as.factor(role)
  ))+
  geom_point(alpha = .6)+
  scale_color_manual(values=c("blue", "red","green4"))+
  geom_line(stat = "smooth", method="lm", alpha = .6, color = "black", se = FALSE)+
  labs(
    x = "Age",
    y = "Technical Skill",
    color = "Role",
    title = "Scatterplot of Colonist Age and Technical Skills by Role"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-12_files/figure-gfm/Plot%20age%20and%20tehcnical%20skill%20by%20role-1.png)<!-- -->

### Problem solving for each colonist

``` r
d_engineer <- rnorm(100, mean = 40, sd = 5)
d_scientist <- rnorm(100, mean = 60, sd = 7)
d_medic <- rnorm(100, mean = 90, sd = 2)



df_colonists <- df_colonists %>%
mutate(problem_solving = case_when(
    role == "engineer" ~ d_engineer,
    role == "scientist" ~ d_scientist,
    role == "medic" ~ d_medic
  ))


df_colonists %>%
  ggplot(aes(
    x = problem_solving,
    color = role,
    fill = role
  ))+
scale_fill_manual(values=c("blue", "red","green4"))+
  scale_color_manual(values=c("blue", "red","green4"))+
  geom_density(alpha = 0.7) +
  theme_bw()+
  labs(
    y = NULL,
    x = "Level of Problem Solving"
  )
```

![](lab-12_files/figure-gfm/problem%20solving-1.png)<!-- -->

## Using mvrnorm

``` r
mean_traits <- c(50, 50)
cov_matrix <- matrix(c(100, 50, 
                       50, 100), ncol = 2)



traits_data <- mvrnorm(n = 100, mu = mean_traits, Sigma = cov_matrix, empirical = FALSE)

colnames(traits_data) <- c("resilience", "agreeableness")

df_colonists <- cbind(df_colonists, traits_data)


df_colonists %>%
  ggplot(aes(
    x = resilience,
    y = agreeableness
  ))+
    geom_point()+
  geom_line(stat = "smooth", method="lm", alpha = .6, color = "blue", se = FALSE)
```

![](lab-12_files/figure-gfm/simulate%20resilience%20and%20agreeableness-1.png)<!-- -->

I used the code provided to create a correlation matrix for the big 5

``` r
seed <- 123
set.seed(123)


n_colonists <- 100


var_names <- c( "EX", "ES", "AG", "CO", "OP")

mean_traits <- c(-.5, .5, .25, .5, 0)
sd_traits <- c(1, .9, 1, 1, 1)


cor_matrix_bigfive <- matrix(c(
1.0000, 0.2599, 0.1972, 0.1860, 0.2949,
0.2599, 1.0000, 0.1576, 0.2306, 0.0720,
0.1972, 0.1576, 1.0000, 0.2866, 0.1951,
0.1860, 0.2306, 0.2866, 1.0000, 0.1574,
0.2949, 0.0720, 0.1951, 0.1574, 1.0000
), nrow=5, ncol=5, byrow=TRUE,
dimnames=list(c("EX", "ES", "AG", "CO", "OP"), 
c("EX", "ES", "AG", "CO", "OP")))


eigen_values <- eigen(cor_matrix_bigfive)$values
is_positive_definite <- all(eigen_values > 0)


cov_matrix_bigfive <- cor_matrix_bigfive * (sd_traits %*% t(sd_traits))


simulated_data <- mvrnorm(n = 100, mu = mean_traits, Sigma = cov_matrix_bigfive)


simulated_data <- cbind.data.frame(colonist_id = 1:n_colonists,
seed = seed,
simulated_data)

simulated_data$seed <- NULL

view(simulated_data)
```

## Preparing for the unexpected

I tried using a while() loop, but chat suggested using a for() loop.

``` r
max_colonies <- 100
n_colonists <- 100
MARSid_counter <- 1

var_names <- c( "EX", "ES", "AG", "CO", "OP")

mean_traits <- c(-.5, .5, .25, .5, 0)
sd_traits <- c(1, .9, 1, 1, 1)

cor_matrix_bigfive <- matrix(c(
1.0000, 0.2599, 0.1972, 0.1860, 0.2949,
0.2599, 1.0000, 0.1576, 0.2306, 0.0720,
0.1972, 0.1576, 1.0000, 0.2866, 0.1951,
0.1860, 0.2306, 0.2866, 1.0000, 0.1574,
0.2949, 0.0720, 0.1951, 0.1574, 1.0000
), nrow=5, ncol=5, byrow=TRUE,
dimnames=list(c("EX", "ES", "AG", "CO", "OP"), 
c("EX", "ES", "AG", "CO", "OP")))


cov_matrix_bigfive <- cor_matrix_bigfive * (sd_traits %*% t(sd_traits))

all_colonies <- list()

for (colony in 1:max_colonies) {
  set.seed(123 + colony)
  
  traits_data <- mvrnorm(n = n_colonists, mu = mean_traits, Sigma = cov_matrix_bigfive)
  
  df <- data.frame(
    colony = colony,
    id_in_colony = 1:n_colonists,
    MARSid = MARSid_counter:(MARSid_counter + n_colonists - 1),
    seed = 123 + colony,
    traits_data
  )
  names(df)[5:9] <- var_names
  
  all_colonies[[colony]] <- df
  MARSid_counter <- MARSid_counter + n_colonists
}

big_simulated_data <- do.call(rbind, all_colonies)
big_simulated_data$seed <- NULL
```

``` r
colonies_means <- big_simulated_data %>% 
  group_by(colony) %>% 
  summarise(mean_ex = mean(EX),
            mean_es = mean(ES),
            mean_ag = mean(AG),
            mean_co = mean(CO),
            mean_op = mean(OP)
            )

means_long <- colonies_means %>%
  pivot_longer(cols = -colony)

colonies_means <- means_long %>%
  mutate(big_5 = case_when(
    name == "mean_ex" ~ "Extraversion",
    name == "mean_es" ~ "Neuroticism",
    name == "mean_ag" ~ "Agreeableness",
    name == "mean_co" ~ "Conscientiousness",
    name == "mean_op" ~ "Openess"
  ))

names(colonies_means)[names(colonies_means) == "value"] <- "Mean"

colonies_sd <- big_simulated_data %>% 
  group_by(colony) %>% 
  summarise(
            sd_ex = sd(EX),
            sd_es = sd(ES),
            sd_ag = sd(AG),
            sd_co = sd(CO),
            sd_op = sd(OP)
            )

sd_long <- colonies_sd %>%
  pivot_longer(cols = -colony)

colonies_sd <- sd_long %>%
  mutate(big_5 = case_when(
    name == "sd_ex" ~ "Extraversion",
    name == "sd_es" ~ "Neuroticism",
    name == "sd_ag" ~ "Agreeableness",
    name == "sd_co" ~ "Conscientiousness",
    name == "sd_op" ~ "Openess"
  ))

names(colonies_sd)[names(colonies_sd) == "value"] <- "Standard Deviation"



colonies_stats <- full_join(colonies_means, colonies_sd, by = c("colony", "big_5"))



view(colonies_stats)
```

``` r
means_big_data %>%
ggplot(
  aes(x = mean_ex)) +
  geom_histogram(color = "black", fill = "blue", bins = 20) +
  labs(
    x = "Mean Extraversion", 
    y = NULL,
    title = "Distribution of Extraversion Mean",
    subtitle = "Possible values range from -1 to 1"
    )+
  geom_vline(xintercept = -0.5, 
             color = "white",
             linetype = "dashed") +
  annotate("text", 
           x = -0.44, y = Inf, 
           label = "Population Mean = -0.5", 
           vjust = 2, 
           color = "black")



means_big_data %>%
ggplot(
  aes(x = sd_ex)) +
  geom_histogram(color = "black", fill = "red3", bins = 20) +
  labs(
    x = "SD Extraversion",
    y = NULL,
    title = "Distribution of Extraversion Standard Deviation")+
  geom_vline(xintercept = 1, 
             color = "black", 
             linetype = "dashed") +
  annotate("text", 
           x = .96, y = Inf, 
           label = "Population SD = 1", 
           vjust = 2,
           color = "black") 

#correlation
means_big_data %>%
  ggplot(aes(
   x = mean_ex,
   y = mean_op
   ))+
  geom_point(alpha = .6)+
  geom_smooth(method='lm', se = FALSE, formula= y~x)+
  stat_cor(method = "pearson", label.x = -.35, label.y = .03)+
  labs(
    x = "Extraversion Means",
    y = "Openness Means",
    title = "Correlation of Extraversion and Openness across Colonies"
  )

#distribution of correlation
means_big_data %>%
ggplot(aes(x = cor_ex_op)) +
  geom_histogram(color = "white", fill = "pink", bins = 20) +
  labs(
    x = "Correlation", 
    y = "Count", 
    title = "Distribution of Correlations of Extraversion and Openness across Colonies")+
    geom_vline(xintercept = 0.2949, 
               color = "black", 
               linetype = "dashed") +
  annotate("text", 
           x = 0.21, y = Inf, 
           label = "Population Correlation = 0.295", 
           vjust = 2, 
           color = "black")
```

For some reason I worked on it earlier and it ran, but now it doesn’t.
This has something to do with me moving things around and not realizing
I altered them. I asked chatGPT to recreate what I wanted.

``` r
# First, augment colonies_stats with cross‐colony correlations of EX and OP
cor_ex_op <- big_simulated_data %>%
  group_by(colony) %>%
  summarize(cor_ex_op = cor(EX, OP))

# Join that back into colonies_means / colonies_sd
colonies_stats <- colonies_stats %>%
  left_join(cor_ex_op, by = "colony")

# -----------------------------------------------------------------
colonies_stats %>%
  filter(big_5 == "Extraversion") %>%
  ggplot(aes(x = Mean)) +
  geom_histogram(color = "black", fill = "blue", bins = 20) +
  labs(
    x = "Mean Extraversion",
    y = NULL,
    title = "Distribution of Extraversion Mean",
    subtitle = "Possible values range from -1 to 1"
  ) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  annotate("text", x = -0.44, y = Inf, label = "Population Mean = -0.5", vjust = 2)
```

![](lab-12_files/figure-gfm/chat%20recreation%20of%20above-1.png)<!-- -->

``` r
# -----------------------------------------------------------------
colonies_stats %>%
  filter(big_5 == "Extraversion") %>%
  ggplot(aes(x = `Standard Deviation`)) +
  geom_histogram(color = "black", fill = "red3", bins = 20) +
  labs(
    x = "SD Extraversion",
    y = NULL,
    title = "Distribution of Extraversion Standard Deviation"
  ) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  annotate("text", x = 0.96, y = Inf, label = "Population SD = 1", vjust = 2)
```

![](lab-12_files/figure-gfm/chat%20recreation%20of%20above-2.png)<!-- -->

``` r
# -----------------------------------------------------------------
# Distribution of per‐colony EX–OP correlations
colonies_stats %>%
  ggplot(aes(x = cor_ex_op)) +
  geom_histogram(color = "white", fill = "pink", bins = 20) +
  labs(
    x = "Correlation",
    y = "Count",
    title = "Distribution of Correlations of Extraversion and Openness"
  ) +
  geom_vline(xintercept = 0.2949, linetype = "dashed") +
  annotate("text", x = 0.21, y = Inf, label = "Population Corr = 0.295", vjust = 2)
```

![](lab-12_files/figure-gfm/chat%20recreation%20of%20above-3.png)<!-- -->
