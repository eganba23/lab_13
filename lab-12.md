Lab 13 - Colonizing Mars
================
Benjamin Egan
Start date 1-29

    ## Warning: package 'purrr' was built under R version 4.3.3

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

![](lab-12_files/figure-gfm/alternate%20plot-1.png)<!-- -->

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

![](lab-12_files/figure-gfm/using%20mvrnorm-1.png)<!-- -->

I used the code provided to create a correlation matrix for the big 5

``` r
# Define the bigfive
bigfive <- c("EX", "ES", "AG", "CO", "OP")

# Create an empty matrix with dimensions equal to the number of bigfive
cor_matrix <- matrix(0, ncol = length(bigfive), 
                 nrow = length(bigfive), 
                 dimnames = list(bigfive, bigfive))


# Initialize the diagonal to 1
diag(cor_matrix) <- 1

# Function to set correlation values ensuring symmetry
set_correlation <- function(matrix, row, column, value) {
  matrix <- as.matrix(matrix)
  matrix[row, column] <- value
  matrix[column, row] <- value
  return(matrix)
}

cor_matrix_bigfive <- cor_matrix <- cor_matrix %>% 
  set_correlation("ES", "AG", 0.1576) %>% 
  set_correlation("ES", "CO", 0.2306) %>% 
  set_correlation("ES", "EX", 0.2599) %>% 
  set_correlation("ES", "OP", 0.072) %>% 
  set_correlation("AG", "CO", 0.2866) %>% 
  set_correlation("AG", "EX", 0.1972) %>% 
  set_correlation("AG", "OP", 0.1951) %>% 
  set_correlation("CO", "EX", 0.186) %>% 
  set_correlation("CO", "OP", 0.1574) %>% 
  set_correlation("EX", "OP", 0.2949)

print(cor_matrix_bigfive)
```

    ##        EX     ES     AG     CO     OP
    ## EX 1.0000 0.2599 0.1972 0.1860 0.2949
    ## ES 0.2599 1.0000 0.1576 0.2306 0.0720
    ## AG 0.1972 0.1576 1.0000 0.2866 0.1951
    ## CO 0.1860 0.2306 0.2866 1.0000 0.1574
    ## OP 0.2949 0.0720 0.1951 0.1574 1.0000
