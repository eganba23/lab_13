Lab 13 - Colonizing Mars
================
Benjamin Egan
Start date 1-29

### Exercise 1

``` r
set.seed(123)

age <- rnorm(100, mean = 30, sd = 5)

df_colonists <- data.frame(id = 1:100, age)
```

``` r
data.frame(df_colonists) %>%
ggplot(
 aes(x=age)
) + geom_histogram(binwidth=2,color="black", fill="blue") +
  labs(title = "Histogram of Colonists Ages by Seed",
       x = "Age", y = "Frequency") + 
  theme_bw()
```

![](lab-12_files/figure-gfm/1.1%20-%20visualizing%20data-1.png)<!-- -->
It created a roughly normal distribution that is centered at 30. It
looks like changing the seed makes the distribution more normal. The 100
seed looks normally distributed and the 1000 seed even more so. Looks
like it’s due to random chance, as 999 seed doesn’t have the nice
bellcurve shape.

``` r
role <- rep(c("engineer", "scientist", "medic"), 
             each = 34, 
             length.out = 100)
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

Most of the reason I chose this was for pure randomness. But I like that
it listed out each role together.

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

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-12_files/figure-gfm/1.4%20heathy-1.png)<!-- -->

### Exercise 2

…

Add exercise headings as needed.
