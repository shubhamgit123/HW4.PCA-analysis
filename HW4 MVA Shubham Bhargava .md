Homework 4 
================
Shubham Bhargava
2024-28-11

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.3.2

``` r
News_Website_Dataset <- read_excel("News Website Dataset.xlsx")
#View(News_Website_Dataset)

correlation_coefficient <- cor(News_Website_Dataset$Total_revenue, News_Website_Dataset$Avg_Session_Duration)



plot(News_Website_Dataset$Avg_Session_Duration, News_Website_Dataset$Total_revenue,
     xlab = "Average Session Duration", ylab = "Total Revenue",
     main = "Scatter Plot of Total Revenue vs. Avg Session Duration")

abline(lm(News_Website_Dataset$Total_revenue ~ News_Website_Dataset$Avg_Session_Duration), col = "red")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
print(paste("Correlation Coefficient between Total Revenue and Avg Session Duration:", correlation_coefficient))
```

    ## [1] "Correlation Coefficient between Total Revenue and Avg Session Duration: 0.536707214155583"

``` r
# correlation  and coefficient B/W Total_revenue and Total Sessions
correlation_coefficient2 <- cor(News_Website_Dataset$Total_revenue, News_Website_Dataset$Total_Sessions)
print(correlation_coefficient2)
```

    ## [1] 1

``` r
plot(News_Website_Dataset$Total_Sessions, News_Website_Dataset$Total_revenue,
     xlab = "Total Sessions", ylab = "Total Revenue",
     main = "Scatter Plot of Total Revenue vs. Total Sessions")

abline(lm(News_Website_Dataset$Total_revenue ~ News_Website_Dataset$Total_Sessions), col = "blue")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
### Assignment 4

<p>
1 Decide how many Principal Components (PCs) you want to keep and why
</p>
<p>
2 Explain the variate representation each PCs
</p>
<p>
3 Perform some visualization using PCs.
</p>

``` r
News_Website_Dataset_num <- read_excel("News Website Dataset.xlsx", range = cell_cols("F:J"))
cor(News_Website_Dataset_num[-1])
```

    ##                      Avg_Session_Duration Total_Sessions Conversion_Rate
    ## Avg_Session_Duration            1.0000000      0.5367072      -0.3166230
    ## Total_Sessions                  0.5367072      1.0000000      -0.1767767
    ## Conversion_Rate                -0.3166230     -0.1767767       1.0000000
    ## Total_revenue                   0.5367072      1.0000000      -0.1767767
    ##                      Total_revenue
    ## Avg_Session_Duration     0.5367072
    ## Total_Sessions           1.0000000
    ## Conversion_Rate         -0.1767767
    ## Total_revenue            1.0000000

``` r
News_Website_Dataset_num_pca <- prcomp(News_Website_Dataset_num[,-1],scale=TRUE)
News_Website_Dataset_num_pca
```

    ## Standard deviations (1, .., p=4):
    ## [1] 1.5814745 0.9858789 0.7259348 0.0000000
    ## 
    ## Rotation (n x k) = (4 x 4):
    ##                             PC1        PC2        PC3           PC4
    ## Avg_Session_Duration -0.4779751 -0.2224537 -0.8497377  0.000000e+00
    ## Total_Sessions       -0.5971722  0.2703526  0.2651318 -7.071068e-01
    ## Conversion_Rate       0.2414759  0.8968463 -0.3706158  4.996004e-16
    ## Total_revenue        -0.5971722  0.2703526  0.2651318  7.071068e-01

``` r
summary(News_Website_Dataset_num_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3 PC4
    ## Standard deviation     1.5815 0.9859 0.7259   0
    ## Proportion of Variance 0.6253 0.2430 0.1318   0
    ## Cumulative Proportion  0.6253 0.8682 1.0000   1

``` r
(eigen_News_Website_Dataset <- News_Website_Dataset_num_pca$sdev^2)
```

    ## [1] 2.5010615 0.9719572 0.5269813 0.0000000

``` r
names(eigen_News_Website_Dataset) <- paste("PC",1:4,sep="")
eigen_News_Website_Dataset
```

    ##       PC1       PC2       PC3       PC4 
    ## 2.5010615 0.9719572 0.5269813 0.0000000

``` r
sumlambdas <- sum(eigen_News_Website_Dataset)
sumlambdas
```

    ## [1] 4

``` r
propvar <- eigen_News_Website_Dataset/sumlambdas
propvar
```

    ##       PC1       PC2       PC3       PC4 
    ## 0.6252654 0.2429893 0.1317453 0.0000000

``` r
cumvar_News_Website_Dataset <- cumsum(propvar)
cumvar_News_Website_Dataset
```

    ##       PC1       PC2       PC3       PC4 
    ## 0.6252654 0.8682547 1.0000000 1.0000000

``` r
matlambdas <- rbind(eigen_News_Website_Dataset,propvar,cumvar_News_Website_Dataset)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
```

    ##                        PC1    PC2    PC3 PC4
    ## Eigenvalues         2.5011 0.9720 0.5270   0
    ## Prop. variance      0.6253 0.2430 0.1317   0
    ## Cum. prop. variance 0.6253 0.8683 1.0000   1

``` r
summary(News_Website_Dataset_num_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3 PC4
    ## Standard deviation     1.5815 0.9859 0.7259   0
    ## Proportion of Variance 0.6253 0.2430 0.1318   0
    ## Cumulative Proportion  0.6253 0.8682 1.0000   1

``` r
News_Website_Dataset_num_pca$rotation
```

    ##                             PC1        PC2        PC3           PC4
    ## Avg_Session_Duration -0.4779751 -0.2224537 -0.8497377  0.000000e+00
    ## Total_Sessions       -0.5971722  0.2703526  0.2651318 -7.071068e-01
    ## Conversion_Rate       0.2414759  0.8968463 -0.3706158  4.996004e-16
    ## Total_revenue        -0.5971722  0.2703526  0.2651318  7.071068e-01

``` r
print(News_Website_Dataset_num_pca)
```

    ## Standard deviations (1, .., p=4):
    ## [1] 1.5814745 0.9858789 0.7259348 0.0000000
    ## 
    ## Rotation (n x k) = (4 x 4):
    ##                             PC1        PC2        PC3           PC4
    ## Avg_Session_Duration -0.4779751 -0.2224537 -0.8497377  0.000000e+00
    ## Total_Sessions       -0.5971722  0.2703526  0.2651318 -7.071068e-01
    ## Conversion_Rate       0.2414759  0.8968463 -0.3706158  4.996004e-16
    ## Total_revenue        -0.5971722  0.2703526  0.2651318  7.071068e-01

``` r
News_Website_Dataset_num_pca$x
```

    ##             PC1        PC2         PC3           PC4
    ##  [1,] -2.404150  0.6271108  0.44420691  6.661338e-16
    ##  [2,]  1.290564  1.5130222  0.06426287  7.216450e-16
    ##  [3,]  1.256024 -0.2145550 -0.85140879 -1.110223e-16
    ##  [4,] -1.264284 -0.8317676 -0.69973980 -2.775558e-16
    ##  [5,]  1.121846 -1.0938104  1.04267882 -7.771561e-16
    ##  [6,] -2.404150  0.6271108  0.44420691  6.661338e-16
    ##  [7,]  1.290564  1.5130222  0.06426287  7.216450e-16
    ##  [8,]  1.256024 -0.2145550 -0.85140879 -1.110223e-16
    ##  [9,] -1.264284 -0.8317676 -0.69973980 -2.775558e-16
    ## [10,]  1.121846 -1.0938104  1.04267882 -7.771561e-16
    ## [11,] -2.404150  0.6271108  0.44420691  6.661338e-16
    ## [12,]  1.290564  1.5130222  0.06426287  7.216450e-16
    ## [13,]  1.256024 -0.2145550 -0.85140879 -1.110223e-16
    ## [14,] -1.264284 -0.8317676 -0.69973980 -2.775558e-16
    ## [15,]  1.121846 -1.0938104  1.04267882 -7.771561e-16
    ## [16,] -2.404150  0.6271108  0.44420691  6.661338e-16
    ## [17,]  1.290564  1.5130222  0.06426287  7.216450e-16
    ## [18,]  1.256024 -0.2145550 -0.85140879 -1.110223e-16
    ## [19,] -1.264284 -0.8317676 -0.69973980 -2.775558e-16
    ## [20,]  1.121846 -1.0938104  1.04267882 -7.771561e-16

``` r
library(FactoMineR)
```

    ## Warning: package 'FactoMineR' was built under R version 4.3.3

``` r
encoded_data <- model.matrix(~Device_Category - 1, data = News_Website_Dataset_num)
numerical_data <- cbind(News_Website_Dataset_num[, -which(names(News_Website_Dataset_num) == "Device_Category")], encoded_data)

#PCA
pca_result <- prcomp(numerical_data, scale = TRUE)

# Scree plot
plot(pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Variance Explained")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
loadings <- pca_result$rotation
print(loadings)
```

    ##                               PC1          PC2         PC3        PC4
    ## Avg_Session_Duration   -0.4106821 -0.374429007 -0.02446949 -0.5400865
    ## Total_Sessions         -0.4471330  0.237378541  0.23649813  0.3887130
    ## Conversion_Rate         0.1978333  0.002972559  0.93005313 -0.2969749
    ## Total_revenue          -0.4471330  0.237378541  0.23649813  0.3887130
    ## Device_CategoryDesktop -0.4908267  0.017567881 -0.03728123 -0.2812453
    ## Device_CategoryMobile   0.3469895  0.535940060 -0.06777750 -0.1079177
    ## Device_CategoryTablet   0.1761639 -0.677906012  0.12867014  0.4766254
    ##                                  PC5         PC6          PC7
    ## Avg_Session_Duration   -0.5083067855 -0.37122684  0.051631846
    ## Total_Sessions         -0.1142508742 -0.26319860 -0.674067900
    ## Conversion_Rate         0.0704434778  0.05144631 -0.007155377
    ## Total_revenue          -0.1941033119  0.03800123  0.705389331
    ## Device_CategoryDesktop  0.8050246026 -0.15421830  0.080273384
    ## Device_CategoryMobile   0.0008156666 -0.74154853  0.161961832
    ## Device_CategoryTablet   0.1945952061 -0.46384136  0.112542699

``` r
# Data projection onto first two PCs
data_projection <- as.data.frame(pca_result$x[, 1:2])

# Data projection onto next two PCs
data_projection2 <- as.data.frame(pca_result$x[, 3:4])

# Plot
plot(data_projection$PC1, data_projection$PC2, 
     xlab = "PC1", ylab = "PC2", 
     main = "Data Visualization using PCs - 1")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# Plot
plot(data_projection2$PC3, data_projection2$PC4, col = "blue",
     xlab = "PC3", ylab = "PC4", 
     main = "Data Visualization using PCs - 2")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->
