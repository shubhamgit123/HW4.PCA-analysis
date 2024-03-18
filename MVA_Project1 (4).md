Project1
================
Satya Shiva Sai Ram Kamma
2024-02-11

## News Website Traffic and Revenue Analysing Dataset

I’m creating the dataset for the News website, which mostly depends on
website traffic and repeated visitors to earn revenue.

From this Dataset, what I’m trying to find

<b>Which time of day across different traffic sources drives the highest
number of sessions and conversions?</b>

<p>

1.  Identifying when each traffic source reaches its peak traffic allows
    us to optimize your website and its resources, such as content
    scheduling, ad campaign scheduling, and improving the responsiveness
    and loading speed of the website.

2.  Understanding how the time of day, the source of traffic, and
    website performance relate to one another allows us to make
    decisions based on data that improve user experience, maximize
    resource allocation

    </p>

<b>Is there a significant correlation between content category and
session duration?</b>

<p>

1.  We can learn what type of content is the most valuable and engaging
    to the audience by studying how long users spend on various content
    categories.

2.  This information can direct the work we do in creating content,
    ensuring that we focus on topics that interest users and hold their
    attention for a longer period of time.

3.  Identifying parts on a website where users quickly lose interest and
    quit can be improved by studying the relationship between content
    categories and session time. This enables to enhance the user
    experience, layout, or quality of the content in specific
    categories.

    </p>

<b>Are there any specific landing pages that are particularly successful
for different traffic sources or campaigns?</b>

## Find and collect data

The variables included in this dataset

    - Time of Day
    - Traffic Source
    - Landing Page
    - Campaign  
    - Device Category   
    - Avg Session Duration
    - Content Category  
    - Total Sessions    
    - Conversion Rate   
    - Total revenue

#### Dependent Variables

    - Total Sessions    
    - Conversion Rate   
    - Total revenue

#### Independent Variables

    - Time of Day
    - Traffic Source
    - Landing Page
    - Campaign  
    - Device Category   
    - Avg Session Duration
    - Content Category  

[View the CSV file](News_Website_Dataset.csv)

## Data Dictionary

<p>

<b>Total Sessions:</b> Total number of unique sessions on the website
for a specific timeframe (e.g., Day, Week, Month).

<b>Conversion Rate:</b> Percentage of visitors who complete a desired
action (e.g., Polls, Newsletter subscription).

<b>Total Revenue:</b> The total amount generated throughout the
sessions.

<b>Time of Day:</b> Categorical variable with 4 levels (Morning,
Afternoon, Evening, Night).

<b>Traffic Source:</b> Categorical variable indicating where visitors
originated from (e.g., Organic Search, Search, Referral, Direct,
Social).

<b>Landing Page:</b> The first page a visitor viewed on your website.

<b>Campaign:</b> Categorical variable indicating which marketing
campaign a visitor originated from.

<b>Device Category:</b> Categorical variable indicating the device used
to access the website (e.g., Desktop, Mobile, Tablet).

<b>Average Session Duration:</b> The average time spent by visitors on
your website per session (continuous).

<b>Content Category:</b> Categorical variable classifying the content
type of the visited page (e.g., Article, Category, About Us, Contact Us,
Home Page(/)).
<p>

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

## Assignment 2

<b>1. Univariate Analysis:</b>
<p>
<b>Question :</b> What is the distribution of total revenue?
</p>
<p>
<b>Visualization:</b> Histogram of Total Revenue
</p>

``` r
library(ggplot2)
hist(News_Website_Dataset$Total_revenue, 
     main = "Distribution of Total Revenue",
     xlab = "Total Revenue",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
<p>
The histogram shows the distribution of total income. It means that most
of the income falls in the lower ranges, and is distributed to the
right. Small amount increases are considered excessive.
</p>
<b>2. Bivariate Analysis:</b>
<p>
<b>Question :</b> Is there a relationship between total revenue and
average session duration?
</p>
<p>
<b>Visualization:</b> Scatter plot of Total Revenue and Avg Session
Duration
</p>

``` r
library(ggplot2)
ggplot(News_Website_Dataset, aes(x = Avg_Session_Duration, y = Total_revenue)) +
  geom_point(color = "blue") +
  labs(title = "Total Revenue and Avg Session Duration",
       x = "Average Session Duration",
       y = "Total Revenue")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
<p>
The scatter plot suggests a positive correlation between total revenue
and average session duration, as higher revenue tends to coincide with
longer session durations.
</p>
<b>3. Bivariate Analysis:</b>
<p>
<b>Question :</b> How does total revenue vary across different traffic
sources?
</p>
<p>
<b>Visualization:</b> Box plot of Total Revenue by Traffic Source
</p>

``` r
library(ggplot2)
ggplot(News_Website_Dataset, aes(x = Traffic_Source, y = Total_revenue, fill = Traffic_Source)) +
  geom_boxplot() +
  labs(title = "Total Revenue by Traffic Source",
       x = "Traffic Source",
       y = "Total Revenue")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
<p>
The box plot shows variations in total revenue across different traffic
sources, with some sources having higher median revenues compared to
others.
</p>
<b>4. Multivariate Analysis:</b>
<p>
<b>Question :</b>How does total revenue vary across different device
categories and time of day?
</p>
<p>
<b>Visualization:</b> Line plot of Total Revenue by Time of Day, color
by Device Category
</p>

``` r
library(ggplot2)
ggplot(News_Website_Dataset, aes(x = Time_of_Day, y = Total_revenue, color = Device_Category)) +  geom_line(size = 1.5) +
  labs(title = "Total Revenue by Time of Day (Colored by Device Category)",
       x = "Time of Day",
       y = "Total Revenue")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](MVA_Project1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
<p>
The line plot illustrates how total revenue varies across different
times of the day, with each line representing a different device
category. It helps identify revenue trends based on the time of day and
device usage.
</p>

## Assignment 3

##### 1. Total Revenue

<p>
Analyzing the univariate mean and variance of the “Total Revenue”
variable
</p>

``` r
library(ggplot2)
mean_revenue <- mean(News_Website_Dataset$Total_revenue)
variance_revenue <- var(News_Website_Dataset$Total_revenue)

print(paste("Mean for Total Revenue:", mean_revenue))
```

    ## [1] "Mean for Total Revenue: 125"

``` r
print(paste("Variance for Total Revenue:", variance_revenue))
```

    ## [1] "Variance for Total Revenue: 5263.15789473684"

``` r
#Box Plot for Total Revenue
ggplot(News_Website_Dataset, aes(x = "", y = Total_revenue)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot of Total Revenue", x = "", y = "Total Revenue")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Q-Q plot for Total Revenue
qqnorm(News_Website_Dataset$Total_revenue, main = "Q-Q Plot of Total Revenue")
qqline(News_Website_Dataset$Total_revenue)
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
<p>
The mean total revenue provides the average amount of revenue generated
across all observations in the dataset. It gives a central measure of
the revenue distribution. The variance of total revenue indicates the
spread or dispersion of revenue values around the mean. A higher
variance suggests that revenue values are more spread out from the mean,
while a lower variance suggests that revenue values are closer to the
mean. This analysis helps us understand the typical revenue amount and
the variability in revenue generation.
</p>

#### 2. Total Revenue by Device Category

<p>
Analyzing the mean and variance of Total Revenue across different Device
Categories
</p>

``` r
library(ggplot2)
mean_revenue_device <- aggregate(Total_revenue ~ Device_Category, data = News_Website_Dataset, mean)
variance_revenue_device <- aggregate(Total_revenue ~ Device_Category, data = News_Website_Dataset, var)

print("Mean Total Revenue by Device Category")
```

    ## [1] "Mean Total Revenue by Device Category"

``` r
print(mean_revenue_device)
```

    ##   Device_Category Total_revenue
    ## 1         Desktop         200.0
    ## 2          Mobile          87.5
    ## 3          Tablet          50.0

``` r
print("Variance of Total Revenue by Device Category")
```

    ## [1] "Variance of Total Revenue by Device Category"

``` r
print(variance_revenue_device)
```

    ##   Device_Category Total_revenue
    ## 1         Desktop     2857.1429
    ## 2          Mobile      178.5714
    ## 3          Tablet        0.0000

``` r
# Bar plot for Mean Total Revenue by Device Category
ggplot(mean_revenue_device, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Total Revenue by Device Category", x = "Device Category", y = "Mean Total Revenue") +
  theme_minimal()
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Violin plot for Distribution of Total Revenue by Device Category
ggplot(News_Website_Dataset, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Total Revenue by Device Category", x = "Device Category", y = "Total Revenue") +
  theme_minimal()
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
# Box plot for Total Revenue by Device Category
ggplot(News_Website_Dataset, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_boxplot() +
  labs(title = "Total Revenue by Device Category", x = "Device Category", y = "Total Revenue") +
  theme_minimal()
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->
<p>
This analysis calculates the mean and variance of total revenue for each
device category separately. It helps us understand how revenue varies
across different device categories. The mean total revenue by device
category provides insight into the average revenue generated by users
using each type of device. The variance of total revenue by device
category shows the variability in revenue generation among users of the
same device category. This analysis can help identify which device
categories contribute the most to revenue and how consistent revenue
generation is across different devices
</p>

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
plot(data_projection2$PC3, data_projection2$PC4, col = "red",
     xlab = "PC3", ylab = "PC4", 
     main = "Data Visualization using PCs - 2")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

### Assignment 5

<p>
For each model, decide the optimal number of clusters and explain why
</p>
<p>
Show the membership for each cluster
</p>
<p>
show a visualization of the cluster and membership using the first two
Principal Components
</p>

``` r
# Load necessary libraries
library(cluster)
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.3.2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(magrittr)
library(NbClust)

data <- read_excel("News Website Dataset.xlsx")

data_num <- data[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue")]
dist_matrix <- dist(data_num)

# Hierarchical clustering
hclust_model <- hclust(dist_matrix)

plot(hclust_model)
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
num_clusters <- 3
clusters <- cutree(hclust_model, k = num_clusters)

# Membership for each cluster
table(clusters)
```

    ## clusters
    ##  1  2  3 
    ##  4 12  4

``` r
# Visualize cluster and membership using first two Principal Components
pca_result <- prcomp(data_num, scale = TRUE)
fviz_cluster(list(data = pca_result$x[, 1:2], cluster = clusters))
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# Non-hierarchical clustering (k-means)
num_clusters <- 2  
kmeans_model <- kmeans(data_num, centers = num_clusters)

# Membership for each cluster
table(kmeans_model$cluster)
```

    ## 
    ##  1  2 
    ##  8 12

``` r
# Visualize cluster and membership using first two Principal Components
fviz_cluster(list(data = pca_result$x[, 1:2], cluster = kmeans_model$cluster))
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
# Load necessary libraries
library(cluster)
library(factoextra)
library(magrittr)
library(NbClust)
library(readxl)

# Read the dataset
data <- read_excel("News Website Dataset.xlsx")

# Select numerical variables for clustering
data_num <- data[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue")]

# Perform hierarchical clustering
dist_matrix <- dist(data_num)
hclust_model <- hclust(dist_matrix)

# Decide on the optimal number of clusters based on the dendrogram
num_clusters_hclust <- 3  # Replace with chosen number of clusters

# Perform non-hierarchical clustering (k-means)
num_clusters_kmeans <- 2  # Replace with the chosen number of clusters
kmeans_model <- kmeans(data_num, centers = num_clusters_kmeans)

# Visualize cluster centers for k-means
fviz_cluster(kmeans_model, data = data_num, geom = "point", frame.type = "convex", 
             pointsize = 2, fill = "white", main = "K-means Cluster Centers")
```

    ## Warning: argument frame is deprecated; please use ellipse instead.

    ## Warning: argument frame.type is deprecated; please use ellipse.type instead.

![](MVA_Project1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Visualize cluster and membership using first two Principal Components for k-means
pca_result <- prcomp(data_num, scale = TRUE)
fviz_cluster(kmeans_model, data = pca_result$x[, 1:2], geom = "point", 
             pointsize = 2, fill = "white", main = "K-means Clustering Result (PCA)")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Calculate silhouette information for k-means clustering
sil <- silhouette(kmeans_model$cluster, dist(data_num))

# Visualize the silhouette plot for k-means clustering
fviz_silhouette(sil, main = "Silhouette Plot for K-means Clustering")
```

    ##   cluster size ave.sil.width
    ## 1       1   12          0.80
    ## 2       2    8          0.46

![](MVA_Project1_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
# Create a data frame with cluster membership
data_clustered <- cbind(data_num, Cluster = kmeans_model$cluster)

# Scatter plot of data points colored by cluster membership
plot(data_clustered$Avg_Session_Duration, data_clustered$Total_Sessions, 
     col = data_clustered$Cluster, pch = 16, 
     xlab = "Avg_Session_Duration", ylab = "Total_Sessions",
     main = "Scatter Plot of Clustering")
legend("topright", legend = unique(data_clustered$Cluster), 
       col = 1:num_clusters_kmeans, pch = 16, title = "Cluster")
```

![](MVA_Project1_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->
