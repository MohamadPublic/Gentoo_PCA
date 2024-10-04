# Import Packages
library(palmerpenguins)
library(mnormt)

# Get Data, Omit NA
df = palmerpenguins::penguins
df = subset(df, df$species == 'Gentoo')
df = data.frame(Sex = df$sex,
                 Flipper_mm = df$flipper_length_mm,
                 Bill_mm = df$bill_length_mm)

# Remove NA rows
df = na.omit(df)

# Filter by female penguins
female = subset(df, df$Sex =='female')

# Store data as a matrix
X_f = data.frame(Flipper_mm = female$Flipper_mm,
                 Bill_mm = female$Bill_mm)

# Mean vector of data
mu_f  = c(mean(female$Flipper_mm), mean(female$Bill_mm))

# Sample Var-Cov Matrix of Data
sigma_f = cov(X_f)

# Visualize Initial Data
x = seq(200, 240, 0.5)
y = seq(40, 60, 0.5)
f_f <- function(x, y) dmnorm(cbind(x, y), mu_f, sigma_f)
z_f <- outer(x, y, f_f)

contour(x, y, z_f, xlab ="Flipper Length (mm)", ylab="Bill Length (mm)",
        main = 'Female Gentoo Penguins')


# Now for centered data
X_f_centered = scale(X_f, center = TRUE, scale = FALSE)

# Compute the variance-covariance matrix of the centered data
sigma_f_centered = cov(X_f_centered)

# Display the centered variance-covariance matrix
print(sigma_f_centered)

# Plot the contour again for comparison (if needed)
mu_f  = colMeans(X_f_centered)
x = seq(-10, 10, 0.5)
y = seq(-5, 5, 0.5)

f_f = function(x, y) dmnorm(cbind(x, y), mu_f, sigma_f_centered)
z_f = outer(x, y, f_f)

contour(x, y, z_f, xlab ="Flipper Length (mm)", ylab="Bill Length (mm)",
        main = 'Female Gentoo Penguins (Centered Data)')


# Perform PCA
pca = prcomp(X_f_centered, scale. = FALSE)

# Map the observations to 1 dimension (first principal component)
X_f_pca_1D = pca$x[,1]

# Plot the first principal component
plot(X_f_pca_1D, type = 'o', col = 'blue', 
     main = 'First Principal Component (1D)',
     xlab = 'Observation Index', ylab = 'PC1 Score')




