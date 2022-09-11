## HEADER ####
## Who: Sam JAckson
## What: Data science bootcamp skills test
## Last edited: 2022-09-11
####

## CONTENTS ####
## Question 1
## Question 2
## Question 3
## Question 4
## Question 5
## Question 6
## Question 7
## Question 8
## Question 9
## Question 10


## Question 1 ####
# Start a reproducible script for the assessment


## Question 2 ####
# Show the R code to produce the following output

# Create matrix containing the required data
mymat <- matrix(data = c(32, 28, 34, 42), nrow = 2)

# Add row and column names
rownames(mymat) <- c("female", "male")
colnames(mymat) <- c("dog", "cat")

# Display matrix as required
mymat


## Question 3 ####
# Show the R code to make a good histogram of the following data,
# describing the height in cm of domestic cats at the shoulder

# Create vector containing data
cat <- c(20.7, 21.7, 23.7, 27.1, 20.0, 27.0, 27.4,
         24.6, 24.3, 18.6, 20.1, 19.8, 24.9, 21.8, 25.7,
         23.0, 25.2, 27.9, 21.8, 25.8, 27.3, 20.1, 24.5,
         19.3, 20.7, 21.9, 18.1, 21.8, 26.7, 21.4, 22.8,
         24.0, 22.9, 19.9, 26.3, 24.7, 25.9, 19.1, 25.2,
         22.1)

# Create histogram of data
hist(cat, xlab = "height (cm) of domestic cats at the shoulder",
     main = "histogram showing cats height")

# Add vertical line showing the grand mean
mean(cat)
abline(v = mean(cat), lty = 2, lwd = 3, col = "red")

# Add label to line showing grand mean
text(x = 24.2, y = 6, labels = "grand mean = 23.1", col = "red")


## Question 4 ####
# Using cat data, perform the Shapiro test to decide if data adhere
# to a Gaussian distribution

# Carry out Shapiro test on cat data
shapiro.test(cat)

# We found no evidence our assumption of Gaussian distribution was violated
# Shapiro-Wilk: W = 0.96, n = 40, p = 0.14)


## Question 5 ####
# Examine the 'CO2' data frame and help file
data("CO2")
help("CO2")
head(CO2)

# Select all columns of the data frame where conc == 350
CO2[CO2$conc == "350", c(1:5)]

# Create boxplot showing uptake as a function of treatment
boxplot(formula = CO2$uptake ~ CO2$Treatment,
        data = CO2[CO2$conc == "350", c(1:5)],
        ylab = "CO2 uptake (μmol/m^2sec)",
        xlab = "Treatment",
        main = "Concentration equivalent to 350 ml/l")


## Question 6 ####
# Replicate the graph for the given data

# Load data for x and y
y <- c(1.36, -0.10, 0.39, -0.05, -1.38, -0.41, -0.39, -0.06, 1.10, 0.76)
x <- c(-0.16, -0.25, 0.70, 0.56, -0.69, -0.71, 0.36, 0.77, -0.11, 0.88)

# Plot graph
plot(x = x, y = y,
     pch = 15,
     col = "blue",
     xlab = "Just an example label",
     ylab = "My y axis label")

# Calculate linear model
mylm <- lm(formula = y ~ x)
mylm
# Intercept = 0.059, slope = 0.467

# Add line to plot
abline(reg = mylm, col = "red", lty = 2, lwd = 3)

# Add text to plot describing line
text(x = 0.5, y = 1, labels = "y = 0.059 + 0.467*x")


## Question 7 ####
# Calculate the residuals of y ~ x from Q6
residuals(mylm)

# Calculate mean and standard deviation of residuals
mean(residuals(mylm))
sd(residuals(mylm))
# mean = 8.33e-17, standard deviation = 0.759

# Create histogram of residuals
hist(residuals(mylm), prob = T)

# Add a density line of residuals
lines(density(residuals(mylm)),
      col = "green4", lty = 1, lwd = 3)

# Draw on theoretical Gaussian for residual parameters
curve(dnorm(x, mean = mean(residuals(mylm)),
            sd = sd(residuals(mylm))),
      add = T,
      col = "blue", lty = 3, lwd = 3)

# Draw on line for mean
abline(v = mean(residuals(mylm)), lty = 2, lwd = 3, col = "red")

# Carry out Shapiro test on residuals
shapiro.test(residuals(mylm))

# We found no evidence our assumption of Gaussian residual distribution
# was violated (Shapiro-Wilk: W = 0.94, n = 10, P = 0.55)


## Question 8 ####
# There should only be raw data, no chart or mean etc.
# Unable to see if there is a dictionary tab
# Is there a heading to column 'A'?
# Spaces shouldn't be used in text e.g. 'mortality percentage' & 'M. euphorbiae'
# Appears good regarding data inputted without formatting


## Question 9 ####
# Perform 1-way ANOVA to evaluate the overall effect of insect spray type
# (spray) on the number of insects counted (count)
data("InsectSprays")
help("InsectSprays")
head(InsectSprays)

# Check for assumption of Gaussian residuals
# Create model object
m1 <- aov(formula = count ~ spray, data = InsectSprays)
# Create histogram and qqplot of residuals
par(mfrow = c(1, 2))
hist(rstandard(m1))
library(car)
qqPlot(x = m1)
par(mfrow = c(1, 1))
# Seems a little out on the qqplot
# Complete a Shapiro Wilk test
shapiro.test(rstandard(m1))
# There is evidence of difference to Guassian in our residuals for our
# ANOVA model (Shapiro-Wilk: W = 0.96, n = 72, P = 0.022)

# Check for assumption of homoscedasticity
plot(formula = rstandard(m1) ~ fitted(m1),
     ylab = "m1: residuals",
     xlab = "m1: fitted values")
abline(h = 0, lty = 2, lwd = 3, col = "red")
# Add points for mean residuals
y1 <- aggregate(rstandard(m1), by = list(InsectSprays$spray), FUN = mean)[,2]
x1 <- unique(round(fitted(m1),6))
points(x = x1, y = y1,
       pch = 19, col = "blue")
# Carry out bartlett test
bartlett.test(formula = count ~ spray, data = InsectSprays)
# There is evidence that variance in count differs between sprays
# (Bartlett test: K-squared = 25.96, df = 5, P < 0.0001)

# As the assumptions of Gaussian residuals, homoscedasticity and eqaulity of
# variance are violated, ANOVA cannot be used

# Carry out Kruskal-Wallis non-parametric test as alternative
kruskal.test(formula = count ~ spray, data = InsectSprays)
# We found evidence of a difference in count for different sprays
# (Kruskal-Wallis: chi-squared = 54.69, df = 5, p < 0.0001)

# Create a boxplot of the data
boxplot(count ~ spray, data = InsectSprays,
        ylab = "Number of insects",
        xlab = "Spray",
        main = "Effect of spray on number of insects",
        cex = 0)
# Add a line showing grand mean
abline(h = mean(InsectSprays$count),
       lty = 2, lwd = 2, col = "red")
# Add raw data
points(x = jitter(rep(1:6, each = 12), amount = .1),
       y = InsectSprays$count,
       pch = 16, cex = .8, col = "blue")

## Question 10 ####
# Create a github repository called 'bootcamp'
# Create an html R Markdown document with answer to question 9
# Push R Markdown file, html file and other related files to the repository
# html link to repository: https://github.com/SamJacksonAB/bootcamp