library(pwr)
library(ggplot2)

#randomly assign
ex <- rep(c("A", "B", "C", "D"), each = 1)
print(ex)
treats <- rep(c("MS", "MDS", "MP", "MDP"), each = 1)
sample(treats)

serial <- c(2.431,9.134,16.120,86.172)  # Execution times for serial execution
parallel <- c(1.337,3.380,6.375,13.004)   # Execution times for parallel execution
# Calculate means and standard deviations for each group
mean_serial <- mean(serial)
mean_parallel <- mean(parallel)
sd_serial <- sd(serial)
sd_parallel <- sd(parallel)
# Compute pooled standard deviation
pooled_sd <- sqrt(((sd_serial^2 + sd_parallel^2) / 2))
# Compute Cohen's d
cohen_d <- (mean_serial - mean_parallel) / pooled_sd


#power analyses for no. of replicates
pwr.t.test(d = cohen_d, sig.level = 0.05, power = 0.87, type = "two.sample")

pilot <- read.csv("Exp1.csv")

str(pilot)

# Convert Type to a factor
pilot$Type <- factor(pilot$Type, levels = c("Sequential", "Parallel"))

# Plot using ggplot
ggplot(pilot, aes(x = CWork, y = ExTime, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  labs(x = "Computational Work", y = "Execution Time", color = "Execution Type") +
  theme_minimal()

plot(ExTime~Type, data = pilot)

real <- read.csv("Exp2.csv")
str(real)
real$Type <- factor(real$Type, levels = c("Sequential", "Parallel"))
bp <- boxplot(ExTime ~ Type, data = real,
              xlab = "Execution Type",
              ylab = "Execution Time",
              main = "Execution Time by Program Type")


ggplot(real, aes(x = CWork, y = ExTime, color = Type, linetype = Algo, group = interaction(Type, Algo))) +
  geom_line() +
  geom_point() +
  labs(x = "Computational Work", y = "Execution Time", color = "Execution Type", linetype = "Algorithm") +
  theme_minimal()

summary(bp)


m2 <- aov(ExTime ~ Type, data = real)
summary.aov(m2)
m2


mean_parallel <- mean(real$ExTime[real$Type == "Parallel"])

# Calculate the mean for Sequential execution
mean_sequential <- mean(real$ExTime[real$Type == "Sequential"])

# Print the means
cat("Mean for Parallel execution:", mean_parallel, "\n")
cat("Mean for Sequential execution:", mean_sequential, "\n")


real_subset <- real[(12 + 1):(nrow(real)), ]
m1 <- t.test(ExTime ~ Type, data = real_subset)
print(m1)

Bp2 <-boxplot(ExTime ~ Type, data = real_subset,
              xlab = "Execution Type",
              ylab = "Execution Time",
              main = "Execution Time by Program Type")
summary(Bp2)
       
#power analyses for no. of replicates
pwr.t.test(d = cohen_d, sig.level = 0.05, power = 0.792, type = "two.sample")

m3 <- aov(ExTime ~ Type, data = real_subset)
m3
summary.aov(m3)
