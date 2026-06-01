# 1. LOAD DATA + FIX DATA TYPES
books <- read.csv("C:/Users/Roopa Reddy/Downloads/archive/books.csv", stringsAsFactors = FALSE)

# CONVERT TO NUMERIC (FIXES NA WARNINGS)
ratings <- as.numeric(books$average_rating)
ratings <- ratings[!is.na(ratings)]  # Remove any NAs after conversion
pages <- as.numeric(books$num_pages)
pages <- pages[!is.na(pages)]

n <- length(ratings)


# ========================================
# CHAPTER 6: POINT ESTIMATES + CLT
mean_rating <- mean(ratings)
sd_rating <- sd(ratings)
se_rating <- sd_rating / sqrt(n)

cat("=== CHAPTER 6: POINT ESTIMATES ===\n")
cat("Mean rating:", round(mean_rating, 3), "\n")
cat("SD rating:", round(sd_rating, 3), "\n")
cat("SE rating:", round(se_rating, 4), "\n\n")

# CENTRAL LIMIT THEOREM PLOT
set.seed(123)
clt_means <- replicate(300, mean(sample(ratings, 30, replace=TRUE)))
hist(clt_means, main="CLT: Sample Means", col="lightblue", breaks=25)
abline(v=mean_rating, col="red", lwd=3)
abline(v=mean(clt_means), col="blue", lwd=2)
legend("topright", c("Population Mean", "Sample Means Mean"), col=c("red","blue"), lwd=2)

# ========================================
# CHAPTER 7: CONFIDENCE INTERVALS

t_crit <- qt(0.975, n-1)  # 95% critical value
ci_rating <- mean_rating + c(-1,1) * t_crit * se_rating

cat("=== CHAPTER 7: CONFIDENCE INTERVALS ===\n")
cat("95% CI Rating: [", round(ci_rating[1], 3), ",", round(ci_rating[2], 3), "]\n")

# Proportion > 4 stars
p_star4 <- mean(ratings > 4)
z_crit <- qnorm(0.975)
ci_prop <- p_star4 + c(-1,1) * z_crit * sqrt(p_star4*(1-p_star4)/n)
cat("95% CI P(>4 stars): [", round(ci_prop[1], 3), ",", round(ci_prop[2], 3), "]\n\n")

# ========================================
# CHAPTER 8: HYPOTHESIS TESTS

cat("=== CHAPTER 8: HYPOTHESIS TESTS ===\n")

# TEST 1: H0: mean = 4.0
t_stat <- (mean_rating - 4) / se_rating
p_t <- 2 * pt(abs(t_stat), n-1, lower.tail=FALSE)
cat("TEST 1: H0 μ=4.0\n")
cat("   t =", round(t_stat, 3), "  p =", round(p_t, 4), "\n")

# TEST 2: H0: P(>4 stars) = 0.5
z_stat <- (p_star4 - 0.5) / sqrt(0.5*0.5/n)
p_z <- 2 * pnorm(abs(z_stat), lower.tail=FALSE)
cat("TEST 2: H0 P>4=0.5\n")
cat("   z =", round(z_stat, 3), "  p =", round(p_z, 4), "\n")

# TEST 3: Mean pages = 300
mean_pages <- mean(pages)
se_pages <- sd(pages) / sqrt(length(pages))
t_pages <- (mean_pages - 300) / se_pages
p_pages <- 2 * pt(abs(t_pages), length(pages)-1, lower.tail=FALSE)
cat("TEST 3: H0 μ_pages=300\n")
cat("   t =", round(t_pages, 3), "  p =", round(p_pages, 4), "\n")
