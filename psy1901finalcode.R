
df <- read.csv("C:\\Users\\kiloc\\Documents\\R\\psy1900data.csv", header = TRUE, stringsAsFactors = TRUE)
View(df)
str(df)

anova <- df <- read.csv("C:\\Users\\kiloc\\Documents\\R\\meandif_2x2anova_psy1900.csv", header = TRUE, stringsAsFactors = TRUE)
View(anova)

boxplot(anova$mean.difference)
hist(anova$mean.difference)
ks.test(anova$mean.difference, 'pnorm', mean = mean(anova$mean.difference), sd = sd(anova$mean.difference))

anova <- anova[, c(4:6)]

slo_col <- anova[c(1:13),]
fas_ach <- anova[14:49,]
slo_ach <- anova[50:67,]
fas_col <- anova[68:99,]

# checking assumptions for conditions
summary(slo_col$mean.difference)
summary(fas_ach$mean.difference)
summary(slo_ach$mean.difference)
summary(fas_col$mean.difference)
boxplot(slo_col$mean.difference, main = "Slow Color")
boxplot(fas_ach$mean.difference, main = "Fast Achromatic")
boxplot(slo_ach$mean.difference, main = 'Slow Achromatic')
boxplot(fas_col$mean.difference, main = 'Fast Color')
boxplot(anova$mean.difference ~ anova$color, ylab = 'Mean Difference', xlab = 'Color', main = 'Mean Difference by Color')
boxplot(anova$mean.difference ~ anova$system, ylab = 'Mean Difference', xlab = 'System', main = 'Mean Difference by Processing Speed')
boxplot(anova$mean.difference ~ anova$system * anova$color, ylab = 'Mean Difference', xlab = 'System and Color', main = 'Mean Difference by Processing Speed and Color', col = 'red')

sd(slo_ach$mean.difference)
?boxplot
anova$color <- as.factor(anova$color)
anova$system <- as.factor(anova$system)
str(anova)

ks.test(slo_col$mean.difference, 'pnorm', sd = sd(slo_col$mean.difference), mean = mean(slo_col$mean.difference)) # normal p-value .71
ks.test(fas_ach$mean.difference, 'pnorm', sd = sd(fas_ach$mean.difference), mean = mean(fas_ach$mean.difference)) # normal p-value .41
ks.test(slo_ach$mean.difference, 'pnorm', sd = sd(slo_ach$mean.difference), mean = mean(slo_ach$mean.difference)) # normal p-value .8249
ks.test(fas_col$mean.difference, 'pnorm', sd = sd(fas_col$mean.difference), mean = mean(fas_col$mean.difference)) # normal p-value .4024

#these are the test results when the outliers are removed from the data set
anova_wo_outliers <- anova[c(1:10, 12:23, 25:77, 79, 80, 82:98),] #removing outliers based on boxplot results for EACH condition above
View(anova_wo_outliers)
final_test_wo_outliers <- aov(formula = mean.difference ~ color * system, data = anova_wo_outliers)
summary(final_test_wo_outliers)

#these are the test results when outliers are not removed from the data set

final_test <- aov(formula = mean.difference ~ color * system, data = anova)
summary(final_test)




for (i in 11:27){
  df[,i] <- as.numeric(df[,i])
}

for (i in 236:255){
  df[,i] <- as.numeric(df[,i])
}
str(df)

slow_color <- df[c(40,43,49,57, 60, 62,63, 67, 69, 76, 84, 85, 91), ]
fast_achr <- df[c(5:7, 10:12, 15, 16, 18, 21, 24, 28, 31, 35, 36, 38, 39, 43, 44, 51, 54, 56, 61, 64, 70, 71, 75, 77, 79, 82, 87, 92, 93, 95, 96, 99 ), ]
fast_color <- df[c(3, 4, 8, 9, 13, 14, 17, 19, 22, 23, 26, 27, 29, 30, 32, 34, 42, 47, 52, 55, 66, 68, 78, 80, 81, 83, 86, 89, 94, 97, 98, 100, 101), ]
slow_achr <- df[c(20, 25, 33, 37, 41, 45, 46, 48, 50, 53, 58, 59, 65, 72, 73, 74, 88, 90), ]
str(slow_color)
slow_color <- slow_color[ , c(8:27, 236:255)]
fast_achr <- fast_achr[ , c(8:27, 236:255)]
fast_color <- fast_color[ , c(8:27, 236:255)]
slow_achr <- slow_achr[ , c(8:27, 236:255)]

slow_color <- slow_color[, c(1,3,5,9,10,12,14,16,17,19,21,23,25,29,30,32,34,36,37,39)]
fast_achr <- fast_achr[, c(1,3,5,9,10,12,14,16,17,19,21,23,25,29,30,32,34,36,37,39)]
slow_achr <- slow_achr[, c(1,3,5,9,10,12,14,16,17,19,21,23,25,29,30,32,34,36,37,39)]
fast_color <- fast_color[, c(1,3,5,9,10,12,14,16,17,19,21,23,25,29,30,32,34,36,37,39)]
View(slow_color)
dim(fast_color)

rowMeans(fast_color[, c(1:10)])
rowMeans(fast_color[, c(11:20)])


fastcolor <- rowSums(fast_color[c(1:10),])

fastcolor
slowcolor <- rowSums(slow_color[c(1:10),])

slowcolor


post_sum_slow_clr <- sum(post_slow_clr[,c(236,238,240,244,245,247,249,251,252,254)], na.rm = TRUE)
nrow_post_slow_clr <- nrow(post_slow_clr)
post_pos_avg_slow_clr <- post_sum_slow_clr / nrow_post_slow_clr

post_pos_avg_slow_clr

# 2. fast achromatic condition
post_sum_fast_achr <- sum(post_fast_achr[,c(236,238,240,244,245,247,249,251,252,254)], na.rm = TRUE)

View(post_fast_achr)

post_fast_achr$result <- colSums(post_fast_achr[,c("V236", "V238", "V240","V244","V245","V247","V249","V251","V252","V254")])  

nrow_post_fast_achr <- nrow(post_fast_achr)
post_pos_avg_fast_achr <- post_sum_fast_achr / nrow_post_fast_achr


# 3. fast color condition
post_sum_fast_clr <- sum(post_fast_clr[,c(236,238,240,244,245,247,249,251,252,254)], na.rm = TRUE)
nrow_post_fast_clr <- nrow(post_fast_clr)
post_pos_avg_fast_clr <- post_sum_fast_clr / nrow_post_fast_clr

post_pos_avg_fast_clr

# 4. slow achromatic condition
post_sum_slow_achr <- sum(post_slow_achr[,c(236,238,240,244,245,247,249,251,252,254)], na.rm = TRUE)
nrow_post_slow_achr <- nrow(post_slow_achr)
post_pos_avg_slow_achr <- post_sum_slow_achr / nrow_post_slow_achr


# analyze data
interaction <- aov(result ~ speed * color, data = data)


interaction
summary(interaction)







