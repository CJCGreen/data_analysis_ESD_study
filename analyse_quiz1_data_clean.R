# C Green 26th August 2021
# analyse_quiz1_data_clean.R
# R script for results in Chapter 5: Results of ESD Learning Outcomes Experiment
################################################################################
#
# Quiz 1 data: Basic and inferential statistics and visualisations of qualitative data
#
# Note on dataframe names:
# quiz1: all participant data (total 106)
# quiz1_no_outlier: the same set minus one outlier (total 105)

# Use xlsx package to import Excel
# install.packages("xlsx")
library(xlsx)

# quiz1 dataframe contains quiz1 data for all groups
quiz1 <- read.xlsx("RDataSources/scores_tidy_ORIGINAL_DO_NOT_DELETE.xlsx",sheetIndex=3)

# Create new ST and Sim properties, one for each factor (necessary for inferential statistical tests)
quiz1$ST <- as.factor(ifelse(quiz1$Group==1, 1, ifelse(quiz1$Group==3, 1, 0)))
quiz1$Sim <- as.factor(ifelse(quiz1$Group==2, 1, ifelse(quiz1$Group==3, 1, 0)))

# Rename the groups and order them as factors (improves graph and table presentation)
quiz1$Group <- ifelse(quiz1$Group==0, "Control", ifelse(quiz1$Group==1, "ST", ifelse(quiz1$Group==2, "Sim","ST+Sim"))) 
quiz1$Group <- factor(quiz1$Group, levels = c("Control", "ST", "Sim", "ST+Sim"))

# Summary figures for all participants:
nrow(quiz1)                     # 106
summary(quiz1$QuizScore)        # Min 40, max 97, mean 74.29, median 77.00
sort(table(quiz1$QuizScore))    # Modes 77, 80
sd(quiz1$QuizScore)             # 12.19599

# Histogram - all scores (INC outlier)
hist(quiz1$QuizScore, main="Histogram: Quiz 1 All Scores", xlab="Percentage Score", col="azure3")

# Histogram - as above, displaying probability density on the y axis and an overlayed fitted density line:
hist(quiz1$QuizScore, 
     main="Histogram: Quiz 1 All Scores with density line", 
     xlab="Percentage Score", 
     col="azure3",
     freq=FALSE)
lines(density(quiz1$QuizScore),col=2) 

###########################
# GROUP 0 (Control group) #
###########################

# Get Group 0 data
quiz1group0 <- quiz1[quiz1$Group == "Control",]

# Summary figures group 0:
nrow(quiz1group0)                   # 28
summary(quiz1group0$QuizScore)      # Min 40, max 92, mean 70.79, median 73.50
sort(table(quiz1group0$QuizScore))  # Modes 74, 80
sd(quiz1group0$QuizScore)           # 10.95566

# Histogram..
hist(quiz1group0$QuizScore, main="Histogram: Quiz 1 Group 0 Scores", xlab="Percentage Score", col="azure3")

# .. with density line
hist(quiz1group0$QuizScore, 
     main="Histogram: Quiz 1 Group 0 Scores", 
     xlab="Percentage Score", 
     col="azure3",
     freq=FALSE)
lines(density(quiz1group0$QuizScore),col=2) 

######################
# GROUP 1 (ST group) #
######################

# Get group 1 data
quiz1group1 <- quiz1[quiz1$Group == "ST",]

# Summary figures for group 1:
nrow(quiz1group1)                   # 26
summary(quiz1group1$QuizScore)      # Min 55, max 97, mean 75.77, median 76.50
sort(table(quiz1group1$QuizScore))  # Mode 77
sd(quiz1group1$QuizScore)           # 11.31833

# Histogram..
hist(quiz1group1$QuizScore, main="Histogram: Quiz 1 Group 1 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz1group1$QuizScore, 
     main="Histogram: Quiz 1 Group 1 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz1group1$QuizScore),col=2) 

#######################
# GROUP 2 (Sim group) #
#######################

# Get group 2 data
quiz1group2 <- quiz1[quiz1$Group == "Sim",]

# Summary figures for group 2:
nrow(quiz1group2)                   # 24
summary(quiz1group2$QuizScore)      # Min 48, max 96, mean 78.38, median 84.00
sort(table(quiz1group2$QuizScore))  # Modes 86 87 89 91
sd(quiz1group2$QuizScore)           # 14.06449

# Histogram..
hist(quiz1group2$QuizScore, main="Histogram: Quiz 1 Group 2 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz1group2$QuizScore, 
     main="Histogram: Quiz 1 Group 2 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz1group2$QuizScore),col=2) 

############################
# GROUP 3 (ST + Sim group) #
############################

# Get Group 3 data
quiz1group3 <- quiz1[quiz1$Group == "ST+Sim",]

# Summary figures for group 3:
nrow(quiz1group3)                   # 28
summary(quiz1group3$QuizScore)      # Min 46, max 97, mean 72.93, median 77.00
sort(table(quiz1group3$QuizScore))  # Modes 77 80
sd(quiz1group3$QuizScore)           # 11.81941

# Histogram..
hist(quiz1group3$QuizScore, main="Histogram: Quiz 1 Group 3 Scores", xlab="Percentage Score", col="bisque2")

# .. with density line
hist(quiz1group3$QuizScore, 
     main="Histogram: Quiz 1 Group 3 Scores", 
     xlab="Percentage Score", 
     col="bisque2",
     freq=FALSE)
lines(density(quiz1group3$QuizScore),col=2) 

###########################
# VISUALISE QUIZ 1 SCORES #
###########################

####################################
# ggplot dotplots and violin plots #
####################################

# install.packages("ggplot2")
library(ggplot2)

# Dot plot
ggplot(quiz1, aes(x=Group, y=QuizScore, fill=Group)) + 
    ggtitle("Dot Plot: Quiz 1 Scores by Group\n(Each group mean shown in red, median in navy)") +
    scale_fill_manual(values = c("azure3", "bisque2", "bisque2", "bisque2")) +
    labs(y="Quiz 1 Scores (%)", x = "Group") +
    geom_dotplot(binaxis='y', stackdir='center', stackratio=1.3, dotsize=1.2, position="dodge", binwidth=1) + 
    scale_y_continuous(breaks = seq(min(quiz1$QuizScore), max(100), by = 10)) +
    theme(legend.position="none") +
    stat_summary(fun=mean, geom="point", shape=18, size=4, color="red") +
    stat_summary(fun=median, geom="point", shape=18, size=4, color="navy")

# This version combines dot plot with violin plot for each group
ggplot(quiz1, aes(x=Group, y=QuizScore, fill=Group)) + 
    ggtitle("Dot Plot and Violin Plot: Quiz 1 Scores by Group") +
    scale_fill_manual(values = c("azure3", "bisque2", "bisque2", "bisque2")) +  
    labs(y="Quiz 1 Scores (%)", x = "Group") +
    theme(legend.position="none") +
    geom_violin(trim = FALSE) +
    geom_dotplot(binaxis='y', stackdir='center', stackratio=1.3, dotsize=1.2, position="dodge", binwidth=1) +
    scale_y_continuous(breaks = seq(min(20), max(100), by = 10))

############
# Boxplots #
############

# Side-by-side boxplots for all groups, and all participants
boxplot(quiz1$QuizScore,
        quiz1group0$QuizScore,
        quiz1group1$QuizScore, 
        quiz1group2$QuizScore, 
        quiz1group3$QuizScore, 
        main="Boxplot: Quiz 1 Scores by Group", 
        names = c("All", "Control", "ST", "Sim", "ST+Sim"),
        ylab = "Score (%)",
        xlab = "Group",
        ylim = c(40, 100),
        col= c("aquamarine3", "azure3", "bisque2", "bisque2", "bisque2"))

####################################
# Density plots (ggplot and dplyr) #
####################################

# Density plot: a smooth density estimate
# Basic plot
a <- ggplot(quiz1, aes(x = QuizScore))
# Change line colours by Group, change colours manually and add titles
a + geom_density(aes(color = Group)) +
    scale_color_manual(values=c("#000000", "#388e3c", "#b71c1c","#1565c0" )) +
    ggtitle("Density Plot: Quiz 1 Scores by Group") +
    labs(x="Quiz 1 Scores (%)", y = "density")+
    theme_classic()

# Add mean line
# install.packages(plyr)
library(plyr)
means <- ddply(quiz1, "Group", summarise, grp.mean=mean(QuizScore))

a + geom_density(aes(color = Group)) +
    geom_vline(data=means, aes(xintercept=grp.mean, color=Group),
               linetype="dashed") +
    scale_color_manual(values=c("#000000", "#388e3c", "#b71c1c","#1565c0" )) +
    ggtitle("Density Plot: Quiz 1 Scores by Group, with Group Means") +
    labs(x="Quiz 1 Scores (%)", y = "density")+
    theme_classic()

############################
# GROUP AND SCORE CROSSTAB #
############################

# To construct a crosstab showing group against score interval, recode score to make it ordinal/categorical

# Function to recode score. Note: First version (thresholds at 60 and 80) did not show many low scores, scoring overall was high
recode_score <- function(x) {
    result <- "Neutral"
    ifelse(x < 65, result <- "Low", ifelse(x < 85, result <- "Medium", result <- "High"))
    return(result)
}

# Create new column and make sure factor is ordered correctly (Low < Medium < High):
# without the last arg they are ordered alphabetically
quiz1$ScoreCategory <- factor(sapply(quiz1$QuizScore, recode_score), levels = c("Low", "Medium", "High"))

# Now create a crosstab (frequency table) of group vs score
table(quiz1$ScoreCategory, quiz1$Group)
# Result:
#        Control ST Sim ST+Sim
# Low          7  6   5      6
# Medium      20 14   7     18
# High         1  6  12      4

##################
# REMOVE OUTLIER #
##################

# Outlier had a score of 40 and was in the Control group
quiz1_no_outlier <- quiz1[quiz1$ParticipantID != 97,]

# Recreate group data for quiz 1
quiz1_no_outlier_group0 <- quiz1_no_outlier[quiz1_no_outlier$Group == "Control",]
quiz1_no_outlier_group1 <- quiz1_no_outlier[quiz1_no_outlier$Group == "ST",]
quiz1_no_outlier_group2 <- quiz1_no_outlier[quiz1_no_outlier$Group == "Sim",]
quiz1_no_outlier_group3 <- quiz1_no_outlier[quiz1_no_outlier$Group == "ST+Sim",]

# Recalculate the summary statistics for all remaining scores, and for each group
nrow(quiz1_no_outlier)                          # 105
summary(quiz1_no_outlier$QuizScore)             # Min 46, max 97, mean 74.62, median 77
sort(table(quiz1_no_outlier$QuizScore))         # Modes 77, 80
sd(quiz1_no_outlier$QuizScore)                  # 11.77953

nrow(quiz1_no_outlier_group0)                   # 27
summary(quiz1_no_outlier_group0$QuizScore)      # Min 51, max 92, mean 71.93, median 74
sort(table(quiz1_no_outlier_group0$QuizScore))  # Modes 74, 80
sd(quiz1_no_outlier_group0$QuizScore)           # 9.318823

nrow(quiz1_no_outlier_group1)                   # 26
summary(quiz1_no_outlier_group1$QuizScore)      # Min 55, max 97, mean 75.77, median 76.50
sort(table(quiz1_no_outlier_group1$QuizScore))  # Modes 77
sd(quiz1_no_outlier_group1$QuizScore)           # 11.31833

nrow(quiz1_no_outlier_group2)                   # 24
summary(quiz1_no_outlier_group2$QuizScore)      # Min 48, max 96, mean 78.38, median 84
sort(table(quiz1_no_outlier_group2$QuizScore))  # Modes 86 87 89 91
sd(quiz1_no_outlier_group2$QuizScore)           # 14.06449

nrow(quiz1_no_outlier_group3)                   # 28
summary(quiz1_no_outlier_group3$QuizScore)      # Min 46, max 97, mean 72.93, median 77
sort(table(quiz1_no_outlier_group3$QuizScore))  # Modes 77, 80
sd(quiz1_no_outlier_group3$QuizScore)           # 11.81941

###############################################################
# COMPARING AVERAGE GROUP SCORES FOR EACH QUESTION IN QUIZ 1 #
###############################################################
# Note: outlier excluded

# How did the groups compare in their scores for Qn 1 (DEER EXP GROWTH CALC)
barplot(c(mean(quiz1_no_outlier_group0$Q1), mean(quiz1_no_outlier_group1$Q1), 
          mean(quiz1_no_outlier_group2$Q1), mean(quiz1_no_outlier_group3$Q1)),
        main="Mean Qn 1 Scores by Group", 
        xlab="Group", ylab="Qn 1 Score", 
        ylim = c(0, 5),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# X # Control group best

# Qn 2 (UNITS VEG EATEN PER YEAR - V SIMPLE)
barplot(c(mean(quiz1_no_outlier_group0$Q2), mean(quiz1_no_outlier_group1$Q2), 
          mean(quiz1_no_outlier_group2$Q2), mean(quiz1_no_outlier_group3$Q2)),
        main="Mean Qn 2 Scores by Group", 
        xlab="Group", ylab="Qn 2 Score", 
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# X # All groups the same

# Qn 3 (MAX CAPACITY - SIMPLE CALC)
barplot(c(mean(quiz1_no_outlier_group0$Q3), mean(quiz1_no_outlier_group1$Q3), 
          mean(quiz1_no_outlier_group2$Q3), mean(quiz1_no_outlier_group3$Q3)),
        main="Mean Qn 3 Scores by Group", 
        xlab="Group", ylab="Qn 3 Score", 
        ylim = c(0, 2),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Groups 1 best, then group 3; all groups better than control

# Qn 4 (CONSEQUENCES OF REACHING MAX CAPACITY)
barplot(c(mean(quiz1_no_outlier_group0$Q4), mean(quiz1_no_outlier_group1$Q4), 
          mean(quiz1_no_outlier_group2$Q4), mean(quiz1_no_outlier_group3$Q4)),
        main="Mean Qn 4 Scores by Group", 
        xlab="Group", ylab="Qn 4 Score", 
        ylim = c(0, 6),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Group 1 > group 2 > group 3; all better than control

# Qn 5 (YEARS TO MAX CAPACITY)
barplot(c(mean(quiz1_no_outlier_group0$Q5), mean(quiz1_no_outlier_group1$Q5), 
          mean(quiz1_no_outlier_group2$Q5), mean(quiz1_no_outlier_group3$Q5)),
        main="Mean Qn 5 Scores by Group", 
        xlab="Group", ylab="Qn 5 Score", 
        ylim = c(0, 7),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Group 1 best, no other groups better than control

# Qn 6 (DEER SUSTAINABILITY DEFINITION)
barplot(c(mean(quiz1_no_outlier_group0$Q6), mean(quiz1_no_outlier_group1$Q6), 
          mean(quiz1_no_outlier_group2$Q6), mean(quiz1_no_outlier_group3$Q6)),
        main="Mean Qn 6 Scores by Group", 
        xlab="Group", ylab="Qn 6 Score",
        ylim = c(0, 4),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Group 2 best, groups 1 and 3 second best, all groups better than control

# Qn 7 (WHETHER HERD SUSTAINABLE)
barplot(c(mean(quiz1_no_outlier_group0$Q7), mean(quiz1_no_outlier_group1$Q7), 
          mean(quiz1_no_outlier_group2$Q7), mean(quiz1_no_outlier_group3$Q7)),
        main="Mean Qn 7 Scores by Group", 
        xlab="Group", ylab="Qn 7 Score", 
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# All treatment groups have the same score, and all are better than control

# Qn 8 (EASY VEG GROWTH QN)
barplot(c(mean(quiz1_no_outlier_group0$Q8), mean(quiz1_no_outlier_group1$Q8), 
          mean(quiz1_no_outlier_group2$Q8), mean(quiz1_no_outlier_group3$Q8)),
        main="Mean Qn 8 Scores by Group", 
        xlab="Group", ylab="Qn 8 Score",
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Groups 1 and 2 best, group 3 = control

# Qn 9 (MULTI-STOCK LEVEL - SIMULATION TEST QN)
barplot(c(mean(quiz1_no_outlier_group0$Q9), mean(quiz1_no_outlier_group1$Q9), 
          mean(quiz1_no_outlier_group2$Q9), mean(quiz1_no_outlier_group3$Q9)),
        main="Mean Qn 9 Scores by Group", 
        xlab="Group", ylab="Qn 9 Score", 
        ylim = c(0,6),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Simulation group much better, groups 1 = control, group 3 a little better

# Qn 10 (CARRYING CAPACITY)
barplot(c(mean(quiz1_no_outlier_group0$Q10), mean(quiz1_no_outlier_group1$Q10), 
          mean(quiz1_no_outlier_group2$Q10), mean(quiz1_no_outlier_group3$Q10)),
        main="Mean Qn 10 Scores by Group", 
        xlab="Group", ylab="Qn 10 Score", 
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# X # No groups better than control

# Qn 11 (INCREASE DEER POPULATION)
barplot(c(mean(quiz1_no_outlier_group0$Q11), mean(quiz1_no_outlier_group1$Q11), 
          mean(quiz1_no_outlier_group2$Q11), mean(quiz1_no_outlier_group3$Q11)),
        main="Mean Qn 11 Scores by Group", 
        xlab="Group", ylab="Qn 11 Score",
        ylim = c(0,6),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# X # Sim a fraction better than control, group 1 about = to control and group 3 worse

# Qn 12 (SUSTAINABLE GRAPHS)
barplot(c(mean(quiz1_no_outlier_group0$Q12), mean(quiz1_no_outlier_group1$Q12), 
          mean(quiz1_no_outlier_group2$Q12), mean(quiz1_no_outlier_group3$Q12)),
        main="Mean Qn 12 Scores by Group", 
        xlab="Group", ylab="Qn 12 Score", 
        ylim = c(0,10),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# All treatment groups better than control, group 2 a little better than the rest

# Qn 13 (STABILITY)
barplot(c(mean(quiz1_no_outlier_group0$Q13), mean(quiz1_no_outlier_group1$Q13), 
          mean(quiz1_no_outlier_group2$Q13), mean(quiz1_no_outlier_group3$Q13)),
        main="Mean Qn 13 Scores by Group", 
        xlab="Group", ylab="Qn 13 Score", 
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Group 2 less than control, others only a little better

# Qn 14 (REASONS FOR KAIBAB COLLAPSE)
barplot(c(mean(quiz1_no_outlier_group0$Q14), mean(quiz1_no_outlier_group1$Q14), 
          mean(quiz1_no_outlier_group2$Q14), mean(quiz1_no_outlier_group3$Q14)),
        main="Mean Qn 14 Scores by Group", 
        xlab="Group", ylab="Qn 14 Score", 
        ylim=c(0,10),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Group 3 best, then group 1; group 2 about = control

# Qn 15 (GRAPH OVERGRAZING BEGINS)
barplot(c(mean(quiz1_no_outlier_group0$Q15), mean(quiz1_no_outlier_group1$Q15), 
          mean(quiz1_no_outlier_group2$Q15), mean(quiz1_no_outlier_group3$Q15)),
        main="Mean Qn 15 Scores by Group", 
        xlab="Group", ylab="Qn 15 Score", 
        ylim=c(0,8),
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# Groups 2 best, group 3 close; group 1 a little less than control

# Qn 16 (POLICIES FOR SUSTAINABLE HERD)
barplot(c(mean(quiz1_no_outlier_group0$Q16), mean(quiz1_no_outlier_group1$Q16), 
          mean(quiz1_no_outlier_group2$Q16), mean(quiz1_no_outlier_group3$Q16)),
        main="Mean Qn 16 Scores by Group", 
        xlab="Group", ylab="Qn 16 Score", 
        names.arg=c("Control", "ST", "Sim", "ST+Sim"))
# All groups better than control, group 2 best

##########################
# INFERENTIAL STATISTICS #
##########################

# Step numbers refer to the steps in the inferential analysis in the chapter

# Step 2b: Check Factorial ANOVA assumptions

# 1 Test homogeneity of variances
library(car)
leveneTest(aov(QuizScore ~ ST * Sim, quiz1_no_outlier))
# Note: The leveneTest requires a saturated, or fully-crossed model, including the interactions. The formula uses the * operator.
# Result:
#    Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.9597 0.4149
#      101

# 2 Test normality of residuals
res <- residuals(aov(QuizScore ~ ST + Sim, quiz1_no_outlier))  
shapiro.test(res)
# Note: The Shapiro.test does not require a saturated model. The formula uses the + operator.
# Result:
# Shapiro-Wilk normality test
# data:  res
# W = 0.98002, p-value = 0.1142

# Repeat these tests with outlier included:

# 1 Test homogeneity of variances
leveneTest(aov(QuizScore ~ ST * Sim, quiz1))
# Result:
#    Levene's Test for Homogeneity of Variance (center = median)
# Df F value Pr(>F)
# group   3  0.4715 0.7028
# 102 

# 2 Test normality of residuals
res_all <- residuals(aov(QuizScore ~ ST + Sim, quiz1))  
shapiro.test(res_all)
# Result:
# Shapiro-Wilk normality test
# 
# data:  res_all
# W = 0.97771, p-value = 0.07137

# Step 3: Run Factorial ANOVA overall test in R on all group scores
model_factors <- aov(QuizScore ~ ST * Sim, quiz1_no_outlier)
summary(model_factors)
# Result:
#              Df Sum Sq Mean Sq F value Pr(>F)  
# ST            1     12    11.6   0.085 0.7714  
# Sim           1     73    72.7   0.533 0.4670  
# ST:Sim        1    564   564.5   4.137 0.0446 *
# Residuals   101  13782   136.5                 
# ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Repeat the Factorial ANOVA test with outlier included:
model_factors_all <- aov(QuizScore ~ ST * Sim, quiz1)
summary(model_factors_all)
# Result:
#              Df Sum Sq Mean Sq F value Pr(>F)  
# ST            1      0     0.0   0.000 0.9973  
# Sim           1    135   135.3   0.935 0.3359  
# ST:Sim        1    718   717.8   4.959 0.0282 *
#     Residuals   102  14765   144.8                 
# ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Step 4: Interaction plot
interaction.plot(x.factor = quiz1_no_outlier$ST, 
                 trace.factor = quiz1_no_outlier$Sim,
                 response = quiz1_no_outlier$QuizScore,
                 ylab="Quiz 1 Mean Scores", xlab="Systems Thinking (ST)", 
                 main = "Interaction plot for ST and Simulation",
                 trace.label="Simulation (Sim)")

# Get the linear model (lm)
lm(model_factors)
# Result:
# Call:
#    lm(formula = model_factors)
#
# Coefficients:
# (Intercept)          ST1         Sim1     ST1:Sim1  
#      71.926        3.843        6.449       -9.290  

summary(lm(model_factors))
# Result:
# ..
# Multiple R-squared:  0.04496,	Adjusted R-squared:  0.01659 
# F-statistic: 1.585 on 3 and 101 DF,  p-value: 0.1977

# Step 5: Remove ST+Sim group data
quiz1_no_outlier_no_gp3 <- quiz1_no_outlier[quiz1_no_outlier$Group!="ST+Sim",]
# Repeat, including the outlier:
quiz1_no_gp3 <- quiz1[quiz1$Group!="ST+Sim",]

# Step 6b: Check assumptions for one-way ANOVA test

# Test homogeneity of variances
leveneTest(aov(QuizScore ~ Group, quiz1_no_outlier_no_gp3))
# Result: 
#    Levene's Test for Homogeneity of Variance (center = median)
#    Df F value Pr(>F)
# group  2  1.3596 0.2631
#      74

# Test normality of residuals
res_no_gp3 <- residuals(aov(QuizScore ~ Group, quiz1_no_outlier_no_gp3))
shapiro.test(res_no_gp3)  
# Result:
# Shapiro-Wilk normality test
#
# data:  res_no_gp3
# W = 0.96565, p-value = 0.03541

# Plot histogram of residuals
hist(x= res_no_gp3, main="Histogram of Residuals (Quiz 1 minus outlier, minus ST+Sim group)", xlab="Residuals")

# Step 7: Run the Kruskal-Wallis overall test
kruskal.test(QuizScore ~ Group, quiz1_no_outlier_no_gp3)

# Result:
#    Kruskal-Wallis rank sum test
# data:  QuizScore by Group
# Kruskal-Wallis chi-squared = 5.3987, df = 2, p-value = 0.06725

# Step 8: Run the Kruskal-Wallis test again with the outlier
kruskal.test(QuizScore ~ Group, data = quiz1_no_gp3)

# Result:
#    Kruskal-Wallis rank sum test
# data:  QuizScore by Group
# Kruskal-Wallis chi-squared = 6.273, df = 2, p-value = 0.04344

# Step 10: Conduct Wilcoxon Rank Sum tests to make pairwise group comparisons

# 1 One-tailed test with Bonferroni correction
pairwise.wilcox.test(quiz1_no_outlier_no_gp3$QuizScore,
                     quiz1_no_outlier_no_gp3$Group,
                     p.adjust.method = "bonferroni", 
                     exact = FALSE, alternative="greater")

# Result:
#     Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# data:  quiz1_no_outliers_no_gp3$QuizScore and quiz1_no_outliers_no_gp3$Group
#     Control   ST   
# ST  0.371     -    
# Sim 0.027   0.448
# P value adjustment method: Bonferroni

# 2 One-tailed test with no correction
pairwise.wilcox.test(quiz1_no_outlier_no_gp3$QuizScore,
                     quiz1_no_outlier_no_gp3$Group, 
                     p.adjust.method = "none", 
                     exact = FALSE, alternative="greater")
# Result:
#     Pairwise comparisons using Wilcoxon rank sum test with continuity  correction 
# data:  quiz1_no_outliers_no_gp3$QuizScore and quiz1_no_outliers_no_gp3$Group 
#     Control     ST    
# ST   0.1235     -     
# Sim  0.0091   0.1493
# P value adjustment method: none

# Step 11: Repeat one-tailed test with no correction - with outlier back in
pairwise.wilcox.test(quiz1_no_gp3$QuizScore,
                     quiz1_no_gp3$Group, 
                     p.adjust.method = "none", 
                     exact = FALSE, alternative="greater")
# Result:
# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# data:  quiz1_no_gp3$QuizScore and quiz1_no_gp3$Group 
#     Control ST    
# ST  0.0883  -     
# Sim 0.0059  0.1493
# P value adjustment method: none 

###############
# EFFECT SIZE #
###############

# install.packages("effsize")
library(effsize)

#############
# Cohen's d #
#############
# Effect sizes are re-categorised as small (0.2), medium (0.4) or large (0.6) in education context 
# (Cumming, G. & Calin-Jageman, R. 2016)

# With outlier
# Using control group sd
cohen.d(quiz1group1$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.4548806 (medium)
cohen.d(quiz1group2$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.6927275 (large)
cohen.d(quiz1group3$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.1955936 (small)
# Pooled sd
cohen.d(quiz1group1$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.4476952 (medium)
cohen.d(quiz1group2$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.6080058 (large)
cohen.d(quiz1group3$QuizScore, quiz1group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.1880405 (small)

# Without outlier
# Using control group sd
cohen.d(quiz1_no_outlier_group1$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.4124239 (medium)
cohen.d(quiz1_no_outlier_group2$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.6920482 (large)
cohen.d(quiz1_no_outlier_group3$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=FALSE, hedges.correction=FALSE) # 0.1075936 (very small)
# Pooled sd
cohen.d(quiz1_no_outlier_group1$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.3714283 (medium)
cohen.d(quiz1_no_outlier_group2$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.5471449 (medium)
cohen.d(quiz1_no_outlier_group3$QuizScore, quiz1_no_outlier_group0$QuizScore, paired=FALSE, pooled=TRUE, hedges.correction=FALSE) # 0.09400202 (very small)

########################
# QUALITATIVE ANALYSIS #
########################

############################################
# General comments about the learning tool #
############################################

# Word cloud
textfile <- "RTextFiles/general_feedback_on_learning_tool_corrected.txt"
myStopWords <- c("found", "can", "made", "used", "ones", "second", "seems", "taking", "back", 
                 "way", "make", "done", "quite", "first", "two", "around", "section","without", 
                 "also", "use", "one", "content", "without", "will")
minWordFreq <- 4
analyseFreqWords(textfile, myStopWords, minWordFreq, wordcloud="True", barplot="False", ngrams="False")

################################
# Feedback on usefulness of ST #
################################

# Quantitative feedback (scores):
# There are blanks (NA) where participants did not have access to certain sections and therefore gave no feedback
feedback <- read.xlsx("RDataSources/scores_tidy_ORIGINAL_DO_NOT_DELETE.xlsx",sheetIndex=5)

# ST Useful score for all participants - include group
STUseful <- data.frame(feedback$Group, feedback$STUsefulScore)
# Strip missing values (NA) for group members who did not see ST section
STUseful <- STUseful[!is.na(STUseful$feedback.STUsefulScore),]

# Create a frequency table of ST Useful scores for all participants
STUseful_freq <- table(STUseful$feedback.STUsefulScore)
STUseful_freq
# Result:
# 1  2  3  4 
# 2 12 29 11 

# Bar graph
barplot(STUseful_freq,
        main="Usefulness of Systems Thinking section: ST and ST + Sim Group Participants",
        col=brewer.pal(nrow(STUseful_freq), "Set2"),
        ylab="Number of participants",
        ylim = c(0, 30),
        names = c("Helped a little", "Helped somewhat", "Helped quite a lot", "Really transformed"), )

# Qualitative feedback (comments):
# Word cloud
textfile <- "RTextFiles/post_st_comments_corrected.txt"
myStopWords <- c("see", "use", "see", "use", "can", "need","often", "gives")
minWordFreq <- 2
analyseFreqWords(textfile, myStopWords, minWordFreq, wordcloud="True", barplot="False", ngrams="False")

#################################
# Feedback on usefulness of Sim #
#################################

# Quantitative feedback (scores):
SimUseful <- data.frame(feedback$Group, feedback$SimUsefulScore)
# Strip missing values (NA) for group members who did not see Simulation section
SimUseful <- SimUseful[!is.na(SimUseful$feedback.SimUsefulScore),]

# Create a frequency table of Simulation Useful scores for all participants
SimUseful_freq <- table(SimUseful$feedback.SimUsefulScore)
SimUseful_freq
# Result:
# 1  2  3  4 
# 1  7 32 13 

# Bar graph
barplot(SimUseful_freq,
        main="Usefulness of Simulation section: Sim and ST + Sim Group Participants",
        col=brewer.pal(nrow(SimUseful_freq), "Set2"),
        ylab="Number of participants",
        names = c("Helped a little", "Helped somewhat", "Helped quite a lot", "Really transformed"), )

# Qualitative feedback (comments):
# Word cloud
textfile <- "RTextFiles/post_sim_comments_corrected.txt"
myStopWords <- c("see", "also", "made", "might", "much", "seeing","thing", "can", "will")
minWordFreq <- 3
analyseFreqWords(textfile, myStopWords, minWordFreq, wordcloud="True", barplot="False", ngrams="False")

###########################################################
# Qualitative differences in definition of sustainability #
###########################################################

# CONTROL GROUP #

# Word cloud, horizontal bar plot and frequency table of bi/trigrams
textfile <- "RTextFiles/group0_deer_sustainability_defn_corrected.txt"
myStopWords <- c("sustainability", "without", "can", "way", "using", 
                 "something", "will", "used", "certain", "use", "means", 
                 "think", "able", "deer", "taking", "adequate", "new", "per")
minWordFreq <- 3
analyseFreqWords(textfile, myStopWords, minWordFreq)
# Result (bi/trigrams):
#                      bitritoken Freq
# 74                  enough food    3
# 241             population park    3
# 4           allowing vegetation    2
# 31                capacity park    2
# 78             enough resources    2
# 83                   every year    2
# 179            maximum capacity    2
# 226            park sustainable    2
# 243 population park sustainable    2
# 244          population remains    2
# 311           resources sustain    2
# 332          support population    2
# 389                 year enough    2

# Repeat for SIM GROUP #

# Word cloud, horizontal bar plot and frequency table of bi/trigrams
textfile <- "RTextFiles/group2_deer_sustainability_defn_corrected.txt"
myStopWords <- c("sustainability", "without", "can", "way", "using", 
                 "something", "will", "used", "certain", "use", "means", 
                 "think", "able", "deer", "taking", "adequate", "new", "per")
minWordFreq <- 3
# Sim group words required word stemming, hence rewriteStems is populated and sent as optional argument
# to analyseFreqWords() function, and stem argument is set to True
rewriteStems <- c("population", "vegetation", "rate", "deaths", "births", "equal", "balance", "number", 
                  "regenerate", "herd", "resource", "park", "eat", "quantity", "continue", "level", 
                  "growth", "cause", "future", "keep", "remain", "enough")
analyseFreqWords(textfile, myStopWords, minWordFreq, stem="True", rewriteStems)
# Result (bi/trigrams), some entries still stemmed:
#             bitritoken Freq
# 59          death rate    6
# 18         birth death    5
# 20    birth death rate    3
# 122        futur popul    3
# 375      veget regener    3
# 12        balanc birth    2
# 13  balanc birth death    2
# 24          birth rate    2
# 190          long term    2
# 240         popul park    2
# 243       popul remain    2
# 244 popul remain stabl    2
# 263         rate death    2
# 264    rate death rate    2
# 267         rate equal    2
# 300       remain stabl    2
# 383       within limit    2