anova_result <- summary(aov(extra ~ group, data = sleep)) #balanced, n=10
ttest_result <- t.test(extra ~ group, data = sleep, var.equal=T)
options(digits = 10)
anova_result[[1]][["F value"]][1] #3.462626761
ttest_result$statistic^2 #3.462626761 

anova_result <- summary(aov(extra ~ group, data = sleep[-5,])) #unbalanced
ttest_result <- t.test(extra ~ group, data = sleep[-5,], var.equal=T)
anova_result[[1]][["F value"]][1] #2.773136741
ttest_result$statistic^2 #2.773136741


anova_result <- summary(aov(extra ~ group + Error(ID / group), data = sleep)) #repeated-measures
ttest_result <- t.test(sleep$extra[sleep$group == 1], 
                       sleep$extra[sleep$group == 2], paired = T) #paired t
anova_result[[2]][[1]][["F value"]][1] #16.50088132
ttest_result$statistic^2 #16.50088132

