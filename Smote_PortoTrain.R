# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

install.packages('unbalanced')
system('defaults write org.R-project.R force.LANG en_US.UTF-8')
install.packages('Amelia')
library('unbalanced')
library('Amelia')
credit.df <- read.csv("porto-training.csv")
summary(credit.df)

colnames(credit.df) <- c('id', 'ps_car_13', 'ps_car_12', 
                         'ps_ind_17_bin', 'ps_car_07_cat', 'ps_reg_02', 
                         'ps_ind_07_bin', 'ps_ind_06_bin', 'ps_car_04_cat', 
                         'ps_car_03_cat', 'ps_car_02_cat', 'ps_reg_03',
                         'ps_ind_05_cat', 'ps_ind_16_bin', 'ps_car_15',
                         'ps_reg_01', 'ps_ind_15', 'ps_car_05_cat',
                         'ps_car_08_cat', 'ps_ind_01', 'ps_car_01_cat', 'target')

credit.df$target <- as.factor(credit.df$target)
credit.df$ps_ind_17_bin <- as.factor(credit.df$ps_ind_17_bin)
credit.df$ps_ind_07_bin <- as.factor(credit.df$ps_ind_07_bin)
credit.df$ps_ind_06_bin <- as.factor(credit.df$ps_ind_06_bin)
credit.df$ps_ind_16_bin <- as.factor(credit.df$ps_ind_16_bin)
credit.df$ps_car_07_cat <- as.ordered(credit.df$ps_car_07_cat)
credit.df$ps_car_04_cat <- as.ordered(credit.df$ps_car_04_cat)
credit.df$past_due_gt_90 <- as.ordered(credit.df$past_due_gt_90)
credit.df$lines <- as.ordered(credit.df$lines)
credit.df$mortgages <- as.ordered(credit.df$mortgages)
credit.df$dependents <- as.ordered(credit.df$dependents)


summary(credit.df)
str(credit.df)

credit.df.imp <- amelia(credit.df, m = 1, p2s = 2, 
                        idvars = c('ID'),
                        ords = c('past_due_30_59', 'past_due_60_89', 'past_due_gt_90',
                                 'lines', 'mortgages', 'dependents', 'target'))

credit.df <- credit.df.imp$imputations[[1]]
credit.df$monthly_income[credit.df$monthly_income < 0] <- 0

ID <- credit.df$ID
credit.df$ID <- NULL
credit.target <- as.factor(credit.df$target)
credit.df$target <- NULL

credit.df.smote.out <-ubSMOTE(X=credit.df, Y=credit.target, verbose=TRUE)
target <- credit.df.smote.out$Y
credit.df.smote <- cbind(credit.df.smote.out$X, target)
write.csv(credit.df.smote, "porto-training-smote.csv")
