library(MatchIt)
data()  # 查看R和MatChIt自带的数据集
data(lalonde)
# print(lalonde)

#提取lalonde中1974年收入无缺失的样本
# re74_nonvoid <- which(lalonde$re74 != 0)
# lalonde1 <- lalonde[re74_nonvoid,]  
# print(lalonde1)

married <- lalonde$married
print(sd(married))
mean1 <- mean(lalonde$re78[lalonde$treat==1])
mean2 <- mean(lalonde$re78[lalonde$treat==0])
print(mean1-mean2)

# 输出pre-matching的table1
Xvars <- cbind("age","educ","black","hispan","married","nodegree","re74","re75")
unmatched.tableone <- CreateTableOne(vars = Xvars, strata = "treat",
                                     data = lalonde, test = FALSE)
print(unmatched.tableone, smd=TRUE)

###################### Doing Propensity Score Matching######################
# Method 1：
# 首先使用logistic Regression估计倾向性得分;
# 然后使用Matching包中的match函数进行匹配.
ps.model <- glm(treat ~ age + educ + black + hispan + married +
                  nodegree + re74 + re75, family = binomial(), 
                data=lalonde)
summary(ps.model)

# 创建Propensity score
pscore <- ps.model$fitted.values
print(min(pscore))
print(max(pscore))

# Do nearest neighbour on logit(propensity score) without caliper
set.seed(931139)
ps.match1 <- Match(Tr=lalonde$treat, M=1, X=pscore,
                   replace=FALSE)

matchdata1 <-lalonde[unlist(ps.match1[c("index.treated","index.control")]),]
# 输出post-matching的table1
xvars <- c("age","educ","black","hispan","married","nodegree","re74","re75")
matched.tableone1 <- CreateTableOne(vars = xvars,strata = "treat", 
                                 data = matchdata1,test = FALSE)
print(matched.tableone1,smd=TRUE)

# Do nearest neighbour on logit(propensity score) with caliper
set.seed(931139)
ps.match2 <- Match(Tr=lalonde$treat, M=1, X=log(pscore),
                   replace=FALSE,caliper=0.1)
matchdata2 <-lalonde[unlist(ps.match2[c("index.treated","index.control")]),]
# 输出post-matching的table1
xvars <- c("age","educ","black","hispan","married","nodegree","re74","re75")
matched.tableone2 <- CreateTableOne(vars = xvars,strata = "treat", 
                                    data = matchdata2,test = FALSE)
print(matched.tableone2, smd=TRUE)

# Method 2:
# 直接使用MatchIt包中的matchit函数，可以直接计算Propensity score
# 并且进行match，无需分成两步来做.
set.seed(931139)
prp.out <- matchit(data = lalonde1,
                   formula = treat ~ age + educ + black + hispan + married +
                     nodegree + re74 + re75,
                   method = "nearest",
                   distance = "logit",
                   replace = FALSE,
                   caliper = 0.2) 
        
plot(prp.out, type = "hist")
plot(prp.out, type = "jitter")
summary(prp.out, standardize = T)$sum.matched #考察配平前后，处理组和实验组各变量的基本统计量

#########################Outcome analysis##############################
y_trt <- matchdata2$re78[matchdata2$treat==1]
y_con <- matchdata2$re78[matchdata2$treat==0]
y_Meandiff <- mean(y_trt)-mean(y_con)
print(y_Meandiff)

# 进行两样本t检验
test <- t.test(y_trt, y_con)
print(test)
# # 进行因果效应的推断，两种方法：直接比较法和线性回归法
# prp.data <- match.data(prp.out)
# 
# # Direct compare
# 
# result <- wtd.t.test(prp.data$re78[prp.data$treat == 1],
#                   prp.data$re78[prp.data$treat == 0],
#                   weight = prp.data$weights[prp.data$treat == 1],
#                   weighty = prp.data$weights[prp.data$treat == 0])
# print(result)
# mu <- result$additional[1]
# std <- result$additional[4]
# cat("Confidence interval: ", sapply(qt(c(0.025, 0.975), coef(result)["df"]), function(x){return(mu+x*std)}), "\n")
# 
# # Fit
# att.fml <- re78 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75
# fit <- lm(att.fml, data = prp.data, weights = prp.data$weights)
# summary(fit)
# cat("Confidence interval: ", confint(fit, "treat", 0.95), "\n")

