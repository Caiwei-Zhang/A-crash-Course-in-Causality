library(MatchIt)
data()  # �鿴R��MatChIt�Դ������ݼ�
data(lalonde)
# print(lalonde)

#��ȡlalonde��1974��������ȱʧ������
# re74_nonvoid <- which(lalonde$re74 != 0)
# lalonde1 <- lalonde[re74_nonvoid,]  
# print(lalonde1)

married <- lalonde$married
print(sd(married))
mean1 <- mean(lalonde$re78[lalonde$treat==1])
mean2 <- mean(lalonde$re78[lalonde$treat==0])
print(mean1-mean2)

# ���pre-matching��table1
Xvars <- cbind("age","educ","black","hispan","married","nodegree","re74","re75")
unmatched.tableone <- CreateTableOne(vars = Xvars, strata = "treat",
                                     data = lalonde, test = FALSE)
print(unmatched.tableone, smd=TRUE)

###################### Doing Propensity Score Matching######################
# Method 1��
# ����ʹ��logistic Regression���������Ե÷�;
# Ȼ��ʹ��Matching���е�match��������ƥ��.
ps.model <- glm(treat ~ age + educ + black + hispan + married +
                  nodegree + re74 + re75, family = binomial(), 
                data=lalonde)
summary(ps.model)

# ����Propensity score
pscore <- ps.model$fitted.values
print(min(pscore))
print(max(pscore))

# Do nearest neighbour on logit(propensity score) without caliper
set.seed(931139)
ps.match1 <- Match(Tr=lalonde$treat, M=1, X=pscore,
                   replace=FALSE)

matchdata1 <-lalonde[unlist(ps.match1[c("index.treated","index.control")]),]
# ���post-matching��table1
xvars <- c("age","educ","black","hispan","married","nodegree","re74","re75")
matched.tableone1 <- CreateTableOne(vars = xvars,strata = "treat", 
                                 data = matchdata1,test = FALSE)
print(matched.tableone1,smd=TRUE)

# Do nearest neighbour on logit(propensity score) with caliper
set.seed(931139)
ps.match2 <- Match(Tr=lalonde$treat, M=1, X=log(pscore),
                   replace=FALSE,caliper=0.1)
matchdata2 <-lalonde[unlist(ps.match2[c("index.treated","index.control")]),]
# ���post-matching��table1
xvars <- c("age","educ","black","hispan","married","nodegree","re74","re75")
matched.tableone2 <- CreateTableOne(vars = xvars,strata = "treat", 
                                    data = matchdata2,test = FALSE)
print(matched.tableone2, smd=TRUE)

# Method 2:
# ֱ��ʹ��MatchIt���е�matchit����������ֱ�Ӽ���Propensity score
# ���ҽ���match������ֳ���������.
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
summary(prp.out, standardize = T)$sum.matched #������ƽǰ�󣬴������ʵ����������Ļ���ͳ����

#########################Outcome analysis##############################
y_trt <- matchdata2$re78[matchdata2$treat==1]
y_con <- matchdata2$re78[matchdata2$treat==0]
y_Meandiff <- mean(y_trt)-mean(y_con)
print(y_Meandiff)

# ����������t����
test <- t.test(y_trt, y_con)
print(test)
# # �������ЧӦ���ƶϣ����ַ�����ֱ�ӱȽϷ������Իع鷨
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
