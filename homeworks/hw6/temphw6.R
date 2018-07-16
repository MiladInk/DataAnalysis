library(dplyr)
library(tidyr)
library(highcharter)
library(psych)
library(ggplot2)
library(corrplot)

###################### Q1
train <- read.csv("~/university/Data_Analysis/hw_06/house/train.csv")
train %>% select(-Id) %>% select_if(is.numeric) -> train.num
cor(train.num, use = "pairwise.complete.obs") -> train.num.cor
hchart(train.num.cor)

cor.mtest(train.num) -> res1
corrplot(train.num.cor, p.mat = res1$p, sig.level = 2e-16,pch.cex = 0.5, tl.cex = 0.5)

train.num.cor %>% as.data.frame() -> train.num.cor
train.num.cor %>% select(SalePrice) %>% mutate(param = rownames(.))%>%
  arrange(-SalePrice) %>% slice(1:11)-> train.sale.factor
train.sale.factor %>% slice(2:11) %>% View()

##################### Q2
train %>% select(train.sale.factor$param) -> train.sale
train.sale %>% plot()
#################### Q3
lm(SalePrice ~ ., train.sale) -> model
summary(model)
#################### Q4
data.frame(predicted = model$fitted.values, real = train.sale$SalePrice, res = model$residuals)->real.est
hchart(real.est, type = "scatter", hcaes(x = real, y = predicted))
hchart(real.est, type = "scatter", hcaes(x = real, y = res))
#################### Q5
model %>% summary() %>% .$adj.r.squared
model %>% summary() %>% .$fstatistic
#################### Q6
train.sale %>% select(-GarageArea,-TotRmsAbvGrd,-FullBath, -X1stFlrSF)->train.sale
lm(SalePrice ~ ., train.sale)->model
summary(model)
data.frame(predicted = model$fitted.values, real = train.sale$SalePrice, res = model$residuals)->real.est
hchart(real.est, type = "scatter", hcaes(x = real, y = predicted))
hchart(real.est, type = "scatter", hcaes(x = real, y = res))
################### Q7
#Const Var
hchart(real.est, type = "scatter", hcaes(x = real, y = res))
#so constant varicance in error term is rejected
train.sale %>% mutate(SalePrice = log(SalePrice)) -> train.sale
lm(SalePrice ~ ., train.sale)->model
summary(model)
data.frame(predicted = exp(model$fitted.values), real = exp(train.sale$SalePrice)) %>% mutate(res = predicted - real)->real.est
hchart(real.est, type = "scatter", hcaes(x = real, y = predicted))
hchart(real.est, type = "scatter", hcaes(x = real, y = res))
#VIF
train %>% select(train.sale.factor$param) -> train.sale.vif
lm(SalePrice ~ ., train.sale.vif)->modelvif
library(car)
vif(modelvif)


#Normailty
car::qqPlot(model, id.methode = "identify", simulate = TRUE, main = "Q-Q Plot")
####################### Q8
n <- nrow(train.sale)
train.train.index <- sample(1:n,(n*0.8), replace = FALSE)
train.valid.index <- setdiff(1:n, train.train.index)
train.train <- train.sale[train.train.index,]
train.valid <- train.sale[train.valid.index,]
lm(SalePrice~., train.train)->model.lim
predict(model.lim, train.valid) -> train.valid$fitted
train.valid %>% mutate(fitted = exp(fitted), SalePrice = exp(SalePrice)) %>%
  mutate(residual = fitted - SalePrice) -> train.valid
hchart(train.valid, type = "scatter", hcaes(x = SalePrice, y = fitted))
hchart(train.valid, type = "scatter", hcaes(x = SalePrice, y = residual))
summary(train.valid$residual)
######################## Q9
lm(SalePrice~.+I(sqrt(TotalBsmtSF)-TotalBsmtSF), train.train)->model.end
summary(model.end)
######################## Q10
read.csv("~/university/Data_Analysis/hw_06/house/test.csv")->test
predict(model.end, test) %>% exp() ->test$SalePrice
test %>% select(Id, SalePrice) %>% write.csv("~/university/Data_Analysis/hw_06/test_pred.csv")
