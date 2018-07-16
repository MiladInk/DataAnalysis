###################################################

###########Q2
class<-c(50, 50, 60, 70, 75, 80, 90, 85)
modern<-c(55, 75, 80, 90, 105, 65)
cm<-data.frame(sell = c(class, modern), 
           type = c(rep('claasic', length(class)),
                    rep('modern', length(modern))))

coin::oneway_test(sell~type, data=cm, distribution="exact")


wilcox.test(sellings~type, data=q2_full_data, distribution="exact")

###########Q3
bef<-c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904)
aft<-c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)
t.test(bef, aft, paired = TRUE)
wilcox.test(bef, aft, paired = TRUE)
###########Q4
sell.color <- data.frame(
white=c(510, 720, 930, 754, 105),
blue=c(925, 735, 753, 685, NA),
red=c(730, 745, 875, 610, NA))
sell.color %>% as.matrix() %>% friedman.test()
###########Q5
tv <- read.csv("data/tv.csv")
tv %>% as.matrix() %>% friedman.test(.)
###########Q6
sat<-data.frame(always = c(151, 802, 753),
           sometime = c(252, 603, 55),
          never = c(603, 405, 408))
chisq.test(sat)
###########Q7
sale <- read.csv("data/consumption.csv")
library(ggplot2)
ggplot(sale, aes(x = B, y = A))+geom_point()
cor(sale$A, sale$B, method = "spearman")
###########Q8
gen<-data.frame(male = c(301, 353, 558),
                female = c(502, 155, 153))
chisq.test(gen)
###########end
options(warn = oldwarn)
