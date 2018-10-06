library(dplyr)
library(ggplot2)

df = read.csv("diet.csv", na.strings=c('',"NA" ))
head(df)

str(df)

#checking for null
colSums(is.na(df))

df$Diet = factor(df$Diet)

df$gender = ifelse(is.na(df$gender), 1, df$gender) %>% factor()

df1 = df %>% select(-Person) %>% mutate(weightloss = pre.weight - weight6weeks)

head(df1)

ggplot(df1, aes(Diet, weightloss, fill = Diet)) + geom_boxplot()

anova = aov(weightloss ~ Diet, df1)


summary(anova)

df1$resid = anova$residuals

head(df1)

ggplot(df1, aes(Diet, resid, fill = Diet)) + geom_boxplot() + geom_hline(yintercept = 0, color="red")

qqnorm(df1$resid)
qqline(df1$resid, col="red")

TukeyHSD(anova)

plot(TukeyHSD(anova))

anova1 = aov(weightloss ~ Diet * gender, df1)

summary(anova1)

plot(TukeyHSD(anova1))