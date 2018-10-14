library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(gridExtra)
library(caret)
library(tidyr)

df = read.table("LungCapData.txt", header = TRUE)
head(df)

str(df)

df = df %>% select(-Caesarean, -Age)

head(df)

desc = df %>% select(LungCap, Age, Height) %>% gather()

desc %>% group_by(key) %>% summarise(mean = mean(value),
                                     std = sd(value),
                                     min = min(value),
                                     max = max(value),
                                     var = var(value)) %>% data.frame()

range(df$Age)

breaks = seq(3,19, by = 3) # half-integer sequence 
breaks

age = cut(df$Age, breaks, right = FALSE)
t = table(age)

x = cbind(t) %>% matrix()

barplot(t)

ggplot() +
    geom_bar(aes(t))

labels = c("3-5", "6-9", "9-12", "13-15", "16-19")
v = cbind(labels, t) %>% data.frame() %>% select(labels, t)
v

colnames(v) = c("bucket", "freq")

v

breaks = c(1, 2,3,4,5)
ggplot(data.frame(t), aes(seq_along(t), t)) + geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks, labels = labels)


#Boxplot to find a relationshipt between smoke and lung cap.
ggplot(df, aes(as.factor(Smoke), LungCap, fill = Smoke)) +
    geom_boxplot() +
    ggtitle("Lung Capacity") +
    xlab("Smoke") +
    theme_bw()

#Histogram on lungCap. 
ggplot(df, aes(LungCap)) +
    geom_histogram(bins = 10, fill = "red", color = "black", alpha = 0.6) +
    ggtitle("Distribution of Lung Capacity") +
    theme_bw()

ggplot(df, aes(Age, LungCap)) +
    geom_point(aes(colour = Smoke)) +
    ggtitle("Age and LungCap") +
    scale_colour_manual(values = c("#D4E157","#9B59B6")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(Height, LungCap)) +
    geom_point(aes(colour = Smoke)) +
    ggtitle("Height and LungCap") +
    scale_colour_manual(values = c("#9B59B6", "#FB8C00")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(Age, LungCap)) +
    geom_point() +
    ggtitle("Age and LungCap") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(Age)) +
    geom_histogram(fill = "blue", color = "black")

prop.table(c)

colSums(is.na(df))

#lets perform a two-sided ttest on smoke to determine if the means of lung capacity is the same btween smoker and non smoker
#Hypothesis
#Ho mu=0
#Ha mu <> 0

t.test(LungCap~Smoke, df)

#Our Test Statistics is -3.64 with a p-value less than alpha therefore we reject the null hypothesis and conclude the mean between smoker and non smoker is not equal.

dim(df)

library(caTools)

split = sample.split(df$LungCap, SplitRatio = 0.80)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

head(train)
head(test)

#lets create our model 

model1 = lm(LungCap ~ ., data = train)
summary(model1)
#based on the summary results the model is performing well we have our p values for each vairable less than alpha indicating predictors are highly significant. 
#We can also see the adjusted r-squared which indicates 86% of the data is explained by the model.


plot(model1)

y_pred = predict(model1, newdata = df)

y_pred

tree = rpart(LungCap ~ ., data = train, method = "anova")

# plot tree 
plot(tree, uniform = TRUE,
    main = "Regression Tree for Mileage ")
text(tree, use.n = TRUE, all = TRUE, cex = .8)

rpart.plot(tree)


summary(tree)

