rm(list=ls())
# Setting the working directory
setwd("C:/Users/HP/Desktop/Project 2/Project 2 datasets")
getwd()

# Importing the day dataset
df = read.csv("day.csv", header =T)
df$instant = NULL

str(df)

# Providing proper datatypes to variables 
df$dteday = as.Date(df$dteday)
df$casual = as.numeric(df$casual)
df$registered = as.numeric(df$registered)
df$cnt = as.numeric(df$cnt)
df$season = as.factor(df$season)
df$yr = as.factor(df$yr)
df$holiday = as.factor(df$holiday)
df$weekday = as.factor(df$weekday)
df$workingday = as.factor(df$workingday)
df$weathersit = as.factor(df$weathersit)
df$mnth = as.factor(df$mnth)

###############
###############

#Outlier Analysis

numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)
library(ggplot2)

for(i in 1:length(cnames)){
  assign(paste0("gn",i), ggplot(aes_string(y =(cnames[i]), x ="yr"), data=subset(df))+
           stat_boxplot(geom ="errorbar", width =0.5)+
           geom_boxplot(outlier.colour="red", fill ="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i],x ="yr")+
           ggtitle(paste("Box plot of yr for", cnames[i])))
}


#Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn7,ncol=2)



# Loop to remove outliers from all variables
for(i in cnames){
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df = df[which(!df[,i] %in% val),]
}

###################
##################

# Missing values
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))

# No missing value in present

###############
###############

# Feature Selection
# Cor relation for continoues variable
library(corrgram)
corrgram( df[,numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main="Correlation Plot")


# Taking the subsets
df = subset(df, select= -c(atemp,hum))



# Chi Square test for categorical variable
factor_index = sapply(df, is.factor)
factor_data = df[,factor_index]

for(i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$weathersit, factor_data[,i])))
}



# Dependent variables season, weathersit, month
# Dependent variables weekday , workingday, holiday



# Dimension Reduction
df = subset(df, select= -c(mnth,weathersit,workingday,holiday))
df$dteday =NULL
df$yr =NULL




############
############

#Feature Scaling
#Normality check
qqnorm(df$variable)
hist(df$cnt)


#Normalisation
cat_names = c("casual","cnt","registered")
for(i in cat_names){
  print(i)
  df[,i] =(df[,i] - min(df[,i]))/(max(df[,i]) - min(df[,i]))
  
}


#################
################


# Dividing the whole df dataset into train and test
library(sampling)
train_index = sample(1:nrow(df), 0.8* nrow(df))
train = df[train_index,]
test = df[-train_index,]


##################
##################


# Linear Regression
library(usdm)
vif(df[,-1])

vifcor(df[,-1], th=0.9)

#Build model
model =lm(cnt ~., data=train)

summary(model)


#Predict test cases
pre = predict(model, test[,-7])
pre



#Calculate MAPE
MAPE = function(y,yhat){
  mean(abs((y-yhat)/y))
}


MAPE(test[,7], pre)



# Alternative method
library(DMwR)
regr.eval(test[,7],pre, stats = c("mae","mse","rmse","mape"))

###################
###################

#Decision tree
library(rpart)
fit = rpart(cnt ~., data =train, method = "anova")
fit

#Predict the test cases
pre_dt = predict(fit,test[,-7])
pre_dt


#Calculate MAPE

MAPE(test[,7], pre_dt)



# Alternative method
library(DMwR)
regr.eval(test[,7],pre_dt, stats = c("mae","mse","rmse","mape"))


##########################
#########################


# Random Forest
library(randomForest)


# Build model
rf_model = randomForest(cnt ~., train, importance = TRUE, ntree = 500)
rf_model


# Predict the test cases
rf_predictions  = predict(rf_model, test[,-7])


# Calculate MAPE
MAPE(test[,7], rf_predictions)


# Alternative method
library(DMwR)
regr.eval(test[,7],rf_predictions, stats = c("mae","mse","rmse","mape"))


#################################  END  ###############################




