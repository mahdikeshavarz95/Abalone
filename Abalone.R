###data entery
abalone=read.csv("Abalone.csv")
summary(abalone)
abalone$Sex=as.factor(abalone$Sex)
summary(abalone)

####charts
plot(abalone$Sex, main="three genders of abalone", xlab="sex", ylab="count",
     horiz=T, col="darkred", xlim=c(0,1600))

plot(abalone$Rings~abalone$Length, main="Length and Rings against each other",
     xlab="Lenght", ylab = "Rings")

plot(abalone$Rings~abalone$Whole.weight, main="Whole weight and Rings against each other",
     xlab="Whole weight", ylab = "Rings")

plot(abalone$Rings~abalone$Shucked.weight, main="Shucked weight and Rings against each other",
     xlab="Shucked weight", ylab = "Rings")

plot(abalone$Rings~abalone$Viscera.weight, main="Viscera weight and Rings against each other",
     xlab="Viscera weight", ylab = "Rings")

plot(abalone$Rings~abalone$Shell.weight, main="Shell weight and Rings against each other",
     xlab="Shell weight", ylab = "Rings")

######data pe-process and Data dictionary

length_LL=0.3*quantile(abalone$Length, 0.01)
length_UL=3*quantile(abalone$Length, 0.99)
length_LL
length_UL
abalone$Length[abalone$Length<length_LL]=length_LL
abalone$Length[abalone$Length>length_UL]=length_UL

diameter_LL=0.3*quantile(abalone$Diameter, 0.01)
diameter_UL=3*quantile(abalone$Diameter, 0.99)
diameter_LL
diameter_UL
abalone$Diameter[abalone$Diameter<diameter_LL]=diameter_LL
abalone$Diameter[abalone$Diameter>diameter_UL]=diameter_UL

height_LL=0.3*quantile(abalone$Height, 0.01)
height_UL=3*quantile(abalone$Height, 0.99)
height_LL
height_UL
abalone$Height[abalone$Height<height_LL]=height_LL
abalone$Height[abalone$Height>height_UL]=height_UL

whole_weight_LL=0.3*quantile(abalone$Whole.weight, 0.01)
whole_weight_UL=3*quantile(abalone$Whole.weight, 0.99)
whole_weight_LL
whole_weight_UL
abalone$Whole.weight[abalone$Whole.weight<whole_weight_LL]=whole_weight_LL
abalone$Whole.weight[abalone$Whole.weight>whole_weight_UL]=whole_weight_UL

shucked_weight_LL=0.3*quantile(abalone$Shucked.weight, 0.01)
shucked_weight_UL=3*quantile(abalone$Shucked.weight, 0.99)
shucked_weight_LL
shucked_weight_UL
abalone$Shell.weight[abalone$Shucked.weight<shucked_weight_LL]=shucked_weight_LL
abalone$Shell.weight[abalone$Shell.weight>shucked_weight_UL]=shucked_weight_UL

viscera_LL=0.3*quantile(abalone$Viscera.weight, 0.01)
viscera_UL=3*quantile(abalone$Viscera.weight, 0.99)
viscera_LL
viscera_UL
abalone$Viscera.weight[abalone$Viscera.weight<viscera_LL]=viscera_LL
abalone$Viscera.weight[abalone$Viscera.weight>viscera_UL]=viscera_UL

shell_weight_LL=0.3*quantile(abalone$Shell.weight, 0.01)
shell_weight_UL=3*quantile(abalone$Shell.weight, 0.99)
shell_weight_LL
shell_weight_UL
abalone$Shell.weight[abalone$Shell.weight<shell_weight_LL]=shell_weight_LL
abalone$Shell.weight[abalone$Shell.weight>shell_weight_UL]=shell_weight_UL

rings_LL=0.3*quantile(abalone$Rings, 0.01)
rings_UL=3*quantile(abalone$Rings, 0.99)
rings_LL
rings_UL
abalone$Rings[abalone$Rings<rings_LL]=rings_LL
abalone$Rings[abalone$Rings>rings_UL]=rings_UL

abalone=dummy.data.frame(abalone)  #dummy variable
abalone=abalone[,-2]  #useless variable reduction

cor(abalone)

#test-train split
set.seed(0)  
x=sample.split(abalone, SplitRatio=0.8)  
training_set= subset(abalone, x==TRUE)  
test_set= subset(abalone, x==FALSE)
  
#model
linear_model1=lm(Rings~., data=training_set)
summary(linear_model1)
plot(linear_model1)

z1=predict(linear_model1, training_set) 
z2=predict(linear_model1, test_set)  
MSE1=mean((training_set$Rings-z1)^2)  #training_set MSE
MSE2=mean((test_set$Rings-z2)^2)  #test_set MSE 

###new approaches
abalone2=abalone
abalone2=abalone2[,c(-3,-4,-6,-7,-8)]

linear_model2=lm(Rings~., data=abalone2)
summary(linear_model2)
plot(linear_model2)

##Ridge regression model
x1=model.matrix(Rings~ . , data=training_set)[,-10]
y1=training_set$Rings

grid=10^seq(10,-2,lenght=100)  
install.packages("glmnet")
ridge_model=glmnet(x1, y1, alpha=0, lambda=grid)  
summary(ridge_model)

CriticalValue_fit = cv.glmnet(x1, y1, alpha=0, lambda=grid)  
plot(CriticalValue_fit)  #ploting Lambda against MSE
optimum_lambda=CriticalValue_fit$lambda.min  #value of Lambda for with minimum MSE

tss=sum((y1-min(y1))^2)  #total sum of square
y_a=predict(ridge_model, s=optimum_lambda, newx=x1)  #predicted values of y 
rss=sum((y_a-y1)^2)  #residual sum of square
RSquared=1-rss/tss #R^2
RSquared

