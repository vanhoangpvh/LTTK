library(ggplot2)
library(corrplot)

df = read.csv("D:\\Học kì 2 năm 2\\THLTTK\\Test\\ROP_data set - ROP_data set.csv")

Hole_Depth = df$Hole.Depth
Weight_on_Bit = df$Weight.on.Bit
Rate_Of_Penetration = df$Rate.Of.Penetration




#(i) Plotting
ggplot(data = df, aes(x = Hole_Depth))+
  geom_line(aes(y = Rate_Of_Penetration), col = 'blue') + 
  geom_line(aes(y = Weight_on_Bit * 10 ), col = 'green')+
  scale_y_continuous(name = "Rate of penetration", sec.axis = sec_axis(~./10, name = "Weight on bit")) +
  labs(title = "Plot", x = 'Hole Depth')



#(ii) Check the data has normal distribution
#Weight on bit
hist(Weight_on_Bit, breaks = floor(2* length(Weight_on_Bit)^(1/3)), freq = FALSE)
mu_weight = mean(Weight_on_Bit)
sd_weight = sqrt(var(Weight_on_Bit))
curve(dnorm(x,mu_weight,sd_weight),add = TRUE, col = 'red', lwd = 2)

#Rate of penetration
hist(Rate_Of_Penetration, breaks = floor (2*length(Rate_Of_Penetration)^(1/3)), freq = FALSE)
mu_pene = mean(Rate_Of_Penetration)
sd_pene = sqrt(var(Rate_Of_Penetration))
curve(dnorm(x,mu_pene,sd_pene),add = TRUE, col = 'red', lwd = 2)

#Hole Depth
hist(Hole_Depth, breaks = floor (2*length(Hole_Depth)^(1/3)), freq = FALSE)
mu_hole = mean(Hole_Depth)
sd_hole = sqrt(var(Hole_Depth))
curve(dnorm(x,mu_hole,sd_hole),add = TRUE, col = 'red', lwd = 2)


#(iii) Split dataset to 70:30
  set.seed(1)
  
  sample <- sample(c(TRUE,FALSE), nrow(df), replace =TRUE, prob = c(0.7,0.3))
  Data_1 <- df[sample,] #train data
  Data_2 <- df[!sample,] #test data
  
  
#(iiii)
#(a) Scatter plot 
#(a1)Weight on bit and Rate of Penetration
ggplot(data = Data_1, aes(x = Data_1$Weight.on.Bit, y = Data_1$Rate.Of.Penetration)) +
  geom_point(col = 'blue') + 
  labs(title = 'Scatter plot 1', x = 'Weight on Bit', y = 'Rate of Penetration')

#(a2)Hole Depth and Rate of penetration
ggplot(data = Data_1, aes(x = Data_1$Hole.Depth, y = Data_1$Rate.Of.Penetration)) +
  geom_point(col = 'red') +
  labs(title = 'Scatter plot 2', x = 'Hole Depth', y ='Rate of Penetration')


#(b) Correlation matrix
#(b1)Weight on bit and Rate of Penetration
df_temp1 = data.frame(
  Data_1$Weight.on.Bit,
  Data_1$Rate.Of.Penetration
)

cor_1 = cor(df_temp1)
colnames(cor_1) <- rownames(cor_1) <- c('Weight on Bit', 'Rate of penetration')
corrplot(cor_1, method = 'color', main = 'Correlation matrix 1')

#(b2)Hole Depth and Rate of penetration
df_temp2 = data.frame(
  Data_1$Hole.Depth,
  Data_1$Rate.Of.Penetration
)

cor_2 = cor(df_temp2)
colnames(cor_2) <- rownames(cor_2) <- c('Hole Depth', 'Rate of penetration')
my_colors <- colorRampPalette(c("yellow", "lightgreen", "skyblue3"))(100)

corrplot(cor_2, method = 'color', main = 'Correlation matrix 2', col =my_colors)


#(c)Linear regression
#(c1)Weight on bit and Rate of Penetration

# Weight on bit is Independent value and Rate of penetration is dependent value
model_1 = lm(Data_1$Rate.Of.Penetration ~ Data_1$Weight.on.Bit) 
summary(model_1)

##Test Data_2
predict(model_1, newdata_1 = data.frame(x = Data_2$Weight.on.Bit))

#(c2)Hole Depth and Rate of penetration

#Hole Depth is independent value and Rate of penetration is dependent value
model_2 = lm(Data_1$Rate.Of.Penetration ~ Data_1$Hole.Depth)
summary(model_2)

##Test Data_2
predict(model_2, newdata_2 = dataframe(x = Data_2$Hole.Depth))



#(d) Correlation test
#(d1) Weight on bit and Rate of Penetration

# Weight on bit is Independent value and Rate of penetration is dependent value
correlation_test1 = cor.test(Data_1$Weight.on.Bit, Data_1$Rate.Of.Penetration)#Pearson correlation test
correlation_test1

corrcoef1 = correlation_test1$estimate #To get the correlation coefficient
p_value1 = correlation_test1$p.value #To get the p-value (measure of the strength of evidence against the null hypothesis)

cat("Correlation coefficient 1 from Weight on bit and Rate of Penetration: ", corrcoef1, '\n')
cat("P-value 1 from Weight on bit and Rate of Penetration: ",p_value1,'\n') 


#(d2) Hole Depth and Rate of penetration

#Hole Depth is independent value and Rate of penetration is dependent value
correlation_test2 = cor.test(Data_1$Hole.Depth, Data_1$Rate.Of.Penetration)#Pearson correlation test
correlation_test2

corrcoef2 = correlation_test2$estimate #To get the correlation coefficient
p_value2 = correlation_test2$p.value #To get the p-value (measure of the strength of evidence against the null hypothesis)

cat("Correlation coefficient from Hole Depth and Rate of Penetration: ", corrcoef2, '\n')
cat("P-value 1 from Hole Depth and Rate of Penetration: ",p_value2,'\n') 



#(e) Draw 90% confidence interval
#(e1) Weight on bit and Rate of Penetration

#Scatter plot with linear regression line and 90% confidence interval
#df_temp1 holds Data_1 Weight on Bit and Data_1 Rate of Penetration
ggplot(df_temp1, aes(x = Data_1$Weight.on.Bit, y = Data_1$Rate.Of.Penetration)) + 
  geom_point() +
  geom_smooth(method = 'lm', level = 0.90)
  labs(title = 'Scatter plot from Weight on bit and Rate of Penetration with 90% confidence interval line',
       x = 'Weight on Bit',
       y = 'Rate of Penetration')
  
  
#(e2) Hole Depth and Rate of penetration
  
#Scatter plot with linear regression line and 90% confidence interval
#df_temp2 holds Data_1 Hole Depth and Data_1 Rate of Penetration
  
ggplot(df_temp2, aes(x = Data_1$Hole.Depth, y = Data_1$Rate.Of.Penetration)) + 
  geom_point() +
  geom_smooth(method = 'lm', level = 0.90) +
  labs(title = 'Scatter plot from Hole Depth and Rate of Penetration with 90% confidence interval line',
       x = 'Weight on Bit',
       y = 'Rate of Penetration')  






