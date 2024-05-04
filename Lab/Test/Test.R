library(ggplot2)
df <- read.csv("D:\\ROP_data set - ROP_data set.csv")

coeff <- 10

ggplot(df, aes(x=Hole.Depth)) +
  geom_line( aes(y = Weight.on.Bit), col="blue") +
  geom_line( aes(y = Rate.Of.Penetration / coeff), col="green") +
  scale_y_continuous(
    name = "Weight on Bit",
    sec.axis = sec_axis(~.*coeff, name="Rate of Penetration")
  )

mean <- lapply(df, mean)
median <- lapply(df, median)
sd <- lapply(df, sd)
max <- lapply(df, max)
min <- lapply(df, min)
statistical_table = data.frame("Mean"=unlist(mean), "Median"=unlist(median), "SD"=unlist(sd), "Max"=unlist(max), "Min"=unlist(min))
View(statistical_table)

x <- df$Weight.on.Bit
h <- hist(x, breaks=10, col="pink", xlab = "Weight on Bit", main = "Histogram of Weight on Bit for each Hole Depth")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="purple", lwd=2)

y <- df$Rate.Of.Penetration
h<- hist(y, breaks=20, col="lavender", xlab= "Rate of Penetration", main = "Histogram of Rate of Penetration for each Hole Depth")
xfit <- seq(min(y), max(y), length = 350)
yfit <- dnorm(xfit, mean = mean(y), sd = sd(y))
yfit <- yfit*diff(h$mids[1:2])*length(y)
lines(xfit, yfit, col="hotpink", lwd=2)


df1 <- read.csv("D:\\Unpivot_data.csv")
boxplot <- ggplot(data = df1, aes (x = Attribute, y = Value, fill = Attribute))
boxplot + geom_boxplot() + labs(title = "Boxplot", x = "Attribute", y = "Value")

