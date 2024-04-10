### Lecture 1 ###

a = 1
b = 3
c = 1

d = c((-b + sqrt(b^2 - 4*a*c))/2*a,(-b - sqrt(b^2 - 4*a*c))/2*a)
d

set.seed(1234)
x <- rnorm(100, mean=.5, sd=.3)
mean(x)
sd(x)
hist(x)

str(paste("R session",1))

x <- 5 : 6 
x

x <- 5 + 6 
y <- x + 3 
z <- y - 10
z

ls()

help(tail)
internal(tail)
?tail

set.seed(1)
x <- rnorm(100) 
head(x)

### Lecture 2 ###

x <- c("b", 2,  TRUE)
str(x)

y <- c(1, 2, 3.14, 4, 5)
y

z <- c(FALSE, TRUE, 2, 3, 4)
z

matrix(c(5,4,3,2,1,0)+2,nrow=2)<5

pi(z)

sin(z)

plot(sin, 0, 2*pi)
plot(sin, -pi, 2*pi)
plot(sin, -pi, pi)
?cat()

cat("Summary of input: \n")

set.seed(1234)
my.data<-rnorm(200)
summary(my.data)

### Lecture 3 ###

for(i in 0:10) { 
  if(i%%2!=0) cat(i) 
}

notfound<-TRUE 
i<-0 
while(notfound) { 
  if(i%%2!=0) { 
    cat(i) 
    notfound<-FALSE 
  } 
}

