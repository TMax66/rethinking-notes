library(tidyverse)
library(rethinking)
library(dagitty)
library(shape)

###Chap 4- Linear regression###

data("Howell1")


d2<-Howell1 %>% 
  filter(age>=18) %>% 
  mutate(xbar=mean(age))

m4.3<-quap(
  alist(
    height~dnorm(mu, sigma),
    mu<-a+b*(weight-xbar),
    a~dnorm(178,20),
    b~dlnorm(0,1),
    sigma~dunif(0,50)
  ), data=d2
)

post<-extract.samples(m4.3)
a_map<-mean(post$a)
b_map<-mean(post$b)

p<-post %>% 
  slice(1:5)  




d2 %>% 
  ggplot(aes(x=weight, y=height))+
  geom_point(size=1.5, alpha=0.2)+
  geom_abline(intercept=114.54, slope=0.8905, col="blue")+
  geom_abline(data=p, aes(intercept= a, slope= b ), col="red", alpha=0.2) 


w.seq<-seq(from=25, to=70, by=1)

mu<-link(m4.3,data=data.frame(weight=w.seq))

mu.mean<- apply(mu, 2, mean)
mu.PI<- apply(mu,2,PI, prob=0.95)

mu<-cbind(as.data.frame(mu.mean),as.data.frame(t(mu.PI)))
 
  ggplot(data=d2,aes(x=weight, y=height))+
  geom_point(size=1.5, alpha=0.2)+
  geom_abline(intercept=114.54, slope=0.8905, col="blue")+
  geom_ribbon(data=mu,aes(ymin = `3%`, ymax = `98%`), fill = "grey70") 



####Chap 5- Spurius Association######

data("WaffleDivorce")
d<-WaffleDivorce


d$D<-standardize(d$Divorce)
d$M<-standardize(d$Marriage)
d$A<-standardize(d$MedianAgeMarriage)


m5.1<-quap(
  alist(
    D~dnorm(mu , sigma),
    mu<-a+bA*A,
    a~dnorm(0, 0.2),
    bA~dnorm(0,0.5),
    sigma~dexp(1)
        ), data=d)
m5.2<-quap(
  alist(
    D~dnorm(mu , sigma),
    mu<-a+bM*M,
    a~dnorm(0, 0.2),
    bM~dnorm(0,0.5),
    sigma~dexp(1)
  ), data=d)

dag5.1<-dagitty("dag{A -> D; A -> M; M -> D }")
coord5.1<-list(x=c(A=0, D=1, M=2), y=c(A=0, D=1, M=0))
drawdag(dag5.1)

###conditional indipendencies###
DMA<-dagitty("dag{D <-A -> M}")
impliedConditionalIndependencies(DMA)

DMA2<-dagitty("dag{D <-A -> M -> D}")
impliedConditionalIndependencies(DMA2)

####Multiple regression####
m5.3<-quap(
  alist(
    D~dnorm(mu , sigma),
    mu<-a+bM*M+bA*A,
    a~dnorm(0, 0.2),
    bM~dnorm(0,0.5),
    bA~dnorm(0,0.5),
    sigma~dexp(1)
  ), data=d)

plot(coeftab(m5.1, m5.2, m5.3), par=c("bA", "bM"))

###relazione tra M e A###

mx<-quap(
  alist(
    M~dnorm(mu , sigma),
    mu<-a+bA*A,
    a~dnorm(0, 0.2),
    bA~dnorm(0,0.5),
    sigma~dexp(1)
  ), data=d)
plot(d$A,d$M)



provaprovata