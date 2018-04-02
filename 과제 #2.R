######################### 탐색적 자료 분석 ######################### 

########################## ASSIGNMENT #2 ###########################

################### 교통공학과 2014874015 유한섭 ###################

########################## R - Object ##############################


########################### 과제 1번 ###############################
x <- matrix(0, 1, 8)
y <- matrix(0, 1, 8)

x[1,] <- c(17,16,20,24,22,15,21,18)
x[]
for (i in 1:8)
{
  if( x[i] >= 20)
  {
    print(x[i]) #1)
    x[i] = 100 
  }
  y[i] = x[i]
}
y[] #2)


########################### 과제 2번 ###############################
x = matrix(c(3,-1,-1,-1,-1,-1,4,-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,6,-1,-1,-1,-1,-1,7), nrow =5 , ncol = 5)
x #1)
y <- x[ ,-5]
y #2)
yinfo <- dim(y)
yinfo #3)

for (i in 1:5)
{
  for (j in 1:4)
  {
    if (i != j)
    {
      y[i,j] = y[i,j] + 1 
    }    
  }
}
y1 <- y
y1 #4)


########################### 과제 3번 ###############################
library(readxl)
raw <- read.csv("rowdata.csv", header = TRUE)
head(raw)

save(raw, file = "rowdata.rda") #1)
load("rowdata.rda") 

library(dplyr)
head(is.na(raw)) #2)

raw$obs <- 1:100
head(raw)

for (i in 1:100)
{
  if(is.na(raw$v2)[i] == FALSE & is.na(raw$v3)[i] == FALSE)
  {
    print((raw$obs)[i])
  }
} #3)

rdata1 <- raw %>% filter(!is.na(v2) & !is.na(v3))
rdata1 #4)


########################### 과제 4번 ###############################
logic <- (3>2)&(3<6)
a = matrix(0, 2, 2)
a[1,1] = 1
a[1,2] = 0
a[2,1] = 0
a[2,2] = 1
temp <- list(logic, a, seq(0,1, length = 100), 1,2,3,4)
temp #1)
temp[[2]] <- NULL
temp #2)
#3) 3번째 위치하는 결과값([[3]]) =  1
length(temp) #4)


########################### 과제 5번 ###############################
#1) a1 + a2 = 0 2 2 4

#2) a1 + a2 = 0 0

#3)    [ ,1] [ ,2]
#[1, ]   3     3   
#[2, ]   4     4

#4)    [ ,1] [ ,2]
#[1, ]   1     0   
#[2, ]   2     0

#5) 1 1 1 1 



######################## R-Programming #############################


############################# 1번 ##################################
line = function(n)
{
  if (n == 1)
  {
    return(1)
  }
  if (n == 2)
  {
    return(3)
  }
  return (0.9*line(n-1)-0.1*line(n-2)+1)
}

line(20)


############################# 2번 ##################################
for (i in 1:20)
{
  if (line(i) > 4)
  {
    print(i)
    break
  }
}


############################# 3번 ##################################
A <- matrix(runif(100), 50, 5)
for (i in 1:50)
{
  v[i] = sum(A[i,])
}
v[2]


############################# 4번 ##################################
tmp = rep(0,10)
a <- 10:1
idx = 1
for(j in a)
{
  if(j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx +1
  }
}
# tmp[] 결과값 = 7 8 9 10 0 0 0 0 0 0


############################# 5번 ##################################
x <- matrix(0, 1000,5)
sid = matrix(0, 1000, 1)
m.mat <- matrix(0, 10, 5)

for (i in 1:1000)
{
  x[i,] <- sample(x = 100, size = 5)
  sid[i] <- sample(x=10, size=1)
}
head(sid[])
head(x[])


############################# 6번 ##################################
sum <- matrix(0,10,5)
head(sum[])
num <- matrix(0,10,1)
for(k in 1: 10)
{
  for( j in 1:1000)
  {
    if(sid[j]==k)
    {
      num[k]=num[k] +1
    }
  }
}
num[]

for (i in 1:5)
{
  for (k in 1 : 10)
  {
    for(j in 1:1000)
    {
      if(sid[j]==k)
      {
        sum[k,i] = sum[k,i] + x[j,i]
      }
    
    m.mat[k,i] = sum[k,i] / num[k] 
    }
  }
}

head(sum[])
head(m.mat[])


############################# 6.1번 ##################################
idist <- matrix(0, 1000, 10)
head(idist)

for (j in 1:10)
{
  for (i in 1:1000)
  {
    idist[i,j] <- sum(m.mat[j,] * x[i,])/(sum(x[i,]^2) * sum(m.mat[j,]^2))^(1/2)
  }
}
head(idist)


############################# 7번 ##################################
ivec <- matrix(0,1000,1)
mini <- matrix(0,1000,1)
for (i in 1:1000)
{
  mini[i] <- min(idist[i,])
}
head(mini)

for (j in 1:1000)
{
  for (i in 1:10)
  {
    if(idist[j,i] == mini[j])
    {
      ivec[j] <- i
    }
  }  
}
head(ivec)


############################# 8번 ##################################
set.seed(1)
a = list()
for (i in 1:1000)
{
  x = rpois(1,4) +1
  x = min(x,10)
  a[[i]] = sample(1:10,x)
}
head(a)

count = function(i)
{
  num = 0
  for (j in 1:10)
  {
    if (is.na(a[[i]][j]) == FALSE)
    {
      num = num +1
    }
  }
  num
}
count(4)

freq <- matrix(0, 1, 9)
num <- matrix(0,1,9)
for(j in 1:9)
{
  for (i in 1:1000)
  {
    if(count(i) == j+1)
    {
      num[j] = num[j] +1
      freq[j] <- num[j]
    }
  }
}
freq[] #1)

rm(score)
score <- matrix(0,1,10)
score[]

for (i in 1:1000)
{
  if (count(i) ==2 | count(i)== 3)
  {
    for (j in 1:10)
    {
      if (a[[i]][1] == j)
      {
        score[,j] = score[,j] +1
      }
    }    
  }
  
  if (count(i) ==4 | count(i)== 5| count(i)== 6)
  {
    for ( j in 1:10)
    {
      if (a[[i]][1] == j)
      {
        score[,j] = score[,j]+2
      }
      if (a[[i]][2] == j)
      {
        score[,j] = score[,j]+1
      }
    }
  }
  
  if (count(i) ==7 | count(i)== 8| count(i)== 9| count(i)== 10)
  {
    for ( j in 1:10)
    {
      if (a[[i]][1] == j)
      {
        score[,j] = score[,j]+3
      }
      if (a[[i]][2] == j)
      {
        score[,j] = score[,j]+2
      }
      if (a[[i]][3] == j)
      {
        score[,j] = score[,j]+1
      }
    }
  }
}

score[]
#2) 위 경기에서 1등은 9번사람(349) 이다.


############################# 9번 ##################################
set.seed(1)
m1 = 10
m2 = 5

cash <- data.frame(A=m1, B= m2)
cash
rm(cash)

func = function(n)
{
  for (i in 1:n)
  {
    num = rbinom(1, 1, 1/2)
    
    if (num == 0)
    {
      cash$A <<- cash$A - 1
      cash$B <<- cash$B + 1
    } 
    if (num == 1)
    {
      cash$A <<- cash$A + 1
      cash$B <<- cash$B - 1
    }
    print(cash)
    if (cash$A ==0|| cash$B==0)
    {
      print(i)
      break
    }
  }
}
func(4) #1)
func(200) #2)

cash <- data.frame(A=m1, B= m2)
cash
rm(cash)
winaa = 0

func = function(n)
{
  wina = 0
  winb = 0
  for (i in 1:n)
  {
    num = rbinom(1, 1, 1/2)
    
    if (num == 0)
    {
      cash$A <<- cash$A - 1
      cash$B <<- cash$B + 1
      winb = winb+1
    } 
    if (num == 1)
    {
      cash$A <<- cash$A + 1
      cash$B <<- cash$B - 1
      wina = wina + 1
    }
    print(cash)
    if (cash$A ==0|| cash$B==0)
    {
      if(cash$B == 0)
      {
        winaa <<- winaa + 1
      }
      print(i)
      break
    }
  }
}

for ( k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 5
  num = 0
  func(1000)
  rm(cash)
  cash <- data.frame(A=m1, B= m2)
}
print(winaa) #3) A가 200번의 실험에서 129회 이긴다. 


############################# 10번 ##################################
cash <- data.frame(A=m1, B= m2)
cash
rm(cash)
rate_a = 0

func = function(n)
{
  wina = 0
  winb = 0
  for (i in 1:n)
  {
    num = rbinom(1, 1, 1/2)
    
    if (num == 0)
    {
      cash$A <<- cash$A - 1
      cash$B <<- cash$B + 1
      winb = winb+1
    } 
    if (num == 1)
    {
      cash$A <<- cash$A + 1
      cash$B <<- cash$B - 1
      wina = wina + 1
    }
    print(cash)
    if (cash$A ==0|| cash$B==0)
    {
      print(i)
      rate_a<<- rate_a + wina
      break
    }
  }
}

for ( k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 10 #(10, 15, 20, 25를 반복한다)
  num = 0
  func(1000)
  rm(cash)
  cash <- data.frame(A=m1, B= m2)
}
print(rate_a/100)

#m2 10일 때 0.5032196 확률로 a가 이긴다.
#m2 15일 때 0.4811423
#m2 20일 때 0.4720002
#m2 25일 때 0.464446

