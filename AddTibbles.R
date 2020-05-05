setone <- tibble(x = c("man","woman","boy"), y = c(2,3,5))
settwo <- tibble(x = c("man","woman","girl"), y = c(3,2,5))
setfull <- full_join(setone,settwo) #if nothing else works work with this
library(dplyr)
df1 <- data.frame(setone$x,setone$y)
names(df1) <- c("word","n")
df2 <- data.frame(settwo$x,settwo$y)
names(df2) <- c("word","n")

melt(list(df1, df2), id.vars = "word")

mydf<- dcast(melt(mget(ls(pattern = "df\\d+")), id.vars = "word"), 
             word ~ variable, value.var = "value", fun.aggregate = sum)
#this has done it - we can now add up two tibbles with all values added up
