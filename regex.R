#https://towardsdatascience.com/a-gentle-introduction-to-regular-expressions-with-r-df5e897ca432
library(stringr)
basicString <- "Drew has 3 watermelons, Alex has 4 hamburgers, Karina has 12 tamales, and Anna has 6 soft pretzels"
#puling out every instance of one person's name from a string
basicExtractAll <- str_extract_all(basicString, "Drew")
#replaing pattern with a replacement string
basicReplaceAll <- str_replace_all(basicString, "Alex", "Shawn")
#matching character set
#here matching vowels
charSets01 <- str_extract_all(basicString, "[aeiou]")
#matching character range - are case sensitive
charSets02 <- str_extract_all(basicString, "[A-Z]")
#matching number range
charSets03 <- str_extract_all(basicString, "[0-9]")
#matching mix of charter types
charset04 <- str_extract_all(basicString, "[A-Ct-z1-6]")
#Metacharacters
#matching spaces
space <- str_extract_all(basicString, "\\s")
#matching alphanumeric characters
alpha <- str_extract_all(basicString, "\\w")
#matching digits
digits <- str_extract_all(basicString, "\\d")
#Quantifiers - specifying how many characters are expected
quantExample <- "B.BB.BBB.BBBB"
quantPlus <- str_extract_all(quantExample, "B+") #matches Bs till end of the group of Bs
quantExact = str_extract_all(quantExample, "B{2}") #matches exactly two Bs till the end of the group of Bs
quantRange <- str_extract_all(quantExample, "B{2,4}") #matches any number from 2 to 4
quantUpperBound <- str_extract_all(quantExample, "B{2,}")
#picking up words using quantifier
combine1 <- str_extract_all(basicString, "[A-Za-z]+")
#find numbers
combine2 <- str_extract_all(basicString, "\\d{1,2}")
#find quantity of each food item
#each number directly followed by name of the food item separated by space
combine3 <- str_extract_all(basicString, "\\d{1,2}\\s\\w+\\s*\\w*")
#Capture groups
captureGroup1 <- str_match_all(basicString, "[A-Z][a-z]+\\s\\w+\\s\\d{1,2}\\s\\w+\\s*\\w*")
#leaving out 'has' and preparing data in columns
#it is basically the same formula as captureGroup1, just brackets around those strings which are collected as list
captureGroup2 <- str_match_all(basicString, "([A-Z][a-z]+)\\s\\w+\\s(\\d{1,2})\\s(\\w+\\s*\\w*)")
#converting to dataframe
picnic <- data.frame(captureGroup2[[1]][,-1])
colnames(picnic) <- c("Name", "Quantity", "Item")
#Stringr cheatsheet https://github.com/rstudio/cheatsheets/blob/main/strings.pdf
#cheatsheet https://regenerativetoday.com/a-complete-beginners-guide-to-regular-expressions-in-r/
urls <- "Here are urls ONE www.jnu.ac.in, TWO https://arjunghosh.in, THREE http://dharti.wordpress.com"
str_extract_all(urls, "(https?)?(://)?(www\\.)?\\w+\\.[a-zA_Z0-9-.]+")
#extract all capitalized words
fewNames <- "My name is Anthony Gonsalves and I am not Amar Akbar Anthony"
str_extract_all(fewNames, "([A-Z][a-z]+)")[[1]]
#two names together
str_extract_all(fewNames, "([A-Z][a-z]+)\\s([A-Z][a-z]+)")
#multiple names together
str_extract_all(fewNames, "(([A-Z][a-z]+)\\s?){1,}") #need to strip extra spaces
trimws(str_extract_all(fewNames, "(([A-Z][a-z]+)\\s?){1,}")[[1]])