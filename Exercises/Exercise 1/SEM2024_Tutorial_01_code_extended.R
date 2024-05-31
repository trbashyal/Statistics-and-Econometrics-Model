# ############################################################################# #
#                                                                               #
#### Code Problem set 1                                                      ####
#                                                                               #
# ############################################################################# #



# ############################################################################# #
##### Problem 1.1 Obtaining and importing data                              #####
# ############################################################################# #

### a) start R, create a script, save the script

        
### b) .csv download and save the .csv


### c) .csv means Comma-Separated values and is a file in which the columns are (usually) separated by commas.
### If we open our .csv in the text editor, we see that the columns are separated by semicolons.


### d) Select working directory:
### Via Session -> Set Working Directory -> Choose Directory...
### alternatively by clicking in R-Studio on: Session -> Set Working Directory -> To Source File Location
### or by using the command setwd()


### e)
?read.table
data <- read.table(file = "Data_Income_Savings.csv", 
                    header = TRUE, 
                    sep = ";")
### via file we specify the file name, the file must be in the working directory, otherwise the directory must be specified.
### via header=TRUE we specify that the column headers are in the first line of the .csv, see help: logical value, can therefore only be TRUE or FALSE
### via sep we specify the semicolon as separator


### f)
str(data)
### The data set contains 7 variables with 11 observations each.
### The variables are: Country, GDP, Incom_net, Income_gross, Savings_net, Savings_gross and Savingsrate_net.




# ############################################################################# #
##### Problem 1.2 Analysis of the gross savings rate                        #####
# ############################################################################# #

### a)
is.data.frame(data) 
head(x = data, n = 3)  ### Display first 3 rows
View(data)             ### Alternatively click in R-Studio


### b)
a <- c(6, 14, 24);      a
b <- 1:3;               b
d <- a-b;               d
p <- d/b;               p


### c)
data[p, 3]      ### p selects the 5th to 7th row, 3 selects the 3rd column
### values: 1470602.1,  119619.3, 1194992.8


### d)
data[5,"Savingsrate_net"]


### e) 
names(data) 
names(data)[3] <- "Income_net" ### Correct spelling mistake in Incom_net to Income_net
### Attention, no numeric or logical value, so use quotation marks!
names(data)


### f)
3 > 6   ### FALSE
6 > 3   ### TRUE
?"<"    ### different variants
a < b   ### the vectors are compared per element
a < 15  ### each element from a is checked for < 15. The 15 is therefore applied 3 times.


### g)
x <- a > b
x
which(x)            ### a is for all 3 entries greater than b
is.vector(which(x)) ### which(x) is also a vector


### h) 
y <- data[,"Savingsrate_net"] > 10
data[y,"Country"]                     ### outputs the countries for which y is TRUE
data[which(y),"Country"]              ### outputs the countries whose rows are saved in which(y)


#### i)
Savingsrate_gross <- (data[,"Savings_gross"] / data[,"Income_gross"])*100
Savingsrate_gross <- round(Savingsrate_gross, digits = 2)
data <- cbind(data, Savingsrate_gross)


### j)
plot(data$Savingsrate_gross)          ### Select with $ the column Savingsrate_gross

### improvement:
order_savings <- order(data$Savingsrate_gross)
order_savings
Savingsrate_gross_ordered <- data$Savingsrate_gross[order_savings]
Country_ordered <- data$Country[order_savings]

plot(Savingsrate_gross_ordered,
     type = "p",
     xaxt = "n",
     main = "Gross savings rate of several european states",
     xlab = "Country",
     ylab = "Gross savings rate")
### Put countries on the x-axis:
axis(side     = 1, 
     at       = 1:11, 
     labels   = Country_ordered,
     cex.axis = 0.5)

### or as a bar plot
barplot(data[,"Savingsrate_gross"][order_savings],
        names.arg = data[,"Country"][order_savings],
        main      = "Gross savings rate of several european states",
        xlab      = "Country",
        ylab      = "Gross savings rate",
        cex.names = 0.5,
        col       = rainbow(11)) 


### k)
mean(data$Savingsrate_gross)
median(data$Savingsrate_gross)
min(data$Savingsrate_gross)
max(data$Savingsrate_gross)
### Alternatively, we can also find the values here:
summary(data$Savingsrate_gross) 


### l)
difference <- (data$Savingsrate_gross - mean(data$Savingsrate_gross))
### method 1:
(SSR1 <- sum((difference)^2))
### method 2:
(SSR2 <- t(difference)%*%difference)
### WARNING: not commutative
difference %*% t(difference)

SSR1 
SSR2 ### both ways lead to the same sum of squared deviations


### empirical standard deviation:
n <- length(data$Savingsrate_gross);    n
empVar <- (1/(n-1))*SSR1;               empVar
empSd <- sqrt(empVar);                  empSd
# check:
var(data$Savingsrate_gross)
sd(data$Savingsrate_gross)




# ############################################################################# #
##### Problem 1.3                                                           #####
# ############################################################################# #

### a) Load data set module_exam.csv into the working directory

### Displays the current working directory
getwd()

### Select suitable working directory 
### via  setwd or via  Session --> Set Working Directory --> Choose Directory...
setwd("~/ownCloud/Dokumente/lehre/2024_SoSe/SEM/Tutorials/01_R")

### Save module_exam.csv in an object scores
scores <- read.table(file = "module_exam.csv", header = TRUE, sep = ",")

### is scores a data.frame?
is.data.frame(scores)


### b)
### overview of the data
View(scores)
str(scores) 
head(scores)

### Division of the data of each exam part into the group boundaries and saving it in 
### dist_part1 and dist_part2
?cut
dist_part1 <- cut(x = scores[,1], breaks = c(0,5,10,15,20,25,30))
dist_part2 <- cut(x = scores[,2], breaks = c(0,5,10,15,20,25,30))

### display the results
dist_part1
dist_part2
### cut divides the range of x into intervals and codes the values in x 
### according to the interval in which they fall.


### c)
### Table per test section with the frequencies of the individual groups
Part_1 <- table(dist_part1)
Part_2 <- table(dist_part2)

### display the results
Part_1
Part_2 

### Summarise results in a table
total <- rbind(Part_1, Part_2)
total 
rbind(total, "Grade" = 6:1)


### d)
### calculate row and column sums
colSums(total)
rowSums(total)

### Appending to the table from before + overwriting the variables total
total <- rbind(total, colSums(total))
rownames(total) <- c("Part 1","Part 2","sum")
total
total <- cbind(total, rowSums(total))
colnames(total) <- c("6", "5", "4", "3", "2", "1", "sum")
total

### With rbind(a,b,c) the vectors a, b and c can be combined in rows to form a matrix.
### In contrast, cbind() joins the vectors column by column to form a matrix.

### Calculation of the joint distribution 
round(total[1:2,1:6]/398, digits = 2) 

### Numerical values can be rounded with round(). 
### It is also possible to specify the number of decimal digits.
### You look at the probability that both occur.


### e)
### Calculation of marginal distribution with relative frequencies. 
marginal_dist <- total[3,1:6]/398
round(marginal_dist, digits = 2)
### calculating the expected grade 
sum(6:1*marginal_dist)
### we get on average a grade of 2.85.


### f)
### calculate conditional distributions 
conditional_dist1 <- total[1, 1:6]/199
conditional_dist2 <- total[2, 1:6]/199

round(conditional_dist1, digits = 2)
round(conditional_dist2, digits = 2)


### g)
### calculate the conditional expectations 
sum(6:1*conditional_dist1)
sum(6:1*conditional_dist2)

### that can also be done using weights
weighted.mean(x = 6:1, w = conditional_dist1)
weighted.mean(x = 6:1, w = conditional_dist2)

### unconditional expected value
sum(6:1*marginal_dist)

### Comparison with unconditional expected value from part e) (2.85). 
### Part 1 is in expectation better, Part 2 worse.









