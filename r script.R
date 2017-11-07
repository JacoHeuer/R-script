# loading csv file(s):	

	<- read.csv("filename.csv")
	<- read.csv("filename.csv", header=FALSE)	# if csv does not contain a header line
	<- read.csv("filename.csv", as.is=TRUE) 	# data is interpreted as strings and not as a factor

# writing to csv files:

 write.csv("x", file="filename", row.names=FALSE)	# no row labels
 write.csv("x", file="filename", col.names=FALSE)	# no collumn labels

# reading data from the web:

	<- read.csv("http://www./data.csv")
	<- read.table("ftp://ftp./data.txt")	# reading data from ftp sites

# reading data from html tables:

## first install "libxml2" before using XML package
## readHTMLTable function is in the XML package

library(XML)
url <- 'http://www./table.html'
    <- readHTMLTable(url)

	<- readHTMLTable(url, which="2")	# reading only specific tables, use "which=" parameter

# reading files with a complex structure:

lines <- read.lines("input.txt")	         # reads lines from a file & return character strings
lines <- read.lines(:"inputs.txt", n="")	  # reading number of lines and stop

	<- scan("input.txt", what=numeric(0))   	# interpret next token as a number
                                integer(0)   	# interpret next token as integer
                                complex(0)	    # interpret next token as complex number
                                character(0)	# interpret next token as character string
                                logical(0)	    # interpret next token as logical value

	<- scan("input.txt", what=list(numeric(0),integer(0),etc.))	# creating a list
	<- scan("input.txt", what=list(high=numeric(0),low=numeric(0),etc.)) 

	<- scan("http://www.",         	
                 skip = 30,		  # lines to skip before reading data
                 nlines = 20,	  # stop after reading this many input lines
                 what = list(),   # na.strings=list - list of strings to be interpreted as NA
                 )

# reading from MySQL database:

##install RMySQL package

"con"	<- dbConnect(MySQL(), user="userid", password="pswd", 	# dbConnect - open database
                   host="hostname", client.flag="") 

sql <- "SELECT * from '' WHERE '' "		# dbGetQuery - submit sql to database and read the results
rows <- dbGetQuery(con, sql)

if "(dbMoreresults(con)) dbNextresult(con)"		# calling db if MySQL complain of unprocessed results
dbDisconnect(con) 						       # closing database

# saving and transporting objects:

save("filename", file="filename.RData")		# save a file
load("filename.Rdata")					  # load a file

dput("filename", file="filename.txt")	  # to save in an ASCII format - dput & dump
dump('filename', file="filename.txt")   	# quotes must be around variable name

## when load() is used, make sure package was loaded from libarary()

library()
load()
plot()

# inserting data into a vector:

append(1:10, 99, after=5)		# append function inserts data into vector by uing "after" parameter
[1] 1 2 3 4 5 99 6 7 8 9 10

append(1:10, 99, after=0)		#after=0 - insert new item at head of vector
[1] 99 1 2 3 4 5 6 7 8 9 10


# recycling rule:

cbind(1:6, 1:3)			# creates column vectors


# combining multiple vectors into one vector & factor:

"variable"	<- stack(list(v1=v1, v2=v2, v3=v3))		# stack - combine list in two-column data frame
aov("columnname" ~ "columnname", data="variable")		# ANOVA analysis



# removing an element from a list:

## assign NULL to the element

list[,c(1, 2, 3)] <- NULL
list(0)


# flatten a list into a vector:

mean(unlist(scores))		# unlist - if its a list of numbers, use unlist


# removing NULL elements from a list:

lst[sapply(lst, is.null)] <- NULL		# sapply - returns vector of logic that are true whereever the list is NULL
							             # lapply - apply function to every element of the list


# initializing a matrix:

data <- c(1, 2, 3, 4, 5)
mat <- matrix(data, 2, 3, byrow=TRUE)	# byrow=TRUE - tells matrix that data is row-by-row, not column-by-column

## creating row & column names

rownames(mat) <- c("rownames1", "rownames2", ...,)
colnames(mat) <- c("colname1", "colname2", ...,)
print(mat)

## quick way to create matrix

data <- c(1, 2, 3, 4, 5)
dim(data) <- c(2, 3)			# dim - 'dimensions' is assigned to the vector, no need for "byrow="


# matrix operations:

t(A)		# matrix transposition of A
solve(A)	# matrix inverse of A
A %*% B	    # matrix multiplication
diag(n)	    # identity matrix



# selecting one row | column from a matrix:

vec <- mat[1,]	# first row
vec <- mat[,2]	# second column

row <- mat[1,,drop=FALSE]	# drop=FLASE - if you want result to be one-row | one-column matrix
col <- mat[,2,drop=FALSE]


# data frame from column data:

	<- data.frame(v1, f1)	# assemble vectors & factors into a data frame
	<- as.data.frame(list)	# if data is in a list that contains v/f


# data frame from row data:

	<- do.call(rbind, obs)	# use do.call & rbind to bind rows into one, large data frame.. 
					         # obs - list of one-row data frames
					          # rbind allows multiple rguments


# preallocating a data frame:

	<- data.frame(colname1=numeric(n), colname2=character(n), ...)	# generic vectors and factors - numeric(n), character(n), factor(n)
												                     # "n" is number of rows needed for data frame


# selecting data frame columns by position:

 dfrm[[n]]				 # returns one column
 dfrm[n]				 # more than one column
 dfrm[c(n1, n2, ...)]		# returns df from the column in position
 dfrm[, n]				 # returns the nth column
 dfrm[, c(n1, n2, ...)]		# returns df from the column in position

## to SELECT data frame columns by name.. use the same consept!


# selecting rows and columns more easily:

 subset(dfrm, select=colname)					   # select- column name | vector of column names
 subset(dfrm, select=c(colname1, colname2, ...))	# you do NOT quote column names
									                 # subset - logical expression.. select rows


subset(dfrm, subset=(response > 0))							        # response - column in data frame, selecting a positive response
subset(dfrm, select=c(predictor,response), subset=(response > 0))		# subset is most useful when combining 'select' & 'subset'


# excluding columns by name:

 subset(dfrm, select = -colname)	# will exclude all colnames except the one with 'select = -colname'


# changing names of data frame columns:

 colnames(dfrm) <- newnames	                 # newnames is a vector of character strings
 colnames() <- c(colname1, colname2, ...)


# editing a data frame:

	<- edit(dfrm)
 dfrm <- 			# overite if you're happy with the changes

 fix(dfrm)			# invokes the editor & overwrite your variable with the result
				     # there is no "undo"..
                      # use editor instead


# removing NA's from a data frame:

	<- na.omit(dfrm)

 data(na.omit(dfrm))


# combining two data frames:

all.cols <- cbind(dfrm1, dfrm2)	 # cbind - combine columns of df side-by-side
all.rows <- rbind(dfrm1, dfrm2)  # rbind - to "stack" the rows of df


# merging data frames by columns:

	<- merge(df1, df2, by="name")		# merge - join data frames into one new data frame
							              # use 'reshape2' & 'plyr' packages in CRAN


# accessing data frame contents more easily:

 with(dataframe, expr)		# exposes the column names
					         # expr - refer to the columns of 'dataframe' by name

 attach(dataframe)		# attach - repetitive access.. can refer to the columns by name, without mentioning the df
					     # creates search list

 detach				# remove data frame from search list


# atomic data types:

 as.character(x)
 as.complex(x)
 as.numeric(x) | as. double(x)
 as.integer(x)
 as.logical(x)



# splitting a vector into groups:

	<- split()				  # returns a list of vectors
	<- unstack(data.frame())	# unstack - if all vectors have the same length, it converts the list into a df


# appluing a function to each list element:

	<- lapply(lst, fun)		# always returns results in list
	<- sapply(lst, fun)		# returns result in a vector, if possible


# apllying a function to every row:

	<- apply(mat, 1, fun)		# mat - matrix, fun - function
						         # 1 - second argument, indicating row-by-row application of a function
						          # can replace 'fun' with "mean"


# applying a function to every column:

	<- apply(mat, 2, fun)		# 2 - column-by-column application


# applying a function to groups of data:

 tapply(x, f, fun)			# x - vector, f - factor


# applying a function to groups of rows:

 by(dfrm, fact, fun)		# dfrm - data frame, fact - grouping factor
                             # by() - collects the list, and returns the list


# applying a function to parallel vectors | list:

 mapply(f, vec1, vec2, ...)			# mapply - apply the function f to an argument 
 mapply(f, list1, list2, ...)		# mapply - also work with list arguments


# strings & dates

## packages

 chron		   # dates & times.. best use in econometrics | time serie analysis
 lubridate		# dates & times & time zones.. clever regarding datetime arithmetic
 mondate	   # dates, handels dates in units of months to days & years.. ideal for accounting
 timeDate		# dates & time & time zones, high powered package.. used in financial modeling


# getting the length of a string:

 nchar()		# takes string & return the number of char. in the string

	 <- c()
 nchar()		# if nchar is applied to a vector of str. it returns the length of each string
			     # length fuunction returns the length of a vector


# concatenating strings:

 paste()							# creating a new string, by joining strings together
 paste("I", "Know", sep="")				# sep - specify a different seperator.. 
								          # sep="" - to run the strings together without separation

 "ok"	 <- c()
 paste(ok, "i", "Kown", collapse=", and")		# collapse=", and" - top-level seperator, will seperate result with ', and ...'


# exstracting substrings:

 substr(string, start ,end)					     # to extract the substring that begins at 'start' and ends at 'end'
 
 substr(string, nchar(string)(n), nchar(string))	# nchar can be used with substr.. '(n)' - number to start string from


# splitting a string accopr. to a delimiter:

 strsplit(string, delimiter)		  			# strsplit takes two arguments - the string & the delimiter of the substring
									              # delimiter - string | regular expression

	    <- "/milk/bread/butter.csv"			# string
 strsplit(, "/")							# delimiter - "/"


# replacing substrings:

 sub(old, new, string)			# replace first instance of a substring
 gsub(old, new, string)			# replace all instances of a substring

 'string'	<- "I. knew. it"
 sub("I", "you", 'string')

 result = "you. knew. it" 



# seeing the special character in a string:

 print()


generating all pairwise combinations of strings:

	<- outer(strings1, strings2, paste, sep="")		# use outer & paste together to generate the matrix of all posib. combinations


# getting current date:

 Sys.Date()


# converting a string into a date:

 as.Date()
						
 as.Date("03/03/2017", format="%m/%d/%Y")		# to convert an american-style date - use 'format'
                                                 # format - capital Y.. 4-digit year
result = "2017/03/03"					    # format - lower case Y.. 2-digit year


# converting a date into a string:

 format(Sys.Date())					  # both functions allow format argument, that controls the formatting
 as.character(Sys.Date())

 format(Sys.Date(), format="%m/%d/%Y")

 %b		# abbreviated month name ("Jan")
 %B		# full month name ("January")
 %d		# days
 %m		# month
 %y		# year without century (00 - 99)
 %Y		# year with century



# converting year, month & day into a date:

 ISOdate(year, month, day)
 ISOdatetime(year, month, day, hour, minute, second)

 as.Date(ISOdate(year, month, day))				     # result is a POSIXct object, that can be converted into a date


# getting the jullian date:

	 <- as.Date()
 julian()				  # in R, the jullian date is the number of days since January 1, 1970


# extracting the parts of a date:

 a  <- as.Date()
 b  <- as.POSIXlt(a)		# POSIXlt - list of date parts.. converting date object to POSIXlt using 'as.POSIXlt'

 b$mday				# day of the month
 b$mon				# month (0 = January)
 b$year +1900			# year


# creating a sequence of dates:

 a <- as.Date()
 b <- as.Date()

 seq(from=a, to=b, by=1)			# from - starting date, to - ending date, by - increment of 1 indicates daily dates

 seq(from=a, by=1, length.out=5)		# length.out - number of dates 



# Probability

 dnorm	# normal density
 pnorm	# normal distribution function
 qnorm	# normal quantile function
 rnorm	# normal random variates


# counting the number of combinations:

 choose(n, k)	# choose - calculate the number of..
			     # n - number of combinations
                # k - items taken at a time
			     # numbers are also known as binomial coefficients


# generating combinations:

 combn(items, k)		# combn - generate all combinations of..
				         # items - n, & k


# generating random numbers:

 runif(1)		 # generating a uniform random number between 0 & 1
 rnorm(1)

 runif(1, min=, max=)
 rnorm(1)
 result = 


# generating reproducible random numbers:

 set.seed()		# to initialize the random number to generate a known state
 runif()


# generating a random sample:

 sample(vec, n)				# randomly select n items from a vector

 sample(presidents$year, 10)		# sample will not select the same item twice
						
 
 median <- numeric(1000)
 for (i in 1:1000) {							    # this repeatedly samples a dataset x and calculates the sample median
      medians[i] <- median(sample(x, replace=TRUE))		# replace=TRUE - specify to sample with replacement
 }


# generating random sequences:

 sample(set, n, replace=TRUE)
 
 sample(c(), n, replace=TRUE)

 sample(c(FALSE,TRUE), 10, replace=TRUE, prob=c(0.1,0.5))		# prob - probability is set for 'FALSE, TRUE'


# randomly permuting a vector:

 sample(v, size=length(v), replace=FALSE)


# calculating probabilities for discrete distributions:

 dbinom(n, size= n, prob= n)

 pbinom(n, size= n, prob= n, lower.tail=FALSE)		# lower.tail=FALSE - all of the distribution functions let you find this right-tail probability


# calculating probabilities for continuous distributions:

## functions

 distribution:             distribution function:

 normal                    pnorm(x,mean,sd)

 student's t               pt(x,df)

 exponential               pexp(x,rate)

 gamma                     pgamma(x,shape,rate)

 chi-squared(X * X)        pchisq(x,df)


# converting probabilities to quantiles:

 qnorm(x, mean, sd)
 
 qnorm(c(x,x))

## discrete distribution functions:

 distribution:             quantile function:

 binomial                  qbinom(p,size,prob)
 
 geometric                 qgeom(p,prob)

 poison                    qpois(p,lambda)


## common continuous distribution functions:

 distribution:             quantile function:

 normal                    qnorm(p,mean,sd)

 student's t               qt(p,df)

 exponential               qexp(p,rate)

 gamma                     qgamma(p,shape,rate=rate) | qgamma(p,shape,scale=scale)

 chi-squared(X * X)        qchisq(p,df)



# plotting a density functuion:

 x <- seq(from, to, length.out)
 plot(x, dnorm(x))

 x <- seq(from=0, to=6, length.out=100)			# define the density domains
 ylim <- c(0, 0.6)

 par(mfrow=c(2,2))						   # create a 2*2 plotting area

 plot(x, dunif(x,min,max), main="Uniform",		# plot a uniform density
         type='l', ylim=ylim)

 plot(x, dnorm(x,mean,sd), main"Normal", 			# plot a normal density
         type='l', ylim=ylim)

 plot(x dexp(x,rate), main"Exponential", 			# plot a exponential density
        type='l', ylim=ylim)

 plot(x, dgamma(x,shape,rate), main="Gamma",		# plot a gamma density
         type='l', ylim=ylim)


 ## using the polygon function:

 x <- seq(from, to, length.out)
 y <- dnorm(x)
 plot(x, y, main, type='l', ylab, xlab)
 abline(h=0)

 ## polygon function will connect the first and last function (x,y)

 ## body of the polygon follows the density curve:

 region.x <- x[1 <= x & x <= 2]
 region.y <- y[1 <= x & x <= 2]

 ## add initial & final segments, drop down to y axis:

 region.x <- c(region.x[1], region.x, tail(region.x,1))
 region.y <- c(          0, region.y,               0)

 ## call polygon to plot the boundary of the region:

 polygon(region.x, region.y, density=-1, col="green")



 # General statistics

 # summarizing your data:

 summary()

 summary(list)

 lapply(list, summary)


 # calculating relative frequencies:

 mean()

 mean(x == x)					

 mean(x > x)

 mean(abs(x-mean(x)) > 2*sd(x))		# fraction of observations that exceed two standard deviations from the mean

 mean(diff(ts) > 0)				# fraction of observations in a time series, larger than the previous observation



 # tabulating factors and creating contingency tables:

 table(f)	
				 # table can produce more than one factor (cross-tabulations)
 table(f1, f2)

 xtabs()			# can also produce a contingency table & has a formula interface


 # testing categorical variables for independence:

 summary(table(f1,f2))		# output includes a p-value


 # calculating quantiles & quartiles of a dataset:

 quantile(vec, f)			# f - fraction, obesrvation x

 quantile(vec)			# for quartiles, omit second argument altogether

 quantile(vec, c(f1, f2))


 # inverting a quantile:

 mean(vec < x)


 # converting data to z-scores:

 ## also called normaliZing data

 scale(x)			      # works for vectors, matrices & data frames

 (y - mean(x)) / sd(x)		# normalize a single value y relative to a dataset x


 # testing the mean of a sample (t Test)

 t.test(x, mu=m)				# mu=m - particular value = 'm'

 x <- rnorm(20, mean=40, sd=10)
 t.test(x, mu=85)				# mu=85 - in this set we are looking for the percentage


 # forming a confidence interval for a mean:

 x <- rnorm(20, mean=40, sd=10)

 t.test(x, conf.level=0.99)			# raise the confidence level by setting - conf.level


 # formating a confidence interval for a median:

 wilcox.test(x, conf.int=TRUE)		# output will contain a confidence interval for the median


 # testing a sample proportion:

 prop.test(x, n, p)			# x - sample
                             # n - sample size
                              # output includes p-value
						
 prop.test(x, n, p, alternative="greater")	# result - alternative hypothesis


 # forming a confidence interval for a proportion:

 prop.test(n, x, p, conf.level)


 # testing for normality:

 shapiro.test(x)		# shapiro-wilk normality test
					     # output includes a p-value


 # testing for runs:

 ## checks a sequence of randomness (yes-no, 0-1, true-false)

 library(tseries)

 runs.test(as.factor())		# output - p-value


 # comparing the means of two samples:

 t.test(x, y)				# call the t.test function

 t.test(x, y, paired=TRUE)		# paired=TRUE - if x is paired with y

 var.equal=TRUE				# to obtain a less conservative test


 # comparing the locations of two samples Nonparametrically:

 wilcox.test(x, y, paired)


 # testing a correlation for significance:

 cor.test(x, y)						# can calculate both p-value & the confidence interval of the correlation
 cor(x, y)

 cor.test(x, y, method="spearman")			# method="spearman" - nonnormal populations


 # testing groups for equal proportions:

 x	<- c(x1, x2, ..., xn)
 y	<- c(y1, y2, ..., yn)

 prop.test(x, y) 


 # performing pairwise comparisons between group means:

 pairwise.t.test(x, f)		# to perform pairwise comparison of means
						     # x - data, f - grouping factor


 # comparing two samples for the same distribution:

 ks.test(x, y) 			# compares 2 samples & test them for being drawn from same distribution


 # Graphics
 
 # creating a scatter plot:
 
  plot(x, y)			# two parallel vectors(x, y)
  
  plot(dfrm)			# if your data is in a two-loumn data frame


# adding titles and labels:

 plot(x, main="the title", xlab="x axis label", ylab="y axis label")


# adding a grid:

 plot(x, y, type="n")			# type="n" - initialize the graphics frame without displaying the data 
 
 grid()							# draw grid
 
 points(x, y)					# to draw graphics overlaid on the grid


# creating a scatter plot of multiple groups:

 plot(x, y, pch=as.integer(f))			# pch - plot each point with a different plotting character


# adding a legend:

 legend(x, y, labels, pch=c(pointtype1, pointtype2, ...))			# legend for points
 legend(x, y, labels, lty=c(linetype1, linetype2, ...))				# legend for lines according to line type
 legend(x, y, labels, lwd=c(width1, width2, ...))					# legend for lines according to line width
 legend(x, y, labels, col=c(color1, color2, ...))					# legend for colors


# plotting the regression line of a scatter plot:

 d	<- lm(x ~ y)
 plot(x ~ y)
 abline(d)					# abline - draws fitted regression line


# plotting all variables against all other variables:

 plot(drfm)				# place in data frame then plot df. R will create one scatter plot for every pair of columns
 plot(x, [, 1:2])


# creating one scatter plot for each factor level:

 coplot(x ~ y | f)				# for a conditioning plot


# creating a bar chart:

 barplot(c(height1, height2, ...))			# first argument is a vector of bar heigths


# adding confidence intervals to a bar chart:

 library(gplots)
 barplot2(x, plot.ci=TRUE, ci.l=lower, ci.u=upper)				#barplot2 - display a bar chart of x and its confidence intervals
 lower <- tapply()
 
 upper <- tapply()


# coloring a bar chart:

 barplot(heights, col=c("colors"))
 
 rel.hts <- rank(heights) / length(heights)				# convert the bars rank to relative heights, between zero and 1
 grays <- gray(1 - rel.hts)								# convert heights into a vector of grayscale colors. taller bars will be dark
 barplot(heights, col=grays)							# creating a shaded bar chart 



 
 
 
  
 


 
 
 

 












 



 








				















 











	





















 

















































 



















	
