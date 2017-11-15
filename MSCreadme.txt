# Group : Anjali and Vipin

#********************************************* Assignment-3 *********************************************
#												READ ME 	

_____________
I. File List:
-------------
MyStatsCalci.r


_____________
II. Approach:
-------------
Dividing the functionality of the program into separate modules. 


____________________________________________
III. Reference for R Tutorials and Formulas:
--------------------------------------------
a. R pdfs.
b. R from Tutorials Point.
c. R Documentation for specific functions descriptions.
d. John E. Freund's Mathematical Statistics with Applications by Irwin Miller.


__________________
IV. Software Used:
------------------
a. R-Studio (3.4.1)
b. R Console (3.4.1)
c. Sublime Text


_______________
Function used : 
---------------

___________________
Inbuilt Functions :
-------------------

a. sqrt(...)                : Computes the square root of the specified value.
b. sort(...)                : Sort a vector or factor into ascending or descending order.
c. cat(...)                 : Outputs the objects, concatenating the representations.
d. readline(...)            : Reads a line from the terminal (in interactive use).
e. install.packages(...)	: To install packages.
f. library(...)			    : To load packages.
g. as.numeric(...)			: Creates or coerces objects of type "numeric".
h. length(...)              : Get or set length of vectors (including lists) and factors.
i. fread(...)    			: To read the file.
j. as.integer(...)          : Converts to an integer value.
k. exp(...)                 : Computes the exponential of the given value.
l. formatC(...)             : Formatting numbers individually and flexibly.
m. qchisq(...)              : Density, distribution function, quantile function and random generation for chi-squared distribution.
n. qt(...)                  : Density, distribution function, quantile function and random generation for the t distribution.
o. qnorm(...)               : Density, distribution function, quantile function and random generation for the normal distribution.
p. pnorm(...)               : Density, distribution function, quantile function and random generation for the normal distribution.
q. hist(...) 				: Computes a histogram of the given data values.
r. plot(...)				: Generic function for plotting of R objects. 
s. barplot(...)				: Creates a bar plot with vertical or horizontal bars.
t. pie(...) 				: Draw a pie chart.
u. boxplot(...) 			: Produce box-and-whisker plot(s) of the given (grouped) values.
v. qqplot(...) 				: QQ plot of observed P-values vs expected P-values.
w. qqnorm(...) 				: Produces a normal QQ plot of the values in y. 
x. qqline(...) 				: Adds a line to a “theoretical” quantile-quantile plot which passes through the probs quantiles.
y. stem(...) 				: Produces a stem-and-leaf plot of the values in x.
z. pareto.chart(...) 		: Plot a Pareto chart.

	
	*(...) depicts number of arguments.


________________________
User Defined Functions :
------------------------

1. MAIN : Executing function.
2. USERINPUT : For takeing command line user input.
3. READFROMCSV : For taking input from file.
4. INPUT : For selecting data entry method (either by USERINPUT or READFROMCSV).

________________________________
Module 1 (Descriptive Analysis):
--------------------------------

1. MEAN 		: For computation of arithmetic mean.
2. VARIANCE2 	: For computation of population variance.
3. VARIANCE2 : For computation of sample variance.
4. SD : For computation of standard deviation.
5. MEDIAN : For computation of median.
6. MODE : For finding number with maximum frequency.
7. MEANABSDEV : For computation of mean absolute deviation.
8. MINIMUM : For finding number with minimum value. 
9. MAXIMUM : For finding number with maximum value.
10. RANGE : For finding range (difference of maximum and minmum value).
11. QUARTILES : For finding the quartiles (q1, q2, q3).
12. IQR : For finding the Inter quartile range.
13. SKEWNESS : For finding skewness(g1).
14. KURTOSIS : For finding kurtosis(g2).
15. MOMENTS : For finding central moments.
16. DESCRIPTIVEANALYSIS : Executing function for Descriptive Analysis.

_______________________________
Module 2 (Predictive Analysis):
-------------------------------

1. CORRELATION : For finding Karl Pearson Coefficient of correlation.
2. MULTREG : For finding line of multiple linear regression for three variables.
3. PREDANALYSIS : Executing function for Predictive Analysis.

________________________________
Module 3 (Probability Analysis):
--------------------------------

1. FACTORIAL : For finding factorial of a number.
2. PERMUTATION : For finding nPr.
3. COMBINATION : For finding nCr.
4. BASICPROBABILITY : For finding Basic probability.
5. BAYESTHEOREM : For finding P(Ai|B) using P(B|Ai) and P(Ai).
6. PROBABILITYANALYSIS : Executing function for Probability Analysis.

___________________________________________
Module 4 (Discrete Distribution Functions):
-------------------------------------------

1. UNIFORMDISTRIBUTION : For finding probability of a random variable following uniform distribution.
2. BERNOULLI : For finding probability of a random variable following bernoulli distribution.
3. BINOMIALDISTRIBUTION : For finding probability of a random variable following binomial distribution.
4. GEOMETRIC : For finding probability of a random variable following geometric distribution.
5. HYPERGEOMETRIC : For finding probability of a random variable following hypergeometric distribution.
6. NEGATIVEBIN : For finding probability of a random variable following negative binomial distribution.
7. POISSON : For finding probability of a random variable following poisson distribution.
8. MULTINOMIAL : For finding probability of a random variable following multinomial distribution.
9. MULTIHYPGEO : For finding probability of a random variable following multi hypergeometric distribution.
10. DISCRETEDISTRIBUTION : Executing function for Discrete Distribution Functions.

_____________________________________________
Module 5 (Continuous Distribution Functions):
---------------------------------------------

1. UNICONTINOUS : For finding probability of a random variable following uniform continuous distribution. 
2. NORMALDIST : For finding probability of a random variable following normal distribution.
3. GAMMADIST : For finding probability of a random variable following gamma distribution.
4. CONTINOUSDISTRIBUTION : Executing function for Continuous Distribution Functions.

______________________________________________
Module 6 (Sample Distribution Test Statistic):
----------------------------------------------

1. CHISQDIST : Density, distribution function, quantile function and random generation for the chi-squared (chi^2) distribution.
2. TDIST : Density, distribution function, quantile function and random generation for the t distribution.
3. FDIST : Density, distribution function, quantile function and random generation for the f distribution.
4. ZDIST : Density, distribution function, quantile function and random generation for the z distribution.
5. SAMPLEDISTTEST : Executing function for Sample Distribution Test Statistic.

_______________________________
Module 7 (Interval Estimation):
-------------------------------

1. INTERVALESTIMATION : Executing function for Interval Estimation.

___________________________________
Module 8 (Non-Parametric Analysis):
-----------------------------------

1. SIGNTEST : For testing hypothesis on basis of sign given by null hypothesis.
2. NONPARAMANALYSIS : Executing function for Non-Parametric Analysis.

__________________________
Module 9 (Visualizations):
--------------------------

1. HISTOGRAM : Computes a histogram of the given data values.
2. LINEGRAPH : Computes a line graph of the given data values.
3. BARGRAPH : Computes a bar graph of the given data values.
4. PIECHART : Computes a pie chart of the given data values.
5. SCATTERPLOT : Computes a scatter plot of the given data values.
6. BOXPLOT : Computes a box plot of the given data values.
7. QQPLOT : Computes a q-q plot of the given data values.
8. STEMLEAFPLOT : Computes a stem leaf plot of the given data values.
9. PARETOCHART : Computes a pareto chart of the given data values.
10. VISUALIZATIONS : Executing function for Visualizations.


______________________________________
BASIC FUNCTION DEFINITIONS AND SYNTAX:
--------------------------------------

1. readline
		Description: It reads a line from the terminal (in interactive use). The prompt string will be truncated to a maximum allowed length, normally 256 chars (but can be changed in the source code).
		Syntax: readline(prompt = "")
				prompt : The string printed when prompting the user for input. Should usually end with a space " ".

2. install.packages
		Download and install packages from CRAN-like repositories or from local files.
		Syntax : install.packages(package)
			package : character vector of the names of packages whose current versions should be downloaded from the repositories.

3. library
		For Loading/Attaching And Listing Of Packages.
		Syntax : library(package)
			package : the name of a package to be loaded

4. as.numeric
		Converts the argument column into a numeric value column
		Syntax : as.numeric(x)
			x : a column from a data set to be converted into numeric.

5. length
		Length Of An Object. Get or set the length of vectors (including lists) and factors, and of any other R object for which a method has been defined.
		Syntax : length(x)
		length(x) <- value
			x : an R object. For replacement, a vector or factor.
			value : a non-negative integer or double (which will be rounded down).

6. fread
		Similar to read.table but faster and more convenient. All controls such as sep, colClasses and nrows are automatically detected. 
		Syntax : fread(input, sep="auto", sep2="auto", skip=0L, na.strings="NA", blank.lines.skip=TRUE)
			Input : Either the file name to read (containing no \n character), a shell command that preprocesses the file (e.g. fread("grep blah filename")) or the input itself as a string (containing at least one \n).

7. sqrt
		Computes the square root of the specified float value.
		Syntax : sqrt(x)
			x : an R object. For replacement, a vector or factor.

8. sort
		Sort a vector or factor into ascending or descending order.
		Syntax : sort(x, decreasing = FALSE, …)
			x : an R object. For replacement, a vector or factor.
			decreasing : logical. Should the sort be increasing or decreasing?

9. cat
		Outputs the objects, concatenating the representations.
		Syntax : cat(… , file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
			… : R objects (see ‘Details’ for the types of objects allowed).
			file : A connection, or a character string naming the file to print to.
			sep : a character vector of strings to append after each element.
			fill : a logical or (positive) numeric controlling how the output is broken into successive lines.
			labels : character vector of labels for the lines printed. Ignored if fill is FALSE.
			append : logical. Only used if the argument file is the name of file 

10. exp
		Computes the exponential of the given value.
		Syntax : exp(x)
			x : Column to compute on.

11. as.integer
		Converts to an integer value.
		Syntax : as.integer(x)
			x : an R object.

12. formatC
		Formatting numbers individually and flexibly.
		Syntax : formatC(x, digits = NULL, format = NULL)
			x : an atomic numerical or character object.
			digits : the desired number of digits after the decimal point.
			format : equal to "d" (for integers), "f", "e", "E", "g", "G", "fg" (for reals), or "s" (for strings).

13. qchisq
		Density, distribution function, quantile function and random generation for the chi-squared 
		distribution with df degrees of freedom and optional non-centrality parameter ncp
		Syntax : qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
			p : vector of probabilities.
			n : number of observations. If length(n) > 1, the length is taken to be the number required.
			df : degrees of freedom (non-negative, but can be non-integer).
			ncp : non-centrality parameter (non-negative).
			log, log.p : logical; if TRUE, probabilities p are given as log(p).
			lower.tail : logical; if TRUE (default), probabilities are P[X≤x], otherwise, P[X>x].

14. qt
		Density, distribution function, quantile function and random generation for the t distribution with df 
		degrees of freedom (and optional non-centrality parameter ncp.
		Syntax : qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
			p : vector of probabilities.
			n : number of observations. If length(n) > 1, the length is taken to be the number required.
			df : degrees of freedom (>0, maybe non-integer). df = Inf is allowed.
			ncp : non-centrality parameter δ; currently except for rt(), only for abs(ncp) <= 37.62. If omitted, use the central t distribution.
			log, log.p : logical; if TRUE, probabilities p are given as log(p).
			lower.tail : logical; if TRUE (default), probabilities are P[X≤x], otherwise, P[X>x].

15. qnorm
		Density, distribution function, quantile function and random generation for the normal distribution 
		with mean equal to mean and standard deviation equal to sd.
		Syntax : qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
			p : vector of probabilities.
			n : number of observations. If length(n) > 1, the length is taken to be the number required.
			mean : vector of means.
			sd : vector of standard deviations.
			log, log.p : logical; if TRUE, probabilities p are given as log(p).
			lower.tail : logical; if TRUE (default), probabilities are P[X≤x] otherwise, P[X>x].

16. pnorm
		Density, distribution function, quantile function and random generation for the normal distribution 
		with mean equal to mean and standard deviation equal to sd.
		Syntax : pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
			q : vector of quantiles.
			n : number of observations. If length(n) > 1, the length is taken to be the number required.
			mean : vector of means.
			sd : vector of standard deviations.
			log, log.p : logical; if TRUE, probabilities p are given as log(p).
			lower.tail : logical; if TRUE (default), probabilities are P[X≤x] otherwise, P[X>x].

17. hist
		The generic function hist computes a histogram of the given data values. If plot = TRUE, the resulting 
		object of class "histogram" is plotted by plot.histogram, before it is returned.
		Syntax : hist(x, main = paste("Histogram of" , xname), xlab = xname, ylab, labels = FALSE, col = color, …)
			x : a vector of values for which the histogram is desired.
			main, xlab, ylab : these arguments to title have useful defaults here.
			col : to fill specified color
			… : further arguments and graphical parameters passed to plot.histogram and thence to title and axis (if plot = TRUE).

18. plot
		Generic function for plotting of R objects. 
		Syntax : plot(x, y, …)
			x : the coordinates of points in the plot. Alternatively, a single plotting structure, function or any R object with a plot method can be provided.
			y : the y coordinates of points in the plot, optional if x is an appropriate structure.
			… : Arguments to be passed to methods, such as graphical parameters. Many methods will accept the following arguments:
				main, xlab, ylab : these arguments to title have useful defaults here.
				col : to fill specified color
				type : what type of plot should be drawn. Possible types are
					"p" for points,
					"l" for lines,
					"b" for both, and more.

19. barplot
		Creates a bar plot with vertical or horizontal bars.
		Syntax : barplot(height, …)
			height : either a vector or matrix of values describing the bars which make up the plot.
			… : arguments to be passed to/from other methods. For the default method these can include further arguments
				main, xlab, ylab : these arguments to title have useful defaults here.
				col : to fill specified color

20. pie
		Draw a pie chart.
		Syntax : pie(x, …)
			x : a vector of non-negative numerical quantities. The values in x are displayed as the areas of pie slices.
			… : graphical parameters can be given as arguments to pie. They will affect the main title and labels only.
				main, xlab, ylab : these arguments to title have useful defaults here.
				col : to fill specified color
				 	col=rainbow(length(x))

21. boxplot
		Produce box-and-whisker plot(s) of the given (grouped) values.
		Syntax : boxplot(x, …)
			x : for specifying data from which the boxplots are to be produced. Either a numeric vector, or a single list containing such vectors. Additional unnamed arguments specify further data as separate vectors (each corresponding to a component boxplot). NAs are allowed in the data.
			… : For the formula method, named arguments to be passed to the default method.
				main, xlab, ylab : these arguments to title have useful defaults here.
				col : to fill specified color

22. qqplot
		QQ plot of observed P-values vs expected P-values, using the empirical (permutation-based) expected p-value distribution.
		Syntax : qqplot(P.perm, P.observed, …)
			P.perm : Expected P-values from NULL distribution, which is generated through permutation in our example.
			P.observed : Observed P-values from true case/control assignment.
			… : For the formula method, named arguments to be passed to the default method.
				main : these arguments to title have useful defaults here.
				col : to fill specified color

23. qqnorm
		qqnorm is a generic function the default method of which produces a normal QQ plot of the values in y. 
		Syntax : qqnorm(y, …)
			y : The only data sample.
			… : graphical parameters.

24. qqline
		Adds a line to a “theoretical”, by default normal, quantile-quantile plot which passes through the probs quantiles, by default the first and third quartiles.
		Syntax : qqline(y, …)
			y : The only data sample.
			… : graphical parameters.

25. stem
		Produces a stem-and-leaf plot of the values in x.
		Syntax : stem(x)
			x : a numeric vector.

26. pareto.chart
		Plot a Pareto chart.
		Syntax : pareto.chart(x, ylab = "Frequency", ylab2 = "Cumulative Percentage", xlab,  main,  col = heat.colors(length(x)), ...)
			x : a vector of values. names(x) are used for labelling the bars.
			ylab : a string specifying the label for the y-axis.
			ylab2 : a string specifying the label for the second y-axis on the right side.
			xlab : a string specifying the label for the x-axis.
			main : a string specifying the main title to appear on the plot.
			col : a value for the color, a vector of colors, or a palette for the bars. See the help for colors and palette.
			...  : other graphical arguments to be passed to the barplot function.
