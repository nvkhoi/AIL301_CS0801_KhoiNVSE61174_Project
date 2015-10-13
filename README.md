# AIL301_CS0801_KhoiNVSE61174_Project
This repo contains source code and output for projects of subject AIL301 made by Nguyen Van Khoi of class CS0801.
Any idea is welcome. 

For Project 1 - clustering
	- Source code in R language for project 1 - clustering:
	   + Using partial color histogram with 3 layers
	   + Clustering by built-in kmeans
	- The output of source code after learning 25 times

	After many time trying:
	- Running time (fastest to slowest): color averaging, color histogram
	- Result: partial color histogram is the best (in my view), many similar images go into same group. It looks like more "reasonable" than the others.


For Project 2 - classification
	- About output file: some desired output files and numbers are missing, read the report for more details.
		+ HTML files: the name structure is output<Method>.html, if the method is SVM it also include the degree of polynomial
		+ CSV files:
			-> trainingErr.csv contains the original training error, the trainingErr_modified.xlsx is that but I modify a little bit for graphing
			-> Others CSV: the name structure is <layer num><name of color generator>.csv, there is two feature selection: color averaging and color histogram, for each I use from 1 to 3 layers
			-> main.R: source code, please read comments for more details
			-> Project2-classification.Rproj and .Rdata: the generated files when I create project in RStudio
			-> .Rhistory: the history of runned code
			-> report.docx or report.pdf (recommended): the report file
	- Some limitation: 
		+ I can't plot the table with some empty elements into graph. Moreover, R intepreter autofill them (if missing) with the first element in the row so I have to edit them a little bit.
		+ RStudio is always run in one core of CPU.
		+ Some algorithm is very slow and comsume a lot of memory (I disable almost everything, including Windows Explorer, to run boosting algorithm). 
		+ Be aware about the path, since RStudio always point working directory to current project folder. If you just use R and change the path, the category (that is generated from file path) can be load incorrectly
	- About used software: LibreOffice, Google Docs and Microsoft Office for draw graphss and writing report; RStudio for developing code; Cyberfox, Chrome for viewing html; Notepad++ for writing editing README. 
	