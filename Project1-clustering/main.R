rm(list = ls());
library(lattice);
library(ggplot2);
library(jpeg);

#read files
files <- list.files("/mit8-images-64x64", full.name = TRUE);
n <- length(files);

#calculate color histogram of img, in ROI((row1, column1), (row1, column2)), channel = color
histo <- function(img, r1, c1, r2, c2, color) {
	h <- 0 * c(1 : 16);
	for (i in r1 : r2) {
		for (j in c1 : c2) {
			x <- floor(255 * img[i, j, color] / 16.0) + 1;
			h[x] <- h[x] + 1;
		}
	}
	return(h);
}

#create dataset
D <- rbind();
for (i in 1 : n) {
	img <- readJPEG(files[i]);
	m <- nrow(img[ , , 1]);
	n <- ncol(img[ , , 1]);
	m1 <- floor(m / 2);
	n1 <- floor(n / 2);
	m2 <- floor(m1 / 2);
    n2 <- floor(n1 / 2);
	x <- c();
	for (color in 1 : 3) {
		#Layer 1: entire image
		x <- c(x, histo(img, 1, 1, m, n, color));

		#Layer 2: 1/4 of image
		x <- c(x, histo(img, 1, 1, m1, n1, color));
		x <- c(x, histo(img, 1, n1 + 1, m1, n, color));
		x <- c(x, histo(img, m1 + 1, 1, m, n1, color));
		x <- c(x, histo(img, m1 + 1, n1 + 1, m, n, color));

		#Layer 3: 1/16 of image
		x <- c(x, histo(img, 1, 1, m2, n2, color));
		x <- c(x, histo(img, m2 + 1, 1, m1, n2, color));
		x <- c(x, histo(img, m1 + 1, 1, 3 * m2, n2, color));
		x <- c(x, histo(img, 3 * m2 + 1, 1, m, n2, color));

		x <- c(x, histo(img, 1, n2 + 1, m2, n1, color));
		x <- c(x, histo(img, m2 + 1, n2 + 1, m1, n1, color));
		x <- c(x, histo(img, m1 + 1, n2 + 1, 3 * m2, n1, color));
		x <- c(x, histo(img, 3 * m2 + 1, n2 + 1, m, n1, color));

		x <- c(x, histo(img, 1, n1 + 1, m2, 3 * n2, color));
		x <- c(x, histo(img, m2 + 1, n1 + 1, m1, 3 * n2, color));
		x <- c(x, histo(img, m1 + 1, n1 + 1, 3 * m2, 3 * n2, color));
		x <- c(x, histo(img, 3 * m2 + 1, n1 + 1, m, 3 * n2, color));

		x <- c(x, histo(img, 1, 3 * n2 + 1, m2, n, color));
		x <- c(x, histo(img, m2 + 1, 3 * n2 + 1, m1, n, color));
		x <- c(x, histo(img, m1 + 1, 3 * n2 + 1, 3 * m2, n, color));
		x <- c(x, histo(img, 3 * m2 + 1, 3 * n2 + 1, m, n, color));
	}
	D <- rbind(D, x);
}

#init number of cluster, iterations and random seed
k <- 8;
max_it <- 25;
set.seed(997);

#cluster using built-in k-means
result <- kmeans(D, k, max_it);
label <- as.integer(result$cluster);

#output will be in the input folder
sink(file = "/mit8-images-64x64/output.html", type = "output");
n <- length(label);
for (i in 1 : k) {
  cat ("<h1> Group ", i, "<h1>");
  for (j in 1 : n) {
    if (label[j] == i) {
      cat ("<img src = ", files[j], ">");
    }
  }
  cat ("<hr>");
}
sink(file = NULL, type = "output");