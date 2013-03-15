# Pat Kujawa, CSCI 548 Pattern Rec
# Project 4, LDA

# Motivation
# This project will acquaint you with the LDA analysis tools available in R. Additionally, you will begin the process of becoming familiar with how to visualize data once transformed by the LDA process. You will also compare some of the plots to PCA and MDS results.

# Big Picture
# You will read in several datasets, perform any necessary manipulation on the data to strip out unnecessary data and or labels, perform MDS upon the data, and plot the results.

# Project Instructions
# 2. Perform LDA on the iris data (in the script). Let's create a random sample from the set of 150 instances of size 125 on which to train. If you don't remember how to do this take a look at the slides.

## This is how you would do LDA if there wasn't a nice lda() function (arrgh!)
#  # Get means for each group, each dim (represents the centroid)
# df.class.means = aggregate(x=iris[-5], by=list(iris$Species), FUN=mean)  # exclude species column (5th one)
# # row.names(df.class.means) = df.class.means[[1]]
# df.class.means$Species = df.class.means$Group.1
# df.class.means[,1] = NULL  # drop grouping column
#
#  # Change each pt to the centroid
# ix = 1
# for (class in unique(iris$Species)) {
#     iris.copy[which(iris$Species == class),] = df.class.means[ix,]  # ??? How do you pull out rows by rowname?
#     ix = ix + 1
# }
#
#  # Run PCA on new dataset
# pca.results = prcomp(iris.copy[-5])

do.lda = function (data, classes, training.count) {
    library(MASS)
    data.cnt = nrow(data)
    lda.input = data[-length(data)]  # exluded class, assumed to be in last col

    train.ixs = sample(1:data.cnt, training.count)
    lda.results = lda(lda.input[train.ixs,], classes[train.ixs], tol=0)

    # 3. Test the accuracy of your classifier on remaining (test) data. Remember the trick with the minus sign to generate the test set (again, see the slides).
    actual.classes = classes[-train.ixs]
    if (length(actual.classes) > 1) {
        predicted.classes = predict(lda.results, lda.input[-train.ixs, ])$class

        # When you acquire the predictions you can compare them to the actual with something like "which(predictions != actual)". This will give you a vector of missed predictions. You can then determine the length of this vector to get a count of those missed. Have the script print the accuracy to the screen. Paste or enter this value along with the appropriate heading into the submission document (mine achieved 98.7% accuracy; yours may vary due to random sampling).
        inaccuracy = length(which(actual.classes != predicted.classes)) / length(actual.classes)
        print(sprintf('Accuracy was %.0f%%', 100*(1-inaccuracy)))
    } else {
        print("Used all data to train, so accuracy is irrelevant.")
    }

    # Remember, the lda function does not project the data for you. You must do this yourself (using the %*% operator; see the slides if you don't remember).
    projectionOntoFirst = as.matrix(lda.input) %*% lda.results$scaling[,1]
    projectionOntoSec = as.matrix(lda.input) %*% lda.results$scaling[,2]
    return(list(projectionOntoFirst, projectionOntoSec))
}

do.plot = function(plot.data, classes, s.label='(A)') {
#     plot.data = lda.plot.data
#     classes = iris$Species
#     s.label = '(A)'
    unique.classes = unique(classes)
    data.cnt = nrow(data)
    classes.count = length(unique.classes)
    colors = c('green', 'yellow', 'orange', 'burlywood1', 'red', 'blue', 'gray', 'pink')
    colors = colors[1:classes.count]  # truncate
    label.colors = as.vector(classes)
    for (i in 1:classes.count) {
        label.colors[which(label.colors == unique.classes[i])] = colors[i]
    }

    margin.top = 0
    margin.right = 0.1
    par(mar=c(2.5, 2.5, margin.top, margin.right), mgp=c(1.5, 0.5, 0))
    plot(plot.data[[1]], plot.data[[2]], pch = 16,
         cex = 1, col = label.colors, # main = title,
         xlab = "",
         ylab = "")
    legend('bottomright', legend=unique.classes, col=colors, pch=16, cex=0.8, bg='transparent')

    # 5. There will be no title for these plots. Instead there will be an (A), (B), and (C) and they will be inside the bounds of the plots.  Since there will be no titles, make sure you make the upper margin zero. We will also not be putting a legend in each (not enough room). Mine looks like the following. I did this with the mtext command. I placed the following command right after the first plot:
    mtext(s.label, line = -2)
}

# 6. Place code in the script to display the LDA, MDS, and PCA plots. Feel free to copy portions of your scripts from previous projects. In journal and conference papers, figures should stand alone. That is, someone should be able to look at a figure and its caption (figures tend to have captions below while tables above) and tell enough about it to interpret the underlying purpose and meaning of the figure. Copy and paste the three panel figure into your document and create a caption that has a short, descriptive title (like "Iris Data" or "Iris Scatter Plots") followed by descriptions for the three panels and the colors. Note: grad students will be expected to use this kind of format in their reports.
do.mds = function (data) {
    distMtx = dist(data)
    numDimensions = 2
    mds = cmdscale(distMtx, numDimensions, eig = TRUE, x.ret = TRUE)
    return(list(mds$points[,1], mds$points[,2]))
}

do.pca = function (data){
    pcaResults = prcomp(data)
    pcaXs = pcaResults$x[,1]
    pcaYs = pcaResults$x[,2]
    return(list(pcaXs, pcaYs))
}

do.all.and.plot = function(data, classes, name, training.count) {
    # 4. Next you are to create a scatter plot of the data in reduced dimensional space (two dimensions represented by the first two Linear Discriminants). Actually, you will create three scatter plots: one for LDA, one for MDS, and one for PCA. They will be placed in a three panel figure and will be colorized by species. Make the plot a 6.6X2.2 window and tighten up the margins like we did in the previous project. The points should be filled-in (solid) points and the plot should have an appropriate title and X and Y labels. Also, don't forget to include a legend.  To get a three panel plot you will use the layout command.
    windows(6.6,2.2)
    layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  );
    layout.show(3)
    # The layout.show command shows the three panel plot (see below). If you were to perform the plot command three times, each would go in an associated panel. Try running matrix(1:3, 1, 3) at the command line. This should give you an idea of how it is used in the layout command. Put these commands in the script.
    # > matrix(1:3, 1, 3)
    # [,1] [,2] [,3]
    # [1,]    1    2    3

    print(name)
    do.plot(do.lda(data, classes, training.count), classes, '(A)')
    do.plot(do.mds(data), classes, '(B)')
    do.plot(do.pca(data[-length(data)]), classes, '(C)')
    savePlot(filename=name, type='png')
}

# Iris ----
do.all.and.plot(iris, iris$Species, 'iris', 125)

# 7. Which if any shows the best separation? Place this question and its answer in the submission document. Don't forget to put the appropriate header, title, and section heading information in the document for the plots and the answers.
## ANSWER: MDS and PCA produce essentially the same results, just mirrored; LDA, on the other hand, produces a different plot of points. I think LDA presents the best separation by class because there is less overlap between versicolor and virginica, while setosa is still off in its own cluster.


# 1. Perform the same operations for the fruit, mouse, and tumor data (you should have a section for each with a three panel figure in each complete with heading and caption).

# Fruit -------------------------------------------------------------------
fruits = read.csv(file='fruits.csv')
do.all.and.plot(fruits, fruits$Class, 'fruit', nrow(fruits)-10)

# Mouse -------------------------------------------------------------------
# 2. Note: for the mouse data classify by proximal and distal (not by mouse type). Colorize that way as well. There are only 20 data points and close to a hundred dimensions. For this reason, let's use the entire data set as both the training data and testing data.

# 3. The data is quite sparse (lots of zeros) and 38 of the dimensions appear to be constant within groups (this is an error you get when you try to run LDA). The help screen indicates that the "function tries hard to detect if the within-class covariance matrix is singular." It checks whether any variable has within-group variance less than tol^2 and if so it will stop and report the variable as constant. Just for the heck of it, let's force it to run anyway (set tol=0 in the lda command). You'll get a warning but it should run (it did for me, though removing certain data points caused singular matrices and failure to compute).

# 4. You should get a fairly strange looking plot (mine was two vertical columns of points with the distribution along LD2 very small. This led to 100% accuracy for my classifier (not trustworthy because the test data was also the training data).
mice = read.table(file='otu_table_L6.txt', header=TRUE, sep='\t', strip.white=TRUE, row.names=1)  # hide taxon as first column
# mice = as.data.frame(mice)
miceWithRowsAsExperiments = as.data.frame(t(mice))
experimentNames = colnames(mice)  #colnames(mice)[-1] would skip the first item
classes = sapply(experimentNames, function(x) {
    if( grepl("[A-Z]+P[0-9]", x) ) {
        return('proximal')
    } else if(grepl("[A-Z]+D[0-9]", x)) {
        return('distal')
    } else {
        return('Taxon')
    }}
)
miceWithRowsAsExperiments$class = classes
data = miceWithRowsAsExperiments  #[-1,]
# levels(data$class) = c('proximal', 'distal')  # How to do this programmatically?

do.all.and.plot(data, data$class, 'Mice', nrow(data))


# Tumor -------------------------------------------------------------------
# 5. When performing LDA on the tumor data, I could not get the function to provide two linear discriminants (possibly it causes the accuracy to go down). So I plotted the distribution values on the first linear discriminant. This is what I want you to do as well. Also, LDA did not seem to like the "?'s" in the data.
# 6. I was able to read in the data with na.strings = "?" and that put NA's in place of "?'s". Then I was able to find the mean of all non-NAs and replace the NAs with the mean by doing the following:

# nonNAs = which(!is.na(mydata[,i]))
# mydata[-nonNAs,i]=mean(mydata[nonNAs,i])

# Where "i" is the column in question. We'll talk about other ways to handle NA's in the next lesson.
cancer = read.csv('breast-cancer-wisconsin.data', header=FALSE, na.strings = "?")
for (i in 1:length(cancer)) {
    nonNAs = which(!is.na(cancer[,i]))
    cancer[-nonNAs,i]=mean(cancer[nonNAs,i])
}
# Give names to data. The head() function lets you do python-style negative indexing, so the following grabs all but the last column name.
names(cancer) = c(head(names(cancer), -1), 'class')

# Remove the first column (ids)
cancer[1] = NULL

cCancer = ncol(cancer)
benignIxs = which(cancer[,cCancer] == 2)
cancer[,cCancer][ benignIxs ] = 'Benign'
cancer[,cCancer][ -benignIxs ] = 'Malignant'

# 7. To generate the density plots I used a kernel density function. It approximates a PDF in that the area under the curve sums to zero. The function (density) defaults to fitting 512 kernels (and it defaults to Gaussian) to the data. First I projected all the data onto LD1 and then found the density plot for everything. This was useful for getting the entire range of x values (both distributions must fit on the X axis so this will come in handy).

# myDens = density(projectionOntoFirst)
# myXrange = range(myDens$x)

# 8. Next, I projected just the benigns and plotted them (colored as  blue).

# plot(myDensBenign$x, myDensBenign$y,type = "l", col = "blue",
# xlim = myXrange, xlab = "Linear Discriminant 1",ylab = "Density")

# Note the $x and $y, also the xlim, and the type = "l" (that's a lower case L for line)

# 9. Next, I used the "lines" command (instead of plot) to add the malignants to the plot. It has the same format (except you don't need the type).
do.all.and.plot(cancer, cancer$class, 'Tumor', nrow(cancer)-10)


# 10. Don't forget to include an answer to the question about which provides the best separation. Also don't forget to include the accuracy for each dataset. It may be difficult for the tumor data since they it isn't a scatter plot.
# > source('~/My Dropbox/UM Grad School/2013 Spring/Pattern Rec 548/R workspace/P4 LDA/p4script.R')
# [1] "iris"
# [1] "Accuracy was 100%"
# [1] "fruit"
# [1] "Accuracy was 90%"
# [1] "Mice"
# [1] "Used all data to train, so accuracy is irrelevant."
# [1] "Tumor"
# [1] "Accuracy was 100%"
# Warning messages:
#     1: In dist(data) : NAs introduced by coercion
# 2: In dist(data) : NAs introduced by coercion
# 3: In lda.default(x, grouping, ...) : variables are collinear
# 4: In dist(data) : NAs introduced by coercion
# 5: In dist(data) : NAs introduced by coercion

# 11. Finish-up the write-up with your opinion as to why the best separation is found in the plots you identified. Also include some speculation as to whether the mouse data LDA plot indicates there is hope for a classifier to be able to identify proximal vs. distal, even across different mice.


