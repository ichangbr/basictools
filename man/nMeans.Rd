% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nMeans.R
\name{nMeans}
\alias{nMeans}
\title{Calculate the means of matrix-like objects divided into equal-sized groups}
\usage{
nMeans(x, n, over = "c", fun = NULL)
}
\arguments{
\item{x}{A matrix or data frame.}

\item{n}{An integer specifying the number of columns or rows to group together.}

\item{over}{A character string indicating whether to group by columns ("c" or "col") or rows ("r" or "row").}

\item{fun}{An optional function to apply to each group. Defaults to \code{mean}.}
}
\value{
A matrix or data frame with the means of every n columns or rows.
}
\description{
Calculate the means of matrix-like objects divided into equal-sized groups
}
\details{
The function takes in a matrix or data frame \code{x} and groups the data by every \code{n} columns or rows, depending on whether \code{over} is set to "c"/"col" or "r"/"row", respectively. 

If \code{over} is set to "c"/"col", the selected columns and all rows will be used to calculate the means, while if \code{over} is set to "r"/"row", the selected rows and all columns will be used.

If \code{x} is a tibble, the resulting data will be returned as a tibble; otherwise, it will be returned as a matrix or data frame depending on the class of the input.
}
\examples{
nMeans(matrix(1:9, ncol = 3), 2, over = "col")
nMeans(data.frame(matrix(1:9, ncol = 3)), 2, over = "row")

}
