% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipca.R
\name{ipca_mono}
\alias{ipca_mono}
\title{interface to incremental PCA from sklearn: mono implies monolithic data handling}
\usage{
ipca_mono(x, n_comp = 2L, batch_size = 25L, ...)
}
\arguments{
\item{x}{either character string to HDF5 matrix or R matrix}

\item{n_comp}{integer number of PC to compute}

\item{batch_size}{}

\item{\dots}{not used}
}
\description{
interface to incremental PCA from sklearn: mono implies monolithic data handling
}
\examples{
d = data.matrix(iris[,1:4])
p = prcomp(d)
dim(p$x)
h5targ = tempfile()
require(rhdf5)
h5createFile(h5targ)
h5write(d, h5targ, "iris")
tst = ipca_mono(h5targ, n_comp=4L)
tstm = tst@fit_transform
colnames(tstm) = paste0("ip", 1:4)
pairs(cbind(p$x, tstm))
}
