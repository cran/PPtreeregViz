% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapPP.R
\name{ppshapr.empirical}
\alias{ppshapr.empirical}
\title{Calculate \code{PPKernelSHAP} values with empirical methods}
\usage{
ppshapr.empirical(PPTreeregOBJ, testObs, final.rule, final.leaf = NULL)
}
\arguments{
\item{PPTreeregOBJ}{PPTreereg class object - a model to be explained}

\item{testObs}{test data observation}

\item{final.rule}{final rule to assign numerical values in the final nodes.
1: mean value in the final nodes
2: median value in the final nodes
3: using optimal projection
4: using all independent variables
5: using several significant independent variables}

\item{final.leaf}{location of final leaf}
}
\value{
List of empirical methods and model values
}
\description{
This function should only be called internally, and not be used as
a stand-alone function.
The original source for much of this came from 'shapr' package
code in github.com/NorskRegnesentral/shapr/blob/master/R/predictions.R
}
\details{
Below is the original license statement for 'shapr' package.

MIT License
Copyright (c) 2019 Norsk Regnesentral
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}
