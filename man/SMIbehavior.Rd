\name{SMIbehavior}
\alias{SMIbehavior}
\alias{weight}
\docType{data}
\title{
Weight Behavior Therapy
}
\description{
A meta-analytic data set with 10 published studies and 2 unpublished studies.

A lower continuous outcome is desired. When using the function \code{forestsens()}, specify the option \code{event.is.good=FALSE}.
}
\usage{data(SMIbehavior)}
\format{
  A data frame with 12 observations on the following 10 variables.
\tabular{lll}{
	\bold{number}   \tab \code{integer}   \tab Study numeric id (optional) \cr
	\bold{study}    \tab \code{character} \tab Name of study or principal investigator \cr
	\bold{year}     \tab \code{integer}   \tab Year (optional) \cr
	\bold{outlook}  \tab \code{factor}    \tab Denotes whether a study is unpublished, and if so, what outlook it has.  \cr
	\bold{ctrl.n}  \tab \code{integer} \tab The sample size of the control arm. \cr  
	\bold{expt.n}  \tab \code{integer} \tab The sample size of the experimental arm. \cr  
	\bold{ctrl.mean}  \tab \code{numeric} \tab The mean effect within the control arm. \cr  
	\bold{expt.mean}  \tab \code{numeric} \tab The mean effect within the experimental arm. \cr
	\bold{ctrl.sd}  \tab \code{numeric} \tab The standard deviation of the outcome within the control arm. \cr  
	\bold{expt.sd}  \tab \code{numeric} \tab The standard deviation of the outcome within the experimental arm. 
	}  
}
\details{
The outlook of a study can be one of the following: \code{published}, \code{very positive}, \code{positive}, 	\code{negative}, \code{very negative}, \code{current effect}, \code{no effect}, \code{very positive CL}, \code{positive CL}, \code{negative CL}, or \code{very negative CL}.

Since the outcome event is undesired, when using the function \code{forestsens()}, specify the option \code{event.is.good=FALSE}. Since this is the default setting for \code{forestsens()}, this does not need to be specified explicitly. 
}
\examples{
library(SAMURAI)
data(SMIbehavior)
	
## To generate a forest plot without reassigning the outlooks of unpublished studies.
forestsens(SMIbehavior, binaryoutcome=FALSE, meanssd=TRUE)

## To generate a forest plot in which all unpublished studies are reassigned the outlook "current effect". 
forestsens(SMIbehavior, binaryoutcome=FALSE, meanssd=TRUE, unpub.oneoutlook="current effect")

## To generate a set of forest plots, one for each defined outlook. 
forestsens(SMIbehavior, binaryoutcome=FALSE, meanssd=TRUE, foralloutlooks=TRUE)
}
\source{
John Williams MD, Duke University School of Medicine
}
\keyword{datasets}
