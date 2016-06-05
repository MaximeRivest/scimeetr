\name{scimeetr-package}
\alias{scimeetr-package}
\alias{scimeetr}
\docType{package}
\title{
\packageTitle{scimeetr}
}
\description{
\packageDescription{scimeetr}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{scimeetr}
\packageIndices{scimeetr}
This package does the following...
}
\author{
\packageAuthor{scimeetr}

Maintainer: \packageMaintainer{scimeetr}
}
\references{
I will cite sebastian grauwin
}
\keyword{ package }
\seealso{
\code{\link{import_wos_files}}
}
\examples{
\dontrun{Since this exemple shows how to load WOS from your system we need to run the following line to find the path to the exemple file}
fpath <- system.file("extdata", package="scimeetr")
fpath <- paste(fpath, "/wos_folder/", sep = "")
\dontrun{Then we can run the actual example}
wos_df <- import_wos_files(files_directory = fpath)
}
