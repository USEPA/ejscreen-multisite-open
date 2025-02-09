% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getblocksnearby2-DRAFT.R
\name{getblocksnearby2}
\alias{getblocksnearby2}
\title{DRAFT / WAS WORK IN PROGRESS  Key buffering function - wrapper redirecting to the right version of getblocksnearby()}
\usage{
getblocksnearby2(
  sitepoints,
  radius = 3,
  maxradius = 31.07,
  avoidorphans = FALSE,
  quadtree = is.null,
  ...
)
}
\arguments{
\item{sitepoints}{see \code{\link[=getblocksnearbyviaQuadTree]{getblocksnearbyviaQuadTree()}} or other such functions}

\item{radius}{see \code{\link[=getblocksnearbyviaQuadTree]{getblocksnearbyviaQuadTree()}} or other such functions}

\item{maxradius}{see \code{\link[=getblocksnearbyviaQuadTree]{getblocksnearbyviaQuadTree()}} or other such functions}

\item{avoidorphans}{see \code{\link[=getblocksnearbyviaQuadTree]{getblocksnearbyviaQuadTree()}} or other such functions}

\item{quadtree}{a large quadtree object created from the SearchTree package example:
SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")}

\item{...}{see \code{\link[=getblocksnearbyviaQuadTree_Clustered]{getblocksnearbyviaQuadTree_Clustered()}} or other such functions}
}
\description{
DRAFT / WAS WORK IN PROGRESS  Key buffering function - wrapper redirecting to the right version of getblocksnearby()
}
\details{
For all examples, see \code{\link[=ejamit]{ejamit()}}

Like getblocksnearby() but tries to handle localtree and quadtree parameter differently
\itemize{
\item not sure how to check if they are in the right environment.
}
}
\seealso{
\code{\link[=getblocksnearby]{getblocksnearby()}}
}
\keyword{internal}
