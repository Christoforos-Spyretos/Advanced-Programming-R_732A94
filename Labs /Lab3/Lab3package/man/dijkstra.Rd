% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dijkstra.R
\name{dijkstra}
\alias{dijkstra}
\title{Dijkstra Algorithm}
\usage{
dijkstra(graph, init_node)
}
\arguments{
\item{graph}{A DataFrame.}

\item{init_node}{An Int.}
}
\value{
Returns the shortest path to every other node from the starting node as a vector.
}
\description{
Implemantation of Dijkstra's Algorithm in R.
}
\examples{
dijkstra(graph = wiki_graph,init_node = 1)
}
\references{
\href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Wikipedia - Dijkstra's algorithm}
}
\author{
Marketos Damgios, Chrystoforos Spyretos
}
