\name{gpuAco}
\alias{gpuAco}
\title{Run ant colony optimization on a network of nodes using a gpu}

\description{
	The ant colony optimization will be run on a network of nodes. There are 2 ways of using this function. If the first argument server is set to NULL, the function assumes that the local system has the gpu card and accordingly run the local code. If the gpu card is on a remote machine, ensure that the aco process is running on that machine and pass the ip address as the first argument.
	The input to the function needs to be an adjacency matrix filled with a cost/distance metric for pairwise nodes. This will be converted into a linear array filled in row major format before 	feeding to the gpu code. The function works for both directed and undirected graph. The output will be the optimum route and the length of the route. 
}

\usage{
gpuAco(distMat_h,rho=0.01,alpha=10,beta=7,Q=2,nAnts=5,maxIterations=1000,convergenceNo=200,nThreads = 512,server=NULL)  
}

\arguments{	
	\item{adjMatrix}{An adjacency matrix of floating point values. It should be a square matrix(or the code gives an error) and the diagonal values should be 0 if not the function sets it to 0}
	\item{rho}{Pheromone evaopration rate. A value between 0 and 1 has to be given. If not given, default value is 0.01}
	\item{alpha}{Input parameter to aco. It controls the influence of pheromones in the route construction. Default value is 10}
	\item{beta}{Input parameter to aco. It controls the influence of heuristic information in the route construction. Default value is 7}
	\item{Q}{Input parameter to aco. It controls the amount of pheromones to be deposited after each iteration. Default value is 2}
	\item{nAnts}{Number of ants to be used. Default value is 5}
	\item{maxIterations}{Maximum iterations to be run before termination. Default value is 10000}
	\item{convergenceNo}{Convergence criteria for the code i.e if the best solution remains same for this number of iterations, the code stops running and the current best solution is returned. Default value is 500}
	\item{nThreads}{Number of threads per block to be fired in the GPU. Default value is 512}
	\item{server}{A string giving the ip address of the server where the aco procedure is running. By default the value is null which means the algorithm will run the local code.}
}

\value{
	The output will be a list. The first element of the list is a  vector of integers which give the optimum route. It assumes the nodes are enumerated 1,2,3,4....so on where 1 would be the node corresponding to the first row of the adjacency matrix.
	The second element is the length of the optimum route. 
	The third element is the time taken by the gpu code. 
	The fourth element is the number of iterations the code ran before it terminated.
}

\examples{
	mat <-matrix(runif(25),5,5) #Generate a square matrix 
	optima <- gpuAco(mat) #Running aco using default arguments
	optma$Optimum_Route	 #Prints the optimum route

	optima1 <- gpuAco(mat,rho=0.05,alpha=4,beta=3,nAnts = 10) #Running aco with changed parameters

}


