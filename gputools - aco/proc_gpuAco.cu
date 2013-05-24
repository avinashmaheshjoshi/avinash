/*
#==============================================================================#
#--  Project Name: Network optimization using ACO                            --#

#--  Task        : RPC Implementation of Ant Colony Optimization             --#
                   - Server Side Implementation

#--  Author      : Avinash Joshi                                             --#

#--	 Date        : 23/01/13                                                  --#

#--  Version     : 1.0                                                       --#

#--  Description : Below code is the server part of the code for a remote 
                   procedure call of aco. This code needs to be compiled and 
				   executed before calling it on the client.                   
				   Also ensure that the rpcgen generated files 
				   (from remotegpuAco.x) remotegpuAco_xdr.c and 
				   remotegpuAco_svc.c are linked while compiling this code   --#
#==============================================================================#
*/

//==============
//  Header Files
//==============

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <cuda.h>
#include <math.h>
#include <cublas.h>
#include <curand.h>
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/scan.h>
#include <rpc/rpc.h>

#include "remotegpuAco.h"


//================
//Kernel functions
//================

//Takes the inverse of the distance 
__global__ void modifyDistMatrix( float *distMatrix, int N, int nNodes){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		if(idx % nNodes == idx / nNodes){
			distMatrix[idx] = 0;		
		}
		else {
			distMatrix[idx] = 1/distMatrix[idx];
		}
	}
}

//Initialize all the pheromones to 1
__global__ void initializeMatrices( float *pherMat, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		pherMat[idx] = 1;
	}
}

//Evaporates pheromones 
__global__ void evapPherMatrix( float *a, float rho, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		a[idx] = (1-rho)*a[idx];
	}
}

//Make a copy so that each ant gets the same pheromone matrix
__global__ void mkCpyPherMatrix( float *a, float *b, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		b[idx] = a[idx];
	}
}

//Add pheromones on the visited arcs
__global__ void addPherMatrix( float *pherMat, int *tour, float ph, float Q, int nNodes){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx<nNodes){
		pherMat[tour[idx]*nNodes + tour[idx+1]] += Q/ph;
	}	
}

//Calculate score all the arcs
__global__ void multMatrix( float *eta, float *tau, float *prob, float alpha, float beta, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		prob[idx] = pow(eta[idx],beta) * pow(tau[idx],alpha);
	}
} 

//Calculate the probabilities for a move from a given node to all the connected to it
__global__ void computeProbMatrix( float *probMatrix, float sumDen, int nNodes){//Calculate probability
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < nNodes) {
		probMatrix[idx] = probMatrix[idx]/sumDen;
	}
}

//Drop the already visited nodes by making the probability 0
__global__ void dropNode( float *probMatrix, int node, int N, int nNodes ){//Drop node
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N){
		if( idx % nNodes == node || idx / nNodes == node){
			probMatrix[idx] = 0;
		}
	}
}

//==========================
//  Start of the server code
//==========================

output_data * rpcgpuaco_1(input_data *input, CLIENT *client){
	
	static output_data result;
	cudaEvent_t tic, toc;
	float elapsedTime;

	cudaEventCreate(&tic);
	cudaEventCreate(&toc);
	cudaEventRecord(tic,0);
	
//================================
//  Filling up the input arguments
//================================
	int	nNodes = input->st_nNodes, nAnts = input->st_nAnts, maxIterations = input->st_maxIterations;
	int  convergenceNo = input->st_convergenceNo;
	float rho = input->st_rho, alpha = input->st_alpha, beta = input->st_beta, Q = input->st_Q;
 
	
//======================
//  Variable Declaration
//======================
	//On Host
	float *distMat_h, *probMat_h, rowSum,  mover, graphLength = 0, bestGraphLength = 1e5, bestPrevGraphLength = 0;

	int *fullNodeList, *tour_h, *bestTour_h, N = nNodes*nNodes;

	//On device
	float *probMat_d, *pherMat_d, *curr_pherMat_d, *distMat_d;
	
	int *tour_d;
	
	int iterNo = 0, starter, counter=0;

	if(N != input->st_distMat_h.st_distMat_h_len){
		printf("Number of elements in the array is not equal to square of number nodes");
		exit(1);
	}
	
//===========================
//	Memory Allocation on host
//===========================
	distMat_h = (float *)malloc(N*sizeof(float));
	probMat_h = (float *)malloc(N*sizeof(float));
	fullNodeList = (int *)malloc(nNodes*sizeof(int));
	tour_h = (int *)malloc((nNodes + 1)*sizeof(int));
	bestTour_h = (int *)malloc((nNodes + 1)*sizeof(int));	

//=============================
//	Memory Allocation on device
//=============================		
	cudaMalloc( (void **)&probMat_d, N*sizeof(float));
	cudaMalloc( (void **)&pherMat_d, N*sizeof(float));
	cudaMalloc( (void **)&curr_pherMat_d, N*sizeof(float));		
	cudaMalloc( (void **)&distMat_d, N*sizeof(float));
	cudaMalloc( (void **)&tour_d, (nNodes + 1)*sizeof(int)); 	
	
//=================
//Filling up Arrays
//=================

/*	FILE *fp;
	float ina;
	fp = fopen("input.txt","r");
	int l = 0;
	while(fscanf(fp,"%f",&ina) != EOF){
		distMat_h[l] = ina;
		l++;
	}
	fclose(fp);
*/
//	for( int i = 0; i < N; i++) distMat_h[i] = i;
	distMat_h = input->st_distMat_h.st_distMat_h_val;
	for( int i = 0; i < nNodes; i++) fullNodeList[i] = i;

//=================================
//Sending required arrays to device
//=================================

	cudaMemcpy( distMat_d, distMat_h, N*sizeof(float), cudaMemcpyHostToDevice);
	
//=============================
// Threads & Blocks calculation
//=============================

	int nThreads = input->st_nThreads;
	int nBlocks   = N / nThreads + ( N % nThreads == 0 ? 0 : 1 );
	
//========================
//	Start of ACO algorithm
//========================

	initializeMatrices <<< nBlocks, nThreads >>> (pherMat_d, N); // Initialize pheromone matrix to 1
	
	modifyDistMatrix <<< nBlocks, nThreads >>> (distMat_d, N, nNodes); // Invert the distances as minimization problem
	
	srand(time(NULL));
	
	while(iterNo <= maxIterations && counter <= convergenceNo){ //Iteration loop

		starter = ((float)(rand()/ (float)RAND_MAX)*nNodes);
		tour_h[0] = tour_h[nNodes] =  fullNodeList[starter];
		mkCpyPherMatrix <<< nBlocks, nThreads>>> (pherMat_d, curr_pherMat_d, N);
		
		for(int ant = 0; ant < nAnts; ant++){//Each ant loop

			multMatrix <<< nBlocks, nThreads >>> (distMat_d, curr_pherMat_d, probMat_d, alpha, beta, N);

			for(int i = 0; i < (nNodes - 1); i++){ //Graph construction loop
				
				rowSum = cublasSasum(nNodes, probMat_d + nNodes*tour_h[i], 1);

				if(rowSum == 0) rowSum = 1;
				
				computeProbMatrix <<< nBlocks, nThreads >>> (probMat_d + nNodes*tour_h[i], rowSum, nNodes);

/*				for(int j = nNodes*tour_h[i]; j < nNodes*(tour_h[i] + 1); j++){//cumulative sum of the prob matrix for the corresponding row

					probMat_h[j] = cublasSasum((j-nNodes*tour_h[i]) + 1, probMat_d + nNodes*tour_h[i], 1);
				}
*/
				cudaMemcpy(probMat_h + nNodes*tour_h[i], probMat_d + nNodes*tour_h[i], nNodes*sizeof(float), cudaMemcpyDeviceToHost);

				thrust::inclusive_scan(probMat_h + nNodes*tour_h[i], probMat_h + nNodes*(tour_h[i] + 1), probMat_h + nNodes*tour_h[i]);
				
				mover = (float)(rand()/ (float)RAND_MAX);

				for(int j = tour_h[i]*nNodes; j < (tour_h[i]*nNodes+nNodes); j++){//Next node selection
					
					if(mover < probMat_h[j]){
						tour_h[i + 1] = fullNodeList[j - (tour_h[i]*nNodes)];
						break;
					}
				}

				dropNode <<< nBlocks, nThreads >>> (probMat_d,tour_h[i], N, nNodes);
				graphLength += distMat_h[nNodes*tour_h[i] + tour_h[i + 1]];
			}//Graph construction loop

			graphLength += distMat_h[nNodes*tour_h[nNodes - 1] + tour_h[nNodes]];

			//Pheromone update
			evapPherMatrix <<< nBlocks, nThreads >>> (pherMat_d, rho, N); //Evaporation
			cudaMemcpy( tour_d, tour_h, (nNodes + 1)*sizeof(int), cudaMemcpyHostToDevice);
			addPherMatrix <<< nBlocks, nThreads >>> (pherMat_d, tour_d, graphLength, Q, nNodes);

			if(graphLength < bestGraphLength){
				bestGraphLength = graphLength;
				for(int k = 0; k < (nNodes + 1); k++) bestTour_h[k] = tour_h[k];
			}
			
		}//Each ant loop
		
		if(bestGraphLength == bestPrevGraphLength){
				counter = counter + 1;
		}
		else{
			bestPrevGraphLength = bestGraphLength;
			counter = 0;
		}

		graphLength = 0;
		iterNo++;
	}//Iteration Loop
			
	cudaEventRecord(toc,0);
    cudaEventElapsedTime(&elapsedTime,tic,toc);
	
//============================
//Filling the output variables
//============================

	result.st_bestTour_h.st_bestTour_h_len = nNodes + 1;
	result.st_bestTour_h.st_bestTour_h_val = bestTour_h;
	result.st_bestGraphLength = bestGraphLength;	
	result.st_elapsedTime = elapsedTime/1000;
	result.st_iterNo = iterNo;

	return(&result);

//============================
//Clearing up allocated memory
//============================

	free(distMat_h);	
	free(probMat_h);
	free(fullNodeList);
	free(tour_h);
	free(bestTour_h);
	
	cudaFree(probMat_d);
	cudaFree(pherMat_d);
	cudaFree(curr_pherMat_d);
	cudaFree(distMat_d);
	cudaFree(tour_d);
}

output_data * rpcgpuaco_1_svc(input_data *input, struct svc_req *){
	CLIENT *client;
	return(rpcgpuaco_1(input, client));
} 
