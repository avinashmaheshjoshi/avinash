/*
#==============================================================================#
#--  Project Name: Network optimization using ACO                            --#

#--  Task        : RPC Implementation of Ant Colony Optimization             --#
                   - Client Side Implementation

#--  Author      : Avinash Joshi                                             --#

#--	 Date        : 23/01/13                                                  --#

#--  Version     : 1.0                                                       --#
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
#include <R.h>

//================
// My Header Files
//================

#include <gpuAco.h>

//================
//Kernel functions
//================
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

__global__ void initializeMatrices( float *pherMat, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		pherMat[idx] = 1;
	}
}

__global__ void evapPherMatrix( float *a, float rho, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		a[idx] = (1-rho)*a[idx];
	}
}

__global__ void mkCpyPherMatrix( float *a, float *b, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		b[idx] = a[idx];
	}
}

__global__ void addPherMatrix( float *pherMat, int *tour, float ph, float Q, int nNodes){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx<nNodes){
		pherMat[tour[idx]*nNodes + tour[idx+1]] += Q/ph;
	}	
}

__global__ void multMatrix( float *eta, float *tau, float *prob, float alpha, float beta, int N){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N) {
		prob[idx] = pow(eta[idx],beta) * pow(tau[idx],alpha);
	}
} 

__global__ void computeProbMatrix( float *probMatrix, float sumDen, int nNodes){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < nNodes) {
		probMatrix[idx] = probMatrix[idx]/sumDen;
	}
}

__global__ void dropNode( float *probMatrix, int node, int N, int nNodes ){
	int idx = blockIdx.x * blockDim.x + threadIdx.x;
	if(idx < N){
		if( idx % nNodes == node || idx / nNodes == node){
			probMatrix[idx] = 0;
		}
	}
}


void gpuAco( int *pt_nNodes, float *distMat_h, int *bestTour_h, float *pt_bestGraphLength, float *pt_rho, float *pt_alpha, float *pt_beta, float *pt_Q, int *pt_nAnts, int *pt_maxIterations, int *pt_convergenceNo, int *pt_nThreads, float *pt_elapsedTime, int *pt_iterNo ){

	cudaEvent_t tic, toc;
	float elapsedTime;

	cudaEventCreate(&tic);
	cudaEventCreate(&toc);
	cudaEventRecord(tic,0);

//================================
//  Filling up the input arguments
//================================
	int	nNodes = pt_nNodes[0], nAnts = pt_nAnts[0], maxIterations = pt_maxIterations[0];
	int  convergenceNo = pt_convergenceNo[0];
	float rho = pt_rho[0], alpha = pt_alpha[0], beta = pt_beta[0], Q = pt_Q[0];
	
//======================
//  Variable Declaration
//======================
	//On Host
	float *probMat_h, rowSum,  mover, graphLength = 0, bestGraphLength = 1e5, bestPrevGraphLength = 0;

	int *fullNodeList, *tour_h, N = nNodes*nNodes;

	//On device
	float *probMat_d, *pherMat_d, *curr_pherMat_d, *distMat_d;
	
	int *tour_d;
	
	int iterNo = 0, starter, counter=0;
	
//===========================
//	Memory Allocation on host
//===========================
	probMat_h = (float *)malloc(N*sizeof(float));
	fullNodeList = (int *)malloc(nNodes*sizeof(int));
	tour_h = (int *)malloc((nNodes + 1)*sizeof(int));

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
	for( int i = 0; i < nNodes; i++) fullNodeList[i] = i;

//=================================
//Sending required arrays to device
//=================================

	cudaMemcpy( distMat_d, distMat_h, N*sizeof(float), cudaMemcpyHostToDevice);
	
//=============================
// Threads & Blocks calculation
//=============================

	int nThreads = pt_nThreads[0];
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

	pt_bestGraphLength[0] = bestGraphLength;	
	pt_elapsedTime[0] = elapsedTime/1000;
	pt_iterNo[0] = iterNo;

//============================
//Clearing up allocated memory
//============================

	free(probMat_h);
	free(fullNodeList);
	free(tour_h);

	cudaFree(probMat_d);
	cudaFree(pherMat_d);
	cudaFree(curr_pherMat_d);
	cudaFree(distMat_d);
	cudaFree(tour_d);
} 