/*
#==============================================================================#
#--  Project Name: Network optimization using ACO                            --#

#--  Task        : RPC Implementation of Ant Colony Optimization             --#
                   - Client Side Implementation

#--  Author      : Avinash Joshi                                             --#

#--	 Date        : 23/01/13                                                  --#

#--  Version     : 1.0                                                       --#

#--  Description : Below code is the client part of the code for a remote 
                   procedure call of aco. Ensure that the server whose ip 
                   address is going to be passed to the below function is 
                   running the required process                              --#
#==============================================================================#
*/

//==============
//  Header Files
//==============

#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>
#include <cuda.h>
#include <math.h>
#include <cublas.h>
#include <curand.h>
#include <R.h>

//================
// My Header Files
//================

#include "rpcGpuAco.h"
#include "remotegpuAco.h"

//==========================
//  Start of the client code
//==========================


void rpcGpuAco( char **pt_server, int *pt_nNodes, float *distMat_h, int *bestTour_h, float *pt_bestGraphLength, float *pt_rho, float *pt_alpha, float *pt_beta, float *pt_Q, int *pt_nAnts, int *pt_maxIterations, int *pt_convergenceNo, int *pt_nThreads, int *pt_iterNo ){

//======================
//  Variable Declaration
//======================

	CLIENT *clnt;

	input_data  input_aco;

	output_data *result_aco;

//================================
//  Filling up the input arguments
//================================

	char *server = pt_server[0]; //server ip address or host name

	input_aco.st_nNodes = pt_nNodes[0];
	input_aco.st_rho = pt_rho[0];
	input_aco.st_alpha = pt_alpha[0];
	input_aco.st_beta = pt_beta[0];
	input_aco.st_Q = pt_Q[0];
	input_aco.st_nAnts = pt_nAnts[0];
	input_aco.st_maxIterations = pt_maxIterations[0];
	input_aco.st_convergenceNo = pt_convergenceNo[0];
	input_aco.st_nThreads = pt_nThreads[0];
   	
//=========================================================
//	Memory Allocation and filling the distance matrix array
//=========================================================

	input_aco.st_distMat_h.st_distMat_h_len = pt_nNodes[0]*pt_nNodes[0];
	input_aco.st_distMat_h.st_distMat_h_val = (float *) malloc(input_aco.st_distMat_h.st_distMat_h_len*sizeof(float));
	input_aco.st_distMat_h.st_distMat_h_val = distMat_h;	
/*
//===================
//	Filling up Arrays
//===================
	
	FILE *fp;
	float ina;
	fp = fopen("dataACO.txt","r");
	int l = 0;
	while(fscanf(fp,"%f",&ina) != EOF){
		input_aco.st_distMat_h.st_distMat_h_val[l] = ina;
		printf("%f\n",input_aco.st_distMat_h.st_distMat_h_val[l]);
		l++;
	}
	fclose(fp);
*/	
	
    printf("%s\n",server);
	clnt = clnt_create(server, RPCGPUACOPROG, RPCGPUACOVERS, "tcp");
   
	if (clnt == NULL) {
		clnt_pcreateerror(server);
		exit(1);
	}

	printf("Calling remote procedure \n");
	printf("Sending input arguments to the server\n");
	result_aco = rpcgpuaco_1(&input_aco,clnt);
	printf("Call Successful\n"); 
//============================
//	Filling the output variables
//============================

	pt_bestGraphLength[0] = result_aco->st_bestGraphLength;	
	pt_iterNo[0] = result_aco->st_iterNo;

	for(int i=0;i<pt_nNodes[0]*pt_nNodes[0];i++){ 
		bestTour_h[i] = result_aco->st_bestTour_h.st_bestTour_h_val[i];
	}
//	bestTour_h = result_aco->st_bestTour_h.st_bestTour_h_val;
//	for(int l=0;l<=pt_nNodes[0];l++) printf("%d\n",bestTour_h[l]);
//=================
//	Clearing memory
//=================

	clnt_destroy( clnt );
   
}
