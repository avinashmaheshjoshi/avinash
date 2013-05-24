/*
#==============================================================================#
#--  Project Name: Network optimization using ACO                            --#

#--  Task        : File needed by the rpcgen compiler to create the server 
                   and clien stubs.                                          --#

#--  Author      : Avinash Joshi                                             --#

#--	 Date        : 23/01/13                                                  --#

#--  Version     : 1.0                                                       --#

#--  Description : Below code is required to create the client and server 
                   stubs                                                     --#
#==============================================================================#
*/

struct input_data {
	int st_nNodes;
	float st_distMat_h<1000000>;
	float st_rho;
	float st_alpha;
	float st_beta;
	float st_Q;
	int st_nAnts;
	int st_maxIterations;
	int st_convergenceNo;
	int st_nThreads;
};

struct output_data {
	int st_bestTour_h<101>;
	float st_bestGraphLength;
	int st_iterNo;
};

typedef struct input_data input_data;
typedef struct output_data output_data;

program RPCGPUACOPROG {
    version RPCGPUACOVERS {
        output_data RPCGPUACO(input_data) = 1;
    } = 1;
} = 22855;


