Agency for MINT Proto

- The latest code on test is mint_main_fn_v6.R

- Correlation are calculated on the raw prices

- Distance metric used for MST is 1-abs(rho)

Please use only the above file to run on test server.

Old Release Notes:

mint_main_fn_v5.R: In this file,

* Correlation are calculated on the raw prices 

* Distance metric used for MST is 1-abs(rho)

* Changes made from previous version : 
  - Sends both 2D and 3D Sammons Projections
  - Changed the combination of sammons and FD objects

* Changes to be made for the next version :
  -This version is same as the next version except for correlations being calculated on log returns of raw prices. 

mint_main_fn_v4.R: In this file,

* Correlation are calculated on the log returns of the prices 

* Distance metric used for MST is 1-abs(rho)

* Changes made from previous version : 
  - Distance metric changed 
  - Code has been optimized and refactored

* Changes to be made for the next version :
  -This version had to be changed as this file sends only the 3D Sammons Projection

1) mint_main_fn_v3.R uses the distance metric 1/abs(rho)

3) The reason for the change in name of get_cor_graph_v2.R is because for middletier, the main function name(which will be called by jade) and the file name need to be same.
It was initially called get_cor_graph and now it has been changed to mint_main_fn hence the change in name of the file. The versioning has been maintained.

 

