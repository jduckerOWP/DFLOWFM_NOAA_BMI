# DFLOWFM_NOAA_BMI
DFLOWFM source code modified for BMI compliancy as well as repository for DFLOWFM BMI

Author: Jason Ducker, Lynker Technologies LLC., Affiliate for Office of Water Prediction at NOAA

# Requirements for compiling DFLOWFM and BMI libraries, currently setup for the RDHPCS NOAA Hera cluster
# to be correctly compiled and linked to BMI shared libraries
1. cmake >= 3.20
2. szip >= 2.1
3. intel >= 2022.3.0
4. impi >= 2022.3.0
5. netcdf >= 4.7.0
6. proj >= 7.1.0
7. gdal
8. petsc >= 3.19
9. metis >= 5.1

# Steps below to compile DFlowFM model source code
1. Unpack DFlowFM source code. Go into directory and open the build.sh file. Go to lines 190-192 and change the following syntax to compile the DFLOWFM model engine source code only: config="dflowfm", generator="Unix Makefiles", compiler="intel21"
2. Go into "src" directory, and edit the "setenv.sh" file to essentially load and link your library paths that are required to compile DFlowFM source code as stated above. *** The current setenv.sh file is already set for the RDHPCS NOAA Hera cluster, so if you're compiling it on there you dont need to change it ***
3. Go back into the top directory and execute "./build.sh" to initiate the DFlowFM model to use cmake and build the entire model engine source code from scratch. If successful, you should see that all DFLOWFM model engine modules have been built in the newly created "build_dflowfm" directory. Go into the "build_dflowfm/install" directory and verify that you see executables built within the "bin" directory and shared libraries built within the "lib" directory. If so, then you should be good to go for the next step. If not, consult with the build issues and adjust libraries accordingly.
4. 
