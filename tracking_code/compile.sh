#!/bin/bash
set -ex

# netcdf configure flag
# ifort -o ooo.x ooo.f $(nc-config --fflags --flibs)

#https://docs.dkrz.de/doc/levante/code-development/compiling-and-linking.html#how-to-build-software-with-netcdf
wlflag="-Wl,-rpath,/sw/spack-levante/netcdf-fortran-4.5.3-r5r3ev/lib"


export I_MPI_PMI=pmi
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

echo $(date +"%Y-%m-%d %H:%M:%S")

COMPILE_COMMAND="mpiifort -free -no-wrap-margin -mcmodel=large -check bounds -debug all -traceback -g -shared-intel -free -heap-arrays 10 $(nc-config --fflags --flibs) ${wlflag}"
#COMPILE_COMMAND='ifort -no-wrap-margin -mcmodel=large -C -debug all -traceback -shared-intel -free -heap-arrays 10'
#COMPILE_COMMAND='ifort -no-wrap-margin -mcmodel=large -debug all -traceback -shared-intel -free -heap-arrays 10'
#COMPILE_COMMAND='gfortran -g -C -O0 -mcmodel=large'

# clean:
set +e
rm irt_parameters.mod
rm irt_objects_release.x
rm irt_advection_field_release.x
rm irt_tracks_release.x
rm irt_trackmask_release.x
rm irt_tracklinks_release.x
set -ex

# compile:
${COMPILE_COMMAND} -o irt_objects_release.x irt_parameters.f90 irt_tools.f90 irt_objects_release.f90 
${COMPILE_COMMAND} -o irt_advection_field_release.x irt_advection_field_release.f90 irt_parameters.f90
${COMPILE_COMMAND} -o irt_tracks_release.x irt_tracks_release.f90 irt_parameters.f90
${COMPILE_COMMAND} -o irt_trackmask_release.x irt_trackmask_release.f90 irt_parameters.f90
#${COMPILE_COMMAND} -o irt_agemask_release.x irt_agemask_release.f90 irt_parameters.f90
#${COMPILE_COMMAND} -o irt_tracklinks_release.x irt_tracklinks_release.f90 irt_parameters.f90
${COMPILE_COMMAND} -o irt_tracklinks_release.x irt_parameters.f90 irt_tracklinks_release_shao.f90

exit
