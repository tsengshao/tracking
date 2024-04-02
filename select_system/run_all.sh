#!/bin/bash
#SBATCH --job-name=dytrack          # Specify job name
#SBATCH --partition=shared     # Specify partition name
#SBATCH --ntasks=3             # Specify max. number of tasks to be invoked
#SBATCH --mem=15G              # Specify amount of memory needed
#SBATCH --time=12:00:00        # Set a limit on the total run time
#SBATCH --account=bb1153       # Charge resources on this project account
#SBATCH --output=dytrack.o%j    # File name for standard output

COMPILE_COMMAND='ifort -no-wrap-margin -mcmodel=large -check bounds -debug all -traceback -g -shared-intel -free -heap-arrays 10'

NPATH=`pwd`
outfolder=../../archive_tropics
if [ ! -d ${outfolder} ]; then mkdir -p ${outfolder}; fi
cp ./select_system.f90 ${outfolder}

#for exp in IMERG FV3 ARPNH MPAS NICAM IFS4KM IFS9KM ICON UM clean CMORPH CWBGFS; do
# for exp in imerg;do
for exp in fv3 arpnh mpas nicam ifs4km ifs9km icon um cwbgfs cmorph;do

  echo ${exp}
  ln -sf ../../archive/${exp}/irt_tracklinks_output.txt .
  ln -sf ../../archive/${exp}/irt_tracks_mask.dat .
  ln -sf ../../archive/${exp}/irt_objects_input_00 .
  ln -sf ../../archive/irt_parameters.f90 .

  
  # get date from previous ctl
  tmp=$(cat ../tracking_data/irt_tracks_mask.ctl |grep tdef)
  i=0
  for ddmmyyyy in ${tmp};do
    i=$((${i}+1))
    if [ "${i}" -eq "4" ];then
      break
    fi
  done
  echo ${ddmmyyyy}

  # start to select systems, subdomain of the input and the lifemask
  ${COMPILE_COMMAND} -o select_system.x irt_parameters.f90 select_system.f90
  ./select_system.x
  ${COMPILE_COMMAND} -o lifemask.x irt_parameters.f90 new_parameter.f90 lifemask.f90
  ./lifemask.x


  fpathout=${outfolder}/${exp}
  mkdir -p ${fpathout}
  mv ./irt_tracklinks_output_select.txt ${fpathout}
  mv ./select_tracks_output.csv ${fpathout}
  for fctl in ./irt_tracks_mask_select ./subdomain_input ./lifemask;do
    sed -i "s/OOXXTIMEXXOO/${ddmmyyyy}/" ${fctl}.ctl
    mv ${fctl}.ctl ${fpathout}
    mv ${fctl}.dat ${fpathout}
  done

  mv select_tracks_info.txt ${outfolder}

  rm -f irt_parameters.f90 irt_parameters.mod
  rm -f irt_tracklinks_output.txt irt_tracks_mask.dat irt_objects_input_00
  rm -f new_parameter.f90 new_parameter.mod
  
done






