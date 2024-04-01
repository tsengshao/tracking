#!/bin/bash
#SBATCH --job-name=dytrack          # Specify job name
#SBATCH --partition=compute     # Specify partition name
#SBATCH --ntasks=1             # Specify max. number of tasks to be invoked
#SBATCH --mem=0              # Specify amount of memory needed
#SBATCH --cpus-per-task=3      # Specify number of CPUs per task
#SBATCH --time=08:00:00        # Set a limit on the total run time
#SBATCH --account=bb1153       # Charge resources on this project account
#SBATCH --output=dytrack.o%j    # File name for standard output


NPATH=`pwd`
outfolder=../../archive_tropics
if [ ! -d ${outfolder} ]; then mkdir -p ${outfolder}; fi
cp ./irt_tracks_mask_all.ctl ${outfolder}
cp ./select_system.f90 ${outfolder}

for exp in IMERG FV3 ARPNH MPAS NICAM IFS4KM IFS9KM ICON UM clean CMORPH CWBGFS; do
  echo ${exp}
  ln -sf ../../archive/${exp}/irt_tracklinks_output.txt .
  ln -sf ../../archive/${exp}/irt_tracks_mask.dat .
  #ln -sf ../../tracking_code/irt_parameters.f90 .
  ln -sf ../../archive/irt_parameters.f90 .

  ./compile.sh
  ./select_system.x

  fpathout=${outfolder}/${exp}
  mkdir -p ${fpathout}
  mv ./irt_tracklinks_output_select.txt ${fpathout}
  mv ./irt_tracks_mask_select.dat ${fpathout}
  mv ./select_tracks_output.csv ${fpathout}
  mv select_tracks_info.txt ${outfolder}
  rm -f irt_parameters.f90 irt_parameters.mod irt_tracklinks_output.txt irt_tracks_mask.dat
  
done






