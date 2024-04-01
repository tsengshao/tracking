#!/bin/bash
#SBATCH --job-name=seletrack          # Specify job name
#SBATCH --partition=shared     # Specify partition name
#SBATCH --ntasks=1             # Specify max. number of tasks to be invoked
#SBATCH --mem=10G              # Specify amount of memory needed
#SBATCH --cpus-per-task=3      # Specify number of CPUs per task
#SBATCH --time=08:00:00        # Set a limit on the total run time
#SBATCH --account=bb1153       # Charge resources on this project account
#SBATCH --output=select.o%j    # File name for standard output


COMPILE_COMMAND='ifort -no-wrap-margin -mcmodel=large -check bounds -debug all -traceback -g -shared-intel -free -heap-arrays 10'


NPATH=`pwd`
datafolder=../tracking_data
datafolder=../select_data
mkdir -p ${datafolder}

# get date from previous ctl
tmp=$(cat ../tracking_data/irt_tracks_mask.ctl |grep tdef)
i=0
echo ${tmp}
for ddmmyyyy in ${tmp};do
  i=$((${i}+1))
  if [ "${i}" -eq "4" ];then
    break
  fi
done
echo ${ddmmyyyy}


# copy select code ( remind the selected condiction )
cp ./select_system.f90 ${datafolder}

#ln -sf ../tracking_data/irt_tracklinks_output.txt .
#ln -sf ../tracking_data/irt_tracks_mask.dat .
#ln -sf ../tracking_code/irt_parameters.f90 .

${COMPILE_COMMAND} -o select_system.x irt_parameters.f90 select_system.f90
./select_system.x
sed -i "s/OOXXTIMEXXOO/${ddmmyyyy}/" ./irt_tracks_mask_select.ctl
mv ./irt_tracks_mask_select.ctl ${datafolder}

${COMPILE_COMMAND} -o lifemask.x irt_parameters.f90 new_parameter.f90 lifemask.f90
./lifemask.x
sed -i "s/OOXXTIMEXXOO/${ddmmyyyy}/" ./lifemask.ctl
mv ./lifemask.ctl ${datafolder}

mv ./irt_tracklinks_output_select.txt ${datafolder}
mv ./irt_tracks_mask_select.dat ${datafolder}
mv ./select_tracks_output.csv ${datafolder}
mv ./select_tracks_info.txt ${datafolder}
mv ./lifemask.dat ${datafolder}

rm -rf new_parameter.mod new_parameter.f90
rm -f irt_parameters.f90 irt_parameters.mod irt_tracklinks_output.txt irt_tracks_mask.dat
  






