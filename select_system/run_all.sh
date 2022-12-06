#!/bin/bash

NPATH=`pwd`
outfolder=../archive_Sea
if [ ! -d ${outfolder} ]; then mkdir -p ${outfolder}; fi
cp ./irt_tracks_mask_all.ctl ${outfolder}
cp ./select_system.f90 ${outfolder}

for exp in IMERG FV3 ARPNH MPAS NICAM IFS4KM IFS9KM ICON UM clean CMORPH; do
#for exp in ICON;do
  echo ${exp}
  ln -sf ../archive/${exp}/irt_tracklinks_output.txt .
  ln -sf ../archive/${exp}/irt_tracks_mask.dat .
  ln -sf ../tracking_code/irt_parameters.f90 .

  ./compile.sh
  ./select_system.x

  fpathout=${outfolder}/${exp}
  mkdir -p ${fpathout}
  mv ./irt_tracklinks_output_select.txt ${fpathout}
  mv ./irt_tracks_mask_select.dat ${fpathout}
  mv ./select_tracks_output.csv ${fpathout}
  rm -f irt_parameters.f90 irt_parameters.mod irt_tracklinks_output.txt irt_tracks_mask.dat
  
done
mv select_tracks_info.txt ${outfolder}






