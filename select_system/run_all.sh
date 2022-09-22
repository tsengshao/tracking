#!/bin/bash

NPATH=`pwd`

for exp in IMERG FV3 ARPNH MPAS NICAM IFS4KM IFS9KM ICON UM clean CMORPH; do
#for exp in ICON;do
  echo ${exp}
  ln -sf ../archive/${exp}/irt_tracklinks_output.txt .
  ln -sf ../archive/${exp}/irt_tracks_mask.dat .
  ln -sf ../tracking_code/irt_parameters.f90 .

  ./compile.sh
  ./select_system.x

  fpathout=../archive_p/${exp}
  mkdir -p ${fpathout}
  mv ./irt_tracklinks_output_select.txt ${fpathout}
  cp ./irt_tracks_mask.ctl ${fpathout}
  mv ./irt_tracks_mask_select.dat ${fpathout}
  mv ./select_tracks_output.csv ${fpathout}
  
done
mv select_tracks_info.txt ../archive_p/





