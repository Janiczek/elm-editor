#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

function run {
  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  ./compile.sh;
}

#echo '' >events.txt
run;

inotifywait -mqr -e close_write --format '%w %e %f' ./src | while read DIR EVENT FILE; do
  #echo "dir ${DIR} event ${EVENT} file ${FILE}" >>events.txt # debugging
  run;
done;
