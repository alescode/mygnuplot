#! /usr/bin/env bash
archivo=${1%.*}
gnuplot $1 > "$archivo.ps"
ps2pdf "$archivo.ps"
rm "$archivo.ps"
