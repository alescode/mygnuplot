#! /usr/bin/env bash
gnuplot $1 > "$1.ps"
ps2pdf "$1.ps"
rm "$1.ps"
