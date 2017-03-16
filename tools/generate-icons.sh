#!/bin/bash

# How to include in Eclipse
# Go to project "properties -> Builders -> New -> Program".
# Go to main tab
# Set the following fields:
# Name: Generate icons
# Location: ${workspace_loc:/LODDS/tools/generate-icons.sh}
# Go to refresh tab
# Check "refresh resources upon completion" 
# Press OK


# make it work when it is called through eclipse builder
export PATH=/usr/local/bin:$PATH
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/ 
svg_dir=${script_dir}../res/
java_res_dir=${script_dir}../java-code/src/studyproject/resources/

# Convert lodds icon in various solution
svg=${svg_dir}lodds.svg
lodds_prefix=${java_res_dir}lodds_icon

resolutions=(16x16 32x32 64x64)
for i in "${resolutions[@]}"
do
: 
   convert -background transparent -resize $i $svg ${lodds_prefix}${i}.png
done

# Convert other svg files
svgs=(file folder-closed folder-open reload send-file shared user x)
for i in "${svgs[@]}"
do
: 
   convert -background transparent ${svg_dir}$i.svg ${java_res_dir}${i}.png
done

