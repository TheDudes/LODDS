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
svg_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/../res/
java_res_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/../java-code/src/studyproject/resources/

# Convert lodds icon in various solution
svg=${svg_dir}lodds.svg
lodds_prefix=${java_res_dir}lodds_icon

resolutions=(16x16 32x32 64x64)
for i in "${resolutions[@]}"
do
: 
   convert -background transparent -resize $i $svg ${lodds_prefix}${i}.png
done


