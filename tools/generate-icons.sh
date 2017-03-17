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
java_res_dir=${script_dir}../java-code/src/studyproject/resources/lodds_icon/
mkdir -p java_res_dir

# Convert svg files
svgs=(lodds file folder-closed folder-open reload send-file shared user x)
for i in "${svgs[@]}"
do
: 
   convert -background transparent ${svg_dir}$i.svg ${java_res_dir}${i}.png
done

