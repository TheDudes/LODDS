#!/bin/bash

# Generates png icons needed for the cl-lodds-qt client.
# convert is part of ImageMagick

convert -background transparent ../res/lodds.svg         -resize 64x64 ../cl-code/res/lodds.png
convert -background transparent ../res/file.svg          -resize 64x64 ../cl-code/res/file.png
convert -background transparent ../res/folder-closed.svg -resize 64x64 ../cl-code/res/folder-closed.png
convert -background transparent ../res/folder-open.svg   -resize 64x64 ../cl-code/res/folder-open.png
convert -background transparent ../res/send-file.svg     -resize 64x64 ../cl-code/res/send-file.png
convert -background transparent ../res/shared.svg        -resize 46x64 ../cl-code/res/shared.png
convert -background transparent ../res/user.svg          -resize 46x64 ../cl-code/res/user.png
convert -background transparent ../res/x.svg             -resize 64x64 ../cl-code/res/x.png
