#!/bin/bash

# Generates png icons needed for the cl-lodds-qt client.
# convert is part of ImageMagick

convert ../res/lodds.svg         -background transparent -resize 64x64 ../cl-code/res/file.png
convert ../res/file.svg          -background transparent -resize 64x64 ../cl-code/res/file.png
convert ../res/folder-closed.svg -background transparent -resize 64x64 ../cl-code/res/folder-closed.png
convert ../res/folder-open.svg   -background transparent -resize 64x64 ../cl-code/res/folder-open.png
convert ../res/send-file.svg     -background transparent -resize 64x64 ../cl-code/res/send-file.png
convert ../res/shared.svg        -background transparent -resize 46x64 ../cl-code/res/shared.png
convert ../res/user.svg          -background transparent -resize 46x64 ../cl-code/res/user.png
convert ../res/x.svg             -background transparent -resize 64x64 ../cl-code/res/x.png
