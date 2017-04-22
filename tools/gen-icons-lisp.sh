#!/bin/bash

# Generates png icons needed for the cl-lodds-qt client.
# convert is part of ImageMagick

cp ../res/spinner.svg ../cl-code/res/spinner.svg
convert -background transparent ../res/lodds.svg            -resize  64x64  ../cl-code/res/lodds.png
convert -background transparent ../res/file.svg             -resize  64x64  ../cl-code/res/file.png
convert -background transparent ../res/folder-closed.svg    -resize  64x64  ../cl-code/res/folder-closed.png
convert -background transparent ../res/folder-open.svg      -resize  64x64  ../cl-code/res/folder-open.png
convert -background transparent ../res/send-file.svg        -resize  32x32  ../cl-code/res/send-file.png
convert -background transparent ../res/send-file-hover.svg  -resize  32x32  ../cl-code/res/send-file-hover.png
convert -background transparent ../res/shared.svg           -resize  23x32  ../cl-code/res/shared.png
convert -background transparent ../res/user.svg             -resize  23x32  ../cl-code/res/user.png
convert -background transparent ../res/user-hover.svg       -resize  23x32  ../cl-code/res/user-hover.png
convert -background transparent ../res/x.svg                -resize  64x64  ../cl-code/res/x.png
convert -background transparent ../res/tree-arrow.svg       -resize   8x12  ../cl-code/res/tree-closed.png
convert -background transparent ../res/tree-arrow.svg       -resize   8x12 -rotate 90 ../cl-code/res/tree-open.png
convert -background transparent ../res/tree-arrow-hover.svg -resize   8x12  ../cl-code/res/tree-closed-hover.png
convert -background transparent ../res/tree-arrow-hover.svg -resize   8x12 -rotate 90 ../cl-code/res/tree-open-hover.png
convert -background transparent ../res/tree-line.svg        -resize 128x192 ../cl-code/res/tree-line.png
convert -background transparent ../res/tree-more.svg        -resize 128x192 ../cl-code/res/tree-more.png
convert -background transparent ../res/tree-end.svg         -resize 128x192 ../cl-code/res/tree-end.png
