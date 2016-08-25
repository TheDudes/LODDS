#!/bin/bash
# generates doc pdf

enscript $1 -o - | ps2pdf - doc.pdf
# paps $1 | ps2pdf - doc.pdf
