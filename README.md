## Overview

This is a small repository of data analysis scripts written in R and Python, structured primarily for backup and collaboration purposes. All of the code is WIP and should not be considered finalized or even functional. Generally speaking, the code aims to structure, analyze, and plot various forms of ecological and meterological data collceted from a network of eddy-covariance towers across cental New Mexico, as part of the New Mexico Elevation Gradient. Several components to the code operate on remote sensing data and data products, in an attempt to integrate remote sensing and ground based observations of ecosystem function.

## WIP

Currently, this repository serves a backup only purpose, and over time the contents will grow and evolve, ultimatley serving as a distribution source for small analysys projects. 

## amfluxDataReader.R

A bit of a misnomer at this point, this script simply reads in all of the Ameriflux files in a directory into the R workspace one at a time, concatenating the files in the process into a single variable. The 'SITE' variable is also appended as part of the loop iteration. During the concatenation process, a concatenated daily file is also created, with a host of commonly desired variables included. Its not very fancy but it works for the time being. 
