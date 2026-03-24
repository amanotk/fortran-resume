#!/bin/bash
gfortran -O2 -Wall dimensions.f90 main.f90 -o test_dimensions && ./test_dimensions
