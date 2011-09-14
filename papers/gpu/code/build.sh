#!/usr/bin/env bash

nvcc -I/usr/local/cuda/include/ -L/usr/local/cuda/lib/ example1.cu -o ex1
nvcc -I/usr/local/cuda/include/ -L/usr/local/cuda/lib/ example2.cu -o ex2
