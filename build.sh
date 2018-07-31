#!/bin/bash

#1.
cd 1_np_nn_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=OFF ../
make
cd ../

#2.主线程阻塞发送、从线程阻塞接收
cd ./2_p_bsbr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_RECV=OFF ../
make
cd ../
#3.主线程阻塞发送、从线程消息池半阻塞接收
cd ./3_p_bshr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_RECV=ON ../
make
cd ../

#4.主线程消息池非阻塞发送、从线程阻塞接收
cd ./4_p_nsbr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=ON -DUSE_HNOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_RECV=OFF ../
make
cd ../
#5.主线程消息池非阻塞发送、从线程消息池半阻塞接收
cd ./5_p_nshr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=ON -DUSE_HNOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_RECV=ON ../
make
cd ../

#6.主线程消息池半阻塞发送、从线程阻塞接收
cd ./6_p_hsbr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_SEND=ON -DUSE_HNOBLOCKING_RECV=OFF ../
make
cd ../

#7.主线程消息池半阻塞发送、从线程消息池半阻塞接收
cd ./7_p_hshr_build
rm * -rf
cmake -DUSE_OPENMP=OFF -DUSE_PTHREAD=ON -DUSE_NOBLOCKING_SEND=OFF -DUSE_HNOBLOCKING_SEND=ON -DUSE_HNOBLOCKING_RECV=ON ../
make
cd ../
