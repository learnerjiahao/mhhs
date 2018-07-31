#!/bin/bash

#-o或--options选项后面接可接受的短选项，如ab:c::，表示可接受的短选项为-a -b -c，其中-a选项不接参数，-b选项后必须接参数，-c选项的参数为可选的
#-l或--long选项后面接可接受的长选项，用逗号分开，冒号的意义同短选项。
#-n选项后接选项解析错误时提示的脚本名字
ARGS=`getopt -o d:p: --long depth:,pronum: -n 'test.sh' -- "$@"`
if [ $? != 0 ]; then
    echo "Usage: ./test.sh -d|--depth <depth> -p|--pronum <process_num>"
    exit 1
fi

#echo $ARGS
#将规范化后的命令行参数分配至位置参数（$1,$2,...)
eval set -- "${ARGS}"

depth=1
pronum=1

while true
do
    case "$1" in
        -d|--depth)
            depth=$2
            shift 2
            ;;
        -p|--pronum)
            pronum=$2
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Usage: ./test.sh -d|--depth <depth> -p|--pronum <process_num>"
            exit 1
            ;;
    esac
done


python3 create_dispatch.bak -d $depth -p $pronum 
if [ $? != 0 ]; then
    echo "python3 create_dispatch -d $depth -p $pronum failed!!!"
    exit 1
fi
python3 check_dispatch.py -j ./dispatch.json
if [ $? != 0 ]; then
    echo "python3 check_dispatch.py -j ./dispatch.json failed!!!"
    exit 1
fi
../cmake-build-debug/bin/disc j2b -j ./dispatch.json -o ./dispatch.dis
if [ $? != 0 ]; then
    echo "../build/bin/disc j2b -j ./dispatch.json -o ./dispatch.dis failed!!!"
    exit 1
fi
mpirun -n $pronum ../cmake-build-debug/bin/pnohs -c ./config.toml
