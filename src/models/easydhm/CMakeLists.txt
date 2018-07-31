# set binary path and lib storage path for compiling process.
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_BUILD_TYPE Release)

#这当前项目里src/models/easydhm目录下的所有文件(包含子目录下的文件)添加到EASYDHM_SRCS这个变量里
#file(GLOB_RECURSE EASYDHM_SRCS RELATIVE /*)
#aux_source_directory(${PROJECT_SOURCE_DIR}/src/models/easydhm EASYDHM_SRCS)
set(    EASYDHM_SRCS
        AllocateVars.f90
        AutoAdjustRunoff.f90
        automet.f90
        AutoTimes.f90
        bestparmain.f90
        CalModelPara.f90
        changemodPar.f90
        CMakeLists.txt
        ConfluxSim.f90
        CurDHMParam+EasyDHMParamMod.f90
        CurrentUnitVars.f90
        DestroyOutData.f90
        DHM.f90
        dhmtype.f90
        Dll2.f90
        EasyDHM.f90
        EasyDHM_Dll.f90
        EasyDHMVar.f90
        ETpotMod.f90
        GlobalFun.f90
        HydroData.f90
        HydroInfo.f90
        HydroObserved.f90
        InitDHMState.f90
        InitStatVars.f90
        LandSurfaceSim.f90
        ModelConfig.f90
        OutputPRMod.f90
        ParamRangeMod.f90
        ReachInfoMod.f90
        ReachParam+CurReachParam.f90
        ReachVarMod.f90
        readbestpar.f90
        readchangepar.f90
        ReadMe.txt
        ResInfo.f90
        ResRange.f90
        ResVarMod.f90
        route.f90
        rtmanning.f90
        rtmusk.f90
        rtvarstor.f90
        run.f90
        Sim_ETDHM.f90
        Sim_SnowDHM.f90
        Sim_SoilDHM.f90
        Sim_SurfQDHM.f90
        SimpleVar.f90
        SingleChangePara.f90
        SoilInfo.f90
        Solution.F90
        ST_OutUnitDHMMod.f90
        ST_OutUnitStateDHM.f90
        ST_PR_OutMod.f90
        StaticMemeber.f90
        StringFiltr.f90
        Subbasin.f90
        temptype.f90
        TimeInfoMod.f90
        UnitInfo.f90
        WaterShed.f90
        WaterShedExMod.f90
        Weather.f90
        WeatherInfoMod.f90)
# 将SRCS编译为mylib这个动态链接库
add_library(${EASYDHMLIB} STATIC ${EASYDHM_SRCS})