! Watershed初始化，卸载
    Module WaterShedExMod
        use WaterShedMod
        use SubbasinMod
        use UnitInfoMod
        use ReachInfoMod
        use ResInfoMod
        use HydroInfoMod
        use HydroDataMod
        use SoilInfoMod
        
        use ResRangeMod
        use ParamRangeMod
        use WeatherInfoMod
        use ModelConfigMod
        
!        use WetSpaParamMod
!        use XAJParamMod
!        use HymodParamMod
        use ReachParamMod

        use InitDHMStateMod
!        use InitWSPStateMod
!        use InitXAJStateMod
!        use initHymodStateMod
        !use DBSQLLinkMod

        contains
            
            ! 初始化WaterShed
!            subroutine InitWaterShed()
!                implicit none
!                integer :: i
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
                !自数据库读入WaterShed的基本信息
                !dcb WaterShed
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select * From ST_WaterShed"//Char(0),SQL_NTSL)
!                !Bind Columns
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,WaterShed.NSubbasin,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,2,SQL_C_SLONG,WaterShed.NUnit,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,3,SQL_C_SLONG,WaterShed.NReach,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,4,SQL_C_SLONG,WaterShed.NParamRange,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,5,SQL_C_SLONG,WaterShed.NHydroStation,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,6,SQL_C_SLONG,WaterShed.NResRange,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,7,SQL_C_SLONG,WaterShed.NLayer,0,cbRet)
!                
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!     
!                ! 水库基本信息
!                do i=1,WaterShed%NResRange
!                    WaterShed%NResIn = WaterShed%NResIn + 1
!                    WaterShed%NResOut = WaterShed%NResOut + 1
!                enddo          
                

!                call InitReachs()
                
                !!!call InitSubbasins()
                
!                call InitUnits()
!                call InitHydroInfos()
!                call InitResInfos()
!                call InitResRanges()
!                call InitParamRanges()
!                call InitSoilInfo()

                !!!call InitHydroData()
!                call InitWeatherInfos()
!                call iniYearTavg()
                
!                call ReadEasyDHMParam()
!                call ReadWetSpaParam()
!                call ReadXAJParam()
!                call ReadHymodParam()
!                call ReadReachParam
                
!                call ReadInitDHMStates()
!                call ReadInitWSPStates()
!                call ReadInitXAJStates()
!                call ReadInitHymodStates()
                
                !!!!call AllocateVars()

!            end subroutine
            
            
            subroutine DestroyWaterShed()
            
                call DestroySubbasins()
                call DestroyReachs()
                call DestroyUnits()
                call Destroysoils()
                                
                call DestroyHydroInfos()
                call DestroyResInfos()
                               
                call DestroyResRanges()
                call DestroyParamRanges()   
                
                call DestroyHydroData()
               ! call DestroyWeatherInfos()
                call DeallocateVars()  
                
                if(allocated(EasyDHMParams))deallocate(EasyDHMParams)
!                if(allocated(WetSpaParams))deallocate(WetSpaParams)
!                if(allocated(XAJParams))deallocate(XAJParams)
!                if(allocated(HymodParams))deallocate(HymodParams)
                if(allocated(ReachParams))deallocate(ReachParams)
                
                if(allocated(InitDHMStates))deallocate(InitDHMStates)      
!                if(allocated(InitWSPStates))deallocate(InitWSPStates)      
!                if(allocated(InitXAJStates))deallocate(InitXAJStates)      
!                if(allocated(InitHymodStates))deallocate(InitHymodStates)      
                
            end subroutine
       end module   