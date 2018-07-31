
    ! 土壤基本信息
    module SoilInfoMod
    
        use WaterShedMod
        !USE DBSQLLinkMod
        use UnitInfoMod
        use SolutionMod
        
        implicit none
        
        ! 水库观测数据
        type SoilInfoClass
            integer :: SubID                   ! 子流域Id
            real::dep_imp
            real::ddrain
            real::sol_crk
            real::solh
            real::solh0
        end type SoilInfoClass        
        
       
        ! 水库基本信息，按水库Id来保存
        type(soilInfoClass), dimension(:), allocatable :: Soils
        type(soilInfoClass)::m_Soil
        
        contains
        
!            subroutine InitSoilInfo
!                integer::i,NResCK
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
!                !dcb 
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select Count(*) From ST_Soil"//Char(0),SQL_NTSL)
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NResCK,0,cbRet)
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
                !分配内存空间
!                if(allocated(Soils))deallocate(Soils)
!                allocate(Soils(Watershed.NSubbasin))
                !dcb InitSoilInfo   
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select * From ST_Soil"//Char(0),SQL_NTSL)
!                
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,m_soil.SubId,0,cbRet)
!                iRet=SQLBindColR4(hSQLStmt,2,SQL_C_FLOAT,m_soil.dep_imp,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,3,SQL_C_FLOAT,m_soil.ddrain,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,4,SQL_C_FLOAT,m_soil.sol_crk,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,5,SQL_C_FLOAT,m_soil.solh,0,cbR4) 
!                
!                i=0
!                DO While(.true.)
!                    iRet=SQLFetch(hSQLStmt)
!                    if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                    i=i+1
!                    soils(i)=m_soil
!                ENDDO
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine
            
            subroutine Destroysoils
                if(allocated(soils))deallocate(soils)
            end subroutine
    end module        

