 ! 计算单元信息
    module UnitInfoMod
        
!        use WaterShedMod
        !use StringFiltrMod
        !use DBSQLLinkMod
    
        implicit none
        
        ! 计算单元   @UnitInfo.txt
        type UnitClass
            integer:: ylanduse
            integer :: SubId
            integer :: UnitId
            integer :: SubUnitId  
            real :: HeiGm
            real :: Slope 
            real :: Area(6)
            real :: With
            real :: Alth
            real :: Runoff
            real :: Conduct
            real :: Porosity
            real :: Fieldcap
            real :: Poreindex
            real :: Wilting
            real :: Residual
            real :: Lai_max(6)
            real :: Lai_min(6)
            real :: Depression
            real :: Rootdepth(6)
            real :: Imp
            real :: Itc_max(6)
            real areasum
        end type UnitClass
        
        ! 计算单元，按计算单元Id来保存
        type(UnitClass), Dimension(:,:), allocatable :: Units
        type(UnitClass)::m_Unit
        integer::UnitNlanduse   
        !integer::NUnitCK
!        integer::NumberCount
 
        contains
        
            ! 读取数据库获得计算单元的基本信息
!            subroutine InitUnits()
!            integer::i,j,k,l
            !integer::LENSTR=200
!            integer::NLine,NUnitTemp
!            INTEGER (SQLRETURN) iRet
!            INTEGER (SQLHANDLE) :: hSQLStmt
            !dcb
!            iRet=SQLAllocHandle(SQL_HANDLE_Stmt,hSQLDbc,hSQLStmt)
!            iRet=SQLExecDirect(hSQLStmt,"Select count(*) From ST_UnitInfo"//Char(0),SQL_NTSL)
!            iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NLine,0,cbRet)
!            iRet=SQLFetch(hSQLStmt)
!            iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
            
!            if(allocated(Units))deallocate(Units)
!            allocate(Units(WaterShed.NSubbasin,WaterShed.NUnit))
            !dcb InitUnits
!            iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!            iRet=SQLExecDirect(hSQLStmt,"Select * From ST_UnitInfo"//Char(0),SQL_NTSL)
!            
!            iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,m_Unit.SubId,0,cbRet)
!            iRet=SQLBindColI4(hSQLStmt,2,SQL_C_SLONG,m_Unit.UnitId,0,cbRet)
!            iRet=SQLBindColR4(hSQLStmt,3,SQL_C_FLOAT,m_Unit.HEIGM,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,4,SQL_C_FLOAT,m_Unit.Slope,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,5,SQL_C_FLOAT,m_Unit.Area,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,6,SQL_C_FLOAT,m_Unit.With,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,7,SQL_C_FLOAT,m_Unit.Alth,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,8,SQL_C_FLOAT,m_Unit.Runoff,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,9,SQL_C_FLOAT,m_Unit.Conduct,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,10,SQL_C_FLOAT,m_Unit.Porosity,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,11,SQL_C_FLOAT,m_Unit.Fieldcap,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,12,SQL_C_FLOAT,m_Unit.Poreindex,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,13,SQL_C_FLOAT,m_Unit.Wilting,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,14,SQL_C_FLOAT,m_Unit.Residual,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,15,SQL_C_FLOAT,m_Unit.Lai_max,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,16,SQL_C_FLOAT,m_Unit.Lai_min,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,17,SQL_C_FLOAT,m_Unit.Depression,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,18,SQL_C_FLOAT,m_Unit.Rootdepth,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,19,SQL_C_FLOAT,m_Unit.Imp,0,cbR4)
!            iRet=SQLBindColR4(hSQLStmt,20,SQL_C_FLOAT,m_Unit.Itc_max,0,cbR4)

!            i=0
!            NumberCount=0
!            DO While(.true.)
!                iRet=SQLFetch(hSQLStmt)
!                i=i+1
!                if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                m_Unit.Area = m_Unit.Area*1000000
!                Units(m_Unit.SubId,m_Unit.UnitId)=m_Unit
!                Units(m_Unit.SubId,m_Unit.UnitId).SubUnitId=NumberCount+1     
!                NumberCount=NumberCount+1
!            ENDDO
!            iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine
            
            subroutine DestroyUnits()
                 if(allocated(Units))deallocate(Units)
            end subroutine
        
    end module