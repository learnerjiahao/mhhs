 module HydroInfoMod
        use WaterShedMod
        !use DBSQLLinkMod
   
        implicit none
        
        ! 水文站/参数分区基本信息
        type HydroInfoClass
            integer(4) ::HydroId                 ! 水文站Id
            Character(len=20) ::Stcd
            Character(len=50) ::Stnm     ! 水文站名称
            real ::Long                ! 控制面积
            real ::Lati
            real ::PointX
            real ::PointY
                   
        end type HydroInfoClass    
        
        ! 水文站基本信息，按水文站Id来保存
        integer :: NHydroInfoCK
        type(HydroInfoClass),dimension(:),allocatable::HydroInfos
        type(HydroInfoClass) :: m_HydroInfo
        
        integer :: RunLoopCount 
        
        contains
            
            ! 初始化HydroInfos
!            subroutine InitHydroInfos()                

!                INTEGER (SQLINTEGER) :: LENStr = 30
!                integer(KIND=4) :: i                
!!                INTEGER (SQLRETURN) iRet
!!                INTEGER (SQLHANDLE) :: hSQLStmt
!                !dcb
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select Count(*) From ST_HydroInfo"//Char(0),SQL_NTSL)
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NHydroInfoCK,0,cbRet)
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                !分配内存空间
!                if (NHydroInfoCK .ne. WaterShed.NHydroStation) then   !dcb 水文站数error
!                    !write(*,*) "水文站数出现异常，按Enter继续"
!                    !pause
!                endif
!                if(allocated(HydroInfos))deallocate(HydroInfos)
!                allocate(HydroInfos(NHydroInfoCK))
!                !dcb InitHydroInfos
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect (hSQLStmt, "SELECT * FROM ST_HydroInfo"//CHAR(0), SQL_NTSL )
!
!                ! bind result columns                
!                iRet = SQLBindColI4 (hSQLStmt, 1, SQL_C_SLONG, m_HydroInfo%HydroId, 0, cbRet)
!                iRet = SQLBindColChar (hSQLStmt, 2, SQL_C_CHAR, m_HydroInfo%Stcd, LENStr, cbStr )
!                iRet = SQLBindColChar (hSQLStmt, 3, SQL_C_CHAR, m_HydroInfo%Stnm, LENStr, cbStr )
!                iRet = SQLBindColR4 (hSQLStmt, 4, SQL_C_FLOAT, m_HydroInfo%Long, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 5, SQL_C_FLOAT, m_HydroInfo%Lati, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 6, SQL_C_FLOAT, m_HydroInfo%PointX, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 7, SQL_C_FLOAT, m_HydroInfo%PointY, 0, cbR4 )
!                
!                i = 0
!                DO WHILE (.TRUE.)
!                    iRet = SQLFetch(hSQLStmt)
!                    i = i + 1
!                    IF ( iRet .EQ. SQL_NO_DATA_FOUND ) EXIT                    
!                    HydroInfos(i) = m_HydroInfo                                        
!                END DO
!                
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine
                      
            ! 输出HydroInfos
            subroutine WriteHydroInfos()
                !write(*,*) HydroInfos
            end subroutine
            
            ! 卸载HydroInfos
            subroutine DestroyHydroInfos
                if(allocated(HydroInfos))deallocate(HydroInfos)
            end subroutine
                    
    end module