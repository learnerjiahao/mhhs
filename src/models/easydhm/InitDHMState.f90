 module InitDHMStateMod
        use WaterShedMod
        !use DBSQLLinkMod
        implicit none
        ! 计算的单元水文要素
        type u_InitDHMClass
            integer SubId
            integer UnitId
		    real init_flwout      !初始河道流量m3/s
		    real init_rchstor      !初始河道槽蓄水量m3
		    real init_rechg        !地下补给量mm
		    real init_rg           !地下径流量mm
		    real init_snow         !积雪量mm
            real init_sint         !冠层截留储量mm
		    real init_sdep         !地表填洼储量mm	
		    real init_gt           !地下水储量mm   
		    real init_ss           !土壤含水率
        end type u_InitDHMClass
        
        type(u_InitDHMClass),Dimension(:,:),allocatable :: InitDHMStates
        type(u_InitDHMClass)::m_InitDHMStates
        
         
!        contains 
            
            ! 读取数据库EasyDHM模型参数
!            subroutine ReadInitDHMStates()
!                integer::i,j

!                INTEGER (SQLRETURN) :: iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
                
                !用WaterShed中的参数分区个数分配EasyDHMStates的大小
!                if(allocated(InitDHMStates))deallocate(InitDHMStates)
!                allocate(InitDHMStates(WaterShed%NSubbasin,WaterShed.NUnit))
                !dcb ReadInitDHMStates
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"SELECT * FROM ST_InitStateDHM"//Char(0),SQL_NTSL) 
!
!                iRet=SQLBindColI4(hSQLStmt,2,SQL_C_SLONG,m_InitDHMStates%SubId,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,3,SQL_C_SLONG,m_InitDHMStates%UnitID,0,cbRet)
!                iRet=SQLBindColR4(hSQLStmt,4,SQL_C_FLOAT,m_InitDHMStates%init_flwout,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,5,SQL_C_FLOAT,m_InitDHMStates%init_rchstor,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,6,SQL_C_FLOAT,m_InitDHMStates%init_rechg,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,7,SQL_C_FLOAT,m_InitDHMStates%init_rg,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,8,SQL_C_FLOAT,m_InitDHMStates%init_snow,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,9,SQL_C_FLOAT,m_InitDHMStates%init_sint,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,10,SQL_C_FLOAT,m_InitDHMStates%init_sdep,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,11,SQL_C_FLOAT,m_InitDHMStates%init_gt,0,cbR4)
!                iRet=SQLBindColR4(hSQLStmt,12,SQL_C_FLOAT,m_InitDHMStates%init_ss,0,cbR4)
!
!                i=0
!                Do While(.True.)
!                    iRet=SQLFetch(hSQLStmt)
!                    if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                    i=i+1
!                    InitDHMStates(m_InitDHMStates.SubID,m_InitDHMStates.UnitID)=m_InitDHMStates
!                End Do
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine
    
    end module