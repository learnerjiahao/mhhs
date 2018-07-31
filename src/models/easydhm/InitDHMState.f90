 module InitDHMStateMod
        use WaterShedMod
        !use DBSQLLinkMod
        implicit none
        ! ����ĵ�Ԫˮ��Ҫ��
        type u_InitDHMClass
            integer SubId
            integer UnitId
		    real init_flwout      !��ʼ�ӵ�����m3/s
		    real init_rchstor      !��ʼ�ӵ�����ˮ��m3
		    real init_rechg        !���²�����mm
		    real init_rg           !���¾�����mm
		    real init_snow         !��ѩ��mm
            real init_sint         !�ڲ��������mm
		    real init_sdep         !�ر����ݴ���mm	
		    real init_gt           !����ˮ����mm   
		    real init_ss           !������ˮ��
        end type u_InitDHMClass
        
        type(u_InitDHMClass),Dimension(:,:),allocatable :: InitDHMStates
        type(u_InitDHMClass)::m_InitDHMStates
        
         
!        contains 
            
            ! ��ȡ���ݿ�EasyDHMģ�Ͳ���
!            subroutine ReadInitDHMStates()
!                integer::i,j

!                INTEGER (SQLRETURN) :: iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
                
                !��WaterShed�еĲ���������������EasyDHMStates�Ĵ�С
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