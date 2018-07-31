! 河道基本信息
    module ReachInfoMod
        use WaterShedMod
        !use DBSQLLinkMod
        !use StringFiltrMod
        
       implicit None
        
        type ReachClass
            integer(2) :: ReachId
            integer(4)::My                              ! 子流域内计算单元个数   
            real :: Area
            real::Xcoor
            real::Ycoor
            real::Lati
            real::BsDem
            real::T_L1
            real::T_S1
            real::T_W1
            real::T_K1
            real::T_N1
            real::M_W2
            real::M_D
            real::M_S2
            real::M_L2
            real::M_N2
            real::M_K2
            real::SLSUBBSN
            character(len=30)::UPRCHs  
            integer :: NUPRCH
            integer, dimension(:), allocatable :: IUPRCH   
            integer :: IStore = 0
            integer :: IDive = 0
            integer :: IRes = 0
            integer StoreAreaID,DiveID,ResID
            real StoreCoef
            real Qlimit,Qdive
            real Vlimit           
                     
        end type ReachClass
        
        
        integer::NReachCK
        type(ReachClass),Dimension(:),allocatable :: Reachs
        type(ReachClass)::m_Reach
        
        
        contains
        !初始化Reachs
!            subroutine InitReachs()
!                INTEGER :: LENStr = 30
!                integer(KIND=4) :: i
!                integer length1,j,itemp
!                INTEGER (SQLRETURN) iRet 
!                INTEGER (SQLHANDLE) :: hSQLStmt             
                
                !dcb NReachCK
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"SELECT COUNT(*) FROM ST_ReachInfo"//Char(0),SQL_NTSL)
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NReachCK,0,cbRet)
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                
!                if (NReachCK .ne. WaterShed.NReach) then
!                    !write(*,*) "河段数出现异常，按Enter继续"
!                    !pause
!                endif

!                if(allocated(Reachs))deallocate(Reachs)
!                allocate(Reachs(NReachCK))
                !dcb Reachs
                !获得记录
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select * FROM ST_ReachInfo"//Char(0),SQL_NTSL)                   
!
!                ! bind result columns                
!                iRet = SQLBindColI2 (hSQLStmt, 1,  SQL_C_SSHORT, m_Reach.ReachId, 0, cbId )
!                iRet = SQLBindColI4 (hSQLStmt, 2,  SQL_C_SLONG,  m_Reach.My, 0, cbRet )
!                iRet = SQLBindColR4 (hSQLStmt, 3,  SQL_C_FLOAT,  m_Reach.Area, 0, cbR4 ) 
!                iRet = SQLBindColR4 (hSQLStmt, 4,  SQL_C_FLOAT,  m_Reach.Xcoor, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 5,  SQL_C_FLOAT,  m_Reach.Ycoor, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 6,  SQL_C_FLOAT,  m_Reach.Lati, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 7,  SQL_C_FLOAT,  m_Reach.BsDem,0, cbR4)
!                iRet = SQLBindColR4 (hSQLStmt, 8,  SQL_C_FLOAT,  m_Reach.T_L1, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 9,  SQL_C_FLOAT,  m_Reach.T_S1, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 10, SQL_C_FLOAT,  m_Reach.T_W1, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 11, SQL_C_FLOAT,  m_Reach.T_K1, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 12, SQL_C_FLOAT,  m_Reach.T_N1, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 13, SQL_C_FLOAT,  m_Reach.M_W2, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 14, SQL_C_FLOAT,  m_Reach.M_D,  0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 15, SQL_C_FLOAT,  m_Reach.M_S2, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 16, SQL_C_FLOAT,  m_Reach.M_L2, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 17, SQL_C_FLOAT,  m_Reach.M_N2, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 18, SQL_C_FLOAT,  m_Reach.M_K2, 0, cbR4 )
!                iRet = SQLBindColR4 (hSQLStmt, 19, SQL_C_FLOAT,  m_Reach.SLSUBBSN, 0, cbR4 )
!                iRet = SQLBindColI4 (hSQLStmt, 20,  SQL_C_SSHORT, m_Reach.NUPRCH, 0, cbRet )             
!                iRet = SQLBindColChar (hSQLStmt, 21, SQL_C_CHAR, m_Reach.UPRCHs, LENStr, cbStr )
!                
!
!                i = 0                
!                DO WHILE (.TRUE.)
!                    iRet = SQLFetch(hSQLStmt) 
!                    i = i + 1
!                    IF ( iRet .EQ. SQL_NO_DATA_FOUND ) EXIT
!                    !CALL CheckSQLReturnCode (3, *994)
!                    m_Reach.Area = m_Reach.Area*1000000.0 
!                    m_Reach.Lati = m_Reach.Lati*3.1415927/180.
!        			
!			        !处理平坡度
!			        if (m_Reach.T_S1 .le. 0) then
!			            m_Reach.T_S1 = 0.0001
!			        endif
!        			
!			        if (m_Reach.M_S2 .le. 0) then
!			            m_Reach.M_S2 = 0.0001
!			        endif                    
!			        Reachs(i) = m_Reach
!			        Reachs(i).lati = Reachs(i).lati / 90.
!                    if (Reachs(i).NUPRCH .ne.0) then
!                        do j=1,30
!                        itemp = iachar(Reachs(i).UPRCHs(j:j))
!                          if(iachar(Reachs(i).UPRCHs(j:j)).eq.0) then
!                            itemp = j
!                            exit
!                          endif
!                        end do
!                        call GetStrSplitterCount(Reachs(i).UPRCHs,itemp,length1,',')
!                        if(allocated(Reachs(i).IUPRCH))deallocate(Reachs(i).IUPRCH)
!                        allocate(Reachs(i).IUPRCH(Reachs(i).NUPRCH))
!                        read(Reachs(i).UPRCHs,*)  (Reachs(i).IUPRCH(j),j=1,length1,1)
!                    endif
!                END DO
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )              
                
!            end subroutine
            
            
            subroutine DestroyReachs()
                if(allocated(Reachs))deallocate(Reachs)
            end subroutine
    
    end module