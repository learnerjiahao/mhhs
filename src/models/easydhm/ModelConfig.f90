module ModelConfigMod
     ! 方案设置信息
        use WaterShedMod
        use StringFiltrMod
!        use DBSQLLinkMod
   
        implicit None
        type ModelConfigClass
            integer :: SID 
            integer :: pid
            integer :: Rtmdy
            integer :: Flooderror
            integer :: RFSimType
            integer :: IParaYear
            integer :: NRunoffGenType
            integer,dimension(:),allocatable:: IRunoffGenType
            integer Runtype
            integer irte
            integer GWSimType
            integer iPet
            integer RtmdyType
            integer AR_NUM
            integer TimeStepOpt

        end type ModelConfigClass
        
        type(ModelConfigClass),dimension(:),allocatable::m_ModelConfig
        integer ::initSuc
        
        contains
            ! 从数据库中读取ModelConfig
!            subroutine InitModelConfig(solutionid,pid)
!                implicit None
!                Integer::solutionid,i,pid
!                Integer::length
!                Integer::LenStr  = 20
!                character(20) IRunoffGenTypeStr
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
!                
!                character(256)::postSql
!                !连接数据库
!                write(postSql,110)"SELECT * FROM ST_ModelConfig where SId=",solutionid," and pid = ",pid,CHAR(0)
!                110 Format(a,i3,a,i3,a)
!                !dcb  InitModelConfig
!                !Allocate Statement
!                iRet = SQLAllocHandle( SQL_HANDLE_STMT, hSQLDbC, hSQLStmt )
!                iRet = SQLExecDirect( hSQLStmt,	postSql,SQL_NTSL )
!
!                ! Bind Columns
!                iRet = SQLBindColI4( hSQLStmt, 1, SQL_C_SLONG, m_Modelconfig%SID, 0, cbRet )
!                iRet = SQLBindColI4( hSQLStmt, 2, SQL_C_SLONG, m_Modelconfig%pid, 0, cbRet )
!                iRet = SQLBindColI4 (hSQLStmt, 3, SQL_C_SLONG, m_ModelConfig%Rtmdy, 0, cbRet)
!!                iRet = SQLBindColI4 (hSQLStmt, 4, SQL_C_SLONG, m_ModelConfig%Flooderror, 0, cbRet)
!!                iRet = SQLBindColI4 (hSQLStmt, 5, SQL_C_SLONG, m_ModelConfig%RFSimType, 0, cbRet)
!!                iRet = SQLBindColChar(hSQLStmt, 5, SQL_C_CHAR, m_ModelConfig%AvaIdStr, LENStr, cbStr )
!                iRet = SQLBindColI4 (hSQLStmt, 6, SQL_C_SLONG, m_ModelConfig%IParaYear, 0, cbRet)
!                iRet = SQLBindColI4 (hSQLStmt, 7, SQL_C_SLONG, m_ModelConfig%NRunoffGenType, 0, cbRet)
!                iRet = SQLBindColChar (hSQLStmt, 8, SQL_C_CHAR,IRunoffGenTypeStr, LENStr, cbStr )
!                iRet = SQLBindColI4( hSQLStmt, 9, SQL_C_SLONG, m_Modelconfig%Runtype, 0, cbRet )
!                iRet = SQLBindColI4( hSQLStmt, 10, SQL_C_SLONG, m_Modelconfig%irte, 0, cbRet )
!                iRet = SQLBindColI4 (hSQLStmt, 11, SQL_C_SLONG, m_ModelConfig%GWSimType, 0, cbRet)
!                iRet = SQLBindColI4( hSQLStmt, 12, SQL_C_SLONG, m_Modelconfig%iPet, 0, cbRet )
!                iRet = SQLBindColI4 (hSQLStmt, 13, SQL_C_SLONG, m_ModelConfig%RtmdyType, 0, cbRet)
!                iRet = SQLBindColI4 (hSQLStmt, 14, SQL_C_SLONG, m_ModelConfig%AR_NUM, 0, cbRet)
!
!                iRet = SQLFetch( hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                
!                if(allocated(m_ModelConfig%IRunoffGenType))deallocate(m_ModelConfig%IRunoffGenType)
!                allocate(m_ModelConfig%IRunoffGenType(m_ModelConfig%NRunoffGenType))
!                call GetStrSplitterCount(IRunoffGenTypeStr,20,length,' ')
!                read(IRunoffGenTypeStr,*) (m_ModelConfig%IRunoffGenType(i),i=1,length,1)
!                initSuc = 1
!            end subroutine
           
            subroutine deleteConfig
                if(initSuc .eq. 1)then
                    initSuc = 0
!                    if(allocated(m_ModelConfig%IRunoffGenType))deallocate(m_ModelConfig%IRunoffGenType)
                endif
            endsubroutine
   end module