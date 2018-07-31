 ! ˮ�������Ϣ
    module ResInfoMod
    
        use WaterShedMod
        !USE DBSQLLinkMod
        
        implicit none
        
        ! ˮ��۲�����
        type ResInfoClass
            integer :: ResId                   ! ˮ��Id
            Character(Len=10)::Stcd
            Character(Len=30) :: ResName       ! ˮ������
            real :: ControledArea               ! �������
            real::long
            real::lati
            real::PointX
            real::PointY
        end type ResInfoClass         
        
       
        ! ˮ�������Ϣ����ˮ��Id������
        type(ResInfoClass), dimension(:), allocatable :: ResInfos
        type(ResInfoClass)::m_ResInfo
        integer::NResCK    
        
        contains
        
!            subroutine InitResInfos
!                integer::LenStr=30,lenStr1=10
!                integer::i
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
               !dcb  
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select Count(*) From ST_ResInfo"//Char(0),SQL_NTSL)
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NResCK,0,cbRet)
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
                !�����ڴ�ռ�
!                if (NResCK .ne. WaterShed.NResRange) then
!                    !write(*,*) "ˮ���������쳣����Enter����"
!                    !pause
!                endif
!                if(allocated(ResInfos))deallocate(ResInfos)
!                allocate(ResInfos(NResCK))
!                !dcb InitResInfos   
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select * From ST_ResInfo"//Char(0),SQL_NTSL)
!                
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,m_ResInfo.ResId,0,cbRet)
!                iRet=SQLBindColChar(hSQLStmt,2,SQL_C_Char,m_ResInfo.Stcd,LenStr1,cbStr) 
!                iRet=SQLBindColChar(hSQLStmt,3,SQL_C_Char,m_ResInfo.ResName,LenStr,cbStr) 
!                iRet=SQLBindColR4(hSQLStmt,4,SQL_C_FLOAT,m_ResInfo.ControledArea,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,5,SQL_C_FLOAT,m_ResInfo.Long,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,6,SQL_C_FLOAT,m_ResInfo.lati,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,7,SQL_C_FLOAT,m_ResInfo.PointX,0,cbR4) 
!                iRet=SQLBindColR4(hSQLStmt,8,SQL_C_FLOAT,m_ResInfo.PointY,0,cbR4) 
!                
!                i=0
!                DO While(.true.)
!                    iRet=SQLFetch(hSQLStmt)
!                    if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                    i=i+1
!                    ResInfos(i)=m_ResInfo
!                ENDDO
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!            end subroutine
            
            subroutine DestroyResInfos
                 if(allocated(ResInfos))deallocate(ResInfos)
            end subroutine
    end module        