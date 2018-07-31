! ����������Ϣ
    module ParamRangeMod
    
!        use WaterShedMod
        !use DBSQLLinkMod
        !use StringFiltrMod
        use ReachInfoMod
        
        implicit none
        
        ! �������� @ParamRange.txt
        type ParamRangeClass
            integer npid
            integer ipid
            integer :: pid              ! ��������Id
            integer :: HydroId          ! ˮ��վId
            
            character(30):: ParamRangeName   ! ������������
            real          :: Area             ! �������
            real          UpArea              ! ���������
            
            integer :: SubId            ! ����������
            
            integer :: NUpStreamParamRange 
             
            ! ���ν��ڲ�������
            integer, Dimension(:), allocatable :: UpstreamParamRanges
            
            integer :: NPartSubbasin
            ! ÿ�����������ڵ����������򣬵������Ƶ�������
            integer, Dimension(:), allocatable :: PartSubbasins
            
            integer :: NAllSubbasin
            ! ÿ�������������Ƶ�������������������������
            integer, Dimension(:), allocatable :: AllSubbasins
            
            integer :: Itype           ! 0��ˮ�⣬1��ˮ��վ��
        end type ParamRangeClass
        
        integer,dimension(:,:),allocatable::ParamRangeUpStreamAll
        ! ��������������������Id������
        integer::NParamRangeCK
        type(ParamRangeClass), Dimension(:), allocatable :: ParamRanges
        type(ParamRangeClass)::m_ParamRange
        
        type paramclass
            integer npid
            integer ipid
        endtype paramclass
        type(paramclass)::param
        type(paramclass)::m_param
        contains
!            subroutine InitParamRanges()
!                integer :: i,j,k
!                INTEGER :: LENStr = 28
!                Character(30)::UpStreamParamRangeStr
!                character(28)::PartSubbasinStr
!                integer::Length1,length2,length3
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
                !dcb 
                !���������¼
!                iRet = SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet = SQLExecDirect ( hSQLStmt, "SELECT  count(*) FROM ST_ParamRange"//CHAR(0), SQL_NTSL )
!                iRet = SQLBindColI4 (hSQLStmt,1,SQL_C_SLONG,NParamRangeCK,0,cbRet)
!                iRet = SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                LENStr=NParamRangeCK 
!                
!                if (NParamRangeCK .ne. WaterShed.NParamRange) then
!                    !write(*,*) "���������������쳣����Enter����"
!                    !pause
!                endif
                
!                !����ռ䲢����ֵ
!                if(allocated(ParamRanges))deallocate(ParamRanges)
!                allocate(ParamRanges(NParamRangeCK))
!                if(allocated(m_ParamRange.UpStreamParamRanges))deallocate(m_ParamRange.UpStreamParamRanges)
!                allocate(m_ParamRange.UpStreamParamRanges(NParamRangeCK))
!                m_ParamRange.UpStreamParamRanges=0
!                if(allocated(m_ParamRange.PartSubbasins))deallocate(m_ParamRange.PartSubbasins)
!                allocate(m_ParamRange.PartSubbasins(WaterShed.NSubbasin))                  
!                m_ParamRange.PartSubbasins=0
!                if(allocated(m_ParamRange.AllSubbasins))deallocate(m_ParamRange.AllSubbasins)
!                allocate(m_ParamRange.AllSubbasins(WaterShed.NSubbasin))
!                m_ParamRange.AllSubbasins=0
!                if(allocated(ParamRangeUpStreamAll))deallocate(ParamRangeUpStreamAll)
!                allocate(ParamRangeUpStreamAll(NParamRangeCK,NParamRangeCK))
!                ParamRangeUpStreamAll=0
!                
!                do i=1,NParamRangeCK,1
!                    if(allocated(ParamRanges(i).UpStreamParamRanges))deallocate(ParamRanges(i).UpStreamParamRanges)
!                    allocate(ParamRanges(i).UpStreamParamRanges(NParamRangeCK))
!                    ParamRanges(i).UpStreamParamRanges=0
!                    if(allocated(ParamRanges(i).PartSubbasins))deallocate(ParamRanges(i).PartSubbasins)
!                    allocate(ParamRanges(i).PartSubbasins(WaterShed.NSubbasin))
!                    ParamRanges(i).PartSubbasins=0
!                    if(allocated(paramRanges(i).AllSubbasins))deallocate(paramRanges(i).AllSubbasins)
!                    allocate(paramRanges(i).AllSubbasins(WaterShed.NSubbasin))
!                    paramRanges(i).AllSubbasins=0
!                end do
                !dcb InitParamRanges               
                !��ȡ��¼
!                iRet = SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet = SQLExecDirect ( hSQLStmt, "SELECT  * FROM ST_ParamRange"//CHAR(0), SQL_NTSL )
!                ! bind result columns 
!                iRet = SQLBindColI4 (hSQLStmt,1,SQL_C_SLONG,m_ParamRange.pid,0,cbRet)
!                iRet = SQLBindColI4 (hSQLStmt,2,SQL_C_SLONG,m_ParamRange.HydroId,0,cbRet)
!                iRet = SQLBindColChar (hSQLStmt,3,SQL_C_CHAR,m_ParamRange.ParamRangeName,LENStr,cbStr)
!                iRet = SQLBindColI4 (hSQLStmt,4,SQL_C_SLONG,m_ParamRange.SubId,0,cbRet)
!                iRet = SQLBindColI4 (hSQLStmt,5,SQL_C_SLONG,m_ParamRange.NUpStreamParamRange,0,cbRet)
!                iRet = SQLBindColChar(hSQLStmt,6,SQL_C_CHAR,UpStreamParamRangeStr,lENStr,cbStr)
!                iRet = SQLBindColI4 (hSQLStmt,7,SQL_C_SLONG,m_ParamRange.NPartSubbasin,0,cbRet)
!                iRet = SQLBindColChar(hSQLStmt,8,SQL_C_CHAR,PartSubbasinStr,LENStr,cbStr)
!                iRet = SQLBindColI4 (hSQLStmt,10,SQL_C_SLONG,m_ParamRange.Itype,0,cbRet)
!           
!
!                i = 0
!                DO WHILE (.TRUE.)
!                    iRet = SQLFetch(hSQLStmt)
!                    i = i + 1
!                    !write(*,*) m_ParamRange.pid
!                    IF ( iRet .EQ. SQL_NO_DATA_FOUND ) EXIT
!                    call GetStrSplitterCount(UpStreamParamRangeStr,20,length1,',')
!                    call GetStrSplitterCount(PartSubbasinStr,20,length2,',')
!           
!                    read(UpStreamParamRangeStr,*)    (m_ParamRange.UpStreamParamRanges(j),j=1,length1,1)
!                    read(PartSubbasinStr,*)          (m_ParamRange.PartSubbasins(j),j=1,length2,1)
!                    ParamRanges(i)=m_ParamRange
!                    ParamRanges(i).Area = 0.
!                    do j = 1,ParamRanges(i).NPartSubbasin
!                        ParamRanges(i).Area = ParamRanges(i).Area + Reachs(ParamRanges(i).PartSubbasins(j)).Area
!                    enddo
!                                
!                END DO
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                ! ����NAllSubbasin��AllSubbasins
!                DO i=1,NParamRangeCK
!                    !��i������Ӧ���������в�����������¼��ParamRangeUpStreamAll��
!                    call GetParamRangeUpStream(i,i)
!
!                    do j=1,ParamRanges(i).NPartSubbasin
!                        ParamRanges(i).AllSubbasins(ParamRanges(i).PartSubbasins(j))=1
!                    end do
!
!                    do k=1,NParamRangeCK
!                        if(ParamRangeUpStreamAll(i,k).eq.1) then
!                            do j=1,ParamRanges(k).NPartSubbasin
!                                ParamRanges(i).AllSubbasins(ParamRanges(k).PartSubbasins(j))=1
!                            enddo
!                        end if
!                    end do
!                END DO                
       
!            end subroutine
                    

            !�ҵ��������в�������������ParamRangeUpStreamAll��
            Recursive subroutine GetParamRangeUpStream(i,upi)
                
                integer::i,j,upi
                if(ParamRanges(upi).NUpStreamParamRange.ne.0) then
                    do j=1,ParamRanges(upi).NUpStreamParamRange
                        ParamRangeUpStreamAll(i,ParamRanges(upi).UpStreamParamRanges(j))=1
                        if(ParamRanges(ParamRanges(upi).UpStreamParamRanges(j)).NUpStreamParamRange.ne.0) then
                            call GetParamRangeUpStream(i,ParamRanges(upi).UpStreamParamRanges(j))
                        end if 
                    end do
                else
                end if

                return
            end subroutine

            subroutine DestroyParamRanges
                if(allocated(ParamRanges))deallocate(ParamRanges)
                if(allocated(ParamRangeUpStreamAll))deallocate(ParamRangeUpStreamAll)
                
            end subroutine
    end module        