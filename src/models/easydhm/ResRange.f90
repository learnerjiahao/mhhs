 ! 水库分区信息
    module ResRangeMod
        !use DBSQLLinkMod
        use WaterShedMod
        use ParamRangeMod
        !use StringFiltrMod
        
        implicit none
        
        type ResRangeClass
            integer :: ResRangeId
            character(30)::Stnm
            integer :: RunYear
            integer::IsInObs
            integer::IsOutObs
            integer::locSub
            integer::locParamRange
            
            integer::NPartSubbasin
            ! 每个水库分区内的所有子流域，单独控制的子流域
            integer, Dimension(:), allocatable :: PartSubbasins
            
            ! 每个水库分区控制的所有子流域，上游所有子流域
            integer, Dimension(:), allocatable :: AllSubbasins
            
            integer::NUpStreamResRange
            ! 上游紧邻水库分区
            integer, Dimension(:), allocatable :: UpstreamResRanges
        end type ResRangeClass
        
       
        ! 水库分区，按水库分区Id来保存
        
        
        integer,dimension(:,:),allocatable::UpStreamResRangesAll
        type(ResRangeClass), Dimension(:), allocatable :: ResRanges 
        type(ResRangeClass)::m_ResRange
        integer::NResRangeCK
        
        contains
        
!            subroutine InitResRanges
!                integer::lenStr=20,lenStr1
!                integer::lenStr2
!                integer::i,j,k,length1,length2
!                character(60)::PartSubbasinString
!                character(20)::UpStreamResRangeString
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLStmt
                
!                lenStr1=60
!                lenStr2=WaterShed.NResRange*2
                !dcb
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select Count(*) From ST_ResRange"//Char(0),SQL_NTSL)
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,NResRangeCK,0,cbRet)
!                iRet=SQLFetch(hSQLStmt)
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
                 !分配内存空间
!                if (NResRangeCK .ne. WaterShed.NResRange) then
!                    !write(*,*) "水库数出现异常，按Enter继续"
!                    !pause
!                endif               
!                if(allocated(ResRanges))deallocate(ResRanges)
!                allocate(ResRanges(NResRangeCK))
!                if(allocated(UpStreamResRangesAll))deallocate(UpStreamResRangesAll)
!                allocate(UpStreamResRangesAll(NResRangeCK,NResRangeCK))
!                UpStreamResRangesAll=0
!                if(allocated(m_ResRange.PartSubbasins))deallocate(m_ResRange.PartSubbasins)
!                allocate(m_ResRange.PartSubbasins(WaterShed.NSubbasin))
!                m_ResRange.PartSubbasins=0
!                if(allocated(m_ResRange.AllSubbasins))deallocate(m_ResRange.AllSubbasins)
!                allocate(m_ResRange.AllSubbasins(WaterShed.NSubbasin))
!                m_ResRange.AllSubbasins=0
!                if(allocated(m_ResRange.UpStreamResRanges))deallocate(m_ResRange.UpStreamResRanges)
!                allocate(m_ResRange.UpStreamResRanges(NResRangeCK))
!                m_ResRange.UpStreamResRanges=0
!                
!                do i=1,NResRangeCK
!                    if(allocated(ResRanges(i).PartSubbasins))deallocate(ResRanges(i).PartSubbasins)
!                    allocate(ResRanges(i).PartSubbasins(WaterShed.NSubbasin))
!                    ResRanges(i).PartSubbasins=0
!                    if(allocated(ResRanges(i).AllSubbasins))deallocate(ResRanges(i).AllSubbasins)
!                    allocate(ResRanges(i).AllSubbasins(WaterShed.NSubbasin))
!                    ResRanges(i).AllSubbasins=0
!                    if(allocated(ResRanges(i).UpStreamResRanges))deallocate(ResRanges(i).UpStreamResRanges)
!                    allocate(ResRanges(i).UpStreamResRanges(NResRangeCK))
!                    ResRanges(i).UpStreamResRanges=0
!                end do
                !dcb InitResRanges
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!                iRet=SQLExecDirect(hSQLStmt,"Select * From ST_ResRange"//Char(0),SQL_NTSL)
!                
!                iRet=SQLBindColI4(hSQLStmt,1,SQL_C_SLONG,m_ResRange.ResRangeId,0,cbRet)
!                iRet=SQLBindColChar(hSQLStmt,2,SQL_C_Char,m_ResRange.stnm,LenStr,cbStr)
!                iRet=SQLBindColI4(hSQLStmt,3,SQL_C_SLONG,m_ResRange.RunYear,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,4,SQL_C_SLONG,m_ResRange.IsInObs,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,5,SQL_C_SLONG,m_ResRange.IsOutObs,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,6,SQL_C_SLONG,m_ResRange.LocSub,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,7,SQL_C_SLONG,m_ResRange.LocParamRange,0,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,8,SQL_C_SLONG,m_ResRange.NPartSubbasin,0,cbRet)
!                iRet=SQLBindColChar(hSQLStmt,9,SQL_C_CHAR,PartSubbasinString,lenStr1,cbRet)
!                iRet=SQLBindColI4(hSQLStmt,10,SQL_C_SLONG,m_ResRange.NUpStreamResRange,0,cbRet)
!                iRet=SQLBindColChar(hSQLStmt,11,SQL_C_CHAR,UpStreamResRangeString,lenStr2,cbRet)
!                
!                i=0
!                DO WHILE(.true.)
!                    iRet=SQLFetch(hSQLStmt)
!                    i=i+1
!                    if(iRet.eq.SQL_NO_DATA_FOUND) EXIT
!                    call GetStrSplitterCount(PartSubbasinString,60,length1,' ')
!                    call GetStrSplitterCount(UpStreamResRangeString,lenStr2,length2,' ')
!                    
!                    read(PartSubbasinString,*) (m_ResRange.PartSubbasins(j),j=1,length1,1)
!                    read(UpStreamResRangeString,*) (m_ResRange.UpStreamResRanges(j),j=1,length2,1) 
!                    
!                    ResRanges(i)=m_ResRange
!                ENDDO
!                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )
!                DO i=1,NResRangeCK,1
!                    call GetUpStreamResRangesAll(i,i)
!                    do j=1,ResRanges(i).NPartSubbasin,1
!                        ResRanges(i).AllSubbasins(ResRanges(i).PartSubbasins(j))=1
!                    end do
!                    Do k=1,NResRangeCK,1
!                        if(UpStreamResRangesAll(i,k).eq.1) then
!                            do j=1,ResRanges(k).NPartSubbasin,1
!                                ResRanges(i).AllSubbasins(ResRanges(k).PartSubbasins(j))=1
!                            enddo
!                        endif
!                    Enddo
!                ENDDO
!            end subroutine
            
            Recursive subroutine GetUpStreamResRangesAll(i,upi)
                integer::i,j,upi
                if(ResRanges(upi).NUpStreamResRange.ne.0) then
                    do j=1,ResRanges(upi).NUpStreamResRange,1
                        UpStreamResRangesAll(i,ResRanges(upi).UpStreamResRanges(j))=1
                        if(ResRanges(ResRanges(upi).UpStreamResRanges(j)).NUpStreamResRange.ne.0) then
                            call GetUpStreamResRangesAll(i,ResRanges(upi).UpStreamResRanges(j))
                        endif
                    enddo
                else
                endif

            end subroutine
            
            subroutine DestroyResRanges
                if(allocated(ResRanges))deallocate(ResRanges)
                if(allocated(UpStreamResRangesAll))deallocate(UpStreamResRangesAll)
            end subroutine
            
            
    end module        