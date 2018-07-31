subroutine readbestpar(nopt,best,RunType,CurRunoffGenType,Solution,ipid)
!!    use DBSQLLinkMod	
    use temptype
    use dhmtype
    use SingleChangePara
    implicit none
    integer nopt,i,j
    real tempbest,best(nopt)
!    character*10 chariiname,CharSolutionId,CharCurRunoffGenType
!    INTEGER (SQLHANDLE) :: hSQLStmt
!    character*500 string
    integer Solution,CurRunoffGenType,RunType,ipid
!	write(CharSolutionId,'(i10)') Solution	
!	write(CharCurRunoffGenType,'(i10)')CurRunoffGenType
	
    
   
     do i=1,nopt
         if(allocated(sinChangeParas))then
            do j = 1,sinChangeParaNck
                if((sinChangeParas(j,ipid).IParaYear .eq. Solution) .and. (sinChangeParas(j,ipid).IRunoffRenType .eq. CurRunoffGenType) .and. (sinChangeParas(j,ipid).IsParasolUsed .eq.1) .and. sinChangeParas(j,ipid).in1 .eq. iiname(i,1))then
                    best(i) = sinChangeParas(j,ipid).Bestpar   
                end if
            end do 
         end if
!        iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbc,hSQLStmt)
!        write(chariiname,'(i10)') iiname(i)
!        write(string,100)"Select * From ST_SingleChangePara  where solutionid = ", trim(adjustl(CharSolutionId)) ," and irunoffgentype=",trim(adjustl(CharCurRunoffGenType))," and IsSenseUsed=1 and in1=",trim(adjustl(chariiname)),Char(0)
!        100format(a,a,a,a,a,a,a)
!        iRet=SQLExecDirect(hSQLStmt,string,SQL_NTSL)
!        iRet = SQLBindColR4 (hSQLStmt, 12, SQL_C_FLOAT, tempbest, 0, cbR4 )
!        iRet=SQLFetch(hSQLStmt)
!        iRet = SQLFreeStmt( hSQLStmt, SQL_UNBIND )
!        best(i)=tempbest
!        iRet=SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmt )     
    end do
    
    

end subroutine