  subroutine automet(RunType,CurRunoffGenType,SolutionID,npid,sid)
        use SolutionMod
        integer RunType,CurRunoffGenType,SolutionID,iobj,isens,npid,sid
        
!        call telobjresp(iobj,isens)		! iobj	要进行参数自动率定的目标参数个数及相应的方法
!        if (Runtype == 2) then
!            call senstivity(RunType,CurRunoffGenType,sid,iobj,isens,npid)
!        elseif(Runtype == 3) then
!            if(Cursolution.TimeStepOpt == 0) then
!                call DDSmain(SolutionID,SID,Runtype,CurRunoffGenType,iobj,isens,npid)
!            elseif(Cursolution.TimeStepOpt == 1) then
!                call parasol(RunType,CurRunoffGenType,SolutionID,iobj,isens,npid)
!            elseif(Cursolution.TimeStepOpt == 2) then
!                call GAmain(SolutionID,Runtype,CurRunoffGenType,iobj,isens,npid)
!!                call mosce_uacd(RunType,CurRunoffGenType,sid,npid)
!            elseif(Cursolution.TimeStepOpt == 3) then
!                call DDSmain(SolutionID,Runtype,CurRunoffGenType)
!            endif
            
!            Runtype = 5
!            call ReRunBestpar(RunType,CurRunoffGenType,SolutionID,iobj,isens,npid,sid)
!        elseif(Runtype==31)then
!            call DDShydroresmain(RunType,CurRunoffGenType,SolutionID,sid)
        if(Runtype==5)then
            call ReRunBestpar(RunType,CurRunoffGenType,sid,iobj,isens,npid,sid,SolutionID)
!        elseif(Runtype==8) then
!            Runtype = 2
!            call senstivity(RunType,CurRunoffGenType,SolutionID,iobj,isens,npid)
!            Runtype = 3
!            if(Cursolution.TimeStepOpt == 0) then
!                call DDSmain(SolutionID,sid,Runtype,CurRunoffGenType,iobj,isens,npid)
!            elseif(Cursolution.TimeStepOpt == 1) then
!                call parasol(RunType,CurRunoffGenType,SolutionID,iobj,isens,npid)
!                call mopso_cd(RunType,CurRunoffGenType,sid,npid)
!                call mosce_uacd(RunType,CurRunoffGenType,sid,npid)
!            elseif(Cursolution.TimeStepOpt == 2) then
!                call GAmain(SolutionID,Runtype,CurRunoffGenType,iobj,isens,npid)
!            elseif(Cursolution.TimeStepOpt == 3) then
!                call mopso_cd(RunType,CurRunoffGenType,SolutionID,npid)
!            elseif(Cursolution.TimeStepOpt == 4) then
!                call ipso_cd(RunType,CurRunoffGenType,SolutionID,npid)

!                call mosce_uacd(RunType,CurRunoffGenType,sid,npid)
!            endif
!            Runtype = 5
!            call ReRunBestpar(RunType,CurRunoffGenType,sid,iobj,isens,npid,sid,SolutionID)
!        elseif(Runtype==51)then
!            call DDSresmain(RunType,CurRunoffGenType,SolutionID,sid)        
!        elseif(Runtype==52)then
!            call ReRunBestpar(RunType,CurRunoffGenType,sid,iobj,isens,npid,sid,SolutionID)
        endif
        
    end
    
    subroutine ReRunBestpar(RunType,CurRunoffGenType,SolutionID,iobj,isens,npid,sid,solution)       
!		use DBSQLLinkMod
		use temptype
		use SingleChangePara	
        use SolutionMod	
		implicit none
		
        integer RunType,CurRunoffGenType,nopt,sid		
		integer iobj,isens
	    integer RunType1,CurRunoffGenType1,SolutionID,Solution,npid,ipid   
		integer ncount,i
		
		character*10  pname,CharSolutionId,CharCurRunoffGenType
	    
!        INTEGER (SQLHANDLE) :: hSQLStmt
	    
	    ipid = cursolution.ithpid
        nCount=0
        if(allocated(sinChangeParas))then
             do i = 1,sinChangeParaNparameter
                if((Solution .eq. sinChangeParas(i,ipid).IParaYear) .and. (CurRunoffGenType .eq. sinChangeParas(i,ipid).IRunoffRenType) .and.( 1 .eq.sinChangeParas(i,ipid).IsParasolUsed)) nCount = nCount + 1
             end do 
             nopt = nCount
        end if

		
        call bestparmain(nopt,RunType,CurRunoffGenType,Solution,iobj,isens,npid,ipid,sid)               		
		
    end

