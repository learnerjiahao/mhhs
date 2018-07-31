module ST_PR_OutMod
    use SolutionMod
    implicit none
    type ST_PRSim
        character(19)   ::PR_TM
        integer         ::PR_sid,PR_IRunoffGenType,PR_pid
        real            ::PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,PR_Emm,PR_Uprain  
    end type ST_PRSim
    
    type(ST_PRSim),dimension(:),allocatable   ::outST_PR
    
    integer ::ST_PRNCK=0
    integer ::outIdx=1
    contains 
    
    subroutine insertST_PR(TM,sid,IRunoffGenType,pid,Pmm,Rsimms,Rmeams,Rsimmm,Rmeamm,PETmm,Uprainmm)
        type(ST_PRSim) ::st_pr
        character(19)   ::TM
        integer         ::sid,IRunoffGenType,pid
        real            ::Pmm,Rsimms,Rmeams,Rsimmm,Rmeamm,PETmm,Emm,Uprainmm
        st_pr.PR_TM                   = TM      
        st_pr.PR_sid                  = sid     
        st_pr.PR_IRunoffGenType       = IRunoffGenType            
        st_pr.PR_pid                  = pid    
        st_pr.PR_Pmm                  = Pmm   
        st_pr.PR_Rsimms               = Rsimms     
        st_pr.PR_Rmeams               = Rmeams        
        st_pr.PR_Rsimmm               = Rsimmm     
        st_pr.PR_Rmeamm               = Rmeamm     
        st_pr.PR_PETmm                = PETmm          
        st_pr.PR_Uprain                  = Uprainmm          
        if(Rmeams<=0.001) then
            st_pr.QFinal =  Rsimms
        else
            st_pr.QFinal =  Rmeams
        endif
        ST_PRNCK  = ST_PRNCK + 1 
        outST_PR(ST_PRNCK) = st_pr
        
    endsubroutine
    
    Function GegPrDataPR(PR_TM,PR_sid,PR_IRunoffGenType,PR_pid,PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal ,Uprainmm)result(N)
!        use SolutionMod
        character(19)   ::PR_TM
        integer         ::PR_sid,PR_IRunoffGenType,PR_pid,N
        real            ::PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal ,Uprainmm
        N = 1
        
        if(allocated(outST_PR))then
            if(outIdx.gt. ST_PRNCK)then
                N=0
                return
            endif
            PR_TM = outST_PR(outIdx).PR_TM
            PR_sid = outST_PR(outIdx).PR_sid
            PR_IRunoffGenType = outST_PR(outIdx).PR_IRunoffGenType
            PR_pid = outST_PR(outIdx).PR_pid
            PR_Pmm = outST_PR(outIdx).PR_Pmm
            PR_Rsimms = outST_PR(outIdx).PR_Rsimms
            PR_Rmeams = outST_PR(outIdx).PR_Rmeams
            PR_Rsimmm = outST_PR(outIdx).PR_Rsimmm
            PR_Rmeamm = outST_PR(outIdx).PR_Rmeamm
            PR_PETmm = outST_PR(outIdx).PR_PETmm
            Uprainmm = outST_PR(outIdx).PR_Uprain
            ! QMs¸ÄÎªEmm lwh
            Qms= outST_PR(outIdx).QMs
            QFinal = outST_PR(outIdx).QFinal
            outIdx = outIdx + 1
        else 
            N = -1
        end if  
    end Function
     
    subroutine DestroyST_PR
        if(allocated(outST_PR))then
        deallocate(outST_PR)
        ST_PRNCK = 0
        outIdx = 1
        endif
    end subroutine
end module