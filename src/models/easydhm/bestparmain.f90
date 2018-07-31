subroutine bestparmain(nopt,RunType,CurRunoffGenType,Solution,iobj,isens,npid,ipid,sid)
		use temptype
		use dhmtype
		use SolutionMod	
		use SingleChangePara  
		use ParamRangeMod  
		use HydroObservedMod
		use HydroDataMod
		use WeatherMod
        use AutoTimes
		implicit none
		integer nopt,iobj,isens, RunType,CurRunoffGenType,solution,ipid,npid,j,uppid,k,maxfun,ncount,ncountpid,sid,iipid
		real best(nopt) !存放最佳参数的变量
		character*10 parname(nopt)
		real sensfun1rank(nopt,2),of(1,1),of2(1,1),xj(1,1),xj2(1,1),totof(1,npid+1),totofRes(1,npid+1),simrunoff(NAllSeries,npid),obsrunoff(NAllSeries,npid)
        integer iflood

	    call GetStartTime   
		if(nopt .ne.0) then
		    ncountpid=0
            call readchangepar(nopt,parname,RunType,CurRunoffGenType,Solution,sensfun1rank,ipid)	
            call readbestpar(nopt,best,RunType,CurRunoffGenType,Solution,ipid)
            call changemodPar(best,nopt,CurRunoffGenType)	

            AdjustRunoff(1:NAllSeries,1)=TotalAdjustRunoff(1:NAllSeries,1,totalSolution(ipid)%IParamRange)
            ncount=0
            do k=1,NAllSeries
                if(AdjustRunoff(k,1)<=0)then
                    ncount=ncount+1
                endif
            enddo
            call run(RunType,CurRunoffGenType,ipid)

	        do k=1,NAllSeries
	            simrunoff(k,ipid)=AdjustRunoff(k,2)*3600*Cursolution%dt*1000/ParamRanges(totalSolution(ipid)%IParamRange)%Area
	            obsrunoff(k,ipid)=AdjustRunoff(k,1)*3600*Cursolution%dt*1000/ParamRanges(totalSolution(ipid)%IParamRange)%Area
            enddo

            call GetEndTime
		endif

		if(allocated(iiname))deallocate(iiname)
        if(allocated(imet))deallocate(imet)
        if(allocated(bbound))deallocate(bbound)
        if(allocated(objirde))deallocate(objirde)
        if(allocated(respirde))deallocate(respirde)
    end subroutine