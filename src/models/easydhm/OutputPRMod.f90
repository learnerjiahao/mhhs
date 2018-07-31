module OutputPRMod
!        use DBSQLLinkMod
        use SolutionMod
        use TimeInfoMod
        use WeatherMod
        use ParamRangeMod
        use ReachVarMod
        use HydroObservedMod
        use ResRangeMod
        use ResVarMod
        use WaterShedMod
        use HydroDataMod
        
        integer IParamRange, NParamRange, ISubbasin,IHydro
        
        contains
            subroutine OutputPR()
                IParamRange = CurSolution.IParamRange
                ISubbasin   = ParamRanges(IParamRange).SubId
                IHydro      = ParamRanges(IParamRange).HydroId
        		
	            return
            end subroutine
            
            subroutine OutputRangePR(CurRunoffGenType,i,dataTemp,ipid)
		        integer i,CurRunoffGenType,ipid
                character(19) ::dataTemp
!                INTEGER (SQLHANDLE) :: hSQLDbs
                
!                call SqlExec(CurRunoffGenType,hSQLDbs,dataTemp,ipid)
                call SqlExec(0,dataTemp,ipid)
		        return
	        end subroutine
        
	        subroutine SqlExec(CurRunoffGenType,dataTemp,ipid)
	            use AutoTimes
	            use ST_PR_OutMod
	            integer CurRunoffGenType,i,ipid,j
	            character(19) ::dataTemp
!                INTEGER (SQLHANDLE) :: hSQLStmtTemp
!                INTEGER (SQLRETURN) iRet
!                INTEGER (SQLHANDLE) :: hSQLDbs
                real area,rsimmm,rmeamm
!                character(1000) psLink
                IParamRange = totalSolution(ipid).IParamRange
                ISubbasin   = ParamRanges(IParamRange).SubId
                IHydro      = ParamRanges(IParamRange).HydroId
                area = ParamRanges(IParamRange).UpArea
                if(ParamRanges(totalSolution(ipid)%IParamRange)%Itype .eq. 0)then ! к╝©Б
                    rsimmm = ReachVars(ISubbasin).flwoutN*Cursolution.dt*1000*3600/area
                    if(HydroObserveds(iLoop).QObserved(watershed.nhydrostation+IHydro)>0.001) then
                        rmeamm = HydroObserveds(iLoop).QObserved(watershed.nhydrostation+IHydro)*Cursolution.dt*1000*3600/area
                    else
                        rmeamm = 0.
                    endif
                    call insertST_PR(dataTemp,totalSolution(ipid)%SolutionId,CurRunoffGenType,totalSolution(ipid).IParamRange, p_Weather(IParamRange).rain, ReachVars(ISubbasin).flwoutN,HydroObserveds(iLoop).QObserved(watershed.nhydrostation+IHydro), rsimmm,rmeamm,p_Weather(IParamRange).PET,Upallrain(ipid,iloop))
                elseif(ParamRanges(totalSolution(ipid)%IParamRange)%Itype .eq. 1)then  !к╝нд 
                    rsimmm = ReachVars(ISubbasin).flwoutN*Cursolution.dt*1000*3600/area
                    if(HydroObserveds(iLoop).QObserved(IHydro)>0.001) then
                        rmeamm = HydroObserveds(iLoop).QObserved(IHydro)*Cursolution.dt*1000*3600/area
                    else
                        rmeamm = 0.
                    endif
                    call insertST_PR(dataTemp,totalSolution(ipid)%SolutionId,CurRunoffGenType,totalSolution(ipid).IParamRange,p_Weather(IParamRange).rain, ReachVars(ISubbasin).flwoutN,HydroObserveds(iLoop).QObserved(IHydro),rsimmm,rmeamm,p_Weather(IParamRange).PET,Upallrain(ipid,iloop))
               
                endif
                !dcb1
!                iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbs,hSQLStmtTemp)
!                
!                write(psLink,'(a,a,a,3(i,","),5(f10.4,","),f10.4,a,a)')"INSERT INTO ST_PR ([TM],sid,IRunoffGenType,pid,[P(mm)],[Rsim(m3/s)],[Rmea(m3/s)],[Rsim(mm)],[Rmea(mm)],[PET(mm)]) VALUES ( '",dataTemp,"',",CurSolution%SolutionId,CurRunoffGenType,ISubbasin	,p_Weather(IParamRange).rain, ReachVars(ISubbasin).flwout,HydroObserveds(iLoop).QObserved(IHydro), ReachVars(ISubbasin).flwout*8.64E7/ParamRanges(IParamRange).Area,HydroObserveds(iLoop).QObserved(IHydro)*8.64E7/ParamRanges(IParamRange).Area,p_Weather(IParamRange).PET,")",char(0)       
!	            iRet = SQLExecDirect( hSQLStmtTemp, psLink, SQL_NTSL )
!	            iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmtTemp )
	            !dcb1
!	            iRet=SQLAllocHandle(SQL_HANDLE_STMT,hSQLDbs,hSQLStmtTemp)
!	            if(CurRunoffGenType .eq. 0)then
!	                if(HydroObserveds(iLoop).QObserved(IHydro) > 0)then
!	                    do i = 1 ,ST_PRNCK
!	                        if(outST_PR(i).PR_IRunoffGenType .eq. 0)outST_PR(i).QFinal = outST_PR(i).PR_Rmeams
!	                    end do 
!!    	                write(psLink,'(a)')"update ST_PR set [QFinal(m3/s)] = [Rmea(m3/s)] WHERE IRunoffGenType = 0 and [Rmea(m3/s)] >0.0"   
!!	                else
!!    	                write(psLink,'(a)')"update ST_PR set [QFinal(m3/s)] = [Rsim(m3/s)] WHERE IRunoffGenType = 0 and [Rmea(m3/s)] <=0.0" 
!	                endif
!!	                iRet = SQLExecDirect( hSQLStmtTemp, psLink, SQL_NTSL )
!!	                iRet = SQLFreeHandle( SQL_HANDLE_STMT, hSQLStmtTemp )
!	            endif

	        end subroutine	        

            
    end module                      