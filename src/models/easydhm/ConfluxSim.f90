subroutine ConfluxSim(CurRunoffGenType,ThisYear,ThisMonth,Thisday,ipid,iloop1)
		use SolutionMod
		use ParamRangeMod
		use ReachVarMod
		use ResVarMod
		use ReachParamMod
		use weatherMod
		use HydroDataMod
		use ResRangeMod
        use InitDHMStateMod
        use ResInfoMod
        use ReachInfoMod    
      
		
		implicit none
		
		integer  IX,CurRunoffGenType,i,j,IParamRange
		character(30) ::dataTemp
		integer ThisYear,ThisMonth,Thisday,ipid,iloop1
        real coef
		real psum
        integer IP
        
		!real tmp,tmpqs,tmpqi,tmpqg,tmpes,tmpep,empeg,empe
			
		!flwin=0
		!flwout=0
!		tmp=0
!		tmpqs=0
!		tmpqi=0
!		tmpqg=0
!		tmpes=0
!		tmpep=0
!		tmpeg=0
!		tmpe=0
		
		IParamRange = CurSolution%IParamRange
		
	    do j=1, ParamRanges(IParamRange)%NPartSubbasin
			IX= ParamRanges(IParamRange)%PartSubbasins(j)		
		    		    
			!设置当期计算单元的参数
		    call GetReachParam(IParamRange,IX)
		    
			! 逐河段，从上游到下游，进行河道演进计算
			call route(IX,ipid)
            if(Cursolution%dt >= 25) then
                if(IX == ParamRanges(IParamRange)%SubId) then
                    ReachVars(IX)%flwout = max(0.001,ReachVars(IX)%flwout + ReachVars(IX)%qg) !flwout:flow out of reach on previous day
                    ReachVars(IX)%flwin = max(0.001,ReachVars(IX)%flwin + ReachVars(IX)%qg)
                endif
            else
                ReachVars(IX)%flwout = max(0.001,ReachVars(IX)%flwout + ReachVars(IX)%qg) !flwout:flow out of reach on previous day
                ReachVars(IX)%flwin = max(0.001,ReachVars(IX)%flwin + ReachVars(IX)%qg)                
            endif
			
			! 水库调度 
!            if(cursolution.runtype==51 .or.cursolution.runtype==52.or.cursolution.runtype == 31)then
!		        do i = 1,NResOptSolution
!		            if (restype(i)%ResDHMID == IX ) then
!                            restype(i)%resQin=ReachVars(IX)%flwout
!		            endif
!		        enddo
!    		    
!                ReachVars(ix)%res1Qout=0
!                ReachVars(ix)%res2Qout=0
!                ReachVars(ix)%res3Qout=0
!                ReachVars(ix)%res1Qin=0
!                ReachVars(ix)%res2Qin=0
!                ReachVars(ix)%res3Qin=0
!    		    
!		        ReachVars(ix)%Area1rat=0		    
!                ReachVars(ix)%Area2rat=0		
!                ReachVars(ix)%Area3rat=0		
!		        do i = 1,NResOptSolution
!		            if (restype(i)%ResDHMID == IX ) then
!                        if(restype(i)%STORETYPE==1)then
!                            restype(i)%NormStorage=restype(i)%NormSt*restype(i)%StoreMod
!                            restype(i)%Storage=restype(i)%St*restype(i)%StoreMod
!                            restype(i)%DeadStorage=restype(i)%DeadSt*restype(i)%StoreMod
!                        else
!                            restype(i)%NormStorage=restype(i)%NormSt
!                            restype(i)%Storage=restype(i)%St
!                            restype(i)%DeadStorage=restype(i)%DeadSt
!                        endif
!                        call resopt2(i,IX,ThisYear,ThisMonth,Thisday,iloop1,ipid)
!                        if ( CurSolution.runtype == 52) then
!                            write(52326,'(i3,i3,i4,6F10.3,3F15.3)') IX,restype(i)%STORETYPE,iloop1,ReachVars(IX)%res1Qin,ReachVars(IX)%res1Qout,ReachVars(IX)%res2Qin,ReachVars(IX)%res2Qout,ReachVars(IX)%res3Qin,ReachVars(IX)%res3Qout,ReachVars(IX)%CurStorage1,ReachVars(IX)%CurStorage2,ReachVars(IX)%CurStorage3
!                        endif
!
!		            endif
!		        enddo
!		        do i = 1,NResOptSolution
!                    if(restype(i)%ResDHMID==IX .and.ReachVars(ix)%Area1rat>0 .and. ReachVars(ix)%Area2rat==0 .and.ReachVars(ix)%Area3rat==0)then
!                        ReachVars(ix)%flwout = ReachVars(IX)%res1Qout+restype(i)%resQin*(1-ReachVars(ix)%Area1rat)
!                    endif
!		        enddo
!		    endif
            
!			if (CurSolution.IRunoffGenType == 1 .or. CurSolution.IRunoffGenType == 2) then							!壤中流参与河道汇流				
!		        !s_qi(IX)   = s_ri(IX)*Subbasins(IX)%Area/1000/dt/3600
!		        !s_qs(IX)   = s_rs(IX)*Subbasins(IX)%Area/1000/dt/3600
!			else					
!				!壤中流不参与河道汇流
!				!s_qs(IX) = s_rs(IX)*Subbasins(IX)%Area/1000/dt/3600
!			endif
            
!            if(Reachs(IX)%IStore == 1) then
!                if(ReachVars(IX)%flwout*Reachs(IX)%StoreCoef>Reachs(IX)%Qlimit) then
!                    ReachVars(IX)%AQStore = ReachVars(IX)%flwout - Reachs(IX)%Qlimit/Reachs(IX)%StoreCoef
!                    ReachVars(IX)%flwout = Reachs(IX)%Qlimit/Reachs(IX)%StoreCoef
!                    
!                else
!                    ReachVars(IX)%AQStore = 0
!                endif
!                ReachVars(IX)%AVStore = ReachVars(IX)%AQStore * Cursolution.dt*3600/1000000
!            endif
!
!            if(Reachs(IX)%IDive == 1) then
!                if(ReachVars(IX)%flwout>Reachs(IX)%QDive) then
!                    ReachVars(IX)%flwout = ReachVars(IX)%flwout - Reachs(IX)%QDive
!                else
!                    ReachVars(IX)%flwout = 0
!                endif
!            endif
!            ModFlood = 0
!            psum = 0.0
!            do ip = 1,iloop
!                psum = psum + Upallrain(ip)
!            enddo
!            psum = sum(Upallrain(1,1:iloop))
!            if (psum>=100) ModFlood = -0.5
!            if(psum>=150) ModFlood = -0.7
!            if(psum>=200) ModFlood = -1
!            if(IX == ParamRanges(IParamRange)%SubId) then
!                
!                coef =  ReachVars(IX)%qg / ReachVars(IX)%flwin
!                if(iloop < (8*24/Cursolution.dt)) then
!                    
!                    if(ParamRanges(IParamRange)%Itype ==0) then
!                        if(HydroObserveds(iLoop)%QObserved(watershed.nhydrostation+ParamRanges(IParamRange)%HydroID)<=0.001) then
!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ
!                        else
!                            ReachVars(IX)%AveiniQ = min(ReachVars(IX)%AveiniQ,HydroObserveds(iLoop)%QObserved(watershed.nhydrostation + ParamRanges(IParamRange)%HydroId))
!                        endif
!                    else
!                        if(HydroObserveds(iLoop)%QObserved(ParamRanges(IParamRange)%HydroID)<=0.001) then
!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ
!                        else
!                            ReachVars(IX)%AveiniQ = min(ReachVars(IX)%AveiniQ,HydroObserveds(iLoop)%QObserved(ParamRanges(IParamRange)%HydroId))
!                        endif
!                    endif
!                    
!                    
!!                    if(ReachVars(IX)%flwout <  ReachVars(IX)%AveiniQ .and. ReachVars(IX)%flwout> ReachVars(IX)%upriverin) then
!!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwoutN * 0.96
!!                    else
!                    ReachVars(IX)%flwoutN = ReachVars(IX)%flwout
!
!!                        if(ReachVars(IX)%flwoutN .lt. ReachVars(IX)%AveiniQ) then
!!                            ReachVars(IX)%flwoutN = ReachVars(IX)%AveiniQ
!!                        endif
!!                    endif
!
!!                    if(ReachVars(IX)%flwout >  ReachVars(IX)%flwoutN*2) then
!!                        
!!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwout * (1 - ModFlood)
!!
!!!                        if(ReachVars(IX)%flwoutN .lt. ReachVars(IX)%AveiniQ) then
!!!                            ReachVars(IX)%flwoutN = ReachVars(IX)%AveiniQ
!!!                        endif
!!                    endif
!!                if(iloop < (24/Cursolution.dt)*3) then
!!                    
!!                    if(ParamRanges(IParamRange)%Itype ==0) then
!!                        if(HydroObserveds(iLoop)%QObserved(watershed.nhydrostation+ParamRanges(IParamRange)%HydroID)<=0.001) then
!!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ*(iLoop-1) + ReachVars(IX)%flwout
!!                        else
!!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ*(iLoop-1) + HydroObserveds(iLoop)%QObserved(watershed.nhydrostation + ParamRanges(IParamRange)%HydroId)
!!                        endif
!!                    else
!!                        if(HydroObserveds(iLoop)%QObserved(ParamRanges(IParamRange)%HydroID)<=0.001) then
!!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ*(iLoop-1) + ReachVars(IX)%flwout
!!                        else
!!                            ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ*(iLoop-1) + HydroObserveds(iLoop)%QObserved(ParamRanges(IParamRange)%HydroId)
!!                        endif
!!                    endif
!!                    ReachVars(IX)%AveiniQ = ReachVars(IX)%AveiniQ / iloop
!!                    
!!                    if(ReachVars(IX)%flwout <  ReachVars(IX)%AveiniQ .and. ReachVars(IX)%flwout> ReachVars(IX)%upriverin) then
!!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwoutN * 0.96
!!                    else
!!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwout
!!
!!!                        if(ReachVars(IX)%flwoutN .lt. ReachVars(IX)%AveiniQ) then
!!!                            ReachVars(IX)%flwoutN = ReachVars(IX)%AveiniQ
!!!                        endif
!!                    endif
!
!                else
!                     
!                    if(ReachVars(IX)%flwout <  ReachVars(IX)%AveiniQ*1.2 .and. ReachVars(IX)%flwout> ReachVars(IX)%upriverin) then
!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwoutN * 0.96
!!                        if (ReachVars(IX)%flwoutN <  ReachVars(IX)%AveiniQ*0.9) then
!!                            ReachVars(IX)%flwoutN = ReachVars(IX)%AveiniQ*0.9
!!                        endif
!                    else 
!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwout
!                    endif
!                    
!
!                endif
!            ReachVars(IX)%qg = ReachVars(IX)%flwin * coef
!            ReachVars(IX)%flwin = ReachVars(IX)%flwin - ReachVars(IX)%qg
!	        ReachVars(IX)%wtrin = ReachVars(IX)%flwin*d

!            endif

!            if(Reachs(IX)%IRes == 1) then
!                if (iLoop>1 .and. iLoop < NAllSeries .and. s_Weather(IX)%Rain(iLoop)>0) then
!                    if(ParamRanges(IParamRange)%Itype ==0) then
!                        psum = HydroObserveds(iLoop)%QObserved(watershed.nhydrostation+ParamRanges(IParamRange)%HydroID)
!                    else
!                        psum = HydroObserveds(iLoop)%QObserved(ParamRanges(IParamRange)%HydroID)
!                    endif
!
!                    if ((s_Weather(IX)%Rain(iLoop) == s_Weather(IX)%Rain(iLoop-1) .or. s_Weather(IX)%Rain(iLoop) == s_Weather(IX)%Rain(iLoop+1)).and. psum<=0.001 ) then
!                        ReachVars(IX)%Reslocalinflow = (ReachVars(IX)%Reslocalinflow  + s_Weather(IX)%Rain(iLoop)*0.3)
!                        ReachVars(IX)%flwoutN = ReachVars(IX)%flwoutN + ReachVars(IX)%Reslocalinflow  * Resinfos(Reachs(IX)%ResID)%ControledArea * 1000/ Cursolution.dt/3600
!                        ReachVars(IX)%Reslocalinflow = s_Weather(IX)%Rain(iLoop) * 0.7
!                    endif
!                endif
!            endif          
             ReachVars(IX)%flwoutN = ReachVars(IX)%flwout
						
		enddo
        if (CurSolution%runtype == 2 .or. CurSolution%runtype == 3 .or. CurSolution%runtype == 31 ) then
            AnalyRunoff(iLoop ,2) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
        elseif(CurSolution%runtype == 8) then
            AnalyRunoff(iLoop ,2) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
            AdjustRunoff(iLoop,2) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
            AdjustRunoff(iLoop,4) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
        else
            if (Cursolution%Rtmdy .ne. 0) then
                AdjustRunoff(iLoop,2) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
                AdjustRunoff(iLoop,4) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN
            endif
            if (Cursolution%NRunoffGenType > 0) then
                AllRunoff(iLoop,CurRunoffGenType) = ReachVars(ParamRanges(IParamRange)%SubId)%flwoutN 
            endif
        endif 
		
		return
	end
	

