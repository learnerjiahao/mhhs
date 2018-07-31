   subroutine route(IX,ipid)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates channel routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnke(:)|none         |Exp(-alpha_bnk(:))
!!    bankst(:)   |m^3 H2O       |bank storage
!!    ch_l2(:)    |km            |length of main channel
!!    ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
!!    ch_w(2,:)   |m             |average width of main channel
!!    da_ha       |ha            |area of watershed in hectares
!!    hru_sub(:)  |none          |subbasin number for HRU
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    irte        |none          |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nhru        |none          |number of HRUs in watershed
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    rchdep      |m             |depth of flow on day
!!    rnum1       |none          |fraction of overland flow 
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    IX          |none          |reach number
!!    delt        |days          |time step
!!    itday       |none          |number of time steps in a day 

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: rchinit, rtover, rtday, rtmusk, rthourly, rtsed, rthsed, watqual
!!    SWAT: noqual, hhwatqual, hhnoqual, rtpest, rthpest, rtbact, irr_rch
!!    SWAT: rchuse, reachout

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

        use SolutionMod
		use ReachVarMod
		use ReachParamMod
		use CurrentUnitVars
		
		implicit none
		
		integer IX,ipid	,IParamRange	
		
		ReachVars(IX).vel_chan = 0.
		ReachVars(IX).dep_chan = 0.
		IParamRange = Cursolution.IParamRange

		!! set time step
		if (CurSolution.RouteDT == 24) then
			delt=1.               !1天
			itday=1               !number of time steps in a day 
		else
			delt=CurSolution.RouteDT/24.
			itday = 24/CurSolution.RouteDT
		end if 
		!! start loop for timestep
		!do irthr =1,itday		
		do irthr=1,1
			!! compute contribution of water in bank storage to streamflow
			qdbank = ReachVars(IX).bankst * (1. - alpha_bnke**1/itday)   !qdbank is the volume of water added to the reach via return flow from bank
			ReachVars(IX).bankst = ReachVars(IX).bankst - qdbank
			
			!! set inputs to reach
			call reachin(IX,IParamRange,ipid)
						
			if (CurSolution.IRouteType == 1) call rtvarstor(IX)
			if (CurSolution.IRouteType == 2) call rtmusk(IX)
			if (CurSolution.IRouteType == 3) call rtmanning(IX)

			!! compute revap from bank storage			
			revapday = p_ch_revap * v_pet * ch_l2 * ch_w2 * delt
			revapday = Min(revapday,ReachVars(IX).bankst)
			ReachVars(IX).bankst = ReachVars(IX).bankst - revapday  
!			write(1,*) ReachVars(IX).s_river,ReachVars(IX).flwout
			
		end do
		
		return
	end
	
    subroutine reachin(IX,IParamRange,ipid)

		use ReachInfoMod
		use ReachVarMod
		use HydroObservedMod
		use ParamRangeMod
		use WeatherMod
		use SolutionMod
        use CurDHMParam
		implicit none 
		real c,d
		integer IX,i,j,IParamRange,ipid
        
		
		!call InsertDebug()
		
		c=Reachs(IX).Area/1000.0			!mm->m3
		d=86400 * delt				!m3/s->m3   天变为秒
		ReachVars(IX).wtrin = (ReachVars(IX).s_river)*c	!自己控制子流域的产流量，只计算地表、壤中两种径流，地下径流直接在子流域出口流出
	

		!加上上游河道来水
        ReachVars(IX).upriverin = 0.
        if(ParamRanges(IParamRange).NUpStreamParamRange > 0) then
            do i = 1,Reachs(IX).NUPRCH
                do j = 1,ParamRanges(IParamRange).NUpStreamParamRange
                    if (TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpReachId == Reachs(IX).IUPRCH(i)) then

                        if(TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpQFinal(iLoop)<0) then
                            TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpQFinal(iLoop) = 0.
                        endif
	                    ReachVars(IX).wtrin = ReachVars(IX).wtrin+TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpQFinal(iLoop)*d
                        ReachVars(IX).upriverin =  ReachVars(IX).upriverin + TotalUpHydroFinal(totalSolution(ipid)%IParamRange)%UpHydroFinal(j)%UpQFinal(iLoop)
	                    exit
	                endif
	            enddo
	        enddo
	    endif
	    
        do i = 1, Reachs(IX).NUPRCH
            do j = 1,ParamRanges(IParamRange).NPartSubbasin
                if(Reachs(IX).IUPRCH(i) == ParamRanges(IParamRange).PartSubbasins(j)) then
                    ReachVars(IX).wtrin = ReachVars(IX).wtrin+ReachVars(Reachs(IX).IUPRCH(i)).flwout*d
                    if(Cursolution.dt >= 25) then
                        ReachVars(IX).qg = ReachVars(IX).qg + ReachVars(Reachs(IX).IUPRCH(i)).qg
                    endif
                    exit
                endif
            enddo
        enddo

		return
	end
	