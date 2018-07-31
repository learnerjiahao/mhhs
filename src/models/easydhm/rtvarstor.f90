  subroutine rtvarstor(IX)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes the daily flow through the reach using a variable storage coefficient

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_k(2,:)   |mm/hr         |effective hydraulic conductivity of main channel alluvium
!!    ch_l2(:)    |km            |length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chside(:)   |none          |change in horizontal distance per unit change in vertical distance on channel side slopes; 
!!                               |always set to 2 (slope=1/2)
!!    evrch       |none          |Reach evaporation adjustment factor.
!!                               |Evaporation from the reach is multiplied by EVRCH. 
!!                               |This variable was created to limit the evaporation predicted in arid regions.
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number

!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel

!!    rnum1       |none          |fraction of overland flow
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    varoute(2,:)|m^3 H2O       |water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    p           |m             |wetted perimeter

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    IX        |none          |reach number
!!    rh          |m             |hydraulic radius
!!    scoef       |none          |Storage coefficient (fraction of water in 
!!                               |reach flowing out on day)
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
!!    wtrin       |m^3 H2O       |amount of water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University
!!    Modified by Ann van Griensven, UNESCO-IHE, Delft, Nl
        
        use SolutionMod
		use ReachVarMod
		use ReachParamMod
		use WeatherMod
		use CurReachParam
		
		implicit none
		
		integer :: IX, idum

		real :: scoef, topw, vol, c, rh
		real :: volrt, maxrt, adddep, addp, addarea
		real :: tmpstor, rttlc1, rttlc2, rtevp1, rtevp2, rttime1
		real :: p, sdti, delth,rttime
		
		!! compute contribution of water in bank storage to streamflow
		qdbank = ReachVars(IX).bankst * (1. - alpha_bnke**1/itday)
		ReachVars(IX).bankst = ReachVars(IX).bankst - qdbank

		!! calculate volume of water in reach
		vol = 0.
		vol = ReachVars(IX).wtrin + ReachVars(IX).rchstor

		!! Find average flowrate in a day
		volrt = vol / (86400 * delt) 

		!! Find maximum flow capacity of the channel at bank full
		c = 0.
		c =    chside
		!湿周
		p =    phi(6) + 2. * ch_d * Sqrt(1. + c * c)        
		!水利半径
		rh =    phi(1) / p                                      
		maxrt = phi(1) * rh ** .6666 * Sqrt(ch_s2) / ch_n2
		
		sdti = 0.
		rchdep = 0.
		p = 0.
		rh = 0.

		if (volrt >= maxrt) then
		    !断面面积
			rcharea =    phi(1)
			!断面深度
			rchdep =    ch_d
			p =    phi(6) + 2. *    ch_d * Sqrt(1. + c * c)
			rh =    phi(1) / p
			sdti = maxrt

			!!  This is the time to empty the volume of water
			!!  at the bankfull discharge rate
			rttime1 = vol / (3600. * sdti)
			if (rttime1 > 24) then
				!! perform flood plain simulation
				!! Increase the discharge in flood plain until all the volume can be emptied
				!! within a day.
				adddep = 0
				delth = 24 * delt
				Do While (rttime1 > delth)
					!!By iteration at 1cm interval find out how much depth of water in flood plain 
					!!for the discharge volume within a day
					adddep = adddep + 0.01
					addarea=rcharea + ((   ch_w2 * 5) + c * adddep) * adddep
					addp = p + (   ch_w2 * 4) + 2. * adddep * Sqrt(17.)
					rh = addarea / addp
					sdti = addarea * rh ** .6666 * Sqrt(ch_s2) / ch_n2 
					rttime1 = vol / (3600. * sdti)
				End Do
				rcharea = addarea
				rchdep = rchdep + adddep
				p = addp
			End if
		else
			!! find the crossectional area and depth for volrt
			!! by iteration method at 1cm interval depth
			!! find the depth until the discharge rate is equal to volrt
			Do While (sdti < volrt)
				rchdep = rchdep + 0.01
				rcharea = (   phi(6) + c * rchdep) * rchdep
				p =    phi(6) + 2. * rchdep * Sqrt(1. + c * c)
				rh = rcharea / p
				sdti = rcharea * rh ** .6666 * Sqrt(ch_s2) / ch_n2
			end do
		end if

		sdti = volrt

		if (sdti > 0.) then
			!! calculate travel time
			ReachVars(IX).vel_chan = sdti / rcharea
			rttime =    ch_l2 * 1000. / (3600. * ReachVars(IX).vel_chan)
			if (rttime == 0) then
				idum=0
			endif

			!! calculate volume of water leaving reach on time step delt
			scoef = 0.
			scoef = 48. * delt / (2. * rttime + 24.) 
			if (scoef > 1.) scoef = 1.
			rtwtr = 0.
			rtwtr = scoef * vol

			!! calculate amount of water in channel at end of day
			ReachVars(IX).rchstor = ReachVars(IX).rchstor + ReachVars(IX).wtrin - rtwtr
			watsum= rtwtr+ReachVars(IX).rchstor
			
			!! calculate transmission losses
			rttlc = 0.
			rttlc = 24 *    ch_k2 *    ch_l2 * p * delt


			!! calculate evaporation
			!! calculate width of channel at water level
			topw = 0.
			if (rchdep <=    ch_d) then
				topw =    phi(6) + 2. * rchdep *    chside
			else
				topw = 5 *    ch_w2 + 2. * (rchdep -    ch_d) * 4.
			end if

			rtevp = evrch * s_Weather(IX).pet(iwLoop) *    ch_l2 * topw  * delt
			rtevp = min(rtevp,watsum-rttlc+qdbank)
			watloss=rttlc+rtevp
			watloss=min(watloss, watsum+qdbank)

			watchange=watloss-qdbank
			rtwtr1=rtwtr
			rtwtr=rtwtr *(1.-watchange/watsum)
			ReachVars(IX).rchstor = ReachVars(IX).rchstor*(1.-watchange/watsum)
		else
			rtwtr = 0.
			sdti = 0.
		end if

		!! precipitation on reach is not calculated because area of HRUs 
		!! in subbasin sums up to entire subbasin area (including channel
		!! area) so precipitation is accounted for in subbasin loop
		
		
		
		if (ReachVars(IX).rchstor < 10.) then
			rtwtr = rtwtr + ReachVars(IX).rchstor
			ReachVars(IX).rchstor = 0.
		end if

		if (rtwtr < 0.) rtwtr = 0.
		if (ReachVars(IX).rchstor < 0.) ReachVars(IX).rchstor = 0.
		
		!if (rchstor(IX) > 0.) !write(*,*) IX, rchstor(IX)
	
		ReachVars(IX).flwin=0
		ReachVars(IX).flwin = ReachVars(IX).wtrin/(86400 * delt)
		
		ReachVars(IX).flwout = rtwtr/(86400 * delt)
		
		!! average daily water depth for sandi doty 09/26/07
		ReachVars(IX).dep_chan = rchdep

		!! add transmission losses to bank storage/deep aquifer in subbasin
		if (rttlc > 0.) then
			ReachVars(IX).bankst = ReachVars(IX).bankst + rttlc * (1. - trnsrch)
			!渗漏到深层地下水
			!@@@@@				
		end if

		!! compute revap from bank storage
		revapday = ch_revap * s_Weather(IX).pet(iwLoop) * ch_l2 * ch_w2 * delt
		revapday = Min(revapday,ReachVars(IX).bankst)
		ReachVars(IX).bankst = ReachVars(IX).bankst - revapday				

		return
	end

