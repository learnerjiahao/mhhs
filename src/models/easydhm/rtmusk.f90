 subroutine rtmusk(IX)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the Muskingum method

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_k(2,:)   |mm/hr         |effective hydraulic conductivity of
!!                               |main channel alluvium
!!    ch_l2(:)    |km            |length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    curyr       |none          |current year of simulation (consecutive)
!!    evrch       |none          |Reach evaporation adjustment factor.
!!                               |Evaporation from the reach is multiplied by
!!                               |EVRCH. This variable was created to limit the
!!                               |evaporation predicted in arid regions.
!!    flwin(:)    |m^3/s H2O     |flow into reach on previous day
!!    flwout(:)   |m^3/s H2O     |flow out of reach on previous day
!!    i           |none          |current day of simulation
!!    id1         |none          |first day of simulation in year
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    msk_co1     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2     |none          |calibration coefficient to control impact 
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x       |none          |weighting factor controlling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    pet_day     |mm H2O        |potential evapotranspiration
!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at
!!                               |bankfull depth
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    phi(10,:)   |hr            |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(13,:)   |hr            |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rnum1       |none          |fraction of overland flow
!!    varoute(2,:)|m^3 H2O       |water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current time step
!!    flwout(:)   |m^3 H2O       |flow out of reach on current time step
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rtevp       |hours         |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on time step
!!    sdti        |m^3/s         |average flow on during time step in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    !           |none          |inverse of channel side slope
!!    c1          |
!!    c2          |
!!    c3          |
!!    c4          |m^3 H2O       |
!!    det         |hr            |time step (hours)
!!    IX        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
!!    wtrin       |m^3 H2O       |water entering reach on day
!!    xkm         |hr            |storage time constant for the reach on
!!                               |current day
!!    yy          |none          |variable to hold intermediate calculation
!!                               |value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    code provided by Dr. Valentina Krysanova, Pottsdam Institute for
!!    Climate Impact Research, Germany
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University
  
		use SolutionMod
		use ReachVarMod
		use ReachParamMod
		use WeatherMod
		use CurReachParam
		
		implicit none
		
		integer IX

		real :: xkm, det, yy, c1, c2, c3, c4,  p, vol, c, rh
		real :: topw
		real :: volrt, maxrt, adddep, addp, addarea
		real :: tmpstor, rttlc1, rttlc2, rtevp1, rtevp2, rttime1
		real :: sdti,rtwtw

		!! calculate volume of water in reach
		vol = 0.
		vol = ReachVars(IX).wtrin + ReachVars(IX).rchstor           !rchstor:water stored in reach
		                                                            !wtrin:water flowing into reach on day


		!! Find average flowrate in a time step delt
		volrt = vol / (86400 * delt)

		!! Find maximum flow capacity of the channel at bank full
        c=0
		c = chside
		p = phi(6) + 2. * ch_d * Sqrt(1. + c * c)      !周长
		rh = phi(1) / p                                !水利半径
		maxrt = phi(1) * rh ** .6666 * Sqrt(ch_s2) / ch_n2  !流量
		
		sdti = 0.                   !average flow on day in reach
		rchdep = 0.                 !depth of flow on day
		p = 0.
		rh = 0.
		det = 24 * delt             !time step (hours)
		
		if (volrt >= maxrt) then
			rcharea = phi(1)
			rchdep = ch_d
			p = phi(6) + 2. * ch_d * Sqrt(1. + c * c)   !phi(6):bottom width of main channel
			rh = phi(1) / p
			sdti = maxrt
			!!  This is the time to empty the volume of water
			!!  at the bankfull discharge rate
			rttime1 = vol / (3600. * sdti)
			if (rttime1 > det) then
				!! perform flood plain simulation
				!! Increase the discharge in flood plain until all the volume can be emptied
				!! within a day.
      			adddep = 0

				Do While (rttime1 > det)
					!!By iteration at 1cm interval find out how much depth of water in flood plain 
					!!for the discharge volume within a day
					adddep = adddep + 0.01
					addarea=rcharea + ((ch_w2 * 5) + c * adddep) * adddep  !漫滩面积
					addp = p + (ch_w2 * 4) + 2. * adddep * Sqrt(17.)        !漫滩周长
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
				rcharea = (phi(6) + c * rchdep) * rchdep
				p = phi(6) + 2. * rchdep * Sqrt(1. + c * c)
				rh = rcharea / p
				sdti = rcharea * rh ** .6666 * Sqrt(ch_s2) / ch_n2
			end do
		end if

		sdti = volrt

		!! Compute storage time constant for reach
		xkm = 0.
		xkm = phi(10) * msk_co1 + phi(13) * msk_co2  !phi(10) is the storage time constant  |storage time constant for reach at 0.1 bankfull depth (low flow) 
		!! Adjust xkm for flexible time steps
		xkm = xkm * delt

        global_msk_K = xkm !保存马斯京根汇流的K值并用于输出
	
		!! Compute coefficients
		yy = 0.
		c1 = 0.
		c2 = 0.
		c3 = 0.
		c4 = 0.
		yy = 2. * xkm * (1. - msk_x) + det
		c1 = (det - 2. * xkm * msk_x) / yy
		
		!lxh
		if (c1 .lt. 0) c1 = 0
		
		c2 = (det + 2. * xkm * msk_x) / yy
		c3 = (2. * xkm * (1. - msk_x) - det) / yy
		c4 = phi(5) * ch_l2 * det / yy
		
		!!write(*,*) c3

		!! Compute water leaving reach on day
		
		!if (dt .lt. 24 .and. IYear == CalYears(1) .and. IMN == CalMonths(1) .and. ID == CalDays(1) .and. IH == CalHours(1)) then
			!rtwtr = c1 * wtrin + c2 * rchstor(IX) + c3 * rchstor(IX) + c4
		!elseif (dt .eq. 24 .and. IYear == CalYears(1) .and. IMN == CalMonths(1) .and. ID == CalDays(1)) then
			!rtwtr = c1 * wtrin + c2 * rchstor(IX) + c3 * rchstor(IX) + c4
		!else
			rtwtr = c1 * vol + c2 * ReachVars(IX).flwin*(86400 * delt) + c3 * ReachVars(IX).flwout*(86400 * delt)
		!end if
		
		if (rtwtr < 0.) rtwtr = 0.
		!! Add statement to make sure the flow out is not more than flow in + storage
		rtwtr = min(rtwtr, ReachVars(IX).rchstor+ReachVars(IX).wtrin)

		!! calculate amount of water in channel at end of day
		ReachVars(IX).rchstor = ReachVars(IX).rchstor + ReachVars(IX).wtrin - rtwtr
		!! Add if statement to keep rchstor from becoming negative
		if (ReachVars(IX).rchstor < 0.0) ReachVars(IX).rchstor = 0.0

		watsum= rtwtr+ReachVars(IX).rchstor
		!! calculate transmission losses
		rttlc = 0.


		rttlc = 24 * ch_k2 * ch_l2 * p * delt
	

		!! calculate evaporation
        !! calculate width of channel at water level
        topw = 0.
        if (rchdep <= ch_d) then
			topw = phi(6) + 2. * rchdep * chside
        else
            topw = 5 * ch_w2 + 2. * (rchdep - ch_d) * 4.
        end if

		rtevp = evrch * s_Weather(IX).pet(iwLoop) * ch_l2 * topw  * delt
		rtevp = min(rtevp,watsum-rttlc+qdbank)
		watloss=rttlc+rtevp
		watloss=min(watloss, watsum+qdbank)

		watchange=watloss-qdbank
		rtwtr1=rtwtr
		
		
		
		!! define flow parameters for current day
		!! route water through reach
		ReachVars(IX).flwin=0
		ReachVars(IX).flwin = ReachVars(IX).wtrin/(86400 * delt)
		ReachVars(IX).flwout=0
		ReachVars(IX).flwout = rtwtr/(86400 * delt)
		
 		if (watsum==0) then
			rtwtw=0
			ReachVars(IX).rchstor=0
		else		
			rtwtr=rtwtr *(1.-watchange/watsum)
			ReachVars(IX).rchstor = ReachVars(IX).rchstor*(1.-watchange/watsum)
		endif
		
		
		!! precipitation on reach is not calculated because area of HRUs 
		!! in subbasin sums up to entire subbasin area (including channel
		!! area) so precipitation is accounted for in subbasin loop

		if (ReachVars(IX).rchstor < 10.) then
			rtwtr = rtwtr + ReachVars(IX).rchstor
			ReachVars(IX).rchstor = 0.
		end if

		if (rtwtr < 0.) rtwtr = 0.
		if (ReachVars(IX).rchstor < 0.) ReachVars(IX).rchstor = 0.

		!!write(*,*) IYear, IMN, ID, IH, rchstor(IX)
		
		!! average daily water depth for sandi doty 09/26/07
		ReachVars(IX).dep_chan = rchdep

		!! add transmission losses to bank storage/deep aquifer in subbasin
		if (rttlc > 0.) then
			ReachVars(IX).bankst = ReachVars(IX).bankst + rttlc * (1. - trnsrch)
			!渗漏到深层地下水
			!@@@@@				
		end if

		!! compute revap from bank storage
		revapday = p_ch_revap * s_Weather(IX).pet(iwLoop) * ch_l2 * ch_w2 * delt
		revapday = Min(revapday,ReachVars(IX).bankst)
		ReachVars(IX).bankst = ReachVars(IX).bankst - revapday				

		return
	end
