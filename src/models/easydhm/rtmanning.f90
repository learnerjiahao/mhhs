subroutine rtmanning(IX)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes the daily flow through the reach using a the manning equation

!!	developed by Ann van Griensven and Jing Yang at EAWAG


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_k(2,:)   |mm/hr         |effective hydraulic conductivity of main channel alluvium
!!    ch_l2(:)    |km            |length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chside(:)   |none          |change in horizontal distance per unit 
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    evrch       |none          |Reach evaporation adjustment factor. Evaporation from the reach is multiplied by EVRCH. 
!!                               |This variable was created to limit the evaporation predicted in arid regions.
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    pet_day     |mm H2O        |potential evapotranspiration
!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    rnum1       |none          |fraction of overland flow
!!    delt        |days          |calculation time step

!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    IX          |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    scoef       |none          |Storage coefficient (fraction of water in reach flowing out on day)
!!    tbase       |none          |flow duration (fraction of 24 hr)
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of day
!!    wtrin       |m^3 H2O       |amount of water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

		use SolutionMod
		use ReachVarMod
		use ReachParamMod
		use WeatherMod
		use CurReachParam
		implicit none

        integer IX
        
		real :: scoef, p, tbase, topw, vol, c, rh

		rchwtr = ReachVars(IX).rchstor
		rcharea = ReachVars(IX).rchstor/ch_l2/1000.

		!! calculate initial depth of flow
		c = 0.
		c = chside
		rchdep = 0.
		if (rcharea <= phi(1)) then
			rchdep = Sqrt(rcharea / c + phi(6) * phi(6) / (4. * c  * c)) - phi(6) / (2. * c)
			if (rchdep < 0.) rchdep = 0.
		else
			rchdep = Sqrt((rcharea - phi(1)) / 4. + 25. * ch_w2  * ch_w2 / 64.) - 5. * ch_w2 / 8.
			if (rchdep < 0.) rchdep = 0.
			rchdep = rchdep + ch_d
		end if

		!! calculate current reach area
		if (ReachVars(IX).wtrin.gt.0.000010) then
			qdin = ReachVars(IX).wtrin / 3600./24./delt

			call CAManning(qdin,IX)
			rchare1=rcharea
			vol=ch_l2 * rcharea * 1000.

			!! calculate travel time
			tbase = vol / ReachVars(IX).wtrin

			!! calculate volume
			if (tbase < 1.) then
				ReachVars(IX).rchstor =vol
			else
				ReachVars(IX).rchstor=rchwtr+(1./tbase)*(vol-rchwtr)
			end if
	
		else
			ReachVars(IX).rchstor=0.
		end if


		!! calculate bruto outflow end of day
		rtwtr=ReachVars(IX).wtrin+rchwtr-ReachVars(IX).rchstor


		!! calculate new cross-sectional area 	
		rcharea = ReachVars(IX).rchstor / (ch_l2 * 1000.)


		!! calculate average velocity
		ReachVars(IX).vel_chan = rtwtr /  rcharea

		rchdep = 0.

		!! calculate wetted perimeter
		p = 0.
		if (rchdep <= ch_d) then
			p = phi(6) + 2. * rchdep * Sqrt(1. + c * c)
		else
			p = phi(6) + 2. * ch_d * Sqrt(1. + c * c) + 4. * ch_w2 + 2. * (rchdep - ch_d) * Sqrt(17.)
		end if

		!! calculate new depth of flow
		c = 0.
		c = chside
		rchdep = 0.
		if (rcharea <= phi(1)) then
			rchdep = Sqrt(rcharea / c + phi(6) * phi(6) / (4. * c  * c)) - phi(6) / (2. * c)
			if (rchdep < 0.) rchdep = 0.
		else
			rchdep = Sqrt((rcharea - phi(1)) / 4. + 25. * ch_w2 * ch_w2 / 64.) - 5. * ch_w2 / 8.
			if (rchdep < 0.) rchdep = 0.
			rchdep = rchdep + ch_d
		end if

		!! calculate transmission losses
		watsum=rtwtr+ReachVars(IX).rchstor
        if (watsum > 0.) then
			rttlc = ch_k2 * 24. * ch_l2 * p * delt
			topw = 0.
			if (rchdep <= ch_d) then
				topw = phi(6) + 2. * rchdep * chside
			else
				topw = 5 * ch_w2 + 2. * (rchdep - ch_d) * 4.
			end if
			rttlc=min(rttlc,watsum+qdbank)
	
			!! calculate evaporation
			rtevp = evrch * s_Weather(IX).pet(iwLoop) * ch_l2 * topw  * delt
			rtevp = min(rtevp,watsum-rttlc+qdbank)
			watloss=rttlc+rtevp
			watloss=min(watloss, watsum)

			watchange=watloss-qdbank
			rtwtr1=rtwtr
			rtwtr=rtwtr *(1.-watchange/watsum)
			ReachVars(IX).rchstor = ReachVars(IX).rchstor*(1.-watchange/watsum)
		end if		
	
		if (rtwtr < 0.) rtwtr = 0.
		
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
	
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine CAManning(qin, i)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates crosssection using Manning's equation (iteration). 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    x1          |m/s           |initial cross area
!!    x2          |m             |initial hydraulic radius
!!    x3          |none          |Manning's "n" value for channel
!!    x4          |m/m           |average slope of channel
!!    x5          |m/s           |inflow

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    crossarea   |m^2           |cross-sectional flow area 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
		use ReachVarMod
		use CurReachParam

		integer i
		real  p, q1, q2,rh
		ii=0
		q1=qin

1		continue

		ii=ii+1
		rh=0.
		if (rh.gt.0.0001)   p =  rcharea / rh
		p=max(p,ch_w2)
		xx=ch_n2*p**(2./3.)/ch_s2**0.5
		rcharea = xx**(3./5.)*q1**(3./5.)
		
		!! calculate depth of flow
		c = 0.
		c = chside
		if (rcharea <= phi(1)) then
			rchdep = Sqrt(rcharea / c + phi(6) * phi(6) / (4. * c  * c)) - phi(6) / (2. * c)
			if (rchdep < 0.) rchdep = 0.
		else
			rchdep = Sqrt((rcharea - phi(1)) / 4. + 25. * ch_w2 * ch_w2 / 64.) - 5. * ch_w2 / 8.
			if (rchdep < 0.) rchdep = 0.
			rchdep = rchdep + ch_d
		end if

		!! calculate wetted perimeter
		p = 0.
		if (rchdep <= ch_d) then
			p = phi(6) + 2. * rchdep * Sqrt(1. + c * c)	    
		else
			p = phi(6) + 2. * ch_d * Sqrt(1. + c * c) + 4. * ch_w2 + 2. * (rchdep - ch_d) * Sqrt(17.)
		end if

		!! calculate hydraulic radius
		rh = 0.
		if (p > 0.0001) then
			rh = rcharea / p
		else
			rh = 0.
		end if

		p=max(p,ch_w2)
		xx2=ch_n2*p**(2./3.)*ch_s2**-0.5
		yy=rcharea/xx2**(3./5.)
		q2=yy**(5./3.)

		tt=abs((q2-q1)/q2)

		if (tt.lt.0.001) go to 2
		if (ii.gt.100) go to 2
		go to 1
		
2		continue

		return
	end
