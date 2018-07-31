    module CurReachParam
    
        real CH_S2
        real CH_L2
        real CH_N2
        real CH_K2
		
		real CH_W2					! 干流宽度（m）
		real CH_D					! 干流深度（m）
        real alpha_bnk
        real alpha_bnke
        real phi(13)
        real chside
        
	    real EVRCH						!Reach evaporation adjustment factor
	    real TRNSRCH						!reach transmission loss partitioning to deep aquifer
	    real MSK_CO1						!Calibration coefficient used to control impact of the storage time constant (Km) for normal flow                              
	    real MSK_CO2						!Calibration coefficient used to control impact of the storage time constant (Km) for low flow                                 
	    real MSK_X						!Weighting factor controlling relative importance of inflow rate and outflow rate in determining water storage in reach segment
 		real p_ch_revap								!revap coeff: this variable controls the amount
       
    end module             

    module ReachParamMod
        use WaterShedMod
        use CurReachParam
        use ReachInfoMod
        use EasyDHMParamMod
        
        type ReachParamClass
            real :: CH_S2M
            real :: CH_L2M
            real :: CH_N2M
            real :: CH_K2M
        end type ReachParamClass
        
        integer::NReachParamRangeCheck                                
        type(ReachParamClass),Dimension(:),allocatable :: ReachParams
        type(ReachParamClass)::m_ReachPara
        
        contains
            ! 获得当前参数分区Reach模型参数
            subroutine GetReachParam(IParamRange,IX)
                integer :: IParamRange,IX

                CH_S2   = ReachParams(IParamRange)%CH_S2M*Reachs(IX)%M_S2
                CH_L2   = ReachParams(IParamRange)%CH_L2M*Reachs(IX)%M_L2
                CH_N2   = ReachParams(IParamRange)%CH_N2M*Reachs(IX)%M_N2
                CH_K2   = ReachParams(IParamRange)%CH_K2M*Reachs(IX)%M_K2
		        p_ch_revap = EasyDHMParams(IParamRange)%ch_revap

                CH_D    = Reachs(IX)%M_D
                CH_W2   = Reachs(IX)%M_W2
			    !!    compute routing coefficients for main channel

			    if (CH_L2 > 0.) call ttcoef()

				if (alpha_bnk <= 0.) then
					alpha_bnk = 0.05
				end if
				alpha_bnke = Exp(-alpha_bnk)

                EVRCH   = 1
                TRNSRCH = 0.
                MSK_CO1 = 0.
                MSK_CO2 = 3.5
                MSK_X   = 0.2

            end subroutine

	        subroutine ttcoef()

            integer jj
                real fps, d, b, p, a, qq1, rh, tt1, tt2, aa

	            phi = 0.
                aa=1
                !a = 0,
                b = 0.
                chside = 2.
                fps = 4.
                d = ch_d                            !干流深度（m）
                b = ch_w2 - 2. * d * chsiDe         !b is the bottom width of the channel;
                                                    !chside is the inverse of the channel side slope,
                                                    !d is the depth of water in the channel when filled to the top of the bank

                !!    check if bottom width (b) is < 0
                if (b <= 0.) then
	                b = 0.
	                chside = 0.
	                b = .5 * ch_w2
	                chside = (ch_w2 - b) / (2. * d)
                end if
                phi(6) = b
                phi(7) = d

                !!    compute flow and travel time at bankfull depth
                p = 0.
                a = 0.
                rh = 0.
                tt2 = 0.
                p = b + 2. * d * Sqrt(chside * chside + 1.)  !p is the wetted perimeter for a given depth of flow周长
                a = b * d + chside * d * d           !a is the cross-sectional area of flow in the channel面积
                rh = a / p              !rh is the hydraulic radius for a given depth of flow水利半径
                phi(1) = a                  !面积
                phi(5) = a * rh ** .6666 * Sqrt(ch_s2) / ch_n2  !phi(5)is the rate of flow in the channel 流量
                phi(8) = aa * rh ** .6666 * Sqrt(ch_s2) / ch_n2  !phi(8) is the flow velocity 流速
                phi(9) = phi(8) * 5. / 3.                                      !celerity
                phi(10) = ch_l2 / phi(9) / 3.6              !phi(10) is the storage time constant
                tt2 = ch_l2 * a / phi(5)   !the travel time

                !!    compute flow and travel time at 1.2 bankfull depth
                d = 0.
                rh = 0.
                qq1 = 0.
                tt1 = 0.
                d = 1.2 * ch_d
                a = a + (ch_w2 * ch_d + fps * (d - ch_d) ** 2)    !漫滩面积
                p = p + 4. * ch_w2 + (0.4 * ch_d * Sqrt(fps * fps + 1.))!漫滩周长
                rh = a / p
                qq1 = a * rh ** .6666 * Sqrt(ch_s2) / ch_n2     !流量
                tt1 = ch_l2 * a / qq1


                !!    compute flow and travel time at 0.1 bankfull depth
                a = 0.
                d = 0.
                p = 0.
                rh = 0.
                qq1 = 0.
                tt1 = 0.
                d = 0.1 * ch_d
                p = b + 2. * d * Sqrt(chside * chside + 1.)
                a = b * d + chside * d * d
                rh = a / p

                qq1 = a * rh ** .6666 * Sqrt(ch_s2) / ch_n2
                tt1 = ch_l2 * a / qq1
                phi(11) = aa * rh ** .6666 * Sqrt(ch_s2) / ch_n2
                phi(12) = phi(11) * 5. / 3.
                phi(13) = ch_l2 / phi(12) / 3.6

                return
            end subroutine

    end module


            !!    chside      |none          |change in horizontal distance per unit
            !!                               |change in vertical distance on channel side
            !!                               |slopes; always set to 2 (slope=1/2)
            !!    phi(1)      |m^2           |cross-sectional area of flow at bankfull depth
            !!    phi(2)      |none          |
            !!    phi(3)      |none          |
            !!    phi(4)      |none          |
            !!    phi(5)      |m^3/s         |flow rate when reach is at bankfull depth
            !!    phi(6)      |m             |bottom width of main channel
            !!    phi(7)      |m             |depth of water when reach is at bankfull depth
            !!    phi(8)      |m/s           |average velocity when reach is at bankfull depth
            !!    phi(9)      |m/s           |wave celerity when reach is at bankfull depth
            !!    phi(10)     |hr            |storage time constant for reach at bankfull depth (ratio of storage to discharge)
            !!    phi(11)     |m/s           |average velocity when reach is at 0.1 bankfull depth (low flow)
            !!    phi(12)     |m/s           |wave celerity when reach is at 0.1 bankfull depth (low flow)
            !!    phi(13)     |hr            |storage time constant for reach at 0.1 bankfull depth (low flow)
            !!                               |(ratio of storage to discharge)
  