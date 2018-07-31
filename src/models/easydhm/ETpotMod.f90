Module ETpotMod
        implicit none
        contains
            subroutine sim_ETPot(IX,IY,v_snow,p_lai)  
		        use CurrentUnitVars
		        use UnitInfoMod
		        use WeatherMod
		        use solutionMod
		        
		        ! ������Ϣ
		        real v_LatHeatVap		!ˮ��Ǳ��
		        real v_ea				!����ˮ��ѹ
		        real v_dlt              !����ˮ��ѹ���¶ȵı仯��
		        real gma				!����ʪ����

		        real :: tk, pb, xl, ea, ed, dlt, ramm, ralb1, ralb, xx
		        real :: rbo, rto, rn, uzz, zz, zom, zov, rv, rn_pet, fvpd
		        real :: rc, rho, rout, d, chz, gsi_adj, pet_alpha
		        real vpd,v_CO2,v_cht,v_indlp,E601Coef
		        integer IGRO
		        real v_snow,p_lai
		        
		        integer IX,IY,ipet
		        ipet = cursolution.ipet
        		
		        !! initialize local variables
		        tk = 0.
		        tk = v_t + 273.15

		        !! calculate mean barometric pressure
		        pb = 0.
		        pb = 101.3 - Units(IX,IY).HEIGm * (0.01152 - 0.544e-6 * Units(IX,IY).HEIGm)

		        !! calculate latent heat of vaporization
		        xl = 0.
		        xl = 2.501 - 2.361e-3 * v_t
		        v_LatHeatVap = xl

		        !! calculate psychrometric constant
		        gma = 0.
		        gma = 1.013e-3 * pb / (0.622 * xl)

		        !! calculate saturation vapor pressure, actual vapor pressure and
		        !! vapor pressure deficit
		        ea = 0.
		        ed = 0.
		        vpd = 0.
		        ea = Ee(v_t)
		        v_ea = ea
		        ed = ea * v_hmd/100.0          !���ʪ�ȵĵ�λ�����루%�������㣨-��
		        vpd = ea - ed

		        !!calculate the slope of the saturation vapor pressure curve
		        dlt = 0.
		        dlt = 4098. * ea / (v_t + 237.3)**2
		        v_dlt = dlt
        		
		        ! ��������շ��� v_raMax
		        call DayRaMax()
        		
		        call albedo()
        		
		        ! �������v_ra
		        call SSH2RA()                   !��������ʱ������̫������
        		
		        !! calculate net short-wave radiation for PET
		        ralb = 0.
		        if (v_snow <= .5) then
			        ralb = v_ra * (1.0 - 0.23) 
		        else
			        ralb = v_ra * (1.0 - 0.8) 
		        end if
        		
		        !! DETERMINE POTENTIAL ET
		        select case (ipet)

		        case (1)   !! PRIESTLEY-TAYLOR POTENTIAL EVAPOTRANSPIRATION METHOD
			        !! calculate net long-wave radiation

			        !! net emissivity  equation 2.2.20 in SWAT manual
			        rbo = 0.
			        rbo = -(0.34 - 0.139 * Sqrt(ed))

			        !! cloud cover factor equation 2.2.19
			        rto = 0.
			        if (v_ramax < 1.e-4) then
				        rto = 0.
			        else
				        ! ���ݷ��������Ӱ������		
				        rto = 0.9 * (v_ra / v_ramax) + 0.1
				        ! ��������ʱ��������Ӱ������
				        !rto=0.9*v_ssh/AMPDH+0.1									! rto = ��Ӱ������
			        end if

			        !! net long-wave radiation equation 2.2.21
			        rout = 0.
			        rout = rbo * rto * 4.9e-9 * (tk**4)			

			        !! calculate net radiation
			        rn_pet = 0.
			        rn_pet = ralb + rout
			        !! net radiation

			        pet_alpha = 1.28
			        v_pet = pet_alpha * (dlt / (dlt + gma)) * rn_pet / xl
			        v_pet = Max(0., v_pet)


		        case (3)   !! HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD

			        !! extraterrestrial radiation
			        !! 37.59 is coefficient in equation 2.2.6 !!extraterrestrial
			        !! 30.00 is coefficient in equation 2.2.7 !!max at surface
        !			ramm = 0.
        !			ramm = v_raMax * 37.59 / 30. 
        !
        !			if (tmx(IX) > tmn(IX)) then
        !				v_pet = harg_petco(hru_sub(IX))*(ramm / xl)*(s_Weather(IX).PA + 17.8)*(tmx(IX) - tmn(IX))**0.5
        !				v_pet = Max(0., v_pet)
        !			else
        !				v_pet = 0.
        !			endif

		        case (2)   !! PENMAN-MONTEITH POTENTIAL EVAPOTRANSPIRATION METHOD

			        !! net radiation

			        !! calculate net short-wave radiation for max plant ET
			        ralb1 = 0.
			        ralb1 = v_ra * (1.0 - v_albday) 

			        !! calculate net long-wave radiation
			        !! net emissivity  equation 2.2.20 in SWAT manual
			        rbo = 0.
			        rbo = -(0.34 - 0.139 * Sqrt(ed))
        			
			        !��������ʱ��������Ӱ������
			        !rto=0.9*SH/AMPDH+0.1									! rto = ��Ӱ������

			        ! ���ݷ��������Ӱ������		
			        !! cloud cover factor equation 2.2.19
			        rto = 0.
			        if (v_ramax < 1.e-4) then
				        rto = 0.
			        else
				        rto = 0.9 * (v_ra / v_raMax) + 0.1
			        end if


			        !! net long-wave radiation equation 2.2.21
			        rout = 0.
			        rout = rbo * rto * 4.9e-9 * (tk**4)

			        !! calculate net radiation
			        rn = 0.
			        rn_pet = 0.
			        rn = ralb1 + rout
			        rn_pet = ralb + rout
			        !! net radiation

			        rho = 0.
			        rho = 1710. - 6.85 * v_t

			        if (v_wnd < 0.01) v_wnd = 0.01				! ����վ��þ���10�׵ķ��٣�
        			
			        !��������Ũ��
			        v_co2 = 330			!@@@@@
        			
			        !! potential ET: reference crop alfalfa at 40 cm height
			        rv = 0.
			        rc = 0.
			        rv = 114. / v_wnd
			        rc = 49. / (1.4 - 0.4 * v_co2 / 330.)
			        v_pet = (dlt * rn_pet + gma * rho * vpd / rv) / (xl * (dlt + gma * (1. + rc / rv)))
			        v_pet = Max(0., v_pet)
         
			        !�ν���������ģ�� @@@@@
			        !land cover status code:
			        !    0 no land cover currently growing
			        !    1 land cover growing
			        Igro = 1     !������������
			        !! maximum plant ET
			        if (Igro <= 0) then
				        v_ep_max = 0.0
			        else
				        !! determine wind speed and height of wind speed measurement 
				        !! adjust to 100 cm (1 m) above canopy if necessary				
				        !ֲ���ڲ�߶� @@@@@
				        v_cht = 1.0
        				
				        uzz = 0.
				        zz = 0.
				        if (v_cht <= 1.0) then
					        uzz = v_wnd
					        zz = 170.0
				        else
					        zz = v_cht * 100. + 100.
					        uzz = v_wnd * (zz / 170.)**0.2
				        end if

				        !! calculate canopy height in cm
				        chz = 0.
				        if (v_cht < 0.01) then
					        chz = 1.
				        else
					        chz = v_cht * 100.
				        end if

				        !! calculate roughness length for momentum transfer
				        zom = 0.
				        if (chz <= 200.) then
					        zom = 0.123 * chz
				        else
					        zom = 0.058 * chz**1.19
				        end if
         
				        !! calculate roughness length for vapor transfer
				        zov = 0.
				        zov = 0.1 * zom

				        !! calculate zero-plane displacement of wind profile
				        d = 0.
				        d = 0.667 * chz

				        !! calculate aerodynamic resistance
				        rv = Log((zz - d) / zom) * Log((zz - d) / zov)
				        rv = rv / ((0.41)**2 * uzz)

				        !! adjust stomatal conductivity for low vapor pressure
				        !! this adjustment will lower maximum plant ET for plants
				        !! sensitive to very low vapor pressure
        				
				        !�ν�����ģ��
				        !�������� @@@@@
!				        v_idplt = 1
				        !Ҷ���ָ�� @@@@@
!				        p_lai = 1.5
        				
				        xx = 0.
				        fvpd = 0.
				        xx = vpd - 1.
				        if (xx > 0.0) then
					        !@@@@@ ��ʱ�޷�����
					        !fvpd = Max(0.1,1.0 - vpd2(v_idplt) * xx)
				        else
					        fvpd = 1.0
				        end if
				        gsi_adj = 0.			!stomatal conductance
				        !@@@@@ ��ʱ�޷�����
				        !gsi_adj = gsi(v_idplt) * fvpd   
				        gsi_adj = 5e-3
                    
				        !! calculate canopy resistance
				        rc = 0.
				        rc = 1. / gsi_adj                    !single leaf resistance
				        rc = rc / (0.5 * (p_lai + 0.01) * (1.4 - 0.4 * v_co2 / 330.))

				        !! calculate maximum plant ET
				        v_ep_max = (dlt * rn + gma * rho * vpd / rv) / (xl * (dlt + gma * (1. + rc / rv)))
				        if (v_ep_max < 0.) v_ep_max = 0.
				        v_ep_max = Min(v_ep_max, v_pet)		!����������� <= Ǳ������
			        end if
               

              
		        case (4)  !! READ IN PET VALUES
			        v_pet = s_weather(IX).PET(iwLoop) * E601Coef
          
		        end select

		        return
	        end subroutine

	        subroutine DayRaMax()
		        use CurrentUnitVars
		        use TimeInfoMod
		        real sd,dd,ch,h,ys,yc
        		
		        !! Calculate Daylength !!
		        !! calculate solar declination: equation 2.1.2 in SWAT manual
		        sd = 0.
		        sd = Asin(.4 * Sin((Real(jdt) - 82.) / 58.09))  !!365/2pi = 58.09
		        !! calculate the relative distance of the earth from the sun the eccentricity of the orbit
		        !! equation 2.1.1 in SWAT manual
		        dd = 0.
		        dd = 1.0 + 0.033 * Cos(Real(jdt) / 58.09)

		        ch = 0.
		        h = 0.
		        ch = -sin(v_DLAT) * Tan(sd) / cos(v_DLAT)
		        if (ch > 1.) then    !! ch will be >= 1. if latitude exceeds +/- 66.5 deg in winter
			        h = 0.
		        elseif (ch >= -1.) then
			        h = Acos(ch)
		        else
			        h = 3.1416         !! latitude exceeds +/- 66.5 deg in summer
		        endif

		        ys = sin(v_DLAT) * Sin(sd)
		        yc = cos(v_DLAT) * Cos(sd)
		        v_raMax = 30. * dd * (h * ys + yc * Sin(h))
        		
		        return
	        end	subroutine
	        
            subroutine albedo
		        use CurrentUnitVars
		        use CurrentUnitWSPDHMVars

		        real :: cej, eaj


		        !! calculate residue on soil surface for current day
		        !�ν�����ģ��@@@@@
        !		v_sol_cov = .8 * bio_ms + sol_rsd
        !		v_sol_cov = Max(sol_cov,0.)
		        v_sol_cov = 0
        		
        		
		        !SHM
		        !ALB1=0.15;    ALB2=0.23;	ALBS=0.30				! ALB1��ALB2��ALBS = �ֵء��ݵء���صĶ̲�������
        		
		        !! calculate albedo
		        cej = -5.e-5
		        eaj = 0.
		        eaj = Exp(cej * (v_sol_cov + .1))   !! equation 2.2.16 in SWAT manual

		        if (v_snow <= .5) then
			        !! equation 2.2.14 in SWAT manual
			        v_albday = 0.05

			        !! equation 2.2.15 in SWAT manual
			        if (p_lai > 0.) v_albday = .23 * (1. - eaj) + 0.05 * eaj
		        else
			        !! equation 2.2.13 in SWAT manual
			        v_albday = 0.8
		        end if

		        return
	        end subroutine
	        
	        subroutine SSH2RA()         !��������ʱ������̫������
		        use CurrentUnitVars
		        use TimeInfoMod
        		
		        real a,b
		        real HL, H0	,PI
		        real Ang, Delta, DiSun, OMIKA, AMPDH
		        
		        PI = 3.1415926
        		
		        !��������ʱ������������
		        Ang    = 2.0*PI*jdt/365.0								! jdt = Julian������1��1�����������
		        Delta  = 0.4093*SIN(Ang-1.405)							! DELTA = ̫�����                          !2017-08-30  ��������ǶԵ�
		        DiSun  = 1.0+0.033*COS(Ang)								! ������̫��֮�����Ծ���                  �������Ҳ�������
        		
		        OMIKA=ACOS(-TAN(v_DLAT)*TAN(DELTA))						! OMIKA = ����ʱ��̫��ʱ��                  !2017-08-30 ���﹫ʽ�ǶԵġ�����γ��ֵ��v_DLAT������ɻ��Ⱥ��ֳ�����90��StaticMember.f90 435�У�����
        		
		        ! SODAILY = ̫���ĵ����������̲�������
!		        SODAILY=(15.392*2.501)*DISUN*(OMIKA*SIN(v_DLAT)*SIN(DELTA)+COS(v_DLAT)*COS(DELTA)*SIN(OMIKA))		
        		
		        AMPDH=OMIKA*24/PI*2										! AMPDH = �����������Сʱ��                 !2017-08-30 �����ǲ���ȱ�ٳ���2���������¡�����GIS�ķ������Ե�������ȵĵ�������ʱ�����㡷 ������� �Ķ�Ϊ *2��2017-08-31��

        !		! incoming shortwave radiation
        !		v_RS=SODAILY*(0.25+0.5*v_ssh/AMPDH)						! v_RS = ����ر���Ķ̲�������

		        H0 = 24/PI * 4.921 * DISUN*(OMIKA*SIN(v_DLAT)*SIN(DELTA)+COS(v_DLAT)*COS(DELTA)*SIN(OMIKA))		
		        HL = 0.8 * H0
        		
		        a = 0.248
		        b = 0.752
		        v_ra = HL * (a+b*v_ssh/AMPDH)
	        end  subroutine

	        function ee(tk)

	            real, intent (in) :: tk
	            real :: ee

	            ee = 0.
	            if (tk + 237.3 /= 0.) then
		            ee = (16.78 * tk - 116.9) / (tk + 237.3)
		            ee = Exp(ee)
	            end if

	            return
	        end	 function             	        
	end module