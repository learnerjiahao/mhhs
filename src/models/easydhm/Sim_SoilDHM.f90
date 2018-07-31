 Module Sim_SoilDHM
		
		implicit none

        contains 
            subroutine crackvol
		        use CurrentUnitWSPDHMVars
		        use CurDHMParam
            
                integer :: l, j
                real :: volcrnew, crlag, crlagdry = .99, crlagwet = 0.

                j = 0
                voltot = 0.

                !! calculate volume of cracks in soil
                do l = 1, sol_nly
                    volcrnew = 0.
                    volcrmin = 0.
                    crlag = 0.
                    volcrnew = crdep(l) * (sol_fc(l) - sol_st(l)) / (sol_fc(l))
                    if (v_ss < .90 * sol_sumfc) then
                        if (volcrnew > volcr(l)) then
                            crlag = crlagdry
                        else
                            crlag = crlagwet
                        end if
                    else
                        crlag = crlagwet
                    end if
                    volcr(l) = crlag * volcr(l) + (1. - crlag) * volcrnew
                    if (volcr(l) < 0.) volcr(l) = 0.
                    voltot = voltot + volcr(l) + volcrmin
                end do

                return
            end subroutine
            
            subroutine solt(IX)
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use WeatherMod
		        use CurDHMParam

                integer :: j, k
                real :: f, dp, ww, b, wc, dd, xx, st0,a
                real :: tlag, df, zd, bcv, tbare, tcov
                integer IX

                tlag = 0.6

                !! calculate damping depth

                !! calculate maximum damping depth
                !! SWAT manual equation 2.3.6
                f = 0.
                dp = 0.
                f = sol_avbd / (sol_avbd + 686. * Exp(-5.63 * sol_avbd))
                dp = 1000. + 2500. * f                 !最大土壤阻尼深度

                !! calculate scaling factor for soil water
                !! SWAT manual equation 2.3.7
                ww = 0.
                wc = 0.
                ww = .356 - .144 * sol_avbd
                wc = v_ss / (ww * sol_z(sol_nly))          !sol_z  depth to bottom of soil layer

                !! calculate daily value for damping depth
                !! SWAT manual equation 2.3.8
                b = 0.
                f = 0.
                dd = 0.
                b = Log(500. / dp)
                f = Exp(b * ((1. - wc) / (1. + wc))**2)
                dd = f * dp


                !! calculate lagging factor for soil cover impact on soil surface temp
                !! SWAT manual equation 2.3.11
                bcv = 0.
                sol_cov=0.
                bcv = sol_cov/(sol_cov + Exp(7.563 - 1.297e-4 * sol_cov))
                if (v_snow /= 0.) then
                if (v_snow <= 120.) then
                  xx = 0.
                  xx = v_snow/(v_snow+ Exp(6.055 - .3002 * v_snow))
                else
                  xx = 1.
                end if
                bcv = Max(xx,bcv)
                end if

                !! calculate temperature at soil surface
                st0 = 0.
                tbare = 0.
                tcov = 0.
                tmp_srf = 0.
                !! SWAT manual equation 2.3.10
                st0 = (v_ra * (1. - v_albday) - 14.) / 20.    !辐射量
                !! SWAT manual equation 2.3.9
                tbare = s_Weather(IX).Tavg(iwLoop) + 0.5 * (s_weather(IX).tmax(iwLoop)  - s_weather(IX).tmin(iwLoop) ) * st0
                !! SWAT manual equation 2.3.12
                tcov = bcv * sol_tmp(2) + (1. - bcv) * tbare


                !!    taking average of bare soil and covered soil as in APEX
                !!    previously using minumum causing soil temp to decrease
                !!    in summer due to high biomass

                tmp_srf = 0.5 * (tbare + tcov)  ! following Jimmy's code




                !! calculate temperature for each layer on current day
                xx = 0.
                do k = 1, sol_nly
                    zd = 0.
                    df = 0.
                    zd = (xx + sol_z(k)) / 2.  ! calculate depth at center of layer
                    zd = zd / dd                 ! SWAT manual equation 2.3.5
                    !! SWAT manual equation 2.3.4
                    df = zd / (zd + Exp(-.8669 - 2.0775 * zd)) !df 深度影响系数
                    !! SWAT manual equation 2.3.3
                    sol_tmp(k) = tlag * sol_tmp(k) + (1. - tlag) * (df * y_weather(IX).TAvg - tmp_srf) + tmp_srf  !各层土壤中心处的日平均温度
                    if ( isnan(sol_tmp(k)) )then
                      a=0
                    elseif (sol_tmp(k) .lt. -50) then
                      !!write(*,*) "soil temperature is too low!"
                      sol_tmp(k)=-50
                    endif
                    xx = sol_z(k)
                end do

                return
            end subroutine
            
            subroutine crackflow
		        use CurrentUnitWSPDHMVars

                !! subtract crack flow from surface runoff
                if (v_rs> voltot) then
                    v_rs= v_rs- voltot
                else
                    v_rs = 0.
                endif

                return
            end subroutine
 
            subroutine percmain(IX,IY,landid)
		        use CurrentUnitWSPDHMVars
		        use CurDHMParam
		        use EasyDHMVarMod
		        integer :: j,j1
		        integer IX,IY,landid
		        real por_air,d,xx,yy,wt_shall,dmod_m,sumqtile

                latq=0
                qtile=0

                sepday = Max(0., v_infil- voltot)
                call percmacro
                
		        do j1 = 1, sol_nly
			        !! add water moving into soil layer from overlying layer
			        sol_st(j1) = sol_st(j1) + sepday
                
			        !! determine gravity drained water in layer
			        sw_excess = 0.
!			        sw_excess = sol_st(j1) - sol_fc(j1) 
			        sw_excess = max(0.,sol_st(j1) - sol_fc(j1) )
			        lland_DHMVars(IX,IY,j1,landid).excess = sw_excess

			        !! initialize variables for current layer
			        sepday = 0.
			        latlyr = 0.
			        lyrtile = 0.

			        if (sw_excess > 1.e-5) then
				        !! calculate tile flow (lyrtile), lateral flow (latlyr) and percolation (sepday)
				        call percmicro(IX,j1)

				        sol_st(j1) = sol_st(j1) - sepday - latlyr - lyrtile
				        sol_st(j1) = Max(1.e-6,sol_st(j1))

				        !! redistribute soil water if above field capacity (high water table)
				        call sat_excess(j1)
			        end if

			        !! summary calculations
			        if (j1 == sol_nly) then
				        sepbtm = sepbtm + sepday
			        endif
			        latq = latq + latlyr
			        qtile = qtile + lyrtile
			        flat(j1) = latlyr + lyrtile
			        sol_prk(j1) = sol_prk(j1) + sepday

		        end do

		        !! compute shallow water table depth and tile flow
		        qtile = 0.
		        if (sol_tmp(2) > 0.) then
			        por_air = 0.5
			        d = dep_imp - ddrain
			        if (v_ss > sol_sumfc) then
				        yy = sol_sumul * por_air
				        if (yy < 1.1 * sol_sumfc) then
					        yy = 1.1 * sol_sumfc
				        end if
				        xx = (v_ss - sol_sumfc) / (yy - sol_sumfc)
				        if (xx > 1.) xx = 1.
				        wt_shall = xx * dep_imp
				        if (ddrain > 0.) then
					        if (wt_shall < d) then
						        qtile = 0.
					        else
						        dmod_m = wt_shall - d
						        sw_excess = (dmod_m / wt_shall) * (v_ss - sol_sumfc)
						        qtile = sw_excess * (1. - Exp(-24. / tdrain))
					        end if
				        end if
			        end if

			        if (qtile > 0.) then
				        !! update soil profile water after tile drainage
				        sumqtile = qtile
				        do j1 = 1, sol_nly
					        xx = sol_st(j1) - sol_fc(j1)
					        if (xx > 0.) then
						        if (xx > sumqtile) then
							        sol_st(j1) = sol_st(j1) - sumqtile
							        sumqtile = 0.
						        else
							        sumqtile = sumqtile - xx
							        sol_st(j1) = sol_fc(j1)
						        end if
					        end if
				        end do
				        if (sumqtile > 0.) then
					        qtile = qtile - sumqtile
					        qtile = amax1(0., qtile)
				        end if
			        end if
		        end if
                
               
                v_ri = latq
		        v_perco = sepbtm
        		
		        !! update soil profile water
		        v_ss= 0.
		        do j1 = 1, sol_nly
			        v_ss = v_ss + sol_st(j1)
		        end do

		        return
	        end subroutine
 
            subroutine percmacro
      
		        use CurrentUnitWSPDHMVars
		        use CurDHMParam
                integer :: j, ly
                real :: sepcrk, crklch = 0.5, xx,crk

                j = 0

                sepcrk = 0.
                sepcrk = Min(voltot, v_infil)
                if (sepcrk > 1.e-4) then
                    do ly = sol_nly, 1, -1
                        crk = 0.
                        xx = 0.
                        if (ly == sol_nly) then
                            crk = crklch * (volcr(ly) / (sol_z(ly) - sol_z(ly-1)) &
                            &                                              * voltot - volcrmin)
                            if (crk < sepcrk) then
                                sepcrk = sepcrk - crk
                                sepbtm = sepbtm + crk
                                sol_prk(ly) = sol_prk(ly) + crk
                            else
                                sepbtm = sepbtm + sepcrk
                                sol_prk(ly) = sol_prk(ly) + sepcrk
                                sepcrk = 0.
                            end if
                        endif
                        xx = sol_fc(ly) - sol_st(ly)
                        if (xx > 0.) then
                            crk = Min(sepcrk, xx)
                            sol_st(ly) = sol_st(ly) + crk
                            sepcrk = sepcrk - crk
                            if (ly /= 1) sol_prk(ly-1) = sol_prk(ly-1) + crk
                        end if
                        if (sepcrk < 1.e-6) exit
                    end do

                    !! if soil layers filled and there is still water attributed to
                    !! crack flow, it is assumed to percolate out of bottom of profile
                    if (sepcrk > 1.e-4) then
                        sepbtm = sepbtm + sepcrk
                        sol_prk(sol_nly) = sol_prk(sol_nly) + sepcrk
                    end if
                end if

                return
	        end subroutine
	        
            subroutine percmicro(IX,ly1)
		        use CurrentUnitWSPDHMVars
		        use CurDHMParam
		        use SolutionMod
		        use ReachInfoMod
		        integer, intent (in) :: IX,ly1
		        integer :: j,nn,ly
		        real :: adjf, yy, dg, ho, ratio,xx

		        j = 0

		        adjf = 0.5
                
                !! COMPUTE LATERAL FLOW USING HILLSLOPE STORAGE METHOD
                if (ly1 == 1) then
			        yy = 0.
                else
			        yy = 0.
			        yy = sol_z(ly1-1)
                end if

                dg = 0.
                ho = 0.
                latlyr = 0.
                dg = sol_z(ly1) - yy
                if (sol_ul(ly1) - sol_fc(ly1) == 0.) then
			        ho = 0.
                else
			        ho = 2. * sw_excess / ((sol_ul(ly1) - sol_fc(ly1)) / dg)
                end if
                
                latlyr = adjf * ho * sol_k(ly1)*Cursolution.dt * UnitSlopeTan/ (Reachs(IX).SLSUBBSN ) * .024

		        if (latlyr < 0.) latlyr = 0. 
		        if (latlyr > sw_excess) latlyr = sw_excess

		        !! compute seepage to the next layer
		        sepday = sw_excess - latlyr
!		        if(sol_hk(ly1) < 1.) sol_hk(ly1) = 1.
!		        sepday = sw_excess * (1. - Exp(-24. / sol_hk(ly1)))

		        !! restrict seepage if next layer is saturated
		        if (ly1 == sol_nly) then
			        xx = (dep_imp - sol_z(ly1)) / 1000.
			        if (xx < 1.e-4) then
				        sepday = 0.
			        else
				        sepday = sepday * xx / (xx + Exp(8.833 - 2.598 * xx))
			        end if
		        end if

		        !! check mass balance
		        if (sepday + latlyr > sw_excess) then
			        ratio = 0.
			        ratio = sepday / (latlyr + sepday)
			        sepday = 0.
			        latlyr = 0.
			        sepday = sw_excess * ratio
			        latlyr = sw_excess * (1. - ratio)
		        endif
        		
		        return
	        end subroutine
	        
            subroutine sat_excess(j1)
		        use CurrentUnitWSPDHMVars
		        use CurDHMParam
		        integer :: j, j1
		        real:: ul_excess
		        integer nn,ly

		        j = 0

		        if (j1 < sol_nly) then
			        if (sol_st(j1) - sol_ul(j1) > 1.e-4) then
				        sepday = sepday + (sol_st(j1) - sol_ul(j1))
				        sol_st(j1) = sol_ul(j1)
			        end if
		        else

			        if (sol_st(j1) - sol_ul(j1) > 1.e-4) then
				        ul_excess = sol_st(j1) - sol_ul(j1)
				        sol_st(j1) = sol_ul(j1)
				        nn = sol_nly
				        do ly = nn - 1, 1, -1
					        sol_st(ly) = sol_st(ly) + ul_excess
					        if (sol_st(ly) > sol_ul(ly)) then
						        ul_excess = sol_st(ly) - sol_ul(ly)
						        sol_st(ly) = sol_ul(ly)
					        else
						        ul_excess = 0.
						        exit
					        end if					
				        end do
			        end if
		        end if

		        return
	        end subroutine    
    end	 module       