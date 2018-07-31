Module Sim_ETDHM
    
		use CurrentUnitWSPDHMVars
		use CurrentUnitVars
		use UnitInfoMod
		use CurDHMParam
		use solutionMod
		use TimeInfoMod
		
		implicit none
        
        contains
        
            subroutine ETSoilPlant()
	        	use CurrentUnitWSPDHMVars
                !计算土壤实际蒸发量
                call CalSoilEvapDHM() 
               
                !如果有作物计算作物实际腾发量
                if (p_lai > 0.8) then
                    call CalPlantTransDHM()
                else
                    v_ep = 0.
                endif

                return
            end subroutine

	        ! 土壤实际蒸发
	        subroutine CalSoilEvapDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use CurDHMParam
		        use solutionMod
    	        !call InsertDebug
		        real v_esmax,yy,esleft,etco,cej,eaj,petsoil,eos1
		        integer ipet
		        integer i
		        real dd,xx,v_esmax1,v_spet
		        
		        ipet = cursolution.ipet
        		
   		        !! compute potential plant evap for methods other that Penman-Monteith
		        if (imp_w>0.0) then
			        v_pet = (v_pet- v_ei-v_ed) *(1-imp_w)
!			        v_pet = v_pet
		        end if
        		
		        if (ipet /= 2) then
			        if (p_lai <= 3.0) then
				        v_ep_max = p_lai * v_pet / 3.               !植物最大蒸发
			        else
				        v_ep_max = v_pet
			        end if
			        if (v_ep_max < 0.) v_ep_max = 0.
		        end if
        		
                !! compute potential soil evaporation
		        cej = -5.e-5
		        eaj = 0.
                sol_cov = 0.2
		        eos1 = 0.

		        if (v_snow >= 0.5) then
			        eaj = 0.5
		        else
			        eaj = Exp(cej * (sol_cov + 0.1))
		        end if
		        v_esmax = v_pet * eaj
		        eos1 = v_pet / (v_esmax + v_ep_max + 1.e-10)
		        eos1 = v_esmax * eos1
		        v_esmax = Min(v_esmax, eos1)
		        v_esmax = Max(v_esmax, 0.)                  !土壤最大蒸发
        		
                !! make sure maximum plant and soil ET doesn't exceed potential ET
		        if (v_pet < v_esmax + v_ep_max) then
		            v_esmax1 = v_esmax
			        v_esmax = v_pet * v_esmax / (v_esmax + v_ep_max)
			        v_ep_max = v_pet * v_ep_max / (v_esmax1 + v_ep_max)
		        end if
        	
	            yy=0.
	            esleft = v_esmax	
	            etco = 0.80
	            do i=1,sol_nly
                   if(sol_st(i) >20000)  sol_st(i) = 0.25 * sol_h(i)
	               dd=(yy+sol_z(i))/2
	               xx=sol_z(i)-yy
	               sol_es(i)=v_esmax * dd / (dd + exp(2.347 - dd * 0.00713))   
	               if (sol_st(i).lt.sol_fc(i)) then
	                   sol_es(i)=sol_es(i)*exp(2.5*(sol_st(i)-sol_fc(i))/(sol_fc(i)))
	                   sol_es(i) = Min(sol_es(i), sol_st(i) * etco, v_esmax)
	               endif
		           if (sol_es(i) < 0.) sol_es(i) = 0.
		           if (sol_es(i) > esleft) sol_es(i) = esleft

		            !! adjust soil storage, potential evap
		           if (sol_st(i) > sol_es(i) ) then
			            esleft = esleft - sol_es(i)
			            sol_st(i) = Max(1.e-6, sol_st(i) - sol_es(i))
		           else
			            esleft = esleft - sol_st(i)
			            sol_st(i) = 0.
		           endif
        	       
	               yy=sol_z(i)  
	            enddo


		        !! update soil profile water
		        v_ss= 0.
		        do i = 1, sol_nly
			        v_ss = v_ss + sol_st(i)
		        end do	
        	
		        !! calculate actual amount of evaporation from soil
		        v_es = v_esmax - esleft
                if(esleft < 0) then
                    v_es = v_esmax
                endif
		        if (v_es < 0.) v_es = 0.
		        return
	        end subroutine
        	
	        !作物实际腾发量
	        subroutine CalPlantTransDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use CurDHMParam
		        use TimeInfoMod
		        integer :: k, ir,j1
		        real, dimension(sol_nly) :: wuse
		        real ubw,uobw,sol_rd,yy
		        real, external :: CalRootDpth
		        real :: sum, xx, gx, reduc, ul4, sump, satco, strsa, epco
        		
		        ir=0
		        sump=0
		        xx = 0.
        		
		        !!  compute aeration stress
		        if (v_ss > sol_sumfc) then
			        satco = (v_ss - sol_sumfc) / (sol_sumul - sol_sumfc)
			        strsa = 1. - (satco / (satco + Exp(.176 - 4.544 * satco)))
		        else
			        strsa = 1.
		        end if
        		
		        epco =1
		        ubw= 10
		        uobw = 1. - exp(-ubw)
        		
		        !当前根深
		        sol_rd= CalRootDpth(jdt,p_lai,lai_max,rootdepth)

		        if (v_ep_max <= 0.01) then
			        !strsw(j) = 1.
		        else
        			
			        do k = 1, sol_nly
				        if (ir > 0) exit

				        if (sol_rd <= sol_z(k)) then
					        gx = sol_rd
					        ir = k
				        else
					        gx = sol_z(k)
				        end if

				        sum = 0.
				        if (sol_rd <= 0.01) then
					        sum = v_ep_max / uobw
				        else
					        sum = v_ep_max * (1. - Exp(-ubw * gx / sol_rd)) / uobw
				        end if

				        !! don't allow compensation for aeration stress
				        if (strsa > .99) then
					        yy = 0.
				        else
					        yy= sump - xx
				        end if
				        wuse(k) = sum - sump + yy * epco
				        wuse(k) = sum - sump + (sump - xx) * epco
				        sump = sum

				        !! adjust uptake if sw is less than 25% of plant available water
				        reduc = 0.
				        if (sol_st(k) < sol_fc(k)/4.) then
					        reduc = Exp(5. * (4. * sol_st(k) / sol_fc(k) - 1.))
				        else
					        reduc = 1.
				        endif
				        reduc = 1.
				        wuse(k) = wuse(k) * reduc

				        if (sol_st(k) < wuse(k)) then
					        wuse(k) = sol_st(k)
				        end if

				        sol_st(k) = Max(1.e-6, sol_st(k) - wuse(k))
				        xx = xx + wuse(k)
			        end do

		            !! update soil profile water
		            v_ss= 0.
		            do j1 = 1, sol_nly
			            v_ss = v_ss + sol_st(j1)
		            end do	

			        v_ep = min(xx,v_ep_max)
		        end if

		        return
	        end subroutine
	end module
