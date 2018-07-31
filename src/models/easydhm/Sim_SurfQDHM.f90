Module Sim_SurfQDHM

        implicit none
        contains 
            subroutine curno(cnn,wrt1,wrt2,smx)
		        use Sim_SnowDHM
		        use CurrentUnitWSPDHMVars
 
                real :: cnn,wrt1,wrt2,smx
                real :: c2, s3, rto3, rtos
                real :: cn3,cn1

                c2 = 0.
                cn3 = 0.
                s3 = 0.
                cn1 = 0.

                !! calculate moisture condition I and III curve numbers
                c2 = 100. - cnn
                cn1 = cnn - 20. * c2 / (c2 + Exp(2.533 - 0.0636 * c2))
                cn1 = Max(cn1, .4 * cnn)
                cn3 = cnn * Exp(.006729 * c2)

                !! calculate maximum retention parameter value
                smx = 254. * (100. / cn1 - 1.)
                !! calculate retention parameter value for CN3
                s3 = 254. * (100. / cn3 - 1.)

                !! calculate fraction difference in retention parameters
                rto3 = 0.
                rtos = 0.
                rto3 = 1. - s3 / smx
                rtos = 1. - 2.54 / smx
                !! calculate shape parameters
!                write(*,*)cn1,cn3,rto3
                call ascrv(rto3,rtos,sol_sumfc,sol_sumul,wrt1,wrt2)
                return
            end subroutine  
            
            subroutine dailycn(cnday,wrt1,wrt2,smx)
		        use CurrentUnitWSPDHMVars
                real :: xx, r2,cnday,smx,wrt1,wrt2


                xx = 0.
                r2 = 0.
                xx = wrt1 - wrt2 * v_ss
                if (xx < -20.) xx = -20.
                if (xx > 20.) xx = 20.


                !! traditional CN method (function of soil water)
                if ((v_ss + Exp(xx)) > 0.001) then
                r2 = smx * (1. - v_ss / ( v_ss + Exp(xx)))
                end if

                r2 = amax1(3.,r2)

                cnday = 25400. / (r2 + 254.)

                return
            end subroutine              
   
            subroutine surq_daycn(cnday)
		        use CurrentUnitWSPDHMVars
                use CurrentUnitVars
                real :: r2, bb, pb,cnday

                v_netp=v_netp+v_sm
                !if (tmp_srf .ge. 0) then
                v_rs=0.
                r2 = 0.
                bb = 0.
                pb = 0.
                r2 = 25400. / cnday - 254.
                bb = .2 * r2
                pb = v_netp - bb
                            
                if (pb > 0.) then
                    v_rs = pb * pb / (v_netp + .8 * r2)          !地表径流mm
                end if

                           
                v_infil=v_netp-v_rs                   !地表入渗量mm
                    		
                if (v_infil<0) then
                    v_infil=0.0
                endif
                if (v_ss>0.9*sol_sumul) then								!for saturation overland flow
                    v_rs=v_netp
                    v_infil=0.0
                end if
                
                return
            end subroutine
     
     
            subroutine calsurqimpDHM
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
                real :: r2, bb, pb,cnday

                v_p=v_p+v_sm
!                v_netp=v_p

                !if (tmp_srf .ge. 0) then
                
                v_infil=0                  !地表入渗量mm
            	v_ei=0.0
		        v_interc=0.0
		        v_sint=0.0
		        v_netp=v_p-imp_w*v_pet
                if(v_netp<0) v_netp=0
                v_rs=v_netp
                if(v_rs<0) v_rs=0
                
                return
            end subroutine
             
            subroutine calsurqwaterDHM
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use EasyDHMParamMod
                real :: r2, bb, pb,cnday

                !if (tmp_srf .ge. 0) then
                v_p=v_p+v_sm
                !if (tmp_srf .ge. 0) then
                v_infil=0                  !地表入渗量mm
            	v_ei=0.0
		        v_interc=0.0
		        v_sint=0.0
		        v_netp=v_p
                v_ed=0
                v_depre=0
                v_netp=v_netp-v_pet
                if(v_netp<0) v_netp=0
                v_rs=v_netp
!                v_rs=v_rs*watercofm
                if(v_rs<0) v_rs=0   
                v_ri=0
                v_rg=0
                
                return
            end subroutine        
	        !冠层截留模拟
	        subroutine CalInterceptionDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        if (v_p>0.0 .or. v_sint>0.0) then
        			
			        !冠层截留蒸发
			        if (v_pet<v_sint) then
				        v_ei=v_pet							
			        else
				        v_ei=v_sint        !冠层截留蒸发量
			        end if
        			
			        !冠层截留
			        if (v_p<=(p_intercap-v_sint)) then
				        v_interc=v_p
			        else
				        v_interc=max(0.0,p_intercap-v_sint)
			        end if
			        v_sint=MAX(0.0,v_sint+v_interc-v_ei)
			        v_netp=MAX(0.0,v_p-v_interc)
		        else
			        v_ei=0.0
			        v_interc=0.0
			        v_sint=0.0
			        v_netp=0.0
		        end if	
        		
		        return
	        end subroutine
        	
	        !地表填洼
	        subroutine CalDepressionDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use CurDHMParam
        		
		        !depression and overland flow
		        if (v_infil>0.0) then
			        if ((v_pet-v_ei)<v_sdep) then !v_ei 冠层截留蒸发量mm, v_pet潜在蒸发量mm,v_sdep地表填洼储量mm
				        v_ed=v_pet-v_ei         !v_ed 地表填洼蒸发量mm
			        else
				        v_ed=v_sdep
			        end if
			        v_sdep1=v_sdep
			        v_crain=v_infil-depression*LOG(1.0-v_sdep/depression)!v_crain累积地表径流mm,depression地表填洼能力
			        v_depre=v_infil*EXP(0.0-v_crain/depression)!v_depre地表填洼量mm
			        v_sdep=MAX(0.0,v_sdep+v_depre-v_ed)
			        if (v_sdep>=depression) then
				        v_depre=depression-v_sdep1
				        v_sdep=depression
			        end if
			        v_infil=v_infil-v_depre
		        else if (v_sdep>0.0) then
			        if ((v_pet-v_ei)<v_sdep) then
				        v_ed=v_pet-v_ei
			        else
				        v_ed=v_sdep
			        end if
			        v_run=runoff_co*v_ss/porosity		       
			        v_infil=max(0.0,(v_sdep-v_ed)*(1-v_run))
			        v_depre=0.0
			        v_sdep=v_sdep-v_ed-v_infil
		        else
			        v_ed=0.0
			        v_depre=0.0
			        v_sdep=0.0
			        v_infil=0.0
		        end if
!		        !write(*,*) v_pet,v_ei,v_sdep,v_rs,v_infil
!		        !write(*,*) porosity,depression
        			
		        return
	        end subroutine
	        
	        subroutine CalDepressionimpDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use CurDHMParam
        		
		        !depression and overland flow
		        if (v_rs>0.0) then
			        if (v_pet<v_sdep) then !v_ei 冠层截留蒸发量mm, v_pet潜在蒸发量mm,v_sdep地表填洼储量mm
				        v_ed=v_pet         !v_ed 地表填洼蒸发量mm
			        else
				        v_ed=v_sdep
			        end if
			        v_sdep1=v_sdep
			        v_crain=v_rs-depression*LOG(1.0-v_sdep/depression)!v_crain累积地表径流mm,depression地表填洼能力
			        v_depre=v_rs*EXP(0.0-v_crain/depression)!v_depre地表填洼量mm
			        v_sdep=MAX(0.0,v_sdep+v_depre-v_ed)
			        if (v_sdep>=depression) then
				        v_depre=depression-v_sdep1
				        v_sdep=depression
			        end if
			        v_rs=v_rs-v_depre
		        else
			        v_ed=0.0
			        v_depre=0.0
			        v_sdep=0.0
			        v_rs=0.0
		        end if
!		        !write(*,*) v_pet,v_ei,v_sdep,v_rs,v_infil
!		        !write(*,*) porosity,depression
        			
		        return
	        end subroutine
	        
	        subroutine CalDepressionwaterDHM()
		        use CurrentUnitWSPDHMVars
		        use CurrentUnitVars
		        use CurDHMParam
        		
		        v_ed=0.0
		        v_depre=0.0
		        v_sdep=0.0

        			
		        return
	        end subroutine
	    end	 module 