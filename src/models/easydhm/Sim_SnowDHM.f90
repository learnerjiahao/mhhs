 Module Sim_SnowDHM
        
        implicit none

        contains 
        
            subroutine ascrv(x1,x2,x3,x4,x5,x6)
              
                real :: xx

                real, intent (in) :: x1, x2, x3, x4
                real, intent (out) :: x5, x6

                xx = 0.0
                x5 = 0.0
                x6 = 0.0

                xx = Log(x3/x1 - x3)
                x6 = (xx - Log(x4/x2 - x4)) / (x4 - x3)
                x5 = xx + (x3 * x6)

                return
            end  subroutine

            subroutine snom(IX)
                use CurrentUnitVars
                use CurrentUnitWSPDHMVars
                use WeatherMod
                use TimeInfoMod
                use CurDHMParam
                
                integer :: j, ib
                real :: smfac, smleb,snotmp
                real :: xx, snofall 
                integer IX
         
                j = IX

                !! estimate snow pack temperature
!                snotmp = snotmp * (1. - p_timp) + v_t * p_timp
                SnowTmp = SnowTmp * (1. - p_timp) + v_t * p_timp
                if (v_t <= p_sftmp) then
                  !! calculate snow fall
                  v_snow= v_snow + v_p            !v_p降雨量
                  snofall = v_p
                  v_p = 0.
                  v_netp=0
                endif
         
                if (s_weather(j).tmax(iwLoop) > p_smtmp .and. v_snow > 0.) then
                  !! adjust melt factor for time of year
                  smfac = 0.
                  v_sm = 0.
                  smfac = (p_smfmx + p_smfmn) / 2.+Sin((jdt - 81) / 58.09) *(p_smfmx - p_smfmn) / 2.    !! 365/2pi = 58.09
                  v_sm = smfac * ((SnowTmp +s_weather(j).tmax(iwLoop)) / 2.) - p_smtmp
                  !! adjust for areal extent of snow cover
                  if (v_snow < p_snocovmx) then
                    xx = 0.
                    xx = v_snow / p_snocovmx
                    snocov = xx / (xx + Exp(snocov1 - snocov2 * xx))
                  else
                    snocov = 1.
                  endif
                  v_sm = v_sm * snocov
                  if (v_sm < 0.) v_sm = 0.
                  if (v_sm > v_snow) v_sm = v_snow
                  v_snow = v_snow - v_sm          !v_snow积雪量  v_sm融雪量
                  v_netp =v_p + v_sm              !v_p降雨量  v_netp 净雨量
               
                  if (v_netp< 0.) v_netp = 0.
                else
                  v_sm = 0.
                end if
     
                return
            end subroutine           

            subroutine snomwater(IX)
                use CurrentUnitVars
                use CurrentUnitWSPDHMVars
                use WeatherMod
                use TimeInfoMod
                use CurDHMParam
                
                integer :: j, ib
                real :: smfac, smleb,snotmp
                real :: xx, snofall 
                integer IX
         
                j = IX

                !! estimate snow pack temperature
!                SnowTmp = SnowTmp * (1. - p_timp) + v_t * p_timp
                if (v_t <= p_sftmp) then
                  !! calculate snow fall
                  v_snow= v_snow + v_p            !v_p降雨量
                  snofall = v_p
                  v_p = 0.
                  v_netp=0
                endif
                
                if (s_weather(j).tmax(iwLoop) > p_smtmp .and. v_snow > 0.) then
                  v_sm = v_snow
                  if (v_sm < 0.) v_sm = 0.
                  if (v_sm > v_snow) v_sm = v_snow
                  v_snow = v_snow - v_sm          !v_snow积雪量  v_sm融雪量
                  v_netp =v_p + v_sm              !v_p降雨量  v_netp 净雨量
               
                  if (v_netp< 0.) v_netp = 0.
                else
                  v_sm = 0.
                end if
     
                return
            end subroutine     
                   
    end	 module 