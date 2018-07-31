! EasyDHM产流算法
    Module DHMMod
        
        implicit none
        integer :: IParamRange
        integer :: IX,IY,landid
        real unitarea,sumland(6)
        contains
        
            !EasyDHM产流模拟主程序
            subroutine Sim_EasyDHM 
                use SolutionMod
                use SubbasinMod  
                use ParamRangeMod
                use UnitInfoMod
                use CurDHMParam
                use EasyDHMVarMod
                integer i
                
                IParamRange = CurSolution%IParamRange
                sumland=0
                do i=1, ParamRanges(IParamRange)%NPartSubbasin
                    IX = ParamRanges(IParamRange)%PartSubbasins(i)

                    do IY=1, Subbasins(IX)%NUnits
                        unitarea=0

                        do landid=1,UnitNlanduse
                            unitarea=unitarea+Units(IX,IY)%Area(landid)
                        enddo
                        Units(IX,IY)%Areasum=unitarea
                        call initunitEasyDHM
!                        landid=4
                        do landid=1,UnitNlanduse

                            call initsublandStat                 
                            
!        		            if(Units(IX,IY)%Area(landid)==0) cycle
                            !初始化Temperal和Spatial变量
                            call TSinitEasyDHM()

                            !读取状态变量
                            call GetUnitVarsEasyDHM()
                            
                            !积雪/融雪过程模拟
                            if(landid .ne. 4)then
                                call CalSnowEasyDHM
                            elseif(landid==4)then
                                call CalSnowwaterEasyDHM
                            endif
                            
!				            if(landid==5)then
!				                call calSurQimpEasyDHM
!				            elseif(landid==4)then
                            if(landid==4)then
                                call calSurQwaterEasyDHM
                                
                            else
                            !地表过程模拟(包含截留和填洼过程)
                                call CalSurfQEasyDHM
                                
                                !土壤过程模拟（包含冻土冻融过程）
                                call CalSoilEasyDHM            
                                
                                !地下水模拟
                                call CalGWEasyDHM
                            
                            endif
                            !保存当前计算单元的所有变量
                            call SaveUnitVarsEasyDHM()	
                            !由各土地分类统计至单元
                            call EasyDHMUnitlandStat
                            !单元各土地分类统计至子流域上的各土地分类	
                            call EasyDHMsublandStat	
                                               
                        enddo

                        do landid=1,UnitNlanduse
                            sumland(landid)=sumland(landid)+(uland_DHMVars(IX,IY,landid)%rs+uland_DHMVars(IX,IY,landid)%ri+uland_DHMVars(IX,IY,landid)%rg)*Units(IX,IY)%Area(landid)/unitarea
                        enddo
                        
                        !计算结果单元统计
                        call EasyDHMUnitStat
                        
            
                    enddo
                enddo
!		        write(6,'(i,<6>f)')IParamRange,(sumland(landid),landid=1,6)
                return
                    
            end subroutine
            
            subroutine initsublandStat
                use EasyDHMVarMod
                sland_DHMVars(IX,IY,landid)%PET =0
                sland_DHMVars(IX,IY,landid)%sm =0
                sland_DHMVars(IX,IY,landid)%ed  =0
                sland_DHMVars(IX,IY,landid)%depre  =0
                sland_DHMVars(IX,IY,landid)%netp   =0
                sland_DHMVars(IX,IY,landid)%rs   =0
                
                
                sland_DHMVars(IX,IY,landid)%ei =0
                sland_DHMVars(IX,IY,landid)%interc =0
                sland_DHMVars(IX,IY,landid)%infil =0
                sland_DHMVars(IX,IY,landid)%ep =0
                sland_DHMVars(IX,IY,landid)%es  =0
                sland_DHMVars(IX,IY,landid)%ri  =0
                sland_DHMVars(IX,IY,landid)%perco =0
                sland_DHMVars(IX,IY,landid)%eg   =0
                sland_DHMVars(IX,IY,landid)%rg =0
                
            end subroutine
            
            subroutine initunitEasyDHM
                use EasyDHMVarMod
                integer i
                
                ! 状态信息
                u_DHMVars(IX,IY)%sint=0		!冠层截留储量mm
                u_DHMVars(IX,IY)%sdep=0		!地表填洼储量mm
                u_DHMVars(IX,IY)%ss	=0	!土壤含水量mm
                u_DHMVars(IX,IY)%gwht=0		!地下水水位mm
                u_DHMVars(IX,IY)%gt	=0	!地下水储量mm
                u_DHMVars(IX,IY)%PET=0			!潜在蒸发mm
                u_DHMVars(IX,IY)%SM  =0    	!融雪量mm
                u_DHMVars(IX,IY)%Snow=0		!融雪量mm
                u_DHMVars(IX,IY)%EI	=0		!冠层截留蒸发量mm
                u_DHMVars(IX,IY)%Interc=0		!冠层截留量mm
                u_DHMVars(IX,IY)%ED	=0		!地表填洼蒸发量mm
                u_DHMVars(IX,IY)%Depre=0		!地表填洼量mm
                u_DHMVars(IX,IY)%NetP=0		!净雨量mm
                u_DHMVars(IX,IY)%RS	=0		!地表径流mm
                u_DHMVars(IX,IY)%Infil=0		!地表入渗量mm
                u_DHMVars(IX,IY)%EP	=0		!植被蒸发mm
                u_DHMVars(IX,IY)%ES	=0		!地表蒸发mm
                u_DHMVars(IX,IY)%RI	=0		!壤中流量mm
                u_DHMVars(IX,IY)%Perco=0		!补给地下水量mm
                u_DHMVars(IX,IY)%EG	=0		!地下蒸发量mm
                u_DHMVars(IX,IY)%RG	=0		!地下径流量mm
                
                ! 退水径流
                u_DHMVars(IX,IY)%QI	=0		!壤中流径流量m3/s
                u_DHMVars(IX,IY)%QG	=0		!地下径流量m3/s
                
                u_DHMVars(IX,IY)%E	=0		!总蒸发量mm
                u_DHMVars(IX,IY)%R	=0		!总径流量mm
                u_DHMVars(IX,IY)%SurTmp=0		!土壤表层温度
                u_DHMVars(IX,IY)%SolFro	=0	!土壤冻结水量
                u_DHMVars(IX,IY)%rchrg	=0	!地下水补给量
                u_DHMVars(IX,IY)%SnowTmp=0
                u_DHMVars(IX,IY)%WiltSum=0
                            
                do i=1,WaterShed%Nlayer
                    l_DHMVars(IX,IY,i)%soltmp=0
                    l_DHMVars(IX,IY,i)%solfro= 0
                    l_DHMVars(IX,IY,i)%ss=0
                    l_DHMVars(IX,IY,i)%excess=0
                    l_DHMVars(IX,IY,i)%solz=0
                    l_DHMVars(IX,IY,i)%solh=0
                enddo
            
            end subroutine
            
            ! 初始化Temporal和Spatial变量
            subroutine TSinitEasyDHM()
                use WeatherMod
                use SubbasinMod 
                use CurrentUnitVars      
                use CurrentUnitWSPDHMVars
                use EasyDHMVarMod
                use TimeInfoMod

!                if (iLoop == 115) then
!                !write(*,*)
!                endif
                ! 状态变量初始化
                v_snow=0.0									!snow storage
                v_sint=0.0									!interception storage
                v_sdep=0.0									!depression storage
                v_ss=0.0									!soil moisture 
                v_gt=0.0									!groundwater storage
                
                ! 过程变量
                v_p=0.0										!rainfall
!		        v_pet=0.0									!potential evapotranspiration
                v_netp=0.0									!net precipitation
                v_infil=0.0									!infiltration
                v_rs=0.0									!excess rainfall
                v_crain=0.0									!cumulative excess rainfall
                v_depre=0.0									!depression
                v_interc=0.0								!interception
                v_run=0.0									!actual runoff coefficient
                v_ei=0.0									!evaporation from interception storage
                v_ed=0.0									!evaporation from depression storage
                v_es=0.0									!evapotranspiSlrration from soil
                v_eg=0.0									!evapotranspiration from groundwater storage				
                v_perco=0.0									!percolation
                v_ri=0.0									!interflow 
                v_rg=0.0									!groundwater flow in mm
                v_qg=0.0									!groundwater flow in m^3/s
                v_sm=0.0									!snowmelt mm
                SepBtm=0.                                   !temperature of snow pack in elevation band
                VolCrMin=0.                                 !minimum crack volume allowed in any soil layer
                
                s_DHMVars(IX)%SnowTmp=0.0                                 !temperature of snow pack in unit
                
                !气象信息
                if (Cursolution%Iweather == 1) then
                    v_p =s_Weather(IX)%Rain(iLoop)				! 降雨
                else
                    v_p =s_Weather(IX)%Rain(iwLoop)*Cursolution%dt/24.0				! 降雨
                endif

                v_t = s_Weather(IX)%Tavg(iwLoop)				! 温度
                v_hmd = s_Weather(IX)%Hmdt(iwLoop)				! 湿度
                v_wnd = s_Weather(IX)%Wind(iwLoop)				! 风速
                v_ssh = s_Weather(IX)%slr(iwLoop)				! 日照时数
                v_DLAT = Subbasins(IX)%Lati			! 形心纬度（弧度）
                    

                !设置当期计算单元的参数
                call SetCurrentEasyDHMPara()

                return
            end subroutine
            
            ! EasyDHM模型 - 修改模型关键参数
            subroutine SetCurrentEasyDHMPara()
                use UnitInfoMod
                use SoilInfoMod
                use EasyDHMVarMod        
                use EasyDHMParamMod
                use CurDHMParam
                use CurrentUnitWSPDHMVars
                real,external ::  CalInterCapacity,CalLai 
                real p_lai_max, p_lai_min, p_itc_max 
                real xx,dg,pormm,sumpor
                integer j     
                
                !产流参数修正
                UnitSlope    = Units(IX,IY)%Slope        * EasyDHMParams(IParamRange)%UnitSlopeM
                runoff_co    = Units(IX,IY)%runoff       * EasyDHMParams(IParamRange)%RunOffCoeM
                porosity     = Units(IX,IY)%porosity     * EasyDHMParams(IParamRange)%PorosityM
                fieldcap     = Units(IX,IY)%fieldcap     * EasyDHMParams(IParamRange)%FieldCapM
                poreindex    = Units(IX,IY)%poreindex
                lai_max      = Units(IX,IY)%lai_max(landid)      * EasyDHMParams(IParamRange)%LaiMaxM(landid)
                if(landid .ne.4)then
                    depression   = Units(IX,IY)%Depression   * EasyDHMParams(IParamRange)%DepressM(landid)
                endif
                rootdepth    = Units(IX,IY)%rootdepth(landid)  * EasyDHMParams(IParamRange)%RtDpthM(landid)
                itc_max      = Units(IX,IY)%itc_max(landid)      * EasyDHMParams(IParamRange)%ItcmaxM(landid)
!				if(landid==5)then
                    imp_w        = Units(IX,IY)%imp          * EasyDHMParams(IParamRange)%impM
!				else
!				    imp_w=0
!				endif
                wilting      = Units(IX,IY)%Wilting

                dep_imp = Soils(IX)%dep_imp * EasyDHMParams(IParamRange)%dep_impM
                ddrain  = Soils(IX)%ddrain * EasyDHMParams(IParamRange)%ddrainM
                sol_crk = Soils(IX)%sol_crk * EasyDHMParams(IParamRange)%sol_crkM
                do j=1,sol_nly
                    sol_k(j) = Units(IX,IY)%conduct *  EasyDHMParams(IParamRange)%ConductM(j)
                enddo

                if(runoff_co .ge.1) then
                    runoff_co=0.95
                endif
                if (lai_max.le.Units(IX,IY)%lai_min(landid)) then
                    lai_max = Units(IX,IY)%lai_min(landid) * 2
                endif
                if(UnitSlope.gt.90.) then
                    UnitSlope=90.	
                endif
                UnitSlopeTan=tan(UnitSlope/180.)
                if(fieldcap.le.Units(IX,IY)%wilting) then
                    fieldcap=porosity*0.9
                endif				
                if(porosity.gt.1.) then
                    porosity=1.
                endif
                if(fieldcap.gt.porosity) then
                    fieldcap=0.9 * porosity
                endif
                
                p_lai_max = lai_max
                p_lai_min = Units(IX,IY)%lai_min(landid)
                p_itc_max = itc_max
                p_lai = CalLai(jdt,p_lai_max,p_lai_min)								!计算叶面积指数
                p_intercap = CalInterCapacity(jdt,p_lai,p_lai_max,p_itc_max)		!计算冠层截留能力
                
                xx = 0.
                sumpor = 0.
                sol_sumul = 0.
                sol_sumfc = 0.
                do j = 1, sol_nly
!                    sol_z(j) = l_DHMVars(IX,IY,j).solz
!	                sol_h(j) = l_DHMVars(IX,IY,j).solh

                    sol_z(j) = lland_DHMVars(IX,IY,j,landid)%solz
                    sol_h(j) = lland_DHMVars(IX,IY,j,landid)%solh

                    dg = 0.
                    pormm = 0.
                    dg = sol_z(j) - xx
                    pormm = Porosity * dg
                    sumpor = sumpor + pormm
                    sol_ul(j) = (Porosity - wilting) * dg
                    sol_sumul = sol_sumul + sol_ul(j)
                    sol_fc(j) = dg * (fieldcap - wilting)
                    sol_sumfc = sol_sumfc + sol_fc(j)

                    sol_hk(j) = (sol_ul(j) - sol_fc(j)) / sol_k(j)

                    if (sol_hk(j) < 1.) sol_hk(j) = 1.
                    xx = sol_z(j)
                    crdep(j) = sol_crk * 0.916 * Exp(-.0012 * sol_z(j)) * dg
!                    volcr(j) = crdep(j) * (sol_fc(j) - l_DHMVars(IX,IY,j).ss) / (sol_fc(j))
                    volcr(j) = crdep(j) * (sol_fc(j) - lland_DHMVars(IX,IY,j,landid)%ss) / (sol_fc(j))
                end do
                sol_avpor = Porosity
                sol_avbd = 2.65 * (1. - sol_avpor)    
                           
                return
            end subroutine

            !获取计算单元产流变量值
            subroutine GetUnitVarsEasyDHM()
                use EasyDHMVarMod     
                use solutionMod   
                use CurrentUnitWSPDHMVars
                use CurDHMParam
                use CurrentUnitVars
                use WeatherMod
                use ETpotMod

                integer i

                !================================================================================
                ! 状态变量
                !================================================================================	
                
                v_SInt  = uland_DHMVars(IX,IY,landid)%SInt			!冠层截留储量mm
                v_SDep  = uland_DHMVars(IX,IY,landid)%SDep			!地表填洼储量mm
                v_Ss    = uland_DHMVars(IX,IY,landid)%Ss				!土壤含水量mm
                v_GT    = uland_DHMVars(IX,IY,landid)%GT				!地下水储量mm
                v_RG    = uland_DHMVars(IX,IY,landid)%RG
                v_Snow  = uland_DHMVars(IX,IY,landid)%Snow			!积雪量mm
                v_rchrg = uland_DHMVars(IX,IY,landid)%rchrg        !地下水补给量mm
                GwHt    = uland_DHMVars(IX,IY,landid)%GwHt              !地下水位
                SnowTmp = s_DHMVars(IX)%SnowTmp
            
                do i=1,Sol_Nly
                    Sol_Fro(i) = lland_DHMVars(IX,IY,i,landid)%SolFro
                    Sol_Tmp(i) = lland_DHMVars(IX,IY,i,landid)%SolTmp
                    Sol_St(i)  = lland_DHMVars(IX,IY,i,landid)%Ss
                enddo
                
                if(IUpdateWeather == 1) then  
                    ! 计算潜在蒸发 v_pet，作物最大蒸发 v_ep_max
                    call sim_ETPot(IX,IY,v_snow,p_lai) 
                
                    v_pet = p_PETM * v_pet * Cursolution%dt / 24
                    v_ep_max = p_PETM * v_ep_max* Cursolution%dt / 24

                else
                    v_pet = uland_DHMVars(IX,IY,landid)%PET
                    v_ep_max = uland_DHMVars(IX,IY,landid)%EPmax
                endif

                uland_DHMVars(IX,IY,landid)%PET = v_pet
                s_Weather(IX)%pet(iwLoop) = v_pet
                uland_DHMVars(IX,IY,landid)%EPmax = v_ep_max

                return
            end subroutine

            !积雪融雪模拟
            subroutine CalSnowEasyDHM
                use CurDHMParam
                use CurrentUnitWSPDHMVars
                use Sim_SnowDHM
                
                if(IY.eq.1) then
                    call Ascrv(.5,.95,p_sno50cov,.95,snocov1,snocov2)
                    call snom(IX)
                    SnowTemp = v_snow
                    SMTemp   = v_sm
                else
                    v_snow   = SnowTemp
                    v_sm     = SMTemp
                endif

                return
            end subroutine
            
            subroutine CalSnowwaterEasyDHM
                use CurDHMParam
                use CurrentUnitWSPDHMVars
                use Sim_SnowDHM
                
                if(IY.eq.1) then
                    call snomwater(IX)
                    SnowTemp = v_snow
                    SMTemp   = v_sm
                else
                    v_snow   = SnowTemp
                    v_sm     = SMTemp
                endif
                
                return
            end subroutine
            

            subroutine CalSurfQEasyDHM
            
                use Sim_SurfQDHM
                use CurrentUnitWSPDHMVars
                use CurrentUnitVars
                use CurDHMParam
                real Wrt1,Wrt2,Smx,cnday

                !  以下子程序计算地表径流模拟中的参数CN（与含水量有关）
                call curno(CN2(landid),Wrt1,Wrt2,Smx)

                call dailycn(cnday,wrt1,wrt2,smx)
                
                if (v_t>p_sftmp) then		!降雨
                
                    !截留模拟
                    call CalInterceptionDHM()
                    
                    !地表径流模拟
                    call surq_daycn(cnday)

                    !地表填洼
                    call CalDepressionDHM()
                else 
                    v_netp=0.
                    call surq_daycn(cnday)
                endif

                return
            end subroutine
            
            
            subroutine calSurQimpEasyDHM
            
                use Sim_SurfQDHM
                use CurrentUnitWSPDHMVars
                use CurrentUnitVars
                use CurDHMParam
                real Wrt1,Wrt2,Smx,cnday

                !  以下子程序计算地表径流模拟中的参数CN（与含水量有关）
!                call curno(CN2,Wrt1,Wrt2,Smx)
!
!                call dailycn(cnday,wrt1,wrt2,smx)
                
                if (v_t>p_sftmp.and.v_p>0) then		!降雨
                
                    !截留模拟
!	                call CalInterceptionDHM()
                    
                    !地表径流模拟
!                    call surq_daycn(cnday)
                    call calsurqimpDHM

                    !地表填洼
                    call CalDepressionimpDHM()
                else 
                    v_netp=0.
                    call calsurqimpDHM()
!                    call surq_daycn(cnday)
                endif

                return
            end subroutine
            
            subroutine calSurQwaterEasyDHM
            
                use Sim_SurfQDHM
                use CurrentUnitWSPDHMVars
                use CurrentUnitVars
                use CurDHMParam
                use Sim_SoilDHM
                real Wrt1,Wrt2,Smx,cnday

                !  以下子程序计算地表径流模拟中的参数CN（与含水量有关）
                if (v_t>p_sftmp.and.v_p>0) then		!降雨

                    call calsurqwaterDHM
                    
!                    call crackvol
!		            if (v_rs > 0.) call crackflow
!                    call CalDepressionwaterDHM()
!                else 
!                    v_netp=0.
!                    v_sm = 0.
!                    call calsurqwaterDHM()
!                    call surq_daycn(cnday)
                endif

                return
            end subroutine



            
            subroutine CalsoilEasyDHM
                use sim_SoilDHM
                use Sim_ETDHM
                use CurrentUnitWSPDHMVars
                use CurDHMParam
                
                integer ii
                real Sol_FroNew
            
                call solt(IX)      !土壤温度
                call Crackvol !土壤孔隙
                         
                if (v_rs > 0.) call crackflow
                
                ! 冻融
                if (tmp_srf .ge. 0) then
                    if (sum(sol_fro) .gt.0) then
                        do ii=1,sol_nly
                            if (sol_tmp(ii).ge.0) then
                                sol_fm(ii) = Csolfm * (sol_tmp(ii)-0)
                                if (sol_fm(ii).gt.sol_fro(ii)) sol_fm(ii)=sol_fro(ii) !sol_fro土壤冻结水量
                                sol_st(ii)=sol_st(ii)+sol_fm(ii)
                                sol_fro(ii)=max(sol_fro(ii)-sol_fm(ii),0.)			            
                            endif
                        enddo  
                    endif

                    call percmain(IX,IY,landid)
                    call etsoilplant()
                else
                    v_ss = 0.
                    do ii=1,sol_nly
                        !冻结
                        if(sol_tmp(ii)<0) then
                            sol_fronew=Csolf *(0-sol_tmp(ii))
                            if(sol_fronew.gt.sol_st(ii)) sol_fronew=sol_st(ii)
                            sol_fro(ii) = sol_fro(ii) + sol_fronew
                            if(sol_fro(ii).gt.(sol_ul(ii)*0.9)) sol_fro(ii)=sol_ul(ii)*0.9
                            sol_st(ii)=sol_st(ii)-sol_fronew
                        endif
                        v_ss = v_ss + sol_st(ii)
                        
                    enddo
                    call percmain(IX,IY,landid)
                    call etsoilplant()
                    
                    v_es=0.
                endif
                return
            end subroutine
 
            subroutine CalGWEasyDHM
                use CurrentUnitWSPDHMVars
                use CurrentUnitVars
                use CurDHMParam
                use InitDHMStateMod
                use UnitInfoMod
                use SolutionMod
                real :: rchrg1,gwseep
                real alpha_bfe, gw_delaye
                
                alpha_bfe = exp(-alpha_bf)
                gw_delaye = Exp(-1./(gw_delay + 1.e-6)) *2/3
                
                rchrg1 = 0.
                rchrg1 = v_rchrg  !地下水补给
                
                !! compute shallow aquifer level for current day
                v_rchrg = 0.
                v_rchrg = (1.-gw_delaye) * sepbtm + gw_delaye * rchrg1
                if (v_rchrg < 1.e-6) v_rchrg = 0.


                !! compute deep aquifer level for day
                gwseep = v_rchrg * rchrg_dp
             !   deepst(j) = deepst(j) + gwseep
                v_gt = v_gt + (v_rchrg - gwseep)  !地下水储量mm
                gwht = gwht * alpha_bfe + v_rchrg * (1. - alpha_bfe) / (800. * gw_spyld + 1.e-6 * alpha_bf + 1.e-6)!gwht地下水水位mm
                gwht = Max(1.e-6, gwht)

                !! compute groundwater contribution to streamflow for day
                if (v_gt > gwqmn) then
                    v_rg = v_rg * alpha_bfe + (v_rchrg - gwseep ) * (1. - alpha_bfe)
                else
                    v_rg = 0.
                end if

                !! compute revap to soil profile/plant roots
                v_eg = ch_revap * (v_pet-v_es-v_ei-v_ed - v_ep)
                if (v_gt < revapmn) then
                    v_eg = 0.
                else
                    v_gt = v_gt - v_eg
                    if (v_gt < revapmn) then
                        v_eg = v_gt + v_eg - revapmn
                        v_gt = revapmn
                    end if
                end if
                v_eg = max(v_eg,0.0)

                !! remove ground water flow from shallow aquifer storage
                if (v_gt >= gwqmn) then    !gwqmn:threshold depth of water in shallow aquifer required before groundwater flow will occur
                    v_gt = v_gt - v_rg      !v_rg:地下径流量mm v_gt地下水储量mm
                    if(v_gt < gwqmn) then
                        v_rg = v_gt + v_rg - gwqmn
                        v_gt = gwqmn
                    end if
                else
                    v_rg = 0.
                end if
                v_rg = max(0.,v_rg)
!                v_qg=v_rg*Units(IX,IY).Area(1)/1000/Cursolution.dt/3600
                return
            end subroutine
        
            !保存单元计算结果
            subroutine SaveUnitVarsEasyDHM()
                use EasyDHMVarMod        
                use EasyDHMParamMod
                use CurrentUnitWSPDHMVars
        
                integer i
                
                !================================================================================
                ! 过程变量
                !================================================================================
                    uland_DHMVars(IX,IY,landid)%sm=v_sm				!融雪量mm

                    uland_DHMVars(IX,IY,landid)%ei=v_ei				!冠层截留蒸发量mm
                    uland_DHMVars(IX,IY,landid)%interc=v_interc		!冠层截留量mm

                    uland_DHMVars(IX,IY,landid)%ed=v_ed				!地表填洼蒸发量mm
                    uland_DHMVars(IX,IY,landid)%depre=v_depre			!地表填洼量mm

                    uland_DHMVars(IX,IY,landid)%netp=v_netp			!净雨量mm
                    uland_DHMVars(IX,IY,landid)%rs=v_rs				!地表径流mm
                    uland_DHMVars(IX,IY,landid)%infil=v_infil			!地表入渗量mm

                    uland_DHMVars(IX,IY,landid)%ep=v_ep             !植物蒸发
                    uland_DHMVars(IX,IY,landid)%es=v_es				!地表蒸发mm
                    uland_DHMVars(IX,IY,landid)%ri=v_ri				!壤中流量mm
                    uland_DHMVars(IX,IY,landid)%perco=v_perco			!补给地下水量mm
                    uland_DHMVars(IX,IY,landid)%eg=v_eg				!地下蒸发量mm
                    uland_DHMVars(IX,IY,landid)%rg=v_rg				!地下径流量mm
                    uland_DHMVars(IX,IY,landid)%qg=v_qg				!地下径流m3/ss
                    
                
                    v_r=v_rs+v_ri+v_rg
                    uland_DHMVars(IX,IY,landid)%r=v_r
                    uland_DHMVars(IX,IY,landid)%e=v_ei + v_ed + v_ep + v_es + v_eg

                    !================================================================================
                    ! 状态变量
                    !================================================================================		
                    uland_DHMVars(IX,IY,landid)%snow=v_snow			!积雪量mm
                    uland_DHMVars(IX,IY,landid)%snowtmp=snowtmp
                    s_DHMVars(IX)%snowtmp=snowtmp
                    uland_DHMVars(IX,IY,landid)%sint=v_sint			!冠层截留储量mm
                    uland_DHMVars(IX,IY,landid)%sdep=v_sdep			!地表填洼储量mm
                    uland_DHMVars(IX,IY,landid)%ss=v_ss				!土壤含水量mm
                    
                    uland_DHMVars(IX,IY,landid)%gt=v_gt				!地下水储量mm
                    uland_DHMVars(IX,IY,landid)%gwht = gwht
                    
                    uland_DHMVars(IX,IY,landid)%surtmp= tmp_srf
                    uland_DHMVars(IX,IY,landid)%rchrg = v_rchrg
                    uland_DHMVars(IX,IY,landid)%wiltsum =  wilting * Soils(IX)%Solh
                    
                    do i=1,sol_nly
                        lland_DHMVars(IX,IY,i,landid)%soltmp = sol_tmp(i)
                        sl_DHMVars(IX,i)%soltmp = 0.

                        lland_DHMVars(IX,IY,i,landid)%solz = sol_z(i)
                        lland_DHMVars(IX,IY,i,landid)%solh = sol_h(i)
                        sl_DHMVars(IX,i)%solz = sol_z(i)
                        sl_DHMVars(IX,i)%solh = sol_h(i)
                        if(sol_st(i) >20000)  sol_st(i) = 0.25 * sol_h(i)
                        lland_DHMVars(IX,IY,i,landid)%ss = sol_st(i)
                        sl_DHMVars(IX,i)%ss = 0.

                        lland_DHMVars(IX,IY,i,landid) %solfro = sol_fro(i)
                        sl_DHMVars(IX,i) %solfro = 0.
                        uland_DHMVars(IX,IY,landid)%solfro=uland_DHMVars(IX,IY,landid)%solfro+ lland_DHMVars(IX,IY,i,landid) %solfro
                    enddo

                return
            end subroutine
            
            

            subroutine EasyDHMUnitStat

                !统计到子流域
                call Sum2SubbasinEasyDHM(IX,IY,unitarea)
!	            !统计到参数分区
!	            call Sum2ParamRangeDHM(IParamRange,IX,IY,1,unitarea)
                call Sum2ParamRangeDHM(IX,IY,1,unitarea)	
                return
            end subroutine
            
             subroutine EasyDHMUnitlandStat
                call sum2UnitEasyDHM(IX,IY,landid,unitarea)

             end subroutine		
                 
            subroutine EasyDHMsublandStat
                call sum2sublandEasyDHM(IX,IY,landid)
            
            end subroutine
    end module