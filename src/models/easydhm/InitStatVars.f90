subroutine InitStatVars(CurRunoffGenType)
    use WeatherMod
    use EasyDHMVarMod

    integer ii, jj, CurRunoffGenType
    call InitStatDHMVars

    do jj = 1, WaterShed%NParamRange
        p_Weather(jj)%rain = 0.
        p_Weather(jj)%Tavg = 0.
        p_Weather(jj)%PET = 0.
        p_Weather(jj)%E = 0.
    enddo
end

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine Sum2SubbasinEasyDHM(IX, IY, unitarea)
    use SubbasinMod
    use UnitInfoMod
    use EasyDHMVarMod
    use ReachVarMod
    use WeatherMod
    use WaterShedMod
    use SoilInfoMod
    !		use SolutionMod

    integer IX, IY
    real unitarea

    !================================================================================
    ! 过程变量 - 子流域
    !================================================================================
    !		s_DHMVars(IX)%PET    = s_DHMVars(IX)%PET    + u_DHMVars(IX,IY)%PET *Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%sm     = s_DHMVars(IX)%sm     + u_DHMVars(IX,IY)%sm*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%ei     = s_DHMVars(IX)%ei     + u_DHMVars(IX,IY)%ei*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%interc = s_DHMVars(IX)%interc + u_DHMVars(IX,IY)%interc*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%ed     = s_DHMVars(IX)%ed     + u_DHMVars(IX,IY)%ed*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%depre  = s_DHMVars(IX)%depre  + u_DHMVars(IX,IY)%depre*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%netp   = s_DHMVars(IX)%netp   + u_DHMVars(IX,IY)%netp*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%rs     = s_DHMVars(IX)%rs     + u_DHMVars(IX,IY)%rs*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%infil  = s_DHMVars(IX)%infil  + u_DHMVars(IX,IY)%infil*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%ep     = s_DHMVars(IX)%ep     + u_DHMVars(IX,IY)%ep*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%es     = s_DHMVars(IX)%es     + u_DHMVars(IX,IY)%es*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%ri     = s_DHMVars(IX)%ri     + u_DHMVars(IX,IY)%ri*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%perco  = s_DHMVars(IX)%perco  + u_DHMVars(IX,IY)%perco*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%eg     = s_DHMVars(IX)%eg     + u_DHMVars(IX,IY)%eg*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%rg     = s_DHMVars(IX)%rg     + u_DHMVars(IX,IY)%rg*Units(IX,IY)%Area/Subbasins(IX)%Area

    s_DHMVars(IX)%PET = s_DHMVars(IX)%PET + u_DHMVars(IX, IY)%PET * unitarea / Subbasins(IX)%Area

    s_DHMVars(IX)%sm = s_DHMVars(IX)%sm + u_DHMVars(IX, IY)%sm * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%ei = s_DHMVars(IX)%ei + u_DHMVars(IX, IY)%ei * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%interc = s_DHMVars(IX)%interc + u_DHMVars(IX, IY)%interc * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%ed = s_DHMVars(IX)%ed + u_DHMVars(IX, IY)%ed * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%depre = s_DHMVars(IX)%depre + u_DHMVars(IX, IY)%depre * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%netp = s_DHMVars(IX)%netp + u_DHMVars(IX, IY)%netp * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%rs = s_DHMVars(IX)%rs + u_DHMVars(IX, IY)%rs * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%infil = s_DHMVars(IX)%infil + u_DHMVars(IX, IY)%infil * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%ep = s_DHMVars(IX)%ep + u_DHMVars(IX, IY)%ep * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%es = s_DHMVars(IX)%es + u_DHMVars(IX, IY)%es * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%ri = s_DHMVars(IX)%ri + u_DHMVars(IX, IY)%ri * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%perco = s_DHMVars(IX)%perco + u_DHMVars(IX, IY)%perco * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%eg = s_DHMVars(IX)%eg + u_DHMVars(IX, IY)%eg * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%rg = s_DHMVars(IX)%rg + u_DHMVars(IX, IY)%rg * unitarea / Subbasins(IX)%Area

    ! 壤中流参与河道汇流
    ReachVars(IX)%s_river = s_DHMVars(IX)%rs + s_DHMVars(IX)%ri
    !		ReachVars(IX)%s_river = s_DHMVars(IX)%rs   + s_DHMVars(IX)%ri + s_DHMVars(IX)%rg

    ReachVars(IX)%qs = ReachVars(IX)%s_river * Subbasins(IX)%Area / 1000 / Cursolution%dt / 3600

    !地下径流m3/s
    s_DHMVars(IX)%qg = s_DHMVars(IX)%rg * Subbasins(IX)%Area / 1000 / Cursolution%dt / 3600
    ReachVars(IX)%qg = s_DHMVars(IX)%qg
    s_DHMVars(IX)%r = s_DHMVars(IX)%r + u_DHMVars(IX, IY)%r * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%e = s_DHMVars(IX)%e + u_DHMVars(IX, IY)%e * unitarea / Subbasins(IX)%Area
    !================================================================================
    ! 状态变量 - 子流域
    !================================================================================
    s_DHMVars(IX)%snow = s_DHMVars(IX)%snow + u_DHMVars(IX, IY)%snow * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%sint = s_DHMVars(IX)%sint + u_DHMVars(IX, IY)%sint * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%sdep = s_DHMVars(IX)%sdep + u_DHMVars(IX, IY)%sdep * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%gt = s_DHMVars(IX)%gt + u_DHMVars(IX, IY)%gt * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%gwht = s_DHMVars(IX)%gwht + u_DHMVars(IX, IY)%gwht * unitarea / Subbasins(IX)%Area


    !		s_DHMVars(IX)%ss     = s_DHMVars(IX)%ss     + (u_DHMVars(IX,IY)%ss+U_DHMVars(IX,IY)%wiltsum)*Units(IX,IY)%Area/Subbasins(IX)%Area
    !		s_DHMVars(IX)%surtmp = s_DHMVars(IX)%surtmp + u_DHMVars(IX,IY)%surtmp*Units(IX,IY)%Area/Subbasins(IX)%Area
    s_DHMVars(IX)%ss = s_DHMVars(IX)%ss + (u_DHMVars(IX, IY)%ss + U_DHMVars(IX, IY)%wiltsum) * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%surtmp = s_DHMVars(IX)%surtmp + u_DHMVars(IX, IY)%surtmp * unitarea / Subbasins(IX)%Area
    s_DHMVars(IX)%solfro = 0

    do i = 1, WaterShed%Nlayer
        sl_DHMVars(IX, i)%soltmp = sl_DHMVars(IX, i)%soltmp + l_DHMVars(IX, IY, i)%soltmp * unitarea / Subbasins(IX)%Area
        sl_DHMVars(IX, i)%ss = sl_DHMVars(IX, i)%ss + (l_DHMVars(IX, IY, i)%ss + units(IX, IY)%wilting * Soils(IX)%Solh) * unitarea / Subbasins(IX)%Area
        sl_DHMVars(IX, i)%solfro = sl_DHMVars(IX, i)%solfro + l_DHMVars(IX, IY, i)%solfro * unitarea / Subbasins(IX)%Area
    enddo

    return
end
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!	subroutine Sum2SubbasinWetSpa(IX,IY)
!		use SubbasinMod
!		use UnitInfoMod
!		use WetSpaVarMod
!		use ReachVarMod
!		
!		integer IX,IY
!
!		!================================================================================
!		! 过程变量 - 子流域
!		!================================================================================
!		s_WSPVars(IX)%PET    = s_WSPVars(IX)%PET    + u_WSPVars(IX,IY)%PET *Units(IX,IY)%Area(1)/Subbasins(IX)%Area             
!		s_WSPVars(IX)%sm     = s_WSPVars(IX)%sm     + u_WSPVars(IX,IY)%sm*Units(IX,IY)%Area(1)/Subbasins(IX)%Area              
!		s_WSPVars(IX)%ei     = s_WSPVars(IX)%ei     + u_WSPVars(IX,IY)%ei*Units(IX,IY)%Area(1)/Subbasins(IX)%Area              
!		s_WSPVars(IX)%interc = s_WSPVars(IX)%interc + u_WSPVars(IX,IY)%interc*Units(IX,IY)%Area(1)/Subbasins(IX)%Area          
!		s_WSPVars(IX)%ed     = s_WSPVars(IX)%ed     + u_WSPVars(IX,IY)%ed*Units(IX,IY)%Area(1)/Subbasins(IX)%Area             
!		s_WSPVars(IX)%depre  = s_WSPVars(IX)%depre  + u_WSPVars(IX,IY)%depre*Units(IX,IY)%Area(1)/Subbasins(IX)%Area           
!		s_WSPVars(IX)%netp   = s_WSPVars(IX)%netp   + u_WSPVars(IX,IY)%netp*Units(IX,IY)%Area(1)/Subbasins(IX)%Area            
!		s_WSPVars(IX)%rs     = s_WSPVars(IX)%rs     + u_WSPVars(IX,IY)%rs*Units(IX,IY)%Area(1)/Subbasins(IX)%Area              
!		s_WSPVars(IX)%infil  = s_WSPVars(IX)%infil  + u_WSPVars(IX,IY)%infil*Units(IX,IY)%Area(1)/Subbasins(IX)%Area           
!		s_WSPVars(IX)%ep     = s_WSPVars(IX)%ep     + u_WSPVars(IX,IY)%ep*Units(IX,IY)%Area(1)/Subbasins(IX)%Area  
!		s_WSPVars(IX)%es     = s_WSPVars(IX)%es     + u_WSPVars(IX,IY)%es*Units(IX,IY)%Area(1)/Subbasins(IX)%Area              
!		s_WSPVars(IX)%ri     = s_WSPVars(IX)%ri     + u_WSPVars(IX,IY)%ri*Units(IX,IY)%Area(1)/Subbasins(IX)%Area             
!		s_WSPVars(IX)%perco  = s_WSPVars(IX)%perco  + u_WSPVars(IX,IY)%perco*Units(IX,IY)%Area(1)/Subbasins(IX)%Area          
!		s_WSPVars(IX)%eg     = s_WSPVars(IX)%eg     + u_WSPVars(IX,IY)%eg*Units(IX,IY)%Area(1)/Subbasins(IX)%Area              
!		s_WSPVars(IX)%rg     = s_WSPVars(IX)%rg     + u_WSPVars(IX,IY)%rg*Units(IX,IY)%Area(1)/Subbasins(IX)%Area
!		
!		! 壤中流参与河道汇流
!		ReachVars(IX)%s_river = s_WSPVars(IX)%rs   + s_WSPVars(IX)%ri
!!		ReachVars(IX)%s_river = s_WSPVars(IX)%rs   + s_WSPVars(IX)%ri + s_WSPVars(IX)%rg
!
!		!地下径流m3/s
!		s_WSPVars(IX)%qg     = s_WSPVars(IX)%rg*Subbasins(IX)%Area/1000/Cursolution.dt/3600
!		ReachVars(IX)%qg     = s_WSPVars(IX)%qg
!		
!		s_WSPVars(IX)%r      = s_WSPVars(IX)%r      + u_WSPVars(IX,IY)%r*Units(IX,IY)%Area(1)/Subbasins(IX)%Area
!		s_WSPVars(IX)%e      = s_WSPVars(IX)%e      + u_WSPVars(IX,IY)%e*Units(IX,IY)%Area(1)/Subbasins(IX)%Area
!		!================================================================================
!		! 状态变量 - 子流域
!		!================================================================================		
!		s_WSPVars(IX)%snow   = s_WSPVars(IX)%snow   + u_WSPVars(IX,IY)%snow*Units(IX,IY)%Area(1)/Subbasins(IX)%Area            
!		s_WSPVars(IX)%sint   = s_WSPVars(IX)%sint   + u_WSPVars(IX,IY)%sint*Units(IX,IY)%Area(1)/Subbasins(IX)%Area            
!		s_WSPVars(IX)%sdep   = s_WSPVars(IX)%sdep   + u_WSPVars(IX,IY)%sdep*Units(IX,IY)%Area(1)/Subbasins(IX)%Area            		             
!		s_WSPVars(IX)%gt     = s_WSPVars(IX)%gt     + u_WSPVars(IX,IY)%gt*Units(IX,IY)%Area(1)/Subbasins(IX)%Area
!		s_WSPVars(IX)%ss     = s_WSPVars(IX)%ss     + u_WSPVars(IX,IY)%ss*Units(IX,IY)%Area(1)/Subbasins(IX)%Area 
!
!	return
!	end
!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!	subroutine Sum2SubbasinSimple(IX,IY)
!		use SubbasinMod
!		use UnitInfoMod
!		use SimpleVarMod
!		use ReachVarMod
!	
!	    integer IX,IY
!		!================================================================================
!		! 过程变量 - 子流域
!		!================================================================================
!		s_SimpleVars(IX)%PET = s_SimpleVars(IX)%PET+u_SimpleVars(IX,IY)%PET*Units(IX,IY)%Area(1)/Subbasins(IX)%Area             
!		s_SimpleVars(IX)%e   = s_SimpleVars(IX)%e+u_SimpleVars(IX,IY)%e*Units(IX,IY)%Area(1)/Subbasins(IX)%Area  
!		
!		s_SimpleVars(IX)%rs  = s_SimpleVars(IX)%rs+u_SimpleVars(IX,IY)%rs*Units(IX,IY)%Area(1)/Subbasins(IX)%Area                
!		s_SimpleVars(IX)%ri  = s_SimpleVars(IX)%ri+u_SimpleVars(IX,IY)%ri*Units(IX,IY)%Area(1)/Subbasins(IX)%Area               
!		s_SimpleVars(IX)%rg  = s_SimpleVars(IX)%rg+u_SimpleVars(IX,IY)%rg*Units(IX,IY)%Area(1)/Subbasins(IX)%Area               
!		s_SimpleVars(IX)%r   = s_SimpleVars(IX)%r+u_SimpleVars(IX,IY)%r*Units(IX,IY)%Area(1)/Subbasins(IX)%Area  
!		
!		! 壤中流、地下径流不参与河道汇流
!		ReachVars(IX)%s_river    = s_SimpleVars(IX)%rs     + s_SimpleVars(IX)%ri
!!		ReachVars(IX)%s_river    = s_SimpleVars(IX)%rs     + s_SimpleVars(IX)%ri + s_SimpleVars(IX)%rg
!		
!		!壤中流m3/s
!		s_SimpleVars(IX)%qi = s_SimpleVars(IX)%qi+u_SimpleVars(IX,IY)%qi
!
!		!地下径流m3/s
!		s_SimpleVars(IX)%qg = s_SimpleVars(IX)%qg+u_SimpleVars(IX,IY)%qg		
!		ReachVars(IX)%qg     = s_SimpleVars(IX)%qg
!
!		return
!	end

subroutine Sum2ParamRange(IX, IY, CurRunoffGenType)
    use WeatherMod
    use UnitInfoMod
    use ParamRangeMod
    use EasyDHMVarMod

    !		use WetSpaVarMod
    !        use SimpleVarMod

    implicit none
    !		integer j,IX,IParamrange,IY
    integer j, IX, IY
    integer, external :: IncludedInParamRange
    integer CurRunoffGenType
    real unitarea

    if (IncludedInParamRange(IX)) then
        j = cursolution%IParamRange
        p_Weather(j)%rain = p_Weather(j)%rain + s_Weather(IX)%rain(iwLoop) * Units(IX, IY)%Area(1) / ParamRanges(j)%Area
        p_Weather(j)%Tavg = p_Weather(j)%Tavg + s_Weather(IX)%Tavg(iwLoop) * Units(IX, IY)%Area(1) / ParamRanges(j)%Area
        p_Weather(j)%PET = p_Weather(j)%PET + s_Weather(IX)%PET(iwLoop) * Units(IX, IY)%Area(1) / ParamRanges(j)%Area

        select case(CurRunoffGenType)
        case(1)

            p_Weather(j)%E = p_Weather(j)%E + u_DHMVars(IX, IY)%E * Units(IX, IY)%Area(1) / ParamRanges(j)%Area
            !                case(2)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_WSPVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area
            !
            !                case(3)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_SimpleVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area
            !
            !                case(4)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_SimpleVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area

        end select


        ! 输出单位为mm
        !					prPNet1(j) = prPNet1(j) +u_netp(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRS1(j) = prRS1(j) + u_rs(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRI1(j) = prRI1(j) + u_ri(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRG1(j) = prRG1(j) + u_rg(IX,IY)*Units(IX,IY)%Area/GSUB(j)

        ! 输出单位为m3/s
        !			    prPNet1 = prPNet1 + u_netp(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRS1 = prRS1 + u_rs(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRI1 = prRI1 + u_ri(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRG1 = prRG1 + u_rg(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
    endif
    return
end
subroutine Sum2ParamRangeDHM(IX, IY, CurRunoffGenType, unitarea)
    use WeatherMod
    use UnitInfoMod
    use ParamRangeMod
    use EasyDHMVarMod

    !		use WetSpaVarMod
    !        use SimpleVarMod

    implicit none
    integer j, IX, IY
    integer, external :: IncludedInParamRange
    integer CurRunoffGenType
    real unitarea

    if (IncludedInParamRange(IX)) then
        j = cursolution%IParamRange
        p_Weather(j)%rain = p_Weather(j)%rain + s_Weather(IX)%rain(iLoop) * unitarea / ParamRanges(j)%Area
        totalp_Weather(j)%rain(iloop) = totalp_Weather(j)%rain(iLoop) + s_Weather(IX)%rain(iLoop) * unitarea / ParamRanges(j)%Area
        p_Weather(j)%PET = p_Weather(j)%PET + s_Weather(IX)%PET(iwLoop) * unitarea / ParamRanges(j)%Area
        totalp_Weather(j)%PET(iloop) = totalp_Weather(j)%PET(iloop) + s_Weather(IX)%PET(iwLoop) * unitarea / ParamRanges(j)%Area
        !		    p_Weather(j)%Tavg  = p_Weather(j)%Tavg  + s_DHMVars(IX)%netp  * unitarea/ParamRanges(j)%Area !该变量当成净雨
        p_Weather(j)%Tavg = p_Weather(j)%Tavg + s_DHMVars(IX)%r * unitarea / ParamRanges(j)%Area !该变量当成净雨
        select case(CurRunoffGenType)
        case(1)

            !                    p_Weather(j)%E = p_Weather(j)%E + u_DHMVars(IX,IY)%E * Units(IX,IY)%Area/ParamRanges(j)%Area
            p_Weather(j)%E = p_Weather(j)%E + u_DHMVars(IX, IY)%E * unitarea / ParamRanges(j)%Area
            totalp_Weather(j)%E(iloop) = totalp_Weather(j)%E(iloop) + u_DHMVars(IX, IY)%E * unitarea / ParamRanges(j)%Area
            !                case(2)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_WSPVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area
            !
            !                case(3)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_SimpleVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area
            !
            !                case(4)
            !
            !                    p_Weather(j)%E = p_Weather(j)%E + u_SimpleVars(IX,IY)%E * Units(IX,IY)%Area(1)/ParamRanges(j)%Area

        end select


        ! 输出单位为mm
        !					prPNet1(j) = prPNet1(j) +u_netp(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRS1(j) = prRS1(j) + u_rs(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRI1(j) = prRI1(j) + u_ri(IX,IY)*Units(IX,IY)%Area/GSUB(j)
        !					prRG1(j) = prRG1(j) + u_rg(IX,IY)*Units(IX,IY)%Area/GSUB(j)

        ! 输出单位为m3/s
        !			    prPNet1 = prPNet1 + u_netp(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRS1 = prRS1 + u_rs(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRI1 = prRI1 + u_ri(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
        !			    prRG1 = prRG1 + u_rg(IX,IY)*Units(IX,IY)%Area/1000/dt/3600
    endif
    return
end
! 判断某子流域是否包含在某参数分区内
integer Function IncludedInParamRange(ISub)
    use ParamRangeMod
    use EasyDHMVarMod
    implicit none
    integer ii, ISub
    IncludedInParamRange = 0
    do ii = 1, ParamRanges(cursolution%IParamRange)%NPartSubbasin
        if (ParamRanges(cursolution%IParamRange)%PartSubbasins(ii) .eq. ISub) then
            IncludedInParamRange = 1
        endif
    enddo

    return
end
subroutine Sum2UnitEasyDHM(IX, IY, landid, unitarea)
    use SubbasinMod
    use UnitInfoMod
    use EasyDHMVarMod
    use ReachVarMod
    use WeatherMod
    use WaterShedMod
    use SoilInfoMod
    integer IX, IY, landid
    real unitarea
    u_DHMVars(IX, IY)%PET = u_DHMVars(IX, IY)%PET + uland_DHMVars(IX, IY, landid)%PET * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%sm = u_DHMVars(IX, IY)%sm + uland_DHMVars(IX, IY, landid)%sm * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%ed = u_DHMVars(IX, IY)%ed + uland_DHMVars(IX, IY, landid)%ed * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%depre = u_DHMVars(IX, IY)%depre + uland_DHMVars(IX, IY, landid)%depre * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%netp = u_DHMVars(IX, IY)%netp + uland_DHMVars(IX, IY, landid)%netp * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%rs = u_DHMVars(IX, IY)%rs + uland_DHMVars(IX, IY, landid)%rs * Units(IX, IY)%Area(landid) / unitarea

    u_DHMVars(IX, IY)%ei = u_DHMVars(IX, IY)%ei + uland_DHMVars(IX, IY, landid)%ei * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%interc = u_DHMVars(IX, IY)%interc + uland_DHMVars(IX, IY, landid)%interc * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%infil = u_DHMVars(IX, IY)%infil + uland_DHMVars(IX, IY, landid)%infil * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%ep = u_DHMVars(IX, IY)%ep + uland_DHMVars(IX, IY, landid)%ep * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%es = u_DHMVars(IX, IY)%es + uland_DHMVars(IX, IY, landid)%es * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%ri = u_DHMVars(IX, IY)%ri + uland_DHMVars(IX, IY, landid)%ri * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%perco = u_DHMVars(IX, IY)%perco + uland_DHMVars(IX, IY, landid)%perco * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%eg = u_DHMVars(IX, IY)%eg + uland_DHMVars(IX, IY, landid)%eg * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%rg = u_DHMVars(IX, IY)%rg + uland_DHMVars(IX, IY, landid)%rg * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%e = u_DHMVars(IX, IY)%ei + u_DHMVars(IX, IY)%ed + u_DHMVars(IX, IY)%ep + u_DHMVars(IX, IY)%es + u_DHMVars(IX, IY)%eg
    u_DHMVars(IX, IY)%rchrg = u_DHMVars(IX, IY)%rchrg + uland_DHMVars(IX, IY, landid)%rchrg * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%r = u_DHMVars(IX, IY)%rs + u_DHMVars(IX, IY)%ri + u_DHMVars(IX, IY)%rg
    u_DHMVars(IX, IY)%snow = u_DHMVars(IX, IY)%snow + uland_DHMVars(IX, IY, landid)%snow * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%gt = u_DHMVars(IX, IY)%gt + uland_DHMVars(IX, IY, landid)%gt * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%gwht = u_DHMVars(IX, IY)%gwht + uland_DHMVars(IX, IY, landid)%gwht * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%surtmp = u_DHMVars(IX, IY)%surtmp + uland_DHMVars(IX, IY, landid)%surtmp * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%Sint = u_DHMVars(IX, IY)%Sint + uland_DHMVars(IX, IY, landid)%Sint * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%Sdep = u_DHMVars(IX, IY)%Sdep + uland_DHMVars(IX, IY, landid)%Sdep * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%gt = u_DHMVars(IX, IY)%gt + uland_DHMVars(IX, IY, landid)%gt * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%ss = u_DHMVars(IX, IY)%ss + uland_DHMVars(IX, IY, landid)%ss * Units(IX, IY)%Area(landid) / unitarea
    u_DHMVars(IX, IY)%wiltsum = u_DHMVars(IX, IY)%wiltsum + uland_DHMVars(IX, IY, landid)%wiltsum * Units(IX, IY)%Area(landid) / unitarea

    do i = 1, WaterShed%Nlayer
        l_DHMVars(IX, IY, i)%soltmp = l_DHMVars(IX, IY, i)%soltmp + lland_DHMVars(IX, IY, i, landid)%soltmp * Units(IX, IY)%Area(landid) / unitarea
        l_DHMVars(IX, IY, i)%ss = l_DHMVars(IX, IY, i)%ss + (lland_DHMVars(IX, IY, i, landid)%ss + units(IX, IY)%wilting * Soils(IX)%Solh) * Units(IX, IY)%Area(landid) / unitarea
        l_DHMVars(IX, IY, i)%solfro = l_DHMVars(IX, IY, i)%solfro + lland_DHMVars(IX, IY, i, landid)%solfro * Units(IX, IY)%Area(landid) / unitarea
        l_DHMVars(IX, IY, i)%excess = l_DHMVars(IX, IY, i)%excess + lland_DHMVars(IX, IY, i, landid)%excess * Units(IX, IY)%Area(landid) / unitarea
        l_DHMVars(IX, IY, i)%solz = l_DHMVars(IX, IY, i)%solz + lland_DHMVars(IX, IY, i, landid)%solz * Units(IX, IY)%Area(landid) / unitarea
        l_DHMVars(IX, IY, i)%solh = l_DHMVars(IX, IY, i)%solh + lland_DHMVars(IX, IY, i, landid)%solh * Units(IX, IY)%Area(landid) / unitarea
    enddo

end subroutine


subroutine sum2sublandEasyDHM(IX, IY, landid)
    use SubbasinMod
    use UnitInfoMod
    use EasyDHMVarMod
    use ReachVarMod
    use WeatherMod
    use WaterShedMod
    use SoilInfoMod
    integer IX, IY, landid
    sland_DHMVars(IX, IY, landid)%PET = sland_DHMVars(IX, IY, landid)%PET + uland_DHMVars(IX, IY, landid)%PET
    sland_DHMVars(IX, IY, landid)%sm = sland_DHMVars(IX, IY, landid)%sm + uland_DHMVars(IX, IY, landid)%sm
    sland_DHMVars(IX, IY, landid)%ed = sland_DHMVars(IX, IY, landid)%ed + uland_DHMVars(IX, IY, landid)%ed
    sland_DHMVars(IX, IY, landid)%depre = sland_DHMVars(IX, IY, landid)%depre + uland_DHMVars(IX, IY, landid)%depre
    sland_DHMVars(IX, IY, landid)%netp = sland_DHMVars(IX, IY, landid)%netp + uland_DHMVars(IX, IY, landid)%netp
    sland_DHMVars(IX, IY, landid)%rs = sland_DHMVars(IX, IY, landid)%rs + uland_DHMVars(IX, IY, landid)%rs

    sland_DHMVars(IX, IY, landid)%ei = sland_DHMVars(IX, IY, landid)%ei + uland_DHMVars(IX, IY, landid)%ei
    sland_DHMVars(IX, IY, landid)%interc = sland_DHMVars(IX, IY, landid)%interc + uland_DHMVars(IX, IY, landid)%interc
    sland_DHMVars(IX, IY, landid)%infil = sland_DHMVars(IX, IY, landid)%infil + uland_DHMVars(IX, IY, landid)%infil
    sland_DHMVars(IX, IY, landid)%ep = sland_DHMVars(IX, IY, landid)%ep + uland_DHMVars(IX, IY, landid)%ep
    sland_DHMVars(IX, IY, landid)%es = sland_DHMVars(IX, IY, landid)%es + uland_DHMVars(IX, IY, landid)%es
    sland_DHMVars(IX, IY, landid)%ri = sland_DHMVars(IX, IY, landid)%ri + uland_DHMVars(IX, IY, landid)%ri
    sland_DHMVars(IX, IY, landid)%perco = sland_DHMVars(IX, IY, landid)%perco + uland_DHMVars(IX, IY, landid)%perco
    sland_DHMVars(IX, IY, landid)%eg = sland_DHMVars(IX, IY, landid)%eg + uland_DHMVars(IX, IY, landid)%eg
    sland_DHMVars(IX, IY, landid)%rg = sland_DHMVars(IX, IY, landid)%rg + uland_DHMVars(IX, IY, landid)%rg

end subroutine
