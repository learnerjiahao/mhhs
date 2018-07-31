module EasyDHMVarMod
    use WaterShedMod
    use SolutionMod
    use SoilInfoMod
    use UnitInfoMod
    use EasyDHMParamMod
    use SubbasinMod
    use ParamRangeMod
    use WeatherMod
    use SolutionMod
    implicit none

    ! 计算的单元水文要素
    type u_DHMClass

        ! 状态信息
        real :: sint        !冠层截留储量mm
        real :: sdep        !地表填洼储量mm
        real :: ss        !土壤含水量mm
        real :: gwht        !地下水水位mm
        real :: gt        !地下水储量mm
        real :: PET            !潜在蒸发mm
        real :: SM        !融雪量mm
        real :: Snow        !融雪量mm
        real :: EI            !冠层截留蒸发量mm
        real :: Interc        !冠层截留量mm
        real :: ED            !地表填洼蒸发量mm
        real :: Depre        !地表填洼量mm
        real :: NetP        !净雨量mm
        real :: RS            !地表径流mm
        real :: Infil        !地表入渗量mm
        real :: EP            !植被蒸发mm
        real :: EPMax
        real :: ES            !地表蒸发mm
        real :: RI            !壤中流量mm
        real :: Perco        !补给地下水量mm
        real :: EG            !地下蒸发量mm
        real :: RG            !地下径流量mm

        ! 退水径流
        real :: QI            !壤中流径流量m3/s
        real :: QG            !地下径流量m3/s

        real :: E            !总蒸发量mm
        real :: R            !总径流量mm
        real :: SurTmp        !土壤表层温度
        real :: SolFro        !土壤冻结水量
        real :: rchrg        !地下水补给量
        real :: SnowTmp
        real :: WiltSum

    end type u_DHMClass

    ! 计算的土壤层水文要素
    type l_DHMClass

        real :: soltmp      ! 土壤温度
        real :: solfro      ! 分层土壤冻结水量
        real :: excess      ! 土壤水多余量
        real :: solz        ! 土壤深度
        real :: solh        ! 土壤分层厚度
        real :: ss         !土壤含水量mm
    end type l_DHMClass

    ! 存储每个子流域、计算单元、土壤层的水循环要素
    type(u_DHMClass), dimension(:), allocatable :: s_DHMVars
    type(u_DHMClass), dimension(:, :), allocatable :: u_DHMVars
    type(l_DHMClass), dimension(:, :, :), allocatable :: l_DHMVars
    type(l_DHMClass), dimension(:, :), allocatable :: sl_DHMVars
    type(u_DHMClass), dimension(:, :, :), allocatable :: uland_DHMVars
    type(l_DHMClass), dimension(:, :, :, :), allocatable :: lland_DHMVars
    type(u_DHMClass), dimension(:, :, :), allocatable :: sland_DHMVars

contains
    subroutine InitDHMVars
        use InitDHMStateMod
        integer i, j, k, IParamRange, Nlayer, ix, landid
        real init_soil

        Nlayer = WaterShed%Nlayer
        IParamRange = Cursolution%IParamRange
        totalp_Weather(IParamRange)%rain = 0
        totalp_Weather(IParamRange)%PET = 0
        totalp_Weather(IParamRange)%E = 0

        do ix = 1, ParamRanges(IParamRange)%NPartSubbasin
            i = ParamRanges(IParamRange)%PartSubbasins(ix)
            do j = 1, Subbasins(i)%NUnits
                init_soil = InitDHMStates(i, j)%init_ss

                if(init_soil< units(i, j)%Wilting*1.3) then
                    init_soil = units(i, j)%Wilting*1.3
                endif
                if(InitDHMStates(i, j)%init_ss > units(i, j)%Fieldcap*EasyDHMParams(IParamRange)%FieldcapM) then
                    init_soil = units(i, j)%Fieldcap*EasyDHMParams(IParamRange)%FieldcapM
                endif
                soils(i)%solh = soils(i)%solh0 * ModSoil
                !                        endif
                do landid = 1, UnitNlanduse
                    do k = 1, Nlayer
                        if(k < Nlayer) then
                            lland_DHMVars(i, j, k, landid)%solz = soils(i)%solh * EasyDHMParams(IParamRange)%solzcoe(k)

                        else
                            lland_DHMVars(i, j, k, landid)%solz = soils(i)%solh
                        endif

                        if(k == 1) then
                            lland_DHMVars(i, j, k, landid)%solh = lland_DHMVars(i, j, k, landid)%solz

                        else
                            lland_DHMVars(i, j, k, landid)%solh = lland_DHMVars(i, j, k, landid)%solz - lland_DHMVars(i, j, k-1, landid)%solz
                        endif

                        lland_DHMVars(i, j, k, landid)%ss =  (init_soil - Units(i, j)%Wilting)*lland_DHMVars(i, j, k, landid)%solh
                        lland_DHMVars(i, j, k, landid)%soltmp = 0.
                        lland_DHMVars(i, j, k, landid)%solfro = 0.      ! 分层土壤冻结水量
                        lland_DHMVars(i, j, k, landid)%excess = 0.

                    enddo

                    uland_DHMVars(i, j, landid)%sint = InitDHMStates(i, j)%init_sint
                    uland_DHMVars(i, j, landid)%sdep = InitDHMStates(i, j)%init_sdep        !地表填洼储量mm
                    uland_DHMVars(i, j, landid)%ss = (init_soil - Units(i, j)%Wilting)*soils(i)%solh    !土壤含水量mm
                    uland_DHMVars(i, j, landid)%gt = InitDHMStates(i, j)%init_gt        !地下水储量mm
                    uland_DHMVars(i, j, landid)%RG = InitDHMStates(i, j)%init_rg * Cursolution%dt / 24
                    uland_DHMVars(i, j, landid)%rchrg = InitDHMStates(i, j)%init_rechg * Cursolution%dt / 24
                    uland_DHMVars(i, j, landid)%Snow = InitDHMStates(i, j)%init_snow        !融雪量mm
                    uland_DHMVars(i, j, landid)%WiltSum = Units(i, j)%Wilting * Soils(i)%solh
                    uland_DHMVars(i, j, landid)%PET = 0            !潜在蒸发mm
                    uland_DHMVars(i, j, landid)%gwht = 0        !地下水水位mm
                    uland_DHMVars(i, j, landid)%SM = 0        !融雪量mm
                    uland_DHMVars(i, j, landid)%SnowTmp = 0
                    uland_DHMVars(i, j, landid)%EI = 0        !冠层截留蒸发量mm
                    uland_DHMVars(i, j, landid)%Interc = 0        !冠层截留量mm
                    uland_DHMVars(i, j, landid)%ED = 0        !地表填洼蒸发量mm
                    uland_DHMVars(i, j, landid)%Depre = 0        !地表填洼量mm
                    uland_DHMVars(i, j, landid)%NetP = 0        !净雨量mm
                    uland_DHMVars(i, j, landid)%RS = 0        !地表径流mm
                    uland_DHMVars(i, j, landid)%Infil = 0        !地表入渗量mm
                    uland_DHMVars(i, j, landid)%EP = 0        !植被蒸发mm
                    uland_DHMVars(i, j, landid)%ES = 0        !地表蒸发mm
                    uland_DHMVars(i, j, landid)%RI = 0        !壤中流量mm
                    uland_DHMVars(i, j, landid)%Perco = 0        !补给地下水量mm
                    uland_DHMVars(i, j, landid)%EG = 0        !地下蒸发量mm

                    ! 退水径流
                    uland_DHMVars(i, j, landid)%QI = 0        !壤中流径流量m3/s
                    uland_DHMVars(i, j, landid)%QG = 0        !地下径流量m3/s

                    uland_DHMVars(i, j, landid)%E = 0        !总蒸发量mm
                    uland_DHMVars(i, j, landid)%R = 0            !总径流量mm
                    uland_DHMVars(i, j, landid)%SurTmp = 0        !土壤表层温度
                    uland_DHMVars(i, j, landid)%SolFro = 0        !土壤冻结水量

                    u_DHMVars(i, j)%PET = 0
                    u_DHMVars(i, j)%sm = 0
                    u_DHMVars(i, j)%ed = 0
                    u_DHMVars(i, j)%depre = 0
                    u_DHMVars(i, j)%netp = 0
                    u_DHMVars(i, j)%rs = 0

                    u_DHMVars(i, j)%ei = 0
                    u_DHMVars(i, j)%interc = 0
                    u_DHMVars(i, j)%infil = 0
                    u_DHMVars(i, j)%ep = 0
                    u_DHMVars(i, j)%es = 0
                    u_DHMVars(i, j)%ri = 0
                    u_DHMVars(i, j)%perco = 0
                    u_DHMVars(i, j)%eg = 0
                    u_DHMVars(i, j)%rg = 0

                enddo
            enddo

        enddo
    end subroutine

    subroutine InitStatDHMVars
        integer ii, jj, IParamRange, Nlayer, ix

        Nlayer = WaterShed%Nlayer
        IParamRange = Cursolution%IParamRange

        do ix = 1, ParamRanges(IParamRange)%NPartSubbasin
            ii = ParamRanges(IParamRange)%PartSubbasins(ix)
            s_DHMVars(ii)%sint = 0
            s_DHMVars(ii)%sdep = 0        !地表填洼储量mm
            s_DHMVars(ii)%ss = 0        !土壤含水量mm
            s_DHMVars(ii)%gwht = 0        !地下水水位mm
            s_DHMVars(ii)%gt = 0        !地下水储量mm
            s_DHMVars(ii)%PET = 0            !潜在蒸发mm
            s_DHMVars(ii)%SM = 0        !融雪量mm
            s_DHMVars(ii)%Snow = 0        !融雪量mm
            s_DHMVars(ii)%EI = 0        !冠层截留蒸发量mm
            s_DHMVars(ii)%Interc = 0        !冠层截留量mm
            s_DHMVars(ii)%ED = 0        !地表填洼蒸发量mm
            s_DHMVars(ii)%Depre = 0        !地表填洼量mm
            s_DHMVars(ii)%NetP = 0        !净雨量mm
            s_DHMVars(ii)%RS = 0        !地表径流mm
            s_DHMVars(ii)%Infil = 0        !地表入渗量mm
            s_DHMVars(ii)%EP = 0        !植被蒸发mm
            s_DHMVars(ii)%ES = 0        !地表蒸发mm
            s_DHMVars(ii)%RI = 0        !壤中流量mm
            s_DHMVars(ii)%Perco = 0        !补给地下水量mm
            s_DHMVars(ii)%EG = 0        !地下蒸发量mm
            s_DHMVars(ii)%RG = 0            !地下径流量mm

            ! 退水径流
            s_DHMVars(ii)%QI = 0        !壤中流径流量m3/s
            s_DHMVars(ii)%QG = 0        !地下径流量m3/s

            s_DHMVars(ii)%E = 0        !总蒸发量mm
            s_DHMVars(ii)%R = 0            !总径流量mm
            s_DHMVars(ii)%SurTmp = 0        !土壤表层温度
            s_DHMVars(ii)%SolFro = 0        !土壤冻结水量
            s_DHMVars(ii)%rchrg = 0        !地下水补给量
            s_DHMVars(ii)%SnowTmp = 0
            do jj = 1, Nlayer
                sl_DHMVars(ii, jj)%soltmp = 0
                sl_DHMVars(ii, jj)%ss = 0
                sl_DHMVars(ii, jj)%solfro = 0
            enddo

        enddo

    end subroutine

end module