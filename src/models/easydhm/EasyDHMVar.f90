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

    ! ����ĵ�Ԫˮ��Ҫ��
    type u_DHMClass

        ! ״̬��Ϣ
        real :: sint        !�ڲ��������mm
        real :: sdep        !�ر����ݴ���mm
        real :: ss        !������ˮ��mm
        real :: gwht        !����ˮˮλmm
        real :: gt        !����ˮ����mm
        real :: PET            !Ǳ������mm
        real :: SM        !��ѩ��mm
        real :: Snow        !��ѩ��mm
        real :: EI            !�ڲ����������mm
        real :: Interc        !�ڲ������mm
        real :: ED            !�ر�����������mm
        real :: Depre        !�ر�������mm
        real :: NetP        !������mm
        real :: RS            !�ر���mm
        real :: Infil        !�ر�������mm
        real :: EP            !ֲ������mm
        real :: EPMax
        real :: ES            !�ر�����mm
        real :: RI            !��������mm
        real :: Perco        !��������ˮ��mm
        real :: EG            !����������mm
        real :: RG            !���¾�����mm

        ! ��ˮ����
        real :: QI            !������������m3/s
        real :: QG            !���¾�����m3/s

        real :: E            !��������mm
        real :: R            !�ܾ�����mm
        real :: SurTmp        !��������¶�
        real :: SolFro        !��������ˮ��
        real :: rchrg        !����ˮ������
        real :: SnowTmp
        real :: WiltSum

    end type u_DHMClass

    ! �����������ˮ��Ҫ��
    type l_DHMClass

        real :: soltmp      ! �����¶�
        real :: solfro      ! �ֲ���������ˮ��
        real :: excess      ! ����ˮ������
        real :: solz        ! �������
        real :: solh        ! �����ֲ���
        real :: ss         !������ˮ��mm
    end type l_DHMClass

    ! �洢ÿ�������򡢼��㵥Ԫ���������ˮѭ��Ҫ��
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
                        lland_DHMVars(i, j, k, landid)%solfro = 0.      ! �ֲ���������ˮ��
                        lland_DHMVars(i, j, k, landid)%excess = 0.

                    enddo

                    uland_DHMVars(i, j, landid)%sint = InitDHMStates(i, j)%init_sint
                    uland_DHMVars(i, j, landid)%sdep = InitDHMStates(i, j)%init_sdep        !�ر����ݴ���mm
                    uland_DHMVars(i, j, landid)%ss = (init_soil - Units(i, j)%Wilting)*soils(i)%solh    !������ˮ��mm
                    uland_DHMVars(i, j, landid)%gt = InitDHMStates(i, j)%init_gt        !����ˮ����mm
                    uland_DHMVars(i, j, landid)%RG = InitDHMStates(i, j)%init_rg * Cursolution%dt / 24
                    uland_DHMVars(i, j, landid)%rchrg = InitDHMStates(i, j)%init_rechg * Cursolution%dt / 24
                    uland_DHMVars(i, j, landid)%Snow = InitDHMStates(i, j)%init_snow        !��ѩ��mm
                    uland_DHMVars(i, j, landid)%WiltSum = Units(i, j)%Wilting * Soils(i)%solh
                    uland_DHMVars(i, j, landid)%PET = 0            !Ǳ������mm
                    uland_DHMVars(i, j, landid)%gwht = 0        !����ˮˮλmm
                    uland_DHMVars(i, j, landid)%SM = 0        !��ѩ��mm
                    uland_DHMVars(i, j, landid)%SnowTmp = 0
                    uland_DHMVars(i, j, landid)%EI = 0        !�ڲ����������mm
                    uland_DHMVars(i, j, landid)%Interc = 0        !�ڲ������mm
                    uland_DHMVars(i, j, landid)%ED = 0        !�ر�����������mm
                    uland_DHMVars(i, j, landid)%Depre = 0        !�ر�������mm
                    uland_DHMVars(i, j, landid)%NetP = 0        !������mm
                    uland_DHMVars(i, j, landid)%RS = 0        !�ر���mm
                    uland_DHMVars(i, j, landid)%Infil = 0        !�ر�������mm
                    uland_DHMVars(i, j, landid)%EP = 0        !ֲ������mm
                    uland_DHMVars(i, j, landid)%ES = 0        !�ر�����mm
                    uland_DHMVars(i, j, landid)%RI = 0        !��������mm
                    uland_DHMVars(i, j, landid)%Perco = 0        !��������ˮ��mm
                    uland_DHMVars(i, j, landid)%EG = 0        !����������mm

                    ! ��ˮ����
                    uland_DHMVars(i, j, landid)%QI = 0        !������������m3/s
                    uland_DHMVars(i, j, landid)%QG = 0        !���¾�����m3/s

                    uland_DHMVars(i, j, landid)%E = 0        !��������mm
                    uland_DHMVars(i, j, landid)%R = 0            !�ܾ�����mm
                    uland_DHMVars(i, j, landid)%SurTmp = 0        !��������¶�
                    uland_DHMVars(i, j, landid)%SolFro = 0        !��������ˮ��

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
            s_DHMVars(ii)%sdep = 0        !�ر����ݴ���mm
            s_DHMVars(ii)%ss = 0        !������ˮ��mm
            s_DHMVars(ii)%gwht = 0        !����ˮˮλmm
            s_DHMVars(ii)%gt = 0        !����ˮ����mm
            s_DHMVars(ii)%PET = 0            !Ǳ������mm
            s_DHMVars(ii)%SM = 0        !��ѩ��mm
            s_DHMVars(ii)%Snow = 0        !��ѩ��mm
            s_DHMVars(ii)%EI = 0        !�ڲ����������mm
            s_DHMVars(ii)%Interc = 0        !�ڲ������mm
            s_DHMVars(ii)%ED = 0        !�ر�����������mm
            s_DHMVars(ii)%Depre = 0        !�ر�������mm
            s_DHMVars(ii)%NetP = 0        !������mm
            s_DHMVars(ii)%RS = 0        !�ر���mm
            s_DHMVars(ii)%Infil = 0        !�ر�������mm
            s_DHMVars(ii)%EP = 0        !ֲ������mm
            s_DHMVars(ii)%ES = 0        !�ر�����mm
            s_DHMVars(ii)%RI = 0        !��������mm
            s_DHMVars(ii)%Perco = 0        !��������ˮ��mm
            s_DHMVars(ii)%EG = 0        !����������mm
            s_DHMVars(ii)%RG = 0            !���¾�����mm

            ! ��ˮ����
            s_DHMVars(ii)%QI = 0        !������������m3/s
            s_DHMVars(ii)%QG = 0        !���¾�����m3/s

            s_DHMVars(ii)%E = 0        !��������mm
            s_DHMVars(ii)%R = 0            !�ܾ�����mm
            s_DHMVars(ii)%SurTmp = 0        !��������¶�
            s_DHMVars(ii)%SolFro = 0        !��������ˮ��
            s_DHMVars(ii)%rchrg = 0        !����ˮ������
            s_DHMVars(ii)%SnowTmp = 0
            do jj = 1, Nlayer
                sl_DHMVars(ii, jj)%soltmp = 0
                sl_DHMVars(ii, jj)%ss = 0
                sl_DHMVars(ii, jj)%solfro = 0
            enddo

        enddo

    end subroutine

end module