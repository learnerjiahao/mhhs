! ˮ�ġ��������ݳ�ʼ����ж��
    module HydroDataMod
        use WaterShedMod
!        use DBSQLLinkMod
        use WeatherMod
        use HydroObservedMod
        use WeatherInfoMod
        use SimpleVarMod
        use SolutionMod
        use ParamRangeMod
        
        implicit none
        
        integer NAnalySeries,NAllSeries

        real, dimension(:,:),allocatable :: AnalyRunoff
        real, dimension(:,:),allocatable :: AdjustRunoff
        real, dimension(:,:),allocatable :: AllRunoff
        real, dimension(:,:,:),allocatable :: TotalAnalyRunoff
        real, dimension(:,:,:),allocatable :: TotalAdjustRunoff
        real, dimension(:,:,:),allocatable :: TotalAllRunoff
!        logical IsDataRead
        
        contains
            ! �����ݿ����ӣ�����ռ�
            subroutine InitHydroData
                
                if(allocated(s_Weather))deallocate(s_Weather)
                allocate(s_Weather(WaterShed.NSubbasin))
                if(allocated(p_Weather))deallocate(p_Weather)
                allocate(p_Weather(WaterShed.NParamRange))
                if(allocated(totalp_Weather))deallocate(totalp_Weather)
                allocate(totalp_Weather(WaterShed.NParamRange))
                if(allocated(ResOutObserveds))deallocate(ResOutObserveds)
                allocate(ResOutObserveds(WaterShed.NResRange))
                
!                IsDataRead = .False.
                ! �����ݿ�����
                
            end subroutine

            
            ! ж���ڴ棬�ر����ݿ�����
            subroutine DestroyHydroData
                if(allocated(s_Weather))deallocate(s_Weather)
                if(allocated(p_Weather))deallocate(p_Weather)
                if(allocated(totalp_Weather))deallocate(totalp_Weather)
                if(allocated(y_Weather))deallocate(y_Weather)
                if(allocated(HydroObserveds))deallocate(HydroObserveds)                
            end subroutine
    
    end module