  ! ˮ�Ĺ۲�
    module HydroObservedMod
        use WaterShedMod
        
        implicit none
    
        type ResOutObservedClass
            integer :: ResId
            real, dimension(:),allocatable  :: ResOutObserved
        end type ResOutObservedClass
        
        type HydroObservedClass
            integer :: HydroId
            real, dimension(:),allocatable :: QObserved !ˮ��
            character(30) ::DateTime
        end type HydroObservedClass 
        
        type UpHydroFinalClass
            integer :: UpReachId
            real, dimension(:),allocatable :: UpQFinal !ˮ�⣬ˮ��
        end type UpHydroFinalClass  
        
        type TotalUpHydroFinalclass
            type(UpHydroFinalClass),   dimension(:),allocatable :: UpHydroFinal
        end type TotalUpHydroFinalclass
        
        
        ! �洢�۲�վ�㡢ˮ�����ľ�������
        type(HydroObservedClass),  dimension(:),allocatable :: HydroObserveds
        type(ResOutObservedClass), dimension(:),allocatable :: ResOutObserveds
!        type(UpHydroFinalclass),   dimension(:),allocatable :: UpHydroFinal
        type(TotalUpHydroFinalclass),   dimension(:),allocatable :: TotalUpHydroFinal        

!        type(UpHydroFinalClass),   dimension(:,:),allocatable :: TotalUpHydroFinal
    end module