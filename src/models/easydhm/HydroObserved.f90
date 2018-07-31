  ! 水文观测
    module HydroObservedMod
        use WaterShedMod
        
        implicit none
    
        type ResOutObservedClass
            integer :: ResId
            real, dimension(:),allocatable  :: ResOutObserved
        end type ResOutObservedClass
        
        type HydroObservedClass
            integer :: HydroId
            real, dimension(:),allocatable :: QObserved !水文
            character(30) ::DateTime
        end type HydroObservedClass 
        
        type UpHydroFinalClass
            integer :: UpReachId
            real, dimension(:),allocatable :: UpQFinal !水库，水文
        end type UpHydroFinalClass  
        
        type TotalUpHydroFinalclass
            type(UpHydroFinalClass),   dimension(:),allocatable :: UpHydroFinal
        end type TotalUpHydroFinalclass
        
        
        ! 存储观测站点、水库出库的径流过程
        type(HydroObservedClass),  dimension(:),allocatable :: HydroObserveds
        type(ResOutObservedClass), dimension(:),allocatable :: ResOutObserveds
!        type(UpHydroFinalclass),   dimension(:),allocatable :: UpHydroFinal
        type(TotalUpHydroFinalclass),   dimension(:),allocatable :: TotalUpHydroFinal        

!        type(UpHydroFinalClass),   dimension(:,:),allocatable :: TotalUpHydroFinal
    end module