 ! 流域类型
    module WaterShedMod
        
        implicit none
        
        ! 流域
        type WaterShedClass
            integer :: NUnit           ! 计算单元个数
            integer :: NReach          ! 河道个数
            integer :: NSubbasin       ! 子流域个数
            integer :: NParamRange     ! 参数分区个数：水文站个数 + 入库观测的水库个数
            integer :: NHydroStation   ! 水文站个数
            integer :: Nlayer          ! 土壤层数
            
            integer :: NResRange       ! 入库观测的水库个数
            integer :: NResIn          ! 入库观测的水库个数
            integer :: NResOut         ! 出库观测的水库个数
            
        end type WaterShedClass
        
        ! 流域
        type(WaterShedClass) :: WaterShed

    end module
    
