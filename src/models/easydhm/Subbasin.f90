 ! 子流域信息
    module SubbasinMod
        use WaterShedMod
        use ReachInfoMod
        
        implicit none
        
        ! 子流域
        type SubbasinClass
        
            integer :: SubId
            real :: Area
            integer :: NUnits
            integer :: ReachId
            real::Lati
            
        end type SubbasinClass
        
        
        ! 子流域，按子流域Id来保存
        type(SubbasinClass), Dimension(:), allocatable :: Subbasins
        
        contains
            
            subroutine InitSubbasins()
                
                integer i
                
                if(allocated(Subbasins))deallocate(Subbasins)
                allocate(Subbasins(WaterShed.NSubbasin))
                
                do i=1,WaterShed.NSubbasin
                    Subbasins(i).SubID  = Reachs(i).ReachId
                    Subbasins(i).NUnits = Reachs(i).MY
                    Subbasins(i).Area   = Reachs(i).Area
                    Subbasins(i).Lati   = Reachs(i).Lati
                enddo
                
            end subroutine
            
            subroutine DestroySubbasins()
                if(allocated(Subbasins))deallocate(Subbasins)
            end subroutine
            
    end module