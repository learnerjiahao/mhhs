 module ResVarMod
        use WaterShedMod
        
        type ResVarClass

            real :: ResIn
            real :: ResOut
            real :: HUP
            real :: HDown
            real :: Storage
            
        end type ResVarClass
        
        integer NResIncluded,iresoptini,isNomlev
        type(ResVarClass), dimension(:), allocatable :: ResVars
        
        contains
            subroutine InitResVars
                implicit none
                integer i
                do i = 1, WaterShed.NResRange
                    
                    ResVars(i).ResIn  = 0.0
		            ResVars(i).ResOut = 0.0
		            
		        enddo

            end subroutine
            
    end module