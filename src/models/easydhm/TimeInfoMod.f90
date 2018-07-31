 module TimeInfoMod
    
		integer  jdt0,CalYears(2),CalMonths(2)
        integer, dimension(:),   allocatable :: MD0,MDE,MD,YD
        integer, dimension(:,:), allocatable :: DH0,DHE,DH
        integer NSimYears,NSimMonths,jdt !,Ndt
        integer IYear, IMonth, ID,IH
        integer MinHourofDay,MaxHourofDay
        integer CalDays(2),CalHours(2)
        integer ::iniTimeInfoSuc 
        

        contains 
            subroutine deleteTimeInfoMod
                if(iniTimeInfoSuc .eq. 1)then                
                    iniTimeInfoSuc = 0
                    if(allocated(MD0))deallocate(MD0)
                    if(allocated(MDE))deallocate(MDE) 
                    if(allocated(MD))deallocate(MD) 
                    if(allocated(YD))deallocate(YD) 
                    if(allocated(DH0))deallocate(DH0) 
                    if(allocated(DH))deallocate(DH) 
                    if(allocated(DHE))deallocate(DHE)                     
                 endif
            endsubroutine    
    endmodule
