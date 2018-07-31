module ST_OutUnitStateDHMMod
        implicit none
    type ST_UnitStateDHMData
        character(19) ::TM
        integer        ::SubId =0
        integer UnitId
        real           ::flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon
    end type ST_UnitStateDHMData
    
    type(ST_UnitStateDHMData),dimension(:),allocatable   ::outST_UnitStateDHM
    integer ::ST_UnitStateDHMNCK 
    integer ::outUnitStateDHMIdx
    contains
    
       
    subroutine insertST_UnitStateDHM(TM,SubId,UnitId,flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)
        type(ST_UnitStateDHMData) ::st_UnitState
        character(19) ::TM
        integer        ::SubId,UnitId
        real           ::flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon
        st_UnitState.TM                      = TM      
        st_UnitState.SubId                   = SubId
        st_UnitState.UnitId                  = UnitId            
        st_UnitState.flwout                  = flwout    
        st_UnitState.Rchstorm3               = Rchstorm3   
        st_UnitState.Rechgmm                 = Rechgmm     
        st_UnitState.RGmm                    = RGmm        
        st_UnitState.Snowmm                  = Snowmm     
        st_UnitState.Sintmm                  = Sintmm     
        st_UnitState.Sdepmm                  = Sdepmm          
        st_UnitState.GTmm                    = GTmm     
        st_UnitState.SoilCon                 = SoilCon         

        ST_UnitStateDHMNCK  = ST_UnitStateDHMNCK + 1    
        outST_UnitStateDHM(ST_UnitStateDHMNCK) = st_UnitState

        
    endsubroutine

    Function GegPrDataUnitStateDHM(TM,SubId,UnitId,flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon)result(N)
        character(19) ::TM
        integer        ::SubId,UnitId,N
        real           ::flwout,Rchstorm3 ,Rechgmm,RGmm,Snowmm,Sintmm,Sdepmm,GTmm,SoilCon
        N = 1
        if(allocated(outST_UnitStateDHM))then
            if(outUnitStateDHMIdx .gt. ST_UnitStateDHMNCK)then
               N = 0
               return
            endif
            if(outST_UnitStateDHM(outUnitStateDHMIdx).SubId==0) then
               N = 0 
               return
            endif
            TM = outST_UnitStateDHM(outUnitStateDHMIdx).TM     
            SubId = outST_UnitStateDHM(outUnitStateDHMIdx).SubId
            UnitId = outST_UnitStateDHM(outUnitStateDHMIdx).UnitId   
            flwout = outST_UnitStateDHM(outUnitStateDHMIdx).flwout   
            Rchstorm3 = outST_UnitStateDHM(outUnitStateDHMIdx).Rchstorm3
            Rechgmm = outST_UnitStateDHM(outUnitStateDHMIdx).Rechgmm  
            RGmm = outST_UnitStateDHM(outUnitStateDHMIdx).RGmm     
            Snowmm = outST_UnitStateDHM(outUnitStateDHMIdx).Snowmm  
            Sintmm = outST_UnitStateDHM(outUnitStateDHMIdx).Sintmm   
            Sdepmm = outST_UnitStateDHM(outUnitStateDHMIdx).Sdepmm   
            GTmm = outST_UnitStateDHM(outUnitStateDHMIdx).GTmm     
            SoilCon = outST_UnitStateDHM(outUnitStateDHMIdx).SoilCon  

            outUnitStateDHMIdx = outUnitStateDHMIdx + 1
        else 
            N = -1
        end if  
    end Function
        
    subroutine DestroyST_UnitState
        if(allocated(outST_UnitStateDHM))then
        deallocate(outST_UnitStateDHM)
        ST_UnitStateDHMNCK = 0
        outUnitStateDHMIdx = 1
        endif
    end subroutine
end module 