module ST_OutUnitDHMMod
     implicit none   
    type ST_UnitDHMData
        character(19)     ::TM
        integer            ::Sid, SubId,UnitId
        real               ::Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
        character(255)    ::StSoillymm,StSoilly,StlyFromm,TmpSoilly
    end type ST_UnitDHMData
 
    type(ST_UnitDHMData),dimension(:),allocatable   ::outST_UnitDHM
    integer ::ST_UnitDHMNCK 
    integer ::outUnitDHMIdx
    contains
    
    subroutine insertST_UnitDHM(TM,Sid, SubId,UnitId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoillymm,StSoil,StSoilly,StFromm,StlyFromm,StGrdmm,GWhtmm,SurTmpC,TmpSoilly)  
        character(19)     ::TM
        integer            ::Sid, SubId,UnitId
        real               ::Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
        character(255)    ::StSoilly,StlyFromm,TmpSoilly ,StSoillymm
        type(ST_UnitDHMData) ::st_unit
        type(ST_UnitDHMData),dimension(:),allocatable   ::outtemp
        st_unit.TM         = TM
        st_unit.Sid        = Sid
        st_unit.SubId      = SubId
        st_unit.UnitId     = UnitId
        st_unit.Pmm        = Pmm
        st_unit.AirTemp    = AirTemp
        st_unit.PETmm      = PETmm 
        st_unit.SnowMeltmm = SnowMeltmm
        st_unit.ETIntercmm = ETIntercmm
        st_unit.Intercmm   = Intercmm
        st_unit.ETDepremm  = ETDepremm
        st_unit.Depremm    = Depremm
        st_unit.PNetmm     = PNetmm
        st_unit.RSmm       = RSmm
        st_unit.Infilmm    = Infilmm
        st_unit.ETSoilmm   = ETSoilmm
        st_unit.ETPlantmm  = ETPlantmm
        st_unit.RImm       = RImm 
        st_unit.Percomm    = Percomm
        st_unit.rchrg      = rchrg
        st_unit.ETGrdmm    = ETGrdmm
        st_unit.RGmm       = RGmm
        st_unit.Emm        = Emm
        st_unit.Rmm        = Rmm
        st_unit.StSnowmm   = StSnowmm
        st_unit.StItcmm    = StItcmm
        st_unit.StDepmm    = StDepmm
        st_unit.StSoilmm   = StSoilmm
        st_unit.StSoillymm = StSoillymm
        st_unit.StSoil     = StSoil
        st_unit.StSoilly   = StSoilly
        st_unit.StFromm    = StFromm
        st_unit.StlyFromm  = StlyFromm
        st_unit.StGrdmm    = StGrdmm 
        st_unit.GWhtmm     = GWhtmm
        st_unit.SurTmpC    = SurTmpC
        st_unit.TmpSoilly  = TmpSoilly
        
        !write(9,'(30F9.3)') Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm
        ST_UnitDHMNCK  = ST_UnitDHMNCK + 1    
        outST_UnitDHM(ST_UnitDHMNCK) = st_unit

        
    end subroutine
 
     Function GegPrDataUnitDHM(TM,Sid, SubId,UnitId, Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoillymm,StSoil,StSoilly,StFromm,StlyFromm,StGrdmm,GWhtmm,SurTmpC,TmpSoilly)result(N)
        character(19)     ::TM
        integer            ::Sid, SubId,UnitId,N
        real               ::Pmm ,AirTemp,PETmm,SnowMeltmm,ETIntercmm,Intercmm,ETDepremm,Depremm,PNetmm,RSmm,Infilmm,ETSoilmm,ETPlantmm,RImm,Percomm,rchrg,ETGrdmm,RGmm,Emm,Rmm,StSnowmm,StItcmm,StDepmm,StSoilmm,StSoil,StFromm,StGrdmm,GWhtmm,SurTmpC
        character(255)    ::StSoilly,StlyFromm,TmpSoilly ,StSoillymm
        N = 1
        if(allocated(outST_UnitDHM))then
            if(outUnitDHMIdx .gt. ST_UnitDHMNCK)then
               N = 0
               return
            endif
        
            TM = outST_UnitDHM(outUnitDHMIdx).TM
            Sid = outST_UnitDHM(outUnitDHMIdx).Sid
            SubId = outST_UnitDHM(outUnitDHMIdx).SubId
            UnitId = outST_UnitDHM(outUnitDHMIdx).UnitId
            Pmm = outST_UnitDHM(outUnitDHMIdx).Pmm
            AirTemp = outST_UnitDHM(outUnitDHMIdx).AirTemp
            PETmm  = outST_UnitDHM(outUnitDHMIdx).PETmm
            SnowMeltmm = outST_UnitDHM(outUnitDHMIdx).SnowMeltmm
            ETIntercmm = outST_UnitDHM(outUnitDHMIdx).ETIntercmm
            Intercmm = outST_UnitDHM(outUnitDHMIdx).Intercmm
            ETDepremm = outST_UnitDHM(outUnitDHMIdx).ETDepremm
            Depremm = outST_UnitDHM(outUnitDHMIdx).Depremm
            PNetmm = outST_UnitDHM(outUnitDHMIdx).PNetmm
            RSmm = outST_UnitDHM(outUnitDHMIdx).RSmm
            Infilmm = outST_UnitDHM(outUnitDHMIdx).Infilmm
            ETSoilmm = outST_UnitDHM(outUnitDHMIdx).ETSoilmm
            ETPlantmm = outST_UnitDHM(outUnitDHMIdx).ETPlantmm
            RImm  = outST_UnitDHM(outUnitDHMIdx).RImm
            Percomm = outST_UnitDHM(outUnitDHMIdx).Percomm
            rchrg = outST_UnitDHM(outUnitDHMIdx).rchrg
            ETGrdmm = outST_UnitDHM(outUnitDHMIdx).ETGrdmm
            RGmm = outST_UnitDHM(outUnitDHMIdx).RGmm
            Emm = outST_UnitDHM(outUnitDHMIdx).Emm
            Rmm = outST_UnitDHM(outUnitDHMIdx).Rmm
            StSnowmm = outST_UnitDHM(outUnitDHMIdx).StSnowmm
            StItcmm = outST_UnitDHM(outUnitDHMIdx).StItcmm
            StDepmm = outST_UnitDHM(outUnitDHMIdx).StDepmm
            StSoilmm = outST_UnitDHM(outUnitDHMIdx).StSoilmm
            StSoillymm = outST_UnitDHM(outUnitDHMIdx).StSoillymm
            StSoil = outST_UnitDHM(outUnitDHMIdx).StSoil
            StSoilly = outST_UnitDHM(outUnitDHMIdx).StSoilly
            StFromm = outST_UnitDHM(outUnitDHMIdx).StFromm
            StlyFromm = outST_UnitDHM(outUnitDHMIdx).StlyFromm
            StGrdmm  = outST_UnitDHM(outUnitDHMIdx).StGrdmm
            GWhtmm = outST_UnitDHM(outUnitDHMIdx).GWhtmm
            SurTmpC = outST_UnitDHM(outUnitDHMIdx).SurTmpC
            TmpSoilly = outST_UnitDHM(outUnitDHMIdx).TmpSoilly

            outUnitDHMIdx = outUnitDHMIdx + 1
        else 
            N = -1
        end if  
    end Function
       
    subroutine DestroyST_UnitDHM
        if(allocated(outST_UnitDHM))then
        deallocate(outST_UnitDHM)
        ST_UnitDHMNCK = 0
        outUnitDHMIdx = 1
        endif
    end subroutine
end module