! 非读入的可变大小数组定义大小及释放内存
    subroutine AllocateVars()
        use WaterShedMod
        use CurrentUnitWSPDHMVars
        use EasyDHMVarMod
        use ReachVarMod
        use ResVarMod
        
        implicit none
        integer NReach,NUnit,NResRange,NLayer,NParamRange
        
        NReach = WaterShed%NReach
        NUnit  = WaterShed%NUnit
        NParamRange = WaterShed%NParamRange
        NResRange   = WaterShed%NResRange
        Nlayer = WaterShed%Nlayer
                
        if(allocated(u_DHMVars))deallocate(u_DHMVars)
        allocate(u_DHMVars(NReach,NUnit))        
        if(allocated(s_DHMVars))deallocate(s_DHMVars)
        allocate(s_DHMVars(NReach))
        if(allocated(sland_DHMVars))deallocate(sland_DHMVars)
        allocate(sland_DHMVars(NReach,NUnit,6))
        if(allocated(l_DHMVars))deallocate(l_DHMVars)
        allocate(l_DHMVars(NReach,NUnit,NLayer))
        if(allocated(sl_DHMVars))deallocate(sl_DHMVars)
        allocate(sl_DHMVars(NReach,nlayer))
        if(allocated(uland_DHMVars))deallocate(uland_DHMVars)
        allocate(uland_DHMVars(NReach,NUnit,6))        
        if(allocated(lland_DHMVars))deallocate(lland_DHMVars)
        allocate(lland_DHMVars(NReach,NUnit,NLayer,6))
        
!        if(allocated(u_SimpleVars))deallocate(u_SimpleVars)
!        allocate(u_SimpleVars(NReach,NUnit))
!        if(allocated(s_SimpleVars))deallocate(s_SimpleVars)
!        allocate(s_SimpleVars(NReach))
!        
!        
!        if(allocated(u_WSPVars))deallocate(u_WSPVars)
!        allocate(u_WSPVars(NReach,NUnit))
!        if(allocated(s_WSPVars))deallocate(s_WSPVars)
!        allocate(s_WSPVars(NReach))
        
        if(allocated(ReachVars))deallocate(ReachVars)
        allocate(ReachVars(NReach))
        if(allocated(ResVars))deallocate(ResVars)
        allocate(ResVars(NResRange))

        if(allocated(sol_fc))deallocate(sol_fc)
        allocate(sol_fc(WaterShed%Nlayer))
		if(allocated(sol_ul))deallocate(sol_ul)
		allocate(sol_ul(WaterShed%Nlayer))
		if(allocated(sol_hk))deallocate(sol_hk)
		allocate(sol_hk(WaterShed%Nlayer))
		if(allocated(sol_k))deallocate(sol_k)
		allocate(sol_k(WaterShed%Nlayer))
		if(allocated(crdep))deallocate(crdep)
		allocate(crdep(WaterShed%Nlayer))
		if(allocated(volcr))deallocate(volcr)
		allocate(volcr(WaterShed%Nlayer))
        if(allocated(sol_st))deallocate(sol_st)
        allocate(sol_st(WaterShed%Nlayer))
        if(allocated(flat))deallocate(flat)
        allocate(flat(WaterShed%Nlayer))
        if(allocated(sol_prk))deallocate(sol_prk)
        allocate(sol_prk(WaterShed%Nlayer))
		if(allocated(sol_tmp))deallocate(sol_tmp)
		allocate(sol_tmp(WaterShed%Nlayer))
        if(allocated(sol_z))deallocate(sol_z)
        allocate(sol_z(WaterShed%Nlayer))
        if(allocated(sol_fro))deallocate(sol_fro)
        allocate(sol_fro(WaterShed%Nlayer))
        if(allocated(sol_fm))deallocate(sol_fm)
        allocate(sol_fm(WaterShed%Nlayer))
	    if(allocated(sol_es))deallocate(sol_es)
	    allocate(sol_es(WaterShed%Nlayer))
	    if(allocated(sol_h))deallocate(sol_h)
	    allocate(sol_h(WaterShed%Nlayer))
       
    end subroutine
    
    subroutine DeallocateVars()
        use WaterShedMod
!        use SimpleVarMod
        use CurrentUnitWSPDHMVars
        use EasyDHMVarMod
!        use WetSpaVarMod
        use ReachVarMod
        use ResVarMod
        
     
!        if(allocated(u_SimpleVars))deallocate(u_SimpleVars)
!        if(allocated(s_SimpleVars))deallocate(s_SimpleVars)
        
        if(allocated(u_DHMVars))deallocate(u_DHMVars)
        if(allocated(s_DHMVars))deallocate(s_DHMVars)
        if(allocated(l_DHMVars))deallocate(l_DHMVars)
        if(allocated(sl_DHMVars))deallocate(sl_DHMVars)
        
!        if(allocated(u_WSPVars))deallocate(u_WSPVars)
!        if(allocated(s_WSPVars))deallocate(s_WSPVars)
        
        if(allocated(ReachVars))deallocate(ReachVars)
        if(allocated(ResVars))deallocate(ResVars)
        
        if(allocated(sol_fc))deallocate(sol_fc)
		if(allocated(sol_ul))deallocate(sol_ul)
		if(allocated(sol_hk))deallocate(sol_hk)
		if(allocated(sol_k))deallocate(sol_k)		
		if(allocated(crdep))deallocate(crdep)	
		if(allocated(volcr))deallocate(volcr)	
        if(allocated(sol_st))deallocate(sol_st)      
        if(allocated(flat))deallocate(flat)        
        if(allocated(sol_prk))deallocate(sol_prk)     
		if(allocated(sol_tmp))deallocate(sol_tmp)      
        if(allocated(sol_z))deallocate(sol_z)    
        if(allocated(sol_fro))deallocate(sol_fro)
        if(allocated(sol_fm))deallocate(sol_fm)
	    if(allocated(sol_es))deallocate(sol_es)
	    if(allocated(sol_h))deallocate(sol_h)
        
    end subroutine