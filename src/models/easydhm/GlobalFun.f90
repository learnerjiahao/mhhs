! rootdepth calculation function
		function CalRootDpth(nd0,lai,lai_max,rtdpth_max)
			integer nd0
			real lai, lai_max, rtdpth_max
			real CalRootDpth
			
			CalRootDpth = lai/lai_max*rtdpth_max
		end	
			
		! interception calculation function
		function CalInterCapacity(nd0,lai,lai_max,itc_max)
			integer nd0
			real lai, lai_max, itc_max
			real CalInterCapacity
			
			CalInterCapacity = lai/lai_max*itc_max
		end
		
		! lai calculation function
		function CalLai(nd0,lai_max,lai_min)
			integer nd0
			real lai_max, lai_min
			real b
			real CalLai
			b = 1.35
			pi =3.14159
	    
			help0=(1.0+SIN(2.0*pi*(nd0-87.0)/365.0))**b
			CalLai=lai_min+(lai_max-lai_min)*help0/2.55
		end
		
        character * 5 Function Cstr(orgInt)
            integer orgInt
            character * 5 ctmp1
            
            write(ctmp1,'(i5)') orgInt
		    ! trim前面的空格，后面本来就没有空格
		    LoopA:do IW2=1,5
			    if (ctmp1(IW2:IW2) .ne. ' ') then
				    ctmp1 = ctmp1(IW2:len(ctmp1))
				    exit LoopA
			    endif
		    enddo LoopA
            
            Cstr = ctmp1
            return
        end