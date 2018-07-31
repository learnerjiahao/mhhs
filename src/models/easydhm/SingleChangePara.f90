module SingleChangePara
    use SolutionMod
    implicit none
    type sinChangePara
        integer ::IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,sensfun1rank,sensfun2rank,sensfun1count
        real ::bl1,bl2,Def,Bestpar,sensfun1mean,sensfun2mean
        character(255) ::pname
    end type sinChangePara

    type(sinChangePara),dimension(:,:),allocatable   ::sinChangeParas
     type(sinChangePara),dimension(:,:),allocatable   ::outChangeParas   
    integer ::sinChangeParaNck
    integer::sinChangeParaNparameter
    integer::ipidsinchangepara=1
    contains

    subroutine initSinChangeParaChars(i,npid,nCount_SinChangePara,pname)
        integer        ::npid,nCount_SinChangePara,ipid,i
        character(255) ::pname
         if(allocated(sinChangeParas))then
            do ipid=1,npid
                sinChangeParas(i,ipid).pname = pname
            enddo           
         end if
    end subroutine
    
    subroutine initSinChangePara(nCount,nSinP,Nparameter,npid)
        integer ::i,Nparameter,npid,ipid
        integer ncount
        real ,dimension(13,nCount*npid)   ::nSinP
        if(nCount .gt. 0)then
            sinChangeParaNck = nCount
            sinChangeParaNparameter = Nparameter
            if(allocated(sinChangeParas))deallocate(sinChangeParas)
            allocate(sinChangeParas(nCount,npid))
            do ipid=1,npid
                do i = 1,nCount
                    sinChangeParas(i,ipid).IParaYear      = nSinP(1,(ipid-1)*nCount+i) 
                    sinChangeParas(i,ipid).Pid            = nSinP(2,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).IRunoffRenType = nSinP(3,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).in1            = nSinP(4,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).in2            = nSinP(5,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).IsSenseUsed    = nSinP(6,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).bl1            = nSinP(7,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).bl2            = nSinP(8,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).Def            = nSinP(9,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).IsParasolUsed  = nSinP(10,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).sensfun1rank  =nSinP(12,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).sensfun1mean  =nSinP(13,(ipid-1)*nCount+i)
                    sinChangeParas(i,ipid).Bestpar  = nSinP(11,(ipid-1)*nCount+i)
                end do         
            end do
        endif
     end subroutine
     
     Function GegPrDataSinChangePara(IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,bl1,bl2,Def,sensfun1rank,sensfun2rank,Bestpar,sensfun1mean,sensfun2mean)result(N)
        use solutionMod
        integer         ::sensfun1rank,sensfun2rank,N,IParaYear,Pid,IRunoffRenType,in1,in2,IsSenseUsed,IsParasolUsed,npid
        real            ::Bestpar,sensfun1mean,sensfun2mean,bl1,bl2,Def
        integer nopt,nCount,i
        nCount = 0
        !输出马斯京根汇流的K值到文件msk_K.txt中
        open(unit = 180116, file = 'msk_K.txt', status = 'REPLACE')
        write(180116,*) 'K in Muskingum is ...', global_msk_K
        close(180116)

        if(allocated(sinChangeParas))then
             do i = 1,sinChangeParaNck
                nCount = nCount + 1
             end do
             nopt = nCount
        end if
        N = 1
        npid=size(outChangeParas,dim=2)
        if(allocated(sinChangeParas))then
            if(outsinIdx .gt. nopt*npid)then
               N = 0
               return
            endif
            if(outsinIdx==nopt*ipidsinchangepara+1)then
                ipidsinchangepara=ipidsinchangepara+1
            endif

            sensfun1rank = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).sensfun1rank
            sensfun2rank = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).sensfun2rank
            Bestpar      = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).Bestpar
            sensfun1mean = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).sensfun1mean
            sensfun2mean = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).sensfun2mean
            bl1 = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).bl1
            bl2 = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).bl2
            Def = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).Def
            IParaYear  = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).IParaYear
            Pid = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).Pid
            IRunoffRenType = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).IRunoffRenType
            in1 = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).in1
            in2 = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).in2
            IsSenseUsed = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).IsSenseUsed           
            IsParasolUsed  = outChangeParas(outsinIdx-(ipidsinchangepara-1)*nopt,ipidsinchangepara).IsParasolUsed
            outsinIdx = outsinIdx + 1
        else 
            N = -1
        end if  
    end Function
       
     subroutine DestroySinChangePara
        use SolutionMod
        if(allocated(sinChangeParas))then
            deallocate(sinChangeParas)
            if(allocated(outChangeParas))deallocate(outChangeParas)
            sinChangeParaNck = 0
            outsinIdx = 1
        endif
     end subroutine   
end module