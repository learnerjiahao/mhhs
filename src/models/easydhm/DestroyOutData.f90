subroutine DestroyOutData
    use ST_PR_OutMod    
    use SingleChangePara

    call DestroyST_PR
    call DestroySinChangePara
end subroutine