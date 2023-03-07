!  
module  version 
character (128), parameter :: compdate= &  
' Tue Mar 7 20:42:39 2023 +0100 ' // & 
' '  
character(128), parameter :: dataversion= & 
'SEMBA_FDTD Version b265c90 ' // &  
'Built on ' // & 
trim(adjustl(compdate))  
end module version 
