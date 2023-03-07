!  
module  version 
character (128), parameter :: compdate= &  
' Tue Mar 7 20:38:46 2023 +0100 ' // & 
' '  
character(128), parameter :: dataversion= & 
'SEMBA_FDTD Version 1acadcc ' // &  
'Built on ' // & 
trim(adjustl(compdate))  
end module version 
