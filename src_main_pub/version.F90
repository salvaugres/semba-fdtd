!  
module  version 
character (128), parameter :: compdate= &  
' Public part commit  ' // & 
' Tue Mar 7 20:15:51 2023 +0100 ' // & 
' Private part commit  ' // & 
' Tue Mar 7 19:36:14 2023 +0100 ' // & 
' '  
character(128), parameter :: dataversion= & 
'SEMBA_FDTD Version 610d6b2 ' // &  
'Built on ' // & 
trim(adjustl(compdate))  
end module version 
