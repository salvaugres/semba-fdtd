cmake_minimum_required (VERSION 3.18)

add_library(smbjson 
	"smbjson.f90"
	"labels.f90"
	"idchildtable.f90"
	"cells.f90"
	"mesh.f90"
	"parser_tools.f90"
	"nfdetypes_extension.f90" 
)

target_link_libraries  (smbjson PRIVATE jsonfortran semba-types fhash)