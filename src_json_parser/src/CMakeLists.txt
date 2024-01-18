cmake_minimum_required (VERSION 3.18)

add_library(smbjson 
	"labels_mod.F90"
	"cells.F90"
	"smbjson.F90"
	"idchildtable.F90"
	"mesh.F90"
	"parser_tools.F90"
	"nfdetypes_extension.F90" 
)

target_link_libraries(smbjson PRIVATE ${JSONFORTRAN_LIB} semba-types fhash)
