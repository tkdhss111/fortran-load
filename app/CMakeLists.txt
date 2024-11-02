set ( EXE fortran-load )
set ( LIBS sqlite3 )
set ( SRCS 
  ~/1_projects/utils/fortran-sqlite3/src/sqlite.f90
  ~/2_tools/fortran-logger/src/logger_mo.f90
  ~/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/load_mo.f90
  )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fcoarray=single -fdefault-real-8")
set ( CMAKE_Fortran_FLAGS_RELEASE
  "-Drelease -O3" )

add_executable ( ${EXE} ${SRCS} )
target_link_libraries ( ${EXE} ${LIBS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
install ( TARGETS ${EXE} )
