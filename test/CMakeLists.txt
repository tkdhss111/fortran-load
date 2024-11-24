set ( EXE unit_test )
set ( ARGS
  --wd "/mnt/DATA/load"
  --table "TEPCO"
  )

set ( SRCS 
  ~/2_tools/fortran-logger/src/logger_mo.f90
  ~/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/load_mo.f90
  )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fcoarray=single -fdefault-real-8")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized" )

add_executable ( ${EXE} ${SRCS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
