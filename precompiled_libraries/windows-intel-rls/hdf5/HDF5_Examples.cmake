#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
cmake_minimum_required (VERSION 3.18)
###############################################################################################################
# This script will build and run the examples from a folder
# Execute from a command line:
#     ctest -S HDF5_Examples.cmake,OPTION=VALUE -C Release -VV -O test.log
###############################################################################################################

set(CTEST_CMAKE_GENERATOR "Ninja")
if("")
  set(CMAKE_GENERATOR_TOOLSET "")
endif()
if("")
  set(CMAKE_GENERATOR_ARCHITECTURE "")
endif()
set(CTEST_DASHBOARD_ROOT ${CTEST_SCRIPT_DIRECTORY})

# handle input parameters to script.
#INSTALLDIR - HDF5 root folder
#CTEST_CONFIGURATION_TYPE - Release, Debug, RelWithDebInfo
#CTEST_SOURCE_NAME - name of source folder; HDF5Examples
#CTEST_TOOLCHAIN_FILE - name and path in source of toolchain file
if(DEFINED CTEST_SCRIPT_ARG)
  # transform ctest script arguments of the form
  # script.ctest,var1=value1,var2=value2
  # to variables with the respective names set to the respective values
  string(REPLACE "," ";" script_args "${CTEST_SCRIPT_ARG}")
  foreach(current_var ${script_args})
    if("${current_var}" MATCHES "^([^=]+)=(.+)$")
      set("${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
    endif()
  endforeach()
endif()

###################################################################
### Following Line is one of [Release, RelWithDebInfo, Debug] #####
set(CTEST_CONFIGURATION_TYPE "$ENV{CMAKE_CONFIG_TYPE}")
if(NOT DEFINED CTEST_CONFIGURATION_TYPE)
  set(CTEST_CONFIGURATION_TYPE "Release")
endif()
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCTEST_CONFIGURATION_TYPE:STRING=${CTEST_CONFIGURATION_TYPE}")
##################################################################

if(NOT DEFINED INSTALLDIR)
  if(WIN32)
    set(INSTALLDIR "\"%ProgramFiles%/HDF_Group/HDF5/1.14.4\"")
  else()
    set(INSTALLDIR "/opt/HDF_Group/HDF5/1.14.4")
  endif()
endif()

if(NOT DEFINED CTEST_SOURCE_NAME)
  set(CTEST_SOURCE_NAME "HDF5Examples")
endif()

if(NOT DEFINED CTEST_SITE)
  set(CTEST_SITE "local")
endif()
if(NOT DEFINED CTEST_BUILD_NAME)
  set(CTEST_BUILD_NAME "examples")
endif()
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSITE:STRING=${CTEST_SITE} -DBUILDNAME:STRING=${CTEST_BUILD_NAME}")

#TAR_SOURCE - name of tarfile
#if(NOT DEFINED TAR_SOURCE)
#  set(CTEST_USE_TAR_SOURCE "HDF5Examples-2.0.1-Source")
#endif()

###############################################################################################################
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ROOT:PATH=${INSTALLDIR}")
set (ENV{HDF5_ROOT} "${INSTALLDIR}")
if(WIN32)
  set(SITE_OS_NAME "Windows")
  set(CTEST_BINARY_NAME ${CTEST_SOURCE_NAME}\\build)
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_BINARY_NAME}")
elseif(APPLE)
  set(ENV{DYLD_LIBRARY_PATH} "${INSTALLDIR}/lib")
  set(CTEST_BINARY_NAME ${CTEST_SOURCE_NAME}/build)
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_BINARY_NAME}")
else()
  set(ENV{LD_LIBRARY_PATH} "${INSTALLDIR}/lib")
  set(CTEST_BINARY_NAME ${CTEST_SOURCE_NAME}/build)
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_BINARY_NAME}")
endif()
### default HDF5_PLUGIN_PATH to where the filter libraries are located
set(ENV{HDF5_PLUGIN_PATH} "${INSTALLDIR}/lib/plugin")
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACKAGE_NAME:STRING=hdf5")
### use a toolchain file (supported everywhere)       ####
#if(NOT DEFINED CTEST_TOOLCHAIN_FILE)
#  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_TOOLCHAIN_FILE:STRING=")
#else()
#  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_TOOLCHAIN_FILE:STRING=${CTEST_TOOLCHAIN_FILE}")
#endif()

###############################################################################################################
# For any comments please contact cdashhelp@hdfgroup.org
#
###############################################################################################################

if(WIN32)
  include(${CTEST_SCRIPT_DIRECTORY}\\HDF5_Examples_options.cmake)
  include(${CTEST_SCRIPT_DIRECTORY}\\CTestScript.cmake)
else()
  include(${CTEST_SCRIPT_DIRECTORY}/HDF5_Examples_options.cmake)
  include(${CTEST_SCRIPT_DIRECTORY}/CTestScript.cmake)
endif()
