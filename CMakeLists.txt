cmake_minimum_required(VERSION 3.10)

project(ornl-hribf-daq-and-analysis)

#Use rpath on Mac OS
set(CMAKE_MACOSX_RPATH TRUE)

if (CMAKE_COMPILER_IS_GNUCXX)
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "7.0.0")
        message(FATAL_ERROR "GCC version 7+ required.")
    endif ()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -fPIC -fdiagnostics-color=auto")
endif ()

#if user does not specify prefix we assign it to the install directory
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    message(STATUS "Install Prefix not specified.")
    file(MAKE_DIRECTORY install)
    get_filename_component(INSTALL_DIR ${CMAKE_BINARY_DIR}/../install REALPATH)
    set(CMAKE_INSTALL_PREFIX ${INSTALL_DIR} CACHE PATH "Install Prefix" FORCE)
endif ()
message(STATUS "Installing to ${CMAKE_INSTALL_PREFIX}")

#Define the default build type to be Release
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING
            "Build type, options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
    message(STATUS "Build type not defined, using default: ${CMAKE_BUILD_TYPE}")
endif (NOT CMAKE_BUILD_TYPE)
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

#Add additional CXX flags if we use the Debug option
if (CMAKE_BUILD_TYPE MATCHES "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg")
endif (CMAKE_BUILD_TYPE MATCHES "Debug")

#------------------------------------------------------------------------------
#Install options
option(BUILD_ACQ1 "Builds ACQ1" OFF)
option(BUILD_ACQ2 "Builds ACQ2" ON)
option(BUILD_UPAK "Builds UPAK analysis tools" OFF)

#------------------------------------------------------------------------------
IF (BUILD_ACQ1)
    add_subdirectory(acq1)
ENDIF (BUILD_ACQ1)

IF (BUILD_ACQ2)
    add_subdirectory(acq2)
ENDIF (BUILD_ACQ2)

IF (BUILD_UPAK)
    add_subdirectory(upak)
ENDIF (BUILD_UPAK)