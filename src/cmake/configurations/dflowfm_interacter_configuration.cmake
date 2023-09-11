#Define parameter WITH_INTERACTER
set(WITH_INTERACTER True)

#Generate an error on Linux
if(UNIX)
    message(SEND_ERROR "dflowfm_interacter is not available on Linux.")
endif()

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dflowfm_configuration_basic.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dflowfm_interacter)
