# Almost all CMake files should start with this
# You should always specify a range with the newest
# and oldest tested versions of CMake. This will ensure
# you pick up the best policies.
cmake_minimum_required(VERSION 3.1...3.29)

# add dependencies
include(cmake/CPM.cmake)

CPMAddPackage("gh:catchorg/Catch2@3.5.4")
CPMAddPackage("gh:fmtlib/fmt#10.2.1")
CPMAddPackage("gh:emil-e/rapidcheck#ff6af6fc683159deb51c543b065eba14dfcf329b")
# CPMAddPackage("gh:ReneNyffenegger/cpp-base64#951de609dbe27ce8864dfe47323c4ade96bee86e")
CPMAddPackage("gh:aklomp/base64#b20a31a997e0b48274fa09e58b65ee9202531e4f")
set(RC_INSTALL_ALL_EXTRAS ON CACHE BOOL "Rapidcheck Catch Support" FORCE)

# This is your project statement. You should always list languages;
# Listing the version is nice here since it sets lots of useful variables
project(
  Hnefatafl
  VERSION 1.0
  LANGUAGES CXX)

# If you set any CMAKE_ variables, that can go here.
# (But usually don't do this, except maybe for C++ standard)

# Export compile_commands.json directly from Cmake.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find packages go here.

# You should usually split this into folders, but this is a simple example

# This is a "default" library, and will match the *** variable setting.
# Other common choices are STATIC, SHARED, and MODULE
# Including header files here helps IDEs but is not required.
# Output libname matches target name, with the usual extensions on your system
# add_library(MyLibExample simple_lib.cpp simple_lib.hpp)

# Link each target with other targets or add options, etc.

# Adding something we can run - Output name matches target name
add_executable(test tests/capture.cpp)
target_link_libraries(test PRIVATE fmt::fmt-header-only Catch2::Catch2WithMain rapidcheck_catch base64)

#set(CMAKE_CXX_FLAGS "-O3 -mbmi -mbmi2 -mlzcnt -mavx -mavx2 -std=c++23 -fsanitize=address")
set(CMAKE_CXX_FLAGS "-O3 -mbmi -mbmi2 -mlzcnt -mavx -mavx2 -std=c++23")

# Make sure you link your targets with this command. It can also link libraries and
# even flags, so linking a target that does not exist will not give a configure-time error.
# target_link_libraries(MyExample PRIVATE MyLibExample)

# add_custom_command(
#     TARGET test
#     POST_BUILD
#     COMMAND test
#     )
