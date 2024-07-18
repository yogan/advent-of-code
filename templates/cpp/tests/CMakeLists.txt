add_executable(tests test_main.cpp)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)

include(CTest)
include(Catch)
catch_discover_tests(tests)
