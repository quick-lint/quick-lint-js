
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DDEBUG")


# Build rules for md2html command line utility

include_directories("${PROJECT_SOURCE_DIR}/src")
add_executable(md2html cmdline.c cmdline.h md2html.c)
target_link_libraries(md2html md4c-html)


# Install rules

install(
    TARGETS md2html
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(FILES "md2html.1" DESTINATION "${CMAKE_INSTALL_MANDIR}/man1")

