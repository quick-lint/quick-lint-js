list(APPEND CMAKE_MODULE_PATH ".")
include(QuickLintJSSublimeTextPackage)

quick_lint_js_sublime_text_get_package_pathname(SUBLIME_TEXT_PACKAGE_PATHNAME)
quick_lint_js_sublime_text_get_package_files(SUBLIME_TEXT_PACKAGE_FILES)
quick_lint_js_sublime_text_get_package_destination(SUBLIME_TEXT_PACKAGE_DESTINATION)

execute_process(
  COMMAND "${CMAKE_COMMAND}" -E
  make_directory "${SUBLIME_TEXT_PACKAGE_PATHNAME}"
  VERBATIM
)
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E
  copy ${SUBLIME_TEXT_PACKAGE_FILES} "${SUBLIME_TEXT_PACKAGE_PATHNAME}"
  VERBATIM
)
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E
  copy "${SUBLIME_TEXT_PACKAGE_PATHNAME}" "${SUBLIME_TEXT_PACKAGE_DESTINATION}"
  VERBATIM
)
