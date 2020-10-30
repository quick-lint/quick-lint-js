# @@@ copyright

add_library(RapidJSON INTERFACE)
target_include_directories(RapidJSON INTERFACE rapidjson/include)
quick_lint_js_add_warning_options_if_supported(
  RapidJSON
  INTERFACE
  -Wno-zero-as-null-pointer-constant
)
