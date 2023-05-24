local M = {}

function M.nvim_lspconfig_update_initialization_options_from_settings(initialize_params, settings)
  if settings == nil or next(settings) == nil then
    return
  end

  local init_options = initialize_params.initializationOptions
  if init_options == nil then
    init_options = {}
    initialize_params.initializationOptions = init_options
  end
  local init_options_configuration = init_options.configuration
  if init_options_configuration == nil then
    init_options_configuration = {}
    init_options.configuration = init_options_configuration
  end

  for section_name, section in pairs(settings) do
    for setting_name, setting_value in pairs(section) do
      local full_setting_name = section_name .. "." .. setting_name
      if init_options_configuration[full_setting_name] == nil then
        init_options_configuration[full_setting_name] = setting_value
      end
    end
  end
end

return M
