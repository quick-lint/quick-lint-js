-m 4GB

@@@ general security notes
* we should restrict the list of supported containers
  * runners.docker.allowed_images https://docs.gitlab.com/runner/configuration/advanced-configuration.html#the-runnersdocker-section
* we should lock down network access somehow
* disable c:\cache

@@@ we should use our own helper to reduce layer bloat.
* started in gitlab-runner-helper-windows/
* https://docs.gitlab.com/runner/configuration/advanced-configuration.html#helper-image-registry
When creating a runner, make sure to set shell="powershell" (not pwsh)
