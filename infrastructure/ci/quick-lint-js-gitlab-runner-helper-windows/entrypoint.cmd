@echo off

rem @@@ https://gitlab.com/gitlab-org/gitlab-runner/-/blob/main/dockerfiles/runner-helper/helpers/entrypoint.cmd

IF NOT DEFINED LOCAL_CA_PATH (SET LOCAL_CA_PATH="C:\GitLab-Runner\certs\ca.crt")

IF EXIST %LOCAL_CA_PATH% (
    echo "installing certificate: %LOCAL_CA_PATH%"
    certutil -addstore "Root" %LOCAL_CA_PATH%
)

%*
