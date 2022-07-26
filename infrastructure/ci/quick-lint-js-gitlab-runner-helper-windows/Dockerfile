FROM mcr.microsoft.com/windows:20H2

SHELL ["cmd", "/S", "/C"]

RUN powershell \
        -NoProfile \
        -InputFormat None \
        -ExecutionPolicy Bypass \
        -Command "[System.Net.ServicePointManager]::SecurityProtocol = 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))"
RUN choco install -y git git-lfs gitlab-runner

# @@@ is git-lfs needed? is this step needed?
RUN git lfs install --skip-repo

COPY entrypoint.cmd ./

ENTRYPOINT ["entrypoint.cmd"]
