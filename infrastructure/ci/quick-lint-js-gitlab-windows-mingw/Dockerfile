FROM mcr.microsoft.com/windows:20H2

SHELL ["cmd", "/S", "/C"]

RUN powershell \
        -NoProfile \
        -InputFormat None \
        -ExecutionPolicy Bypass \
        -Command "[System.Net.ServicePointManager]::SecurityProtocol = 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))"
RUN choco install -y git --params=/NoAutoCrlf
RUN choco install -y ninja python3

# @@@ merge
#RUN curl --location --output msys2-x86_64.exe https://repo.msys2.org/distrib/x86_64/msys2-x86_64-20220603.exe
#RUN .\msys2-x86_64.exe install --root C:\msys64 --confirm-command
#RUN del /q msys2-x86_64.exe
RUN choco install -y msys2

#mingw-w64-i686-gcc
#mingw-w64-ucrt-x86_64-gcc
