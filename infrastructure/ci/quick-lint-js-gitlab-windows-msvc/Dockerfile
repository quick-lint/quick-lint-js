FROM mcr.microsoft.com/windows:20H2

SHELL ["cmd", "/S", "/C"]

# Reference:
# https://docs.microsoft.com/en-us/visualstudio/install/build-tools-container?view=vs-2022

# @@@ merge
RUN curl --location --output vs_BuildTools.exe https://aka.ms/vs/17/release/vs_BuildTools.exe

RUN .\vs_BuildTools.exe --quiet --wait --norestart --nocache \
        --installPath "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\BuildTools" \
        --add Microsoft.Component.MSBuild \
        --add Microsoft.Component.VC.Runtime.UCRTSDK \
        --add Microsoft.VisualStudio.Component.VC.CMake.Project \
        --add Microsoft.VisualStudio.Component.VC.CoreBuildTools \
        --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 \
        --add Microsoft.VisualStudio.Component.Windows10SDK.20348

RUN del /q vs_BuildTools.exe

RUN powershell \
        -NoProfile \
        -InputFormat None \
        -ExecutionPolicy Bypass \
        -Command "[System.Net.ServicePointManager]::SecurityProtocol = 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))"

RUN choco install -y git --params=/NoAutoCrlf
RUN choco install -y ninja python3

# ENTRYPOINT ["C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\BuildTools\\Common7\\Tools\\VsDevCmd.bat", "&&", "powershell.exe", "-NoLogo", "-ExecutionPolicy", "Bypass"]
# C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\Common7\Tools\VsDevCmd.bat
