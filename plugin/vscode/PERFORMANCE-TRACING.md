## quick-link-js performance tracing
The vscode plugin file performance-writer.js provides a module to output the speed in which qljs is linting each keystroke pressed in a given js file using qljs.
## performance tracing output file location
The folder location of the performance output file is determined by vscode's [extension context log uri](https://code.visualstudio.com/api/references/vscode-api#Uri). Inside this folder, you will find a file called *qljsPerformanceTrace.txt*
## performance tracing output file format
The log file will contain 4 columns: **timestamp**, **file-ref**, **event-id** and **file-name-or-duration**
- **timestamp**
	- This is the time that the logging of the keystroke occurred formatted in [Unix time](https://kb.narrative.io/what-is-unix-time)
- **file-ref**
	- This is the file reference number for the file that is currently being logged. For example, if you had 2 files you were editing, the log would refer to them as either "1" or "2" depending on which file you're editing
- **event-id**
	- This could either be **"n"** which denotes that a new file has been opened in the current vscode session or **"p"** which denotes that a performance logging event has occured
- **file-name-or-duration**
	- This could either be the file path of the js script that you're editing in the case of it being a new file opened during the current vscode session or it is the duration of the how quickly qljs linted the keystroke in nanoseconds
