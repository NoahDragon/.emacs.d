The irony-server.exe is build for Win10 via VS2015. 
        Detailed steps: https://github.com/Sarcasm/irony-mode/wiki/Setting-up-irony-mode-on-Windows (also have a clip in evernote)

libclang.dll is from llvm 8.0 build.
vc_redist.x64.exe is the installation tool for Microsoft Visual C++ Redistributable for Visual Studio 2015.


The irony-server is build for MacOS, in order to use is properly llvm is epected to be already installed (brew install llvm).
The following build command is used:

```
cmake -DLIBCLANG_INCLUDE_DIR\=/usr/local/opt/llvm/include/ -DLIBCLANG_LIBRARY\=/usr/local/opt/llvm/lib/libclang.dylib -DCMAKE_INSTALL_PREFIX\=/Users/Haochen/.emacs.d/irony/  /Users/Haochen/.emacs.d/elpa/irony-20190703.1732/server && cmake --build . --use-stderr --config Release --target install
```
