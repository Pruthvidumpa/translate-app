# Revision history for translate-app

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released.

# Bug/Info identified during development: 
1)  cabal.exe: The package has a './configure' script. If you are on Windows, This
    requires a Unix compatibility toolchain such as MinGW+MSYS or Cygwin. If you
    are not on Windows, ensure that an 'sh' command is discoverable in your path.

    Solution: Seems to be a windows specific issue. Haskell is not well supported on Windows, so one option is to run Haskell under WSL or directly on Linux. If that is not an option, this error seems like it can also be solved by installing msys on windows system. https://www.msys2.org/
