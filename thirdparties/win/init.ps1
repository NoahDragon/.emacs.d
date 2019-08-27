# -*- mode: powershell -*-

if (!(Test-Path 'env:HOME')){
    echo "Adding HOME environment variable..."
    [System.Environment]::SetEnvironmentVariable('HOME',$env:USERPROFILE,[System.EnvironmentVariableTarget]::User)
}

if (!(Test-Path 'env:GTAGSLABLE')){
    echo "Adding GTAGSLABLE environment variable..."
    [System.Environment]::SetEnvironmentVariable('GTAGSLABLE','new-ctags',[System.EnvironmentVariableTarget]::User)
}

if (!(Test-Path 'env:GTAGSFORCECPP')){
    echo "Adding GTAGSFORCECPP environment variable..."
    [System.Environment]::SetEnvironmentVariable('GTAGSFORCECPP',1,[System.EnvironmentVariableTarget]::User)
}

if (!(Get-Command 'scoop' -errorAction SilentlyContinue)){
    echo "Installing scoop.sh..."
    # Install the scoop
    iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
}

scoop bucket add extras
scoop install aria2
scoop install global ag ripgrep universal-ctags pandoc everything

pause
