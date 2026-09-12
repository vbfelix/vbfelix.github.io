param(
    [ValidateSet('render', 'check', 'preview')][string]$Action = 'preview',
    [string]$Python = 'python',
    [string]$Quarto = '',
    [ValidateRange(1024,65533)][int]$Port = 4321
)
$ErrorActionPreference = 'Stop'
$repository = Split-Path -Parent $PSScriptRoot

function Invoke-Checked {
    param([string]$Executable, [string[]]$Arguments)
    & $Executable @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Executable failed with exit code $LASTEXITCODE" }
}

Push-Location -LiteralPath $repository
try {
    if (-not $Quarto) {
        $command = Get-Command quarto -ErrorAction SilentlyContinue
        if ($command) { $Quarto = $command.Source }
        else {
            $bundled = Join-Path $env:ProgramFiles 'RStudio/resources/app/bin/quarto/bin/quarto.exe'
            if (Test-Path -LiteralPath $bundled) { $Quarto = $bundled }
            else { throw 'Quarto not found. Install Quarto or provide -Quarto with its executable path.' }
        }
    }
    if ($Action -eq 'check') {
        Invoke-Checked $Python @('scripts/sync-portfolio.py', '--check')
        Invoke-Checked $Python @('scripts/build-writing.py', '--check')
    } else {
        Invoke-Checked $Python @('scripts/sync-portfolio.py')
        Invoke-Checked $Python @('scripts/build-writing.py')
        Invoke-Checked $Quarto @('render', '--no-clean')
    }
    Invoke-Checked $Python @('scripts/check-site.py')
    Invoke-Checked $Quarto @('run', 'scripts/build-agent-assets.ts', '--check')
    Invoke-Checked $Quarto @('run', 'scripts/test-content-source.ts')
    if ($Action -eq 'preview') {
        $backendPort = $Port + 2
        $proxyProcess = Start-Process -FilePath $Python -ArgumentList @('scripts/preview-proxy.py', '--port', "$Port", '--backend', "$backendPort") -WindowStyle Hidden -PassThru
        try {
            Invoke-Checked $Quarto @('preview', '--port', "$backendPort", '--host', '127.0.0.1', '--no-browser')
        } finally {
            if (-not $proxyProcess.HasExited) { Stop-Process -Id $proxyProcess.Id }
        }
    }
} finally {
    Pop-Location
}
