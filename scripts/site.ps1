param(
    [ValidateSet('sync', 'test', 'check', 'render', 'preview', 'verify', 'doctor')]
    [string]$Action = 'preview',
    [ValidateSet('auto', 'staged', 'all')][string]$Scope = 'auto',
    [ValidateSet('auto', 'render', 'none')][string]$Refresh = 'auto',
    [string]$Python = '',
    [string]$Quarto = '',
    [ValidateRange(1024,65533)][int]$Port = 4321
)
$ErrorActionPreference = 'Stop'
$repository = Split-Path -Parent $PSScriptRoot
$stateDirectory = Join-Path $repository '.codex/state'
$stateFile = Join-Path $stateDirectory 'preview.json'

function Resolve-Executable {
    param([string]$Requested, [string]$Name, [string[]]$Fallbacks)
    if ($Requested) {
        if (Test-Path -LiteralPath $Requested) { return (Resolve-Path -LiteralPath $Requested).Path }
        $requestedCommand = Get-Command $Requested -ErrorAction SilentlyContinue
        if ($requestedCommand) { return $requestedCommand.Source }
        throw "$Name not found: $Requested"
    }
    $command = Get-Command $Name -ErrorAction SilentlyContinue
    if ($command) { return $command.Source }
    foreach ($candidate in $Fallbacks) {
        if ($candidate -and (Test-Path -LiteralPath $candidate)) { return $candidate }
    }
    throw "$Name not found. Provide its executable path."
}

function Invoke-Checked {
    param([string]$Executable, [string[]]$Arguments)
    & $Executable @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Executable failed with exit code $LASTEXITCODE" }
}

function Get-ChangedFiles {
    if ($Scope -eq 'staged') { return @(& git diff --cached --name-only --diff-filter=ACMRD) }
    if ($Scope -eq 'all') { return @('*') }
    $files = @(& git diff --name-only --diff-filter=ACMRD)
    $files += @(& git diff --cached --name-only --diff-filter=ACMRD)
    $files += @(& git ls-files --others --exclude-standard)
    return @($files | Sort-Object -Unique)
}

function Invoke-UnitTests {
    Invoke-Checked $script:PythonPath @('-m', 'unittest', 'discover', '-s', 'scripts', '-p', 'test_*.py')
    Invoke-Checked $script:PythonPath @('scripts/check-ai-workflow.py')
}

function Invoke-Checks {
    Resolve-Quarto
    Invoke-Checked $script:PythonPath @('scripts/sync-portfolio.py', '--check')
    Invoke-Checked $script:PythonPath @('scripts/sync-blog.py', '--check')
    Invoke-Checked $script:PythonPath @('scripts/build-writing.py', '--check')
    Invoke-Checked $script:PythonPath @('scripts/check-site.py')
    Invoke-Checked $script:QuartoPath @('run', 'scripts/build-agent-assets.ts', '--check')
    Invoke-Checked $script:QuartoPath @('run', 'scripts/test-content-source.ts')
}

function Invoke-Render {
    Resolve-Quarto
    Stop-OwnedPreview
    Invoke-Checked $script:PythonPath @('scripts/sync-portfolio.py')
    Invoke-Checked $script:PythonPath @('scripts/sync-blog.py')
    Invoke-Checked $script:PythonPath @('scripts/build-writing.py')
    Invoke-Checked $script:QuartoPath @('render', '--no-clean')
    Invoke-Checks
}

function Test-RenderNeeded {
    $homepage = Join-Path $repository 'docs/index.html'
    if (-not (Test-Path -LiteralPath $homepage)) { return $true }
    $outputTime = (Get-Item -LiteralPath $homepage).LastWriteTimeUtc
    $sourceFiles = @(
        Get-ChildItem -LiteralPath (Join-Path $repository '_content'), (Join-Path $repository 'posts'), (Join-Path $repository 'portfolio'), (Join-Path $repository 'assets') -Recurse -File
        Get-ChildItem -LiteralPath $repository -File | Where-Object { $_.Extension -in '.qmd', '.scss', '.css' -or $_.Name -eq '_quarto.yml' }
        Get-Item -LiteralPath (Join-Path $repository 'scripts/build-writing.py'), (Join-Path $repository 'scripts/sync-portfolio.py'), (Join-Path $repository 'scripts/sync-blog.py'), (Join-Path $repository 'scripts/build-agent-assets.ts'), (Join-Path $repository 'scripts/content-source.ts'), (Join-Path $repository 'scripts/test-content-source.ts')
        Get-ChildItem -LiteralPath (Join-Path $repository 'scripts/filters') -Recurse -File
    )
    return [bool]($sourceFiles | Where-Object LastWriteTimeUtc -gt $outputTime | Select-Object -First 1)
}

function Stop-ProcessTree {
    param([int]$ProcessId)
    # Quarto launches deno as a child process, and deno is the one holding the port.
    # Stopping only the parent leaves the port bound and blocks the next preview.
    foreach ($child in @(Get-CimInstance Win32_Process -Filter "ParentProcessId = $ProcessId" -ErrorAction SilentlyContinue)) {
        Stop-ProcessTree -ProcessId ([int]$child.ProcessId)
    }
    Stop-Process -Id $ProcessId -Force -ErrorAction SilentlyContinue
}

function Stop-OwnedPreview {
    if (-not (Test-Path -LiteralPath $stateFile)) { return }
    try { $state = Get-Content -Raw -LiteralPath $stateFile | ConvertFrom-Json } catch { return }
    foreach ($entry in @(
        @{ Id = $state.proxyPid; Started = $state.proxyStarted },
        @{ Id = $state.quartoPid; Started = $state.quartoStarted }
    )) {
        $processId = $entry.Id
        if (-not $processId -or -not $entry.Started) { continue }
        $process = Get-Process -Id $processId -ErrorAction SilentlyContinue
        if ($process -and $process.StartTime.ToUniversalTime().ToString('o') -eq $entry.Started) {
            Stop-ProcessTree -ProcessId $processId
        }
    }
    Remove-Item -LiteralPath $stateFile -Force -ErrorAction SilentlyContinue
}

function Start-Preview {
    Resolve-Quarto
    $backendPort = $Port + 2
    Stop-OwnedPreview
    New-Item -ItemType Directory -Path $stateDirectory -Force | Out-Null
    $quartoProcess = Start-Process -FilePath $script:QuartoPath -ArgumentList @('preview', '--port', "$backendPort", '--host', '127.0.0.1', '--no-browser', '--no-watch-inputs') -WorkingDirectory $repository -WindowStyle Hidden -PassThru
    $proxyProcess = Start-Process -FilePath $script:PythonPath -ArgumentList @('scripts/preview-proxy.py', '--port', "$Port", '--backend', "$backendPort") -WorkingDirectory $repository -WindowStyle Hidden -PassThru
    @{
        quartoPid = $quartoProcess.Id
        quartoStarted = $quartoProcess.StartTime.ToUniversalTime().ToString('o')
        proxyPid = $proxyProcess.Id
        proxyStarted = $proxyProcess.StartTime.ToUniversalTime().ToString('o')
        port = $Port
        backendPort = $backendPort
    } |
        ConvertTo-Json | Set-Content -LiteralPath $stateFile -Encoding utf8
    $uri = "http://127.0.0.1:$Port/"
    for ($attempt = 0; $attempt -lt 20; $attempt++) {
        Start-Sleep -Milliseconds 500
        try {
            $response = Invoke-WebRequest $uri -UseBasicParsing -TimeoutSec 2
            if ($response.StatusCode -eq 200) {
                $disk = Get-Content -Raw -LiteralPath (Join-Path $repository 'docs/index.html') -Encoding utf8
                # Invoke-WebRequest decodes the body with the default single-byte codepage when the
                # response declares no charset, which corrupts accents and breaks the comparison.
                $served = [System.Text.Encoding]::UTF8.GetString($response.RawContentStream.ToArray())
                $servedMain = [regex]::Match($served, '(?s)<main.*?</main>').Value
                $diskMain = [regex]::Match($disk, '(?s)<main.*?</main>').Value
                if ($diskMain.Length -gt 0 -and $servedMain -ceq $diskMain) {
                    Write-Host "PASS: fresh preview available at $uri"
                    Start-Process $uri
                    return
                }
            }
        } catch { }
    }
    Stop-OwnedPreview
    throw "Preview did not serve the current docs/index.html at $uri"
}

Push-Location -LiteralPath $repository
try {
    $pythonFallback = Join-Path $env:USERPROFILE '.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe'
    $quartoFallback = if ($env:ProgramFiles) { Join-Path $env:ProgramFiles 'RStudio/resources/app/bin/quarto/bin/quarto.exe' } else { '' }
    $script:PythonPath = Resolve-Executable $Python 'python' @($pythonFallback)
    $script:QuartoPath = ''
    function Resolve-Quarto {
        if (-not $script:QuartoPath) {
            $script:QuartoPath = Resolve-Executable $Quarto 'quarto' @($quartoFallback)
        }
    }
    switch ($Action) {
        'doctor' {
            Write-Host "Python: $script:PythonPath"
            try { Resolve-Quarto; Write-Host "Quarto: $script:QuartoPath" }
            catch { Write-Host "Quarto: unavailable ($($_.Exception.Message))" }
            Write-Host "Preview: http://127.0.0.1:$Port/"
            Invoke-Checked $script:PythonPath @('scripts/check-ai-workflow.py')
        }
        'sync' {
            Invoke-Checked $script:PythonPath @('scripts/sync-portfolio.py')
            Invoke-Checked $script:PythonPath @('scripts/sync-blog.py')
            Invoke-Checked $script:PythonPath @('scripts/build-writing.py')
        }
        'test' { Invoke-UnitTests }
        'check' { Invoke-Checks }
        'render' { Invoke-Render }
        'verify' {
            $changed = Get-ChangedFiles
            Invoke-Checked $script:PythonPath @('scripts/check-ai-workflow.py')
            if ($Scope -eq 'all' -or $changed -match '^scripts/') { Invoke-UnitTests }
            if ($Scope -eq 'all' -or $changed -match '\.(qmd|yml|yaml|scss|css|lua)$' -or $changed -match '^scripts/') { Invoke-Render }
            else { Write-Host 'PASS: no rendered site inputs changed.' }
        }
        'preview' {
            if ($Refresh -eq 'render' -or ($Refresh -eq 'auto' -and (Test-RenderNeeded))) { Invoke-Render }
            else { Invoke-Checks }
            Start-Preview
        }
    }
} finally {
    Pop-Location
}
