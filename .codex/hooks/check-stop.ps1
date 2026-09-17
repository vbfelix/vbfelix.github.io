$ErrorActionPreference = 'Stop'
$repository = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$checker = Join-Path $repository 'scripts/check-ai-workflow.py'
$pythonCommand = Get-Command python -ErrorAction SilentlyContinue
$pythonPath = if ($pythonCommand) { $pythonCommand.Source } else { '' }
if (-not $pythonPath) {
    $bundled = Join-Path $env:USERPROFILE '.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe'
    if (Test-Path -LiteralPath $bundled) { $pythonPath = $bundled }
}
if (-not $pythonPath) {
    @{ systemMessage = 'Python não foi encontrado. Execute scripts/site.ps1 -Action doctor.' } | ConvertTo-Json -Compress
    exit 0
}

& $pythonPath $checker --quiet
if ($LASTEXITCODE -eq 0) {
    @{ continue = $true; suppressOutput = $true } | ConvertTo-Json -Compress
    exit 0
}

@{
    decision = 'block'
    reason = 'A configuração local de IA falhou na validação. Execute ./scripts/site.ps1 -Action test, corrija os erros e tente concluir novamente.'
} | ConvertTo-Json -Compress
