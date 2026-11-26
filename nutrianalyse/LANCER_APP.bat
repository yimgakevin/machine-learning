@echo off
REM ═══════════════════════════════════════════════════════════════════════
REM  🚀 Lanceur NutriAnalyse Pro v2 (Windows)
REM ═══════════════════════════════════════════════════════════════════════

cd /d "C:\Users\Yimga kevin\Documents\Shiny_entreprise"

echo.
echo ╔════════════════════════════════════════════════════════════════╗
echo ║                                                                ║
echo ║  🚀 Lancement NutriAnalyse Pro v2 Level 3                    ║
echo ║                                                                ║
echo ║  28+ modules ^| 4 Agents IA ^| Cartographie mondiale          ║
echo ║                                                                ║
echo ╚════════════════════════════════════════════════════════════════╝
echo.

REM Localiser Rscript (PATH ou installation standard)
set "RSCRIPT=Rscript"
if exist "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

REM Lancer Rscript avec l'application (app.R est le point d'entree)
"%RSCRIPT%" -e "shiny::runApp('app.R', launch.browser=TRUE)"

pause
