
$INSTALLDIR="C:\Program Files\Certara\NLME_Engine"
if($INSTALLDIR -eq "" -or $INSTALLDIR -eq $null)
{
  "Installation directory is not specified"
  exit 1
}
powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR\generic_run.ps1 local_mpi $INSTALLDIR $shared_directory C:\Data\GitHubProjects\R-Certara\inst\Certara_R_School\Lesson_5\wtClmod C:\Data\GitHubProjects\R-Certara\inst\Certara_R_School\Lesson_5\wtClmod\jobControlFile.txt 4 WorkFlow
