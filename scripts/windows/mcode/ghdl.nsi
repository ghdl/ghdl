; ghdl.nsi
;
; This script is based on example2.nsi.
;  remember the directory, 
;  Check if administrator
;  uninstall support
; TODO:
;  * allow multiple version
;  * command line installation
;  * Allow user install

;--------------------------------
!include version.nsi
;--------------------------------

; The name of the installer
Name "Ghdl"

; The file to write
OutFile "ghdl-installer-${VERSION}.exe"

SetDateSave on

; The default installation directory
InstallDir $PROGRAMFILES\Ghdl

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Ghdl" "Install_Dir"

LicenseData ..\..\..\COPYING
; LicenseForceSelection

;--------------------------------

; Pages

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------
Function .onInit
  Call IsNT
  pop $R0
  StrCmp $R0 1 nt_ok
  MessageBox MB_OK|MB_ICONEXCLAMATION "You must use Windows NT (XP/2000/Me...)"
  Quit

nt_ok:
  Call IsUserAdmin
  Pop $R0
  StrCmp $R0 "true" Admin
  MessageBox MB_OK|MB_ICONEXCLAMATION "You must have Admin rights"
  Quit

Admin:

  ;;;  Check if already installed.
  ReadRegStr $0 HKLM "Software\Ghdl" "Install_Dir"
  IfErrors not_installed
  ReadRegStr $0 HKLM "Software\Ghdl" "Version"
  IfErrors unknown_prev_version
  Goto known_version
unknown_prev_version:
  StrCpy $0 "(unknown)"
known_version:
  MessageBox MB_OKCANCEL|MB_ICONEXCLAMATION "You already have GHDL version $0 installed.  Deinstall ?" IDCANCEL install_abort IDOK deinstall
install_abort:
  Abort "Installation aborted"
deinstall:
  ReadRegStr $0 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl" "UninstallString"
  IfErrors deinstall_failed

  ; First version of the GHDL installer adds quotes
  StrCpy $1 $0 1
  StrCmp $1 '"' 0 str_ok
  StrCpy $1 $0 "" 1
  StrCpy $0 $1 -1
str_ok:

  ; Read install dir
  ReadRegStr $1 HKLM "Software\Ghdl" "Install_Dir"
  IfErrors deinstall_failed

;  MessageBox MB_OK 'copy $0 to $TEMP'

  ClearErrors
;  MessageBox MB_OK 'copy $0 to $TEMP'
  CopyFiles $0 $TEMP
  IfErrors deinstall_failed
  ExecWait '"$TEMP\uninst-ghdl.exe" /S _?=$1'
  IfErrors deinstall_failed
  Delete "$TEMP\uninst-ghdl.exe"
  Return
deinstall_failed:
  Delete $TEMP\uninst-ghdl.exe
  MessageBox MB_YESNO|MB_ICONSTOP "Can't deinstall GHDL: de-installer not found or failed.  Continue installation ?" IDNO install_abort
not_installed:
  Return
FunctionEnd

;--------------------------------

; The stuff to install
Section "Ghdl Compiler (required)"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\bin
  File /oname=ghdl.exe ..\build\ghdl.exe
  
  SetOutPath $INSTDIR
  File /oname=COPYING.txt ..\..\..\COPYING

  ; Write the installation path into the registry
  WriteRegStr HKLM "Software\Ghdl" "Install_Dir" $INSTDIR
  ; Write te version
  WriteRegStr HKLM "Software\Ghdl" "Version" ${VERSION}
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl" "DisplayName" "Ghdl"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl" "UninstallString" $INSTDIR\uninst-ghdl.exe
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl" "NoRepair" 1
  WriteUninstaller $INSTDIR\uninst-ghdl.exe"
  
SectionEnd

Section "VHDL standard and ieee libraries"
  SectionIn RO
  SetOutPath $INSTDIR\lib\v87
  File /r ..\lib\v87\std ..\lib\v87\ieee
  SetOutPath $INSTDIR\lib\v93
  File /r ..\lib\v93\std ..\lib\v93\ieee
  SetOutPath $INSTDIR\lib\v08
  File /r ..\lib\v08\std ..\lib\v08\ieee
SectionEnd

Section "Synopsys libraries (Recommended)"
  SetOutPath $INSTDIR\lib\v87
  File /r ..\lib\v87\synopsys
  SetOutPath $INSTDIR\lib\v93
  File /r ..\lib\v93\synopsys
SectionEnd

Section "Documentation (Recommended)"
  SetOutPath $INSTDIR
  File /oname=ghdl.htm ..\..\..\doc\ghdl.html
SectionEnd

Section "Add in PATH (Recommended)"
  WriteRegDWORD HKLM "Software\Ghdl" "PathSet" 1
  Push $INSTDIR\Bin
  Call AddToPath
SectionEnd

; Optional section (can be disabled by the user)
;Section "Start Menu Shortcuts"
;
;  CreateDirectory "$SMPROGRAMS\Ghdl"
;  CreateShortCut "$SMPROGRAMS\Ghdl\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
;  CreateShortCut "$SMPROGRAMS\Ghdl\Ghdl.lnk" "$INSTDIR\example2.nsi" "" "$INSTDIR\example2.nsi" 0
;  
;SectionEnd
;

;--------------------------------

; Uninstaller

Section "Uninstall"

  ReadRegDWORD $0 HKLM "Software\Ghdl" "PathSet"
  StrCmp $0 "1" "" path_not_set
  Push $INSTDIR\Bin
  Call un.RemoveFromPath

path_not_set:

  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Ghdl"
  DeleteRegKey HKLM Software\Ghdl

  ; Remove files and uninstaller
  Delete $INSTDIR\bin\ghdl.exe
  Delete $INSTDIR\bin\ghdl.htm
  Delete $INSTDIR\uninst-ghdl.exe
  Delete $INSTDIR\COPYING.txt
  RMDir $INSTDIR\bin
  RMDir /r $INSTDIR\lib


  ; Remove shortcuts, if any
  ; Delete "$SMPROGRAMS\Ghdl\*.*"

  ; Remove directories used
  ; RMDir "$SMPROGRAMS\Ghdl"
  RMDir "$INSTDIR"

SectionEnd

;;;;;;;; Misc functions

; Author: Lilla (lilla@earthlink.net) 2003-06-13
; function IsUserAdmin uses plugin \NSIS\PlusgIns\UserInfo.dll
; This function is based upon code in \NSIS\Contrib\UserInfo\UserInfo.nsi
; This function was tested under NSIS 2 beta 4 (latest CVS as of this writing).
;
; Usage:
;   Call IsUserAdmin
;   Pop $R0   ; at this point $R0 is "true" or "false"
;
Function IsUserAdmin
Push $R0
Push $R1
Push $R2
 
ClearErrors
UserInfo::GetName
IfErrors Win9x
Pop $R1
UserInfo::GetAccountType
Pop $R2
 
StrCmp $R2 "Admin" 0 Continue
; Observation: I get here when running Win98SE. (Lilla)
; The functions UserInfo.dll looks for are there on Win98 too, 
; but just don't work. So UserInfo.dll, knowing that admin isn't required
; on Win98, returns admin anyway. (per kichik)
; MessageBox MB_OK 'User "$R1" is in the Administrators group'
StrCpy $R0 "true"
Goto Done
 
Continue:
; You should still check for an empty string because the functions
; UserInfo.dll looks for may not be present on Windows 95. (per kichik)
StrCmp $R2 "" Win9x
StrCpy $R0 "false"
;MessageBox MB_OK 'User "$R1" is in the "$R2" group'
Goto Done
 
Win9x:
; comment/message below is by UserInfo.nsi author:
; This one means you don't need to care about admin or
; not admin because Windows 9x doesn't either
;MessageBox MB_OK "Error! This DLL can't run under Windows 9x!"
StrCpy $R0 "true"
 
Done:
;MessageBox MB_OK 'User= "$R1"  AccountType= "$R2"  IsUserAdmin= "$R0"'
 
Pop $R2
Pop $R1
Exch $R0
FunctionEnd


!define ALL_USERS
 
!ifndef WriteEnvStr_RegKey
  !ifdef ALL_USERS
    !define WriteEnvStr_RegKey \
       'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
  !else
    !define WriteEnvStr_RegKey 'HKCU "Environment"'
  !endif
!endif

!verbose 3
!include "WinMessages.NSH"
!verbose 4

; AddToPath - Adds the given dir to the search path.
;        Input - head of the stack
;        Note - Win9x systems requires reboot
 
Function AddToPath
  Exch $0
  Push $1
  Push $2
  Push $3
 
  # don't add if the path doesn't exist
  IfFileExists "$0\*.*" "" AddToPath_done
 
  ReadEnvStr $1 PATH
  Push "$1;"
  Push "$0;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$0\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  GetFullPathName /SHORT $3 $0
  Push "$1;"
  Push "$3;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$3\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
 
  ReadRegStr $1 ${WriteEnvStr_RegKey} "PATH"
  StrCpy $2 $1 1 -1 # copy last char
  StrCmp $2 ";" 0 +2 # if last char == ;
    StrCpy $1 $1 -1 # remove last char
  StrCmp $1 "" AddToPath_NTdoIt
    StrCpy $0 "$1;$0"
 AddToPath_NTdoIt:
   WriteRegExpandStr ${WriteEnvStr_RegKey} "PATH" $0
   SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
 
  AddToPath_done:
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd
 
; RemoveFromPath - Remove a given dir from the path
;     Input: head of the stack
 
Function un.RemoveFromPath
  Exch $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  Push $6
 
  IntFmt $6 "%c" 26 # DOS EOF
 
  ReadRegStr $1 ${WriteEnvStr_RegKey} "PATH"
  StrCpy $5 $1 1 -1 # copy last char
  StrCmp $5 ";" +2 # if last char != ;
    StrCpy $1 "$1;" # append ;
  Push $1
  Push "$0;"
  Call un.StrStr ; Find `$0;` in $1
  Pop $2 ; pos of our dir
  StrCmp $2 "" unRemoveFromPath_done
    ; else, it is in path
    # $0 - path to add
    # $1 - path var
    StrLen $3 "$0;"
    StrLen $4 $2
    StrCpy $5 $1 -$4 # $5 is now the part before the path to remove
    StrCpy $6 $2 "" $3 # $6 is now the part after the path to remove
    StrCpy $3 $5$6
 
    StrCpy $5 $3 1 -1 # copy last char
    StrCmp $5 ";" 0 +2 # if last char == ;
      StrCpy $3 $3 -1 # remove last char
 
    WriteRegExpandStr ${WriteEnvStr_RegKey} "PATH" $3
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
 
  unRemoveFromPath_done:
    Pop $6
    Pop $5
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

###########################################
#            Utility Functions            #
###########################################
 
; IsNT
; no input
; output, top of the stack = 1 if NT or 0 if not
;
; Usage:
;   Call IsNT
;   Pop $R0
;  ($R0 at this point is 1 or 0)
 
!macro IsNT un
Function ${un}IsNT
  Push $0
  ReadRegStr $0 HKLM "SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
  StrCmp $0 "" 0 IsNT_yes
  ; we are not NT.
  Pop $0
  Push 0
  Return
 
  IsNT_yes:
    ; NT!!!
    Pop $0
    Push 1
FunctionEnd
!macroend
!insertmacro IsNT ""
;!insertmacro IsNT "un."
 
; StrStr
; input, top of stack = string to search for
;        top of stack-1 = string to search in
; output, top of stack (replaces with the portion of the string remaining)
; modifies no other variables.
;
; Usage:
;   Push "this is a long ass string"
;   Push "ass"
;   Call StrStr
;   Pop $R0
;  ($R0 at this point is "ass string")
 
!macro StrStr un
Function ${un}StrStr
Exch $R1 ; st=haystack,old$R1, $R1=needle
  Exch    ; st=old$R1,haystack
  Exch $R2 ; st=old$R1,old$R2, $R2=haystack
  Push $R3
  Push $R4
  Push $R5
  StrLen $R3 $R1
  StrCpy $R4 0
  ; $R1=needle
  ; $R2=haystack
  ; $R3=len(needle)
  ; $R4=cnt
  ; $R5=tmp
  loop:
    StrCpy $R5 $R2 $R3 $R4
    StrCmp $R5 $R1 done
    StrCmp $R5 "" done
    IntOp $R4 $R4 + 1
    Goto loop
done:
  StrCpy $R1 $R2 "" $R4
  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Exch $R1
FunctionEnd
!macroend
!insertmacro StrStr ""
!insertmacro StrStr "un."
 
