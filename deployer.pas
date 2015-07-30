unit Deployer;

interface

uses
  Winapi.ActiveX, Winapi.MsXML,
  System.Zip,
  System.Sysutils, System.Classes, System.IOUtils, System.Win.Registry, Winapi.Windows;

type
  TDeployClass = record
    Required  : Boolean;
    Name      : String;
    RemoteDir : String;
    Operation : Integer;
    Extensions: String;           // What is it used for?
  end;

  TDeployFile = record
    Enabled      : Boolean;
    ClassName    : String;
    LocalName    : String;
    RemoteName   : String;
    RemoteDir    : String;
    Operation    : Integer;
    Configuration: String;    // Release/Debug/Base/Others...
  end;

  TDeployer = class
  private
    fDelphiPath   : String;
    fDelphiVersion: String;  // The version number - 9.0, 10.0, 11.0, etc
    fConfig       : String;
    fProjectName  : String;
    fPlatform     : String;
    fProjectRoot  : String;
    fRemoteProfile: String;
    fDeployClasses: array of TDeployClass;
    fDeployFiles  : array of TDeployFile;
    fIgnoreErrors : Boolean;
    fPaclientPath : String;
    function  CallPaclient(const aCommand: String): Boolean;
    procedure CheckRemoteProfile;
    procedure GetEmbarcaderoPaths;
    procedure ParseProject(const aProjectPath: String);
  public
    constructor Create(const aDelphiVersion: String);
    procedure BundleProject(const aProjectPath, aZIPName: String);
    procedure DeployProject(const aProjectPath: String);
    procedure ExecuteCommand(const aProjectPath, aCommand: String);
    property Config       : String  read fConfig        write fConfig;
    property IgnoreErrors : Boolean read fIgnoreErrors  write fIgnoreErrors;
    property Platform     : String  read fPlatform      write fPlatform;
    property ProjectRoot  : String  read fProjectRoot   write fProjectRoot;
    property RemoteProfile: String  read fRemoteProfile write fRemoteProfile;
  end;

implementation

const
  // Paclient commands and the parameters to be substituted
  PACLIENT_CLEAN = '--Clean="%s,%s"';     //0 - project root name, 1 - path to a temp file with containing a list of files
  PACLIENT_PUT   = '--put="%s,%s,%d,%s"'; //0 - local name, 1 - remote path, 2 - operation, 3 - remote name


// Execute the PaClient.exe app and pass it the command and profile
// Filter some of the paclient output by capturing the out and err pipes
function TDeployer.CallPaclient(const aCommand: String): Boolean;
var
  Security           : TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
  BytesInPipe        : Cardinal;
  Buffer             : array[0..2000] of AnsiChar;
  Output             : TStringList;
  StartInfo          : TStartupInfo;
  ProcInfo           : TProcessInformation;
  I                  : Integer;
  ExCode             : Cardinal;
begin
  Result := false;

  // Create a pipe to capture the output
  Security.nLength              := SizeOf(TSecurityAttributes);
  Security.bInheritHandle       := true;
  Security.lpSecurityDescriptor := nil;
  if not CreatePipe(PipeRead, PipeWrite, @Security, 0) then
    RaiseLastOSError;

  try
    ZeroMemory(@StartInfo, SizeOf(StartInfo));
    ZeroMemory(@ProcInfo, SizeOf(ProcInfo));
    StartInfo.cb          := SizeOf(StartInfo);
    StartInfo.hStdOutput  := PipeWrite;
    StartInfo.hStdError   := PipeWrite;
    StartInfo.dwFlags     := STARTF_USESTDHANDLES;  // use output redirect pipe

    if CreateProcess(nil, PChar(fPaclientPath + ' ' + aCommand + ' ' + fRemoteProfile), nil, nil, true,
                          CREATE_NO_WINDOW, nil, nil, StartInfo, ProcInfo) then
      try
        WaitForSingleObject(ProcInfo.hProcess, Infinite);
        // The process has finished, so close the write pipe and read the output
        CloseHandle(PipeWrite);
        ZeroMemory(@Buffer, Length(Buffer));
        ReadFile(PipeRead, Buffer[0], Length(Buffer), BytesInPipe, nil);
        // Parse the output, delete the first 4 lines, that are not very useful, and display the rest indented
        Output := TStringList.Create;
        try
          Output.Text := String(Buffer);
          Output.Delete(0); Output.Delete(0); Output.Delete(0); Output.Delete(0);
          for I := 0 to Output.Count - 1 do
            WriteLn('  ' + Output[I]);
        finally
          Output.Free;
        end;

        // Check the exit code of paclient.exe / 0 is success
        Result := GetExitCodeProcess(ProcInfo.hProcess, ExCode) and (ExCode = 0);
      finally
        CloseHandle(ProcInfo.hProcess);
        CloseHandle(ProcInfo.hThread);
      end;
  finally
    CloseHandle(PipeRead);
    {$IFNDEF  DEBUG}  // The PipeWrite handle is closed twice, which is acceptable in Release, but raises exceptions when running with the debugger
    CloseHandle(PipeWrite);
    {$ENDIF}
  end;
end;

// Check if there is a remote profile assigned, or try to find the default one for the platform
procedure TDeployer.CheckRemoteProfile;
var
  Reg: TRegistry;
begin
  if fRemoteProfile.IsEmpty then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('Software\Embarcadero\BDS\' + fDelphiVersion + '\RemoteProfiles', false) then
        if Reg.ValueExists('Default_' + fPlatform) then
          fRemoteProfile := Reg.ReadString('Default_' + fPlatform);

      if fRemoteProfile.IsEmpty then
        raise Exception.Create('Default remote profile not found. Please specify a profile.');
      Writeln('Using default profile: ' + fRemoteProfile);
    finally
      Reg.Free;
    end;
  end;
end;

// Try to find the last version of Delphi installed to get the Redist folder and Paclient path
procedure TDeployer.GetEmbarcaderoPaths;
var
  Reg: TRegistry;
  Versions: TStringList;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // If no Delphi version is supplied try to get the latest one from the registry
    if fDelphiVersion.IsEmpty then
    begin
      Versions := TStringList.Create;
      try
        if Reg.OpenKey('SOFTWARE\Embarcadero\BDS', false) then
        begin
          Reg.GetKeyNames(Versions);
          Reg.CloseKey;
          if (Versions.Count > 0) then
            fDelphiVersion := Versions[Versions.Count - 1];
        end;
      finally
        Versions.Free;
      end;
    end;

    if Reg.OpenKey('SOFTWARE\Embarcadero\BDS\' + fDelphiVersion, false) then
      if Reg.ValueExists('RootDir') then
        fDelphiPath := Reg.ReadString('RootDir');
    if fDelphiPath.IsEmpty then
      raise Exception.Create('The Delphi install path could not be found.');

    fPaclientPath := IncludeTrailingPathDelimiter(fDelphiPath) + 'bin\paclient.exe'
  finally
    Reg.Free;
  end;
end;

constructor TDeployer.Create(const aDelphiVersion: String);
begin
  inherited Create;
  fDelphiVersion := aDelphiVersion;
  GetEmbarcaderoPaths;
end;

// Produce a ZIP archive of the files to be deployed (useful to produce archives of the OSX .APP bundles on Win)
procedure TDeployer.BundleProject(const aProjectPath, aZIPName: String);
var
  Zip: TZipFile;
  I: Integer;
begin
  ParseProject(aProjectPath);

  WriteLn(Format('Archiving %d files from project %s, config %s', [Length(fDeployFiles), aProjectPath, fConfig]));

  // Create the folders in the zip name, if any
  ForceDirectories(ExtractFilePath(ExpandFileName(aZIPName)));

  Zip := TZipFile.Create;
  try
    if FileExists(aZIPName) then
      TFile.Delete(aZIPName);
    Zip.Open(aZIPName, zmWrite);

    // Add each file to the archive with its path. The \ is replaced with / to make the archive work in OSX
    for I := 0 to Length(fDeployFiles) - 1 do
      Zip.Add(fDeployFiles[I].LocalName, fDeployFiles[I].RemoteDir.Replace('\', '/') + fDeployFiles[I].RemoteName.Replace('\', '/'));
  finally
    Zip.Free;
  end;
end;

// Deploy the project to the remote server
procedure TDeployer.DeployProject(const aProjectPath: String);
var
  S, TempFile: String;
  I: Integer;
begin
  ParseProject(aProjectPath);

  // Check if there is a remote profile and try to find one. Must be after the project is parsed
  CheckRemoteProfile;

  WriteLn(Format('Deploying %d files from project %s, config %s', [Length(fDeployFiles), aProjectPath, fConfig]));

  // Build a temp file list to clean the remote project folder
  TempFile := TPath.GetTempFileName;
  for I := 0 to Length(fDeployFiles) - 1 do
    S := S + fDeployFiles[I].RemoteDir + fDeployFiles[I].RemoteName + sLineBreak;
  TFile.WriteAllText(TempFile, S);
  // Execute the clean command and delete the temp file
  Writeln('Cleaning remote project folder');
  if not CallPaclient(Format(PACLIENT_CLEAN, [fProjectRoot, TempFile])) and not fIgnoreErrors then
    raise Exception.Create('Paclient error. Deployment stopped.');
  TFile.Delete(TempFile);

  // Deploy the files
  for I := 0 to Length(fDeployFiles) - 1 do
  begin
    Writeln('Deploying file: ' + fDeployFiles[I].LocalName);
    if not CallPaclient(Format(PACLIENT_PUT, [fDeployFiles[I].LocalName, fDeployFiles[I].RemoteDir,
                                              fDeployFiles[I].Operation, fDeployFiles[I].RemoteName])) and not fIgnoreErrors then
      raise Exception.Create('Paclient error. Deployment stopped.');
  end;
end;

// Parse the project file to find the list of files to be deployed
procedure TDeployer.ParseProject(const aProjectPath: String);
var
  XmlDoc: IXMLDOMDocument;
  Node  : IXMLDOMNode;
  Nodes : IXMLDOMNodeList;
  I, J, Count : Integer;
begin
  CoInitialize(nil);
  XmlDoc := CoDOMDocument.Create;

  try
    // Set the project name the same as the file
    fProjectName := ChangeFileExt(ExtractFileName(aProjectPath), '');

    // Load the project file
    if not XmlDoc.load(aProjectPath) then
      raise Exception.Create('Project file could not be loaded');

    // Read the default platform if not set with a parameter (OSX32, Win32/64, etc)
    if fPlatform.IsEmpty then
    begin
      Node := XmlDoc.selectSingleNode('/Project/PropertyGroup/Platform/node()');
      if not Assigned(Node) then
        raise Exception.Create('Default platform not found in the project. Please specify a platform.');
      fPlatform := Node.nodeValue;
    end;

    // Read the default config if not set with a parameter (Release or Debug or another)
    if fConfig.IsEmpty then
    begin
      Node := XmlDoc.selectSingleNode('/Project/PropertyGroup/Config/node()');
      if not Assigned(Node) then
        raise Exception.Create('Default build config(Release/Debug/...) not found in the project. Please specify a config.');
      fConfig := Node.nodeValue;
    end;

    // Read the ProjectRoot for building the deploy file names
    if fProjectRoot.IsEmpty then
    begin
      Node := XmlDoc.selectSingleNode('//Deployment/ProjectRoot[@Platform="' + fPlatform + '"]');
      if not Assigned(Node) then
        raise Exception.Create('ProjectRoot not found in the project.');
      fProjectRoot := Node.attributes.getNamedItem('Name').nodeValue;
      fProjectRoot := fProjectRoot.Replace('$(PROJECTNAME)', fProjectName);
    end;

    // Get all the Platform subnodes of the DeployClass nodes for the specified platform
    Nodes := XmlDoc.selectNodes('//DeployClass/Platform[@Name="' + fPlatform + '"]');
    SetLength(fDeployClasses, Nodes.length);
    Count := 0;
    for I := 0 to Nodes.length - 1 do
    begin
      // Get the Name and Required attributes of the DeployClass node (the parent)
      if Nodes.item[I].parentNode.attributes.getNamedItem('Name') <> nil then
        fDeployClasses[Count].Name := Nodes.item[I].parentNode.attributes.getNamedItem('Name').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Required') <> nil then
        fDeployClasses[Count].Required := StrToBool(Nodes.item[I].parentNode.attributes.getNamedItem('Required').nodeValue);

      // Get the RemoteDir, Operation, Extensions subnode values
      if Nodes.item[I].selectSingleNode('RemoteDir/node()') <> nil then
        fDeployClasses[Count].RemoteDir := Nodes.item[I].selectSingleNode('RemoteDir/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('Operation/node()') <> nil then
        fDeployClasses[Count].Operation := Nodes.item[I].selectSingleNode('Operation/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('Extensions/node()') <> nil then
        fDeployClasses[Count].Extensions := Nodes.item[I].selectSingleNode('Extensions/node()').nodeValue;

      // VG 300715: The XE8 sets the ProjectOSXEntitlements RemoteFolder incorrectly to "../" instead of "Contents"
      // Hardcode a quick fix for it and watch for other such problems
      if fDeployClasses[Count].Name.Equals('ProjectOSXEntitlements') and fDeployClasses[Count].RemoteDir.Equals('../') then
        fDeployClasses[Count].RemoteDir := 'Contents';

      Inc(Count);
    end;
    SetLength(fDeployClasses, Count);

    // Get all the Platform subnodes of the DeployFile nodes for the specified platform
    Nodes := XmlDoc.selectNodes('//DeployFile/Platform[@Name="' + fPlatform + '"]');
    SetLength(fDeployFiles, Nodes.length);
    Count := 0;
    for I := 0 to Nodes.length - 1 do
    begin
      // Get the LocalName, Configuration, Class attributes of the DeployFiles node (the parent)
      if Nodes.item[I].parentNode.attributes.getNamedItem('LocalName') <> nil then
        fDeployFiles[Count].LocalName     := Nodes.item[I].parentNode.attributes.getNamedItem('LocalName').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Configuration') <> nil then
        fDeployFiles[Count].Configuration := Nodes.item[I].parentNode.attributes.getNamedItem('Configuration').nodeValue;
      if Nodes.item[I].parentNode.attributes.getNamedItem('Class') <> nil then
        fDeployFiles[Count].ClassName     := Nodes.item[I].parentNode.attributes.getNamedItem('Class').nodeValue;

      // Get the Enabled, RemoteDir, RemoteName subnode values
      if Nodes.item[I].selectSingleNode('Enabled/node()') <> nil then
        fDeployFiles[Count].Enabled := StrToBool(Nodes.item[I].selectSingleNode('Enabled/node()').nodeValue)
      else
        fDeployFiles[Count].Enabled := true;
      if Nodes.item[I].selectSingleNode('RemoteDir/node()') <> nil then
        fDeployFiles[Count].RemoteDir := Nodes.item[I].selectSingleNode('RemoteDir/node()').nodeValue;
      if Nodes.item[I].selectSingleNode('RemoteName/node()') <> nil then
        fDeployFiles[Count].RemoteName := Nodes.item[I].selectSingleNode('RemoteName/node()').nodeValue;

      Inc(Count);
    end;
    SetLength(fDeployFiles, Count);

    // Disable files that are not for the specified Configuration or are duplicated (e.g. both Base and Release configs)
    for I := 0 to Length(fDeployFiles) - 1 do
    begin
      // Check the Base configurations if there is a file with a Release/Debug config and use it, otherwise use the Base
      if (fDeployFiles[I].Configuration = 'Base') or fDeployFiles[I].Configuration.IsEmpty then
      begin
        for J := 0 to Length(fDeployFiles) - 1 do
          if (fDeployFiles[I].LocalName = fDeployFiles[J].LocalName) and (fDeployFiles[J].Configuration = fConfig) then
          begin
            // There is a more detailed config found, so disable the Base
            fDeployFiles[I].Enabled := false;
            Break;
          end;
      end else
        // Check if the file is for a different non-Base config and disable it
        // Second check added because Delphi seems to save incorrect info for some files in the Dproj
        // E.g. the ICNS file with class ProjectOSXResource appears only under a Release config, but not under a
        // Sandbox config. If Sandbox is selected from the IDE and deployed, the Dproj changes and the ICNS is included
        // in Sandbox, but sometimes excluded from Release. No pattern found
        // The only solution is not to disable 'standard' Class files, e.g. non user added
        if (fDeployFiles[I].Configuration <> fConfig) and (fDeployFiles[I].ClassName = 'File') then
          fDeployFiles[I].Enabled := false;
    end;

    // Shrink the array to skip the disabled items
    Count := 0;
    for I := 0 to Length(fDeployFiles) - 1 do
      if fDeployFiles[I].Enabled then
      begin
        fDeployFiles[Count] := fDeployFiles[I];
        Inc(Count);
      end;
    SetLength(fDeployFiles, Count);

    // Match the above DeployFile and DeployClass entries and build the complete Remote/Local names, dirs, etc
    for I := 0 to Length(fDeployFiles) - 1 do
    begin
      // Find if there is the same DeployClass and copy the attributes from it if they don't exist already
      for J := 0 to Length(fDeployClasses) - 1 do
      begin
        if fDeployFiles[I].ClassName = fDeployClasses[J].Name then
        begin
          if fDeployFiles[I].RemoteDir.IsEmpty then
            fDeployFiles[I].RemoteDir := fDeployClasses[J].RemoteDir;
          fDeployFiles[I].Operation := fDeployClasses[J].Operation;
          Break;
        end;
      end;

      // Replace the BDS paths for the Redist files from Embarcadero
      fDeployFiles[I].LocalName := fDeployFiles[I].LocalName.Replace('$(BDS)\', fDelphiPath);

      // Finalize the Remote names and dirs
      fDeployFiles[I].RemoteDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(fProjectRoot) + fDeployFiles[I].RemoteDir);
      if fDeployFiles[I].RemoteName.IsEmpty then
        fDeployFiles[I].RemoteName := ExtractFileName(fDeployFiles[I].LocalName);
    end;

  finally
    Node   := nil;
    Nodes  := nil;
    XmlDoc := nil;
  end;
end;

// Execute a custom command on the remote server
// This is done by creating a temporary file with the command and executing it in the shell
procedure TDeployer.ExecuteCommand(const aProjectPath, aCommand: String);
var
  Text, Cmd, TempFile:  String;
begin
  ParseProject(aProjectPath);

  // Check if there is a remote profile and try to find one. Must be after the project is parsed
  CheckRemoteProfile;

  // Replace any custom parameters in the command
  Cmd := aCommand.Replace('$PROOT', fProjectRoot, [rfReplaceAll]);

  // Build a temp file with the command as content
  TempFile := TPath.GetTempFileName;

  // Build different files for Windows or OSX
  if (fPlatform = 'Win32') or (fPlatform = 'Win64') then
    Text := Cmd
  else
    Text := '#!/bin/bash' + #10 + Cmd;
  TFile.WriteAllText(TempFile, Text);

  // Deploy the file and execute it with the 5 operation
  // The file is deployed to the root deployment folder (the one above the project root folder) and executed there
  Writeln('Executing custom command: ' + aCommand);
  if not CallPaclient(Format(PACLIENT_PUT, [TempFile, '.', 5, ''])) and not fIgnoreErrors then
    raise Exception.Create('Command error.');
  TFile.Delete(TempFile);
end;

end.
