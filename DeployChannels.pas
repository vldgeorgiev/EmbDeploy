unit DeployChannels;

interface

type
  IDeployChannelBasic = interface
    ['{E9BA7BAA-8CAF-49F4-AEBD-C622C455458C}']
    procedure SetVerbose(const newVerbose: Boolean);
    function GetVerbose: boolean;
    procedure SetFileListName (const newFileList: string);
    function GetFileListName: string;
    procedure SetProjectRoot (const newProjectRoot: string);
    function GetProjectRoot: string;
    procedure SetChannelName (const newChannelName: string);
    function GetChannelName: string;

    property Verbose: boolean read GetVerbose write SetVerbose;
    property FileListName: string read GetFileListName write SetFileListName;
    property ProjectRoot: string read GetProjectRoot write SetProjectRoot;
    property ChannelName: string read GetChannelName write SetChannelName;
  end;

  IDeployChannel = interface (IDeployChannelBasic)
    ['{0AC82C72-6867-45DA-A37F-901C471D1A47}']
    procedure SetupChannel;
    function CleanChannel: boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    procedure CloseChannel;
  end;

  TDeployBaseChannel = class (TInterfacedObject, IDeployChannelBasic)
  private
    fVerbose: Boolean;
    fFileListName,
    fProjectRoot: string;
    fChannelName: string;
  public
    procedure SetVerbose(const newVerbose: Boolean);
    function GetVerbose: boolean;
    procedure SetFileListName (const newFileList: string);
    function GetFileListName: string;
    procedure SetProjectRoot (const newProjectRoot: string);
    function GetProjectRoot: string;
    procedure SetChannelName (const newChannelName: string);
    function GetChannelName: string;
  end;

  TPAClientChannel = class(TDeployBaseChannel, IDeployChannel)
  private
    fRemoteProfile,
    fPAClientPath,
    fDelphiVersion,
    fPlatfrom: string;
    function CallPAClient(const aCommand: string): Boolean;
  public
    constructor Create (const newRemoteProfile: string; const newPAClientPAth: string;
                        const newDelphiVersion: string; const newPlatform: string);
    procedure SetupChannel;
    function CleanChannel:boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    procedure CloseChannel;
  end;

  TFolderChannel = class(TDeployBaseChannel, IDeployChannel)
  private
    fFolder: string;
    fProjectName: string;
    fBaseProjectName: string;
  public
    constructor Create (const newFolder: string; const ProjectName: string;
                          const BaseProjectName: string);
    procedure SetupChannel;
    function CleanChannel:boolean;
    function DeployFile(const localName: string; const remoteDir: string;
                        const operation: Integer; const remoteName: string):boolean;
    procedure CloseChannel;
  end;

implementation

uses
	System.SysUtils, System.Win.Registry, Winapi.Windows, System.Classes, System.IOUtils;

const
  // Paclient commands and the parameters to be substituted
  PACLIENT_CLEAN = '--Clean="%s,%s"';     //0 - project root name, 1 - path to a temp file with containing a list of files
  PACLIENT_PUT   = '--put="%s,%s,%d,%s"'; //0 - local name, 1 - remote path, 2 - operation, 3 - remote name

{ TPAClientChannel }

// Execute the PaClient.exe app and pass it the command and profile
// Filter some of the paclient output by capturing the out and err pipes
function TPAClientChannel.CallPAClient(const aCommand: string): Boolean;
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
  fullProcessPath    : string;
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

    fullProcessPath:='"'+fPaclientPath+'"' + ' ' + aCommand + ' "' + fRemoteProfile+'"';
    if fVerbose then
      Writeln('Full Command Line: '+fullProcessPath);

    if CreateProcess(nil, PChar(fullProcessPath), nil, nil, true,
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
          Output.Text:=String(Buffer);
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
    {$IFNDEF  DEBUG}  // The PipeWrite handle is closed twice,
                      //which is acceptable in Release,
                      //but raises exceptions when running with the debugger
    CloseHandle(PipeWrite);
    {$ENDIF}
  end;
end;

function TPAClientChannel.CleanChannel: boolean;
begin
  result:=CallPaclient(Format(PACLIENT_CLEAN, [fProjectRoot, fFileListName]));
end;

procedure TPAClientChannel.CloseChannel;
begin

end;

constructor TPAClientChannel.Create(const newRemoteProfile, newPAClientPAth,
  newDelphiVersion, newPlatform: string);
begin
  inherited Create;
  fRemoteProfile:=newRemoteProfile;
  fPAClientPath:=newPAClientPAth;
  fDelphiVersion:=newDelphiVersion;
  fPlatfrom:=newPlatform;
end;

function TPAClientChannel.DeployFile(const localName, remoteDir: string;
  const operation: Integer; const remoteName: string): boolean;
begin
  result:=CallPaclient(Format(PACLIENT_PUT, [localName, remoteDir,
                              operation, remoteName]));
end;


// Check if there is a remote profile and try to find one. Must be after the project is parsed
procedure TPAClientChannel.SetupChannel;
var
  Reg: TRegistry;
  regKey: string;
begin
  if fRemoteProfile.IsEmpty then
  begin
    Reg:=TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      regKey:='Software\Embarcadero\BDS\' + fDelphiVersion + '\RemoteProfiles';
      if fVerbose then
        Writeln('Looking at Registry Key: '+regKey);
      if Reg.OpenKey(regKey, false) then
        if Reg.ValueExists('Default_'+fPlatfrom) then
          fRemoteProfile := Reg.ReadString('Default_' + fPlatfrom);
        if fRemoteProfile.IsEmpty then
          raise Exception.Create('Default remote profile not found. Please specify a profile');
    finally
      Reg.Free;
    end;
  end;

  Writeln('Using default profile: ' + fRemoteProfile);
end;

{ TFolderChannel }

function TFolderChannel.CleanChannel:boolean;
begin
  Result:=true;
end;

procedure TFolderChannel.CloseChannel;
begin

end;

constructor TFolderChannel.Create(const newFolder: string; const ProjectName: string;
                                      const BaseProjectName: string);
begin
  inherited Create;
  fFolder:=newFolder;
  fProjectName:=ProjectName;
  fBaseProjectName:=BaseProjectName;
end;

function TFolderChannel.DeployFile(const localName, remoteDir: string;
  const operation: Integer; const remoteName: string): boolean;
var
  Source,
  Target: TFileStream;
  targetDir,
  targetPath: string;
  fileAttributes: TFileAttributes;
begin
  result:=True;
  Source:= TFileStream.Create(localName, fmOpenRead);
 try
   if not DirectoryExists(remoteDir) then
   begin
    targetDir:=TPath.Combine(fFolder,remoteDir);
    TDirectory.CreateDirectory(targetDir);
    fileAttributes:=TDirectory.GetAttributes(targetDir);
    Exclude(fileAttributes, TFileAttribute.faReadOnly);
    TDirectory.SetAttributes(targetDir,fileAttributes);
   end;

   targetPath:=TPath.Combine(
                TPath.Combine(fFolder, remoteDir), remoteName);
   Target := TFileStream.Create(targetPath, fmOpenWrite or fmCreate );
   try
     Target.CopyFrom(Source, Source.Size ) ;
   finally
     Target.Free;
   end;

   fileAttributes:=TFile.GetAttributes(targetPath);
   Exclude(fileAttributes, TFileAttribute.faReadOnly);
   TFile.SetAttributes(targetPath, fileAttributes);

   if remoteName=fBaseProjectName then
   begin
     Include(fileAttributes, TFileAttribute.faArchive);
     TFile.SetAttributes(targetPath, fileAttributes);
   end;

 finally
   Source.Free;
 end;

end;

procedure TFolderChannel.SetupChannel;
  procedure DeleteDirectory(const Name: string);
  var
    F: TSearchRec;
    str: string;
  begin
    if FindFirst(Name + '\*', faAnyFile, F) = 0 then begin
      try
        repeat
          if (F.Attr and faDirectory <> 0) then
          begin
            if (F.Name <> '.') and (F.Name <> '..') then
            begin
              DeleteDirectory(Name + '\' + F.Name);
            end;
          end
          else
          begin
            str:=Name + '\' + F.Name;
            DeleteFile(PWideChar(str));
          end;
        until FindNext(F) <> 0;
      finally
        FindClose(F.FindHandle);
      end;
      RemoveDir(Name);
    end;
  end;

begin
  DeleteDirectory(TPath.Combine(fFolder, fProjectName));
end;

{ TDeployBaseChannel }

function TDeployBaseChannel.GetChannelName: string;
begin
  result:=fChannelName;
end;

function TDeployBaseChannel.GetFileListName: string;
begin
  result:=fFileListName;
end;

function TDeployBaseChannel.GetProjectRoot: string;
begin
  result:=fProjectRoot;
end;

function TDeployBaseChannel.GetVerbose: boolean;
begin
  result:=fVerbose;
end;

procedure TDeployBaseChannel.SetChannelName(const newChannelName: string);
begin
  fChannelName:=newChannelName;
end;

procedure TDeployBaseChannel.SetFileListName(const newFileList: string);
begin
  fFileListName:=newFileList;
end;

procedure TDeployBaseChannel.SetProjectRoot(const newProjectRoot: string);
begin
  fProjectRoot:=newProjectRoot;
end;

procedure TDeployBaseChannel.SetVerbose(const newVerbose: Boolean);
begin
  fVerbose:=newVerbose;
end;

end.
