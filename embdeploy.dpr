{
Automated deployer for Embarcadero RAD Studio projects
Created by Vladimir Georgiev, 2013

MIT License (MIT)
}

program embdeploy;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Deployer in 'Deployer.pas';

const
  VERSION = '1.3';

var
  Deployer: TDeployer;
  Project, Param, DelphiVer: String;

// Display the parameters usage information
procedure ShowUsage;
  procedure ShowParam(aParam, aText: String);
  begin
    Writeln(Format('  %-16s %s', [aParam, aText]));
  end;
begin
  WriteLn('');
  Writeln('Usage: embdeploy [-delphiver "ver"] -deploy|(-cmd "command")|(-bundle "zip") [-platform|-profile|-config|-proot "name"] [-ignore] ProjectName');
  WriteLn('');
  ShowParam('ProjectName', 'Name (relative or absolute) of the project file (.dproj)');
  ShowParam('-delphiver "ver"',  'Delphi version to use the paclient from. It is the number from the HKCU/Software/Emb...');
  ShowParam('-deploy', 'Deploy the project to the remote profile');
  ShowParam('-platform "name"', 'Platform to deploy (Win32, OSX32, iOSDevice, etc). If not specified the default one from ' +
                                'the project is used');
  ShowParam('-profile "name"',  'Name of the remote profile to use. If not specified the default one for the platform is used');
  ShowParam('-config "name"',   'Release or Debug configuration. If not specified the default one from the project file is used');
  ShowParam('-proot "name"',    'Remote project root folder. If not specified a default one is generated from the project name');
  ShowParam('-cmd "command"', 'Execute an arbitrary command line on the remote server. The command is anything that ' +
                              'can be executed from a terminal or command line prompt. It is executed from ' +
                              'above the remote project folder. The command can contain the $PROOT parameter, which is ' +
                              'replaced with the project root folder, e.g. $PROOT/Contents/... becomes myproject.app/Contents/...');
  ShowParam('-ignore', 'Ignore errors reported by paclient.exe and continue deploying');
  ShowParam('-bundle "zipname"', 'Produce a ZIP archive of the files to be deployed. Useful for making a ZIP of an OSX project APP bundle');
end;

// Check if the valid combination of parameters is passed
function ValidateParams: Boolean;
begin
  Project := ParamStr(ParamCount);
  if not FileExists(Project) then
    raise Exception.Create('Project "' + Project +'" not found');

  Result := FindCmdLineSwitch('deploy') or FindCmdLineSwitch('cmd') or FindCmdLineSwitch('bundle');
end;

// Main application body
begin
  try
    ExitCode := 1; // Default to error and change to success later

    Writeln('Automated deployer for Embarcadero RAD Studio projects - Version ' + VERSION);
    Writeln('Written by Vladimir Georgiev, 2013');

    if FindCmdLineSwitch('?') or not ValidateParams then
    begin
      ShowUsage;
      Exit;
    end;

    if FindCmdLineSwitch('delphiver', Param) then
      DelphiVer := Param;
    Deployer := TDeployer.Create(DelphiVer);
    try
      if FindCmdLineSwitch('platform', Param) then
        Deployer.Platform := Param;
      if FindCmdLineSwitch('profile', Param) then
        Deployer.RemoteProfile := Param;
      if FindCmdLineSwitch('config', Param) then
        Deployer.Config := Param;
      if FindCmdLineSwitch('proot', Param) then
        Deployer.ProjectRoot := Param;
      Deployer.IgnoreErrors := FindCmdLineSwitch('ignore');

      // Deploy the project
      if FindCmdLineSwitch('deploy') then
      begin
        Deployer.DeployProject(Project);
        Writeln('Deployment complete');
      end;

      // Execute a custom remote command
      if FindCmdLineSwitch('cmd', Param) then
      begin
        Deployer.ExecuteCommand(Project, Param);
        Writeln('Command executed');
      end;

      // Make a ZIP bundle of the project deployment files
      if FindCmdLineSwitch('bundle', Param) then
      begin
        Deployer.BundleProject(Project, Param);
        Writeln('ZIP bundle complete');
      end;

      ExitCode := 0; // Success
    finally
      Deployer.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error deploying project:');
      Writeln(E.Message);
    end;
  end;
end.
