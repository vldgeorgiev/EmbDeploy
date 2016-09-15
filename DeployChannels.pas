unit DeployChannels;

interface

type
  IDeployChannel = interface
    ['{0AC82C72-6867-45DA-A37F-901C471D1A47}']
    procedure SetupChannel;
    procedure CleanChannel;
    procedure DeployProjectToChannel;
    procedure CloseChannel;
  end;

  TPAClientChannel = class(TInterfacedObject, IDeployChannel)
  private
    fRemoteProfile,
    fPAClientPath: string;
  public
    constructor Create (const newRemoteProfile: string; const newPAClientPAth: string);
    procedure SetupChannel;
    procedure CleanChannel;
    procedure DeployProjectToChannel;
    procedure CloseChannel;
  end;

  TFolderChannel = class(TInterfacedObject, IDeployChannel)
  private
    fFolder: string;
  public
    constructor Create (const newFolder: string);
    procedure SetupChannel;
    procedure CleanChannel;
    procedure DeployProjectToChannel;
    procedure CloseChannel;
  end;


implementation

{ TPAClientChannel }

procedure TPAClientChannel.CleanChannel;
begin

end;

procedure TPAClientChannel.CloseChannel;
begin

end;

constructor TPAClientChannel.Create(const newRemoteProfile,
  newPAClientPAth: string);
begin
  inherited Create;
  fRemoteProfile:=newRemoteProfile;
  fPAClientPath:=newPAClientPAth;
end;

procedure TPAClientChannel.DeployProjectToChannel;
begin

end;

procedure TPAClientChannel.SetupChannel;
begin

end;

{ TFolderChannel }

procedure TFolderChannel.CleanChannel;
begin

end;

procedure TFolderChannel.CloseChannel;
begin

end;

constructor TFolderChannel.Create(const newFolder: string);
begin
  inherited Create;
  fFolder:=newFolder;
end;

procedure TFolderChannel.DeployProjectToChannel;
begin

end;

procedure TFolderChannel.SetupChannel;
begin

end;

end.
