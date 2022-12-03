unit uBackEnd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, um3uloader, epg, MPV_Engine, OpenGLContext;

type

  { TBackend }

  TBackend = class
  public
    List: TM3ULoader;
    EpgData: TEpg;
    MpvEngine: TMPVEngine;
  public
    function InitializeEngine(Renderer: TOpenGLControl): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

function BackEnd: TBackend;

implementation

var
  fBackend: TBackend;

function BackEnd: TBackend;
begin
  if not Assigned(fBackend) then
    fBackend := TBackend.Create;
  Result := fBackend;
end;

{ TBackend }

function TBackend.InitializeEngine(Renderer: TOpenGLControl): boolean;
begin
  mpvengine := TMPVEngine.Create;
  Result := MpvEngine.Initialize(Renderer);
end;

constructor TBackend.Create;
begin
  List := TM3ULoader.Create;
  EpgData := TEpg.Create;

end;

destructor TBackend.Destroy;
begin
  List.Free;
  EpgData.Free;
  MpvEngine.Free;
  inherited Destroy;
end;


initialization
  fBackend := nil;

finalization
  fBackend.Free;
end.
