unit uSplash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfSplash }

  TfSplash = class(TForm)
    Logo: TImage;
    Panel1: TPanel;
    stName: TStaticText;
  private

  public

  end;

var
  fSplash: TfSplash;

implementation

{$R *.lfm}

end.

