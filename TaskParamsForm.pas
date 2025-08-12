unit TaskParamsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit, Vcl.FileCtrl, AccessToDLLIntf,
  System.Actions, Vcl.ActnList;

type
  TFormTaskParams = class(TForm)
    ValueListEditor: TValueListEditor;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ActionList1: TActionList;
    actOk: TAction;
    actCancel: TAction;
    OpenDialog1: TOpenDialog;
    procedure ValueListEditorEditButtonClick(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  private
    { Private declarations }
    Parameters: TStrings;
  public
    { Public declarations }
    function Show(aParameters: TStrings): boolean;
  end;

var
  FormTaskParams: TFormTaskParams;

implementation

{$R *.dfm}

function TFormTaskParams.Show(aParameters: TStrings): boolean;
var
  i: Integer;
  LRect: TRect;
  X, Y: Integer;
begin
  Parameters := aParameters;
  Left := Application.MainForm.Left + (Application.MainForm.Width - Width) div 2;
  Top := Application.MainForm.Top + (Application.MainForm.Height - Height) div 2;

  ValueListEditor.Strings.Clear;

  for i := 0 to Parameters.Count - 1 do
  begin
    ValueListEditor.Strings.Add(Parameters[i]);
    with ValueListEditor.ItemProps[Parameters.Names[i]] do
    begin
      if TParamType(Parameters.Objects[i]) in [ptFolder, ptFile] then
        begin
          EditStyle := esEllipsis;
          ReadOnly := True;
        end else
        begin
          EditStyle := esSimple;
          ReadOnly := False;
        end;
    end;
  end;

  Result := FormTaskParams.ShowModal = mrOk;

  if Result then
  for i := 0 to Parameters.Count - 1 do
     Parameters.Values[Parameters.KeyNames[i]] := ValueListEditor.Strings.Values[Parameters.KeyNames[i]];
end;



procedure TFormTaskParams.actCancelExecute(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormTaskParams.actOkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;


procedure TFormTaskParams.ValueListEditorEditButtonClick(Sender: TObject);
var
  Path: string;
  Row: Integer;
  Key: string;
  ParamIndex: Integer;
begin
  with ValueListEditor do Key := Keys[Row];
  ParamIndex := Parameters.IndexOfName(Key);
  if TParamType(TParamType(Parameters.Objects[ParamIndex])) = ptFolder then
  begin
    if SelectDirectory('Выберите папку для поиска файлов:', '', Path, [sdNewUI], Self) then
      ValueListEditor.Values[Key] := Path;
  end else
  begin
    OpenDialog1.Filter := 'Pascal files (*.pas)|*.pas|Rich text format files (*.rtf)|*.rtf|(All files *.*)|*.*';
    if OpenDialog1.Execute then
      ValueListEditor.Values[Key] := OpenDialog1.FileName;
  end;
end;

end.
