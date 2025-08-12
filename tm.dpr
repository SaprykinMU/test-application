program tm;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  AccessToDLLIntf in 'AccessToDLLIntf.pas',
  TaskParamsForm in 'TaskParamsForm.pas' {FormTaskParams};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTaskParams, FormTaskParams);
  Application.Run;
end.
