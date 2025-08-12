unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, AccessToDLLIntf,
  Vcl.Menus, Vcl.ToolWin, Vcl.CheckLst, ListViewEx, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList, ShellAPI,
  System.Generics.Collections;

type

  TForListItem = reference to procedure(aListItem: TListItem);
  TTaskStatuses = set of TTaskStatus;

  TTaskUI = class(TObject)
    FTask: ITask;
    FProgressBar: TProgressBar;
    FListView: TListViewEx;
    FResults: TStrings;
    FLog: TStrings;
    FProgress: integer;
    FStatus: TTaskStatus;
    FCommand: TTaskCommand;
    FListItem: TListItem;
    FCurrentFile: string;
  private
    procedure ReadDataFromTask;
    procedure WriteCommandToTask;
    procedure AdjustProgressBar;
  public
    constructor Create(aTaskProvider: ITaskProvider; aListView: TListViewEx);
    destructor Destroy; override;
    procedure Show;
    property Task: ITask read FTask;
    property ProgressBar: TProgressBar read FProgressBar;
    property Results: TStrings read FResults;
    property Status: TTaskStatus read FStatus write FStatus;
    property Progress: integer read FProgress;
    property Command: TTaskCommand read FCommand write FCommand;
    property Log: TStrings read FLog;
    property CurrentFile: string read FCurrentFile write FCurrentFile;
  end;

  TFormMain = class(TForm)
    ToolBarMain: TToolBar;
    ToolButtonStart: TToolButton;
    ToolButtonPause: TToolButton;
    PopupMenuAvailableTasks: TPopupMenu;
    StatusBar1: TStatusBar;
    ImageListMain: TImageList;
    ActionListMain: TActionList;
    actPause: TAction;
    actStop: TAction;
    actStart: TAction;
    actNew: TAction;
    ToolButtonNew: TToolButton;
    ToolButtonStop: TToolButton;
    ListBoxResult: TListBox;
    Splitter1: TSplitter;
    TimerListViewTasksChange: TTimer;
    PageControlBottom: TPageControl;
    TabSheetResult: TTabSheet;
    TabSheetLog: TTabSheet;
    ListBoxLog: TListBox;
    actDelete: TAction;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxResultDblClick(Sender: TObject);
    procedure ListViewTasksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure TimerListViewTasksChangeTimer(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actStartUpdate(Sender: TObject);
    procedure actPauseUpdate(Sender: TObject);
    procedure actStopUpdate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
  private
    { Private declarations }
    FTaskProviders: TInterfaceList;
    FTasks: TList<TTaskUI>;
    ListViewTasks: TListViewEx;
    FSelectedItem: TListItem;
    procedure LoadDLLs;
    procedure ExecuteTask(aTaskUI: TTaskUI);
    procedure MenuItemNewTaskClick(Sender: TObject);
    procedure ListViewEx1EndColumnResize(Sender: TCustomListView; aColumnIndex, aColumnWidth: integer);
    procedure OnSynchronize(aTaskUI: pointer);
    procedure CopyResults(aTaskUI: pointer);
    procedure UpdateAction_(aAction: TObject; aStatuses: TTaskStatuses);
    procedure ExecuteAction_(aAction: TObject; aCommand: TTaskCommand);
    procedure Start(aTaskUI: TTaskUI; aStartNew: Boolean);
    procedure CopyLog(aTaskUI: pointer);
    procedure GetDataFromTask(aTaskUI: pointer; aListBox: TListBox;  aSrc, aDest: TStrings);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Types, IOUtils, TaskParamsForm;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var
  TaskProvider: ITaskProvider;
  NewMenuItem: TMenuItem;
  LC: TListColumn;
  s: string;
begin
  ListViewTasks := TListViewEx.Create(Self);
  ListViewTasks.Parent := Self;
  ListViewTasks.ViewStyle := vsReport;
  ListViewTasks.ReadOnly := true;
//  ListViewTasks.Show
 // ShowScrollBar(ListViewTasks.Handle, SB_HORZ, True);
  LC := ListViewTasks.Columns.Add;
  LC.Caption := 'Наименование';
  LC.Width := 300;
  LC := ListViewTasks.Columns.Add;
  LC.Caption := 'Выполнено';
  LC.Width := 100;

  LC := ListViewTasks.Columns.Add;
  LC.Caption := 'Статус';
  LC.Width := 200;

  ListViewTasks.OnEndColumnResize := ListViewEx1EndColumnResize;
  ListViewTasks.OnChange := ListViewTasksChange;
  ListViewTasks.Top := 100;
  ListViewTasks.Align := alTop;
  Splitter1.Align := alTop;

  PageControlBottom.Align := alClient;

  FTasks := TList<TTaskUI>.Create;

  FTaskProviders := TInterfaceList.Create;

  LoadDLLs;

  for var i := 0 to FTaskProviders.Count - 1 do
  begin
    TaskProvider := ITaskProvider(FTaskProviders[i]);
    s := TaskProvider.GetName;

    NewMenuItem := NewItem(s, 0, false, true, MenuItemNewTaskClick, 0, 'MenuItem' + IntToStr(i));
    NewMenuItem.Tag := i;
    PopupMenuAvailableTasks.Items.Add(NewMenuItem);
  end;

  PageControlBottom.ActivePage := TabSheetResult;

  Application.OnIdle := ApplicationIdle;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  for var i := FTasks.Count - 1 downto 0 do
    FTasks[i].Free;

  FTasks.Free;
  FTaskProviders.Free;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  UncompletedTaskExists: Boolean;
  AttemptCounter: integer;
begin
  // Если существуют работающие задачи, пытаемся их зевершить командой tcCancel.
  //  Если в течение 10 секунд какие-то задачи остались не законченными, их наличие игнорируется, а приложение закрывается.

  AttemptCounter := 0;
  repeat
    UncompletedTaskExists := false;
    for var i := 0 to FTasks.Count - 1 do
    begin
      if FTasks[i].Status in [tsWaiting, tsRunning] then
      begin
        FTasks[i].Command := tcCancel;
        UncompletedTaskExists := true;
      end;
      Inc(AttemptCounter);
      sleep(100);
      Application.ProcessMessages;
    end;
  until (not UncompletedTaskExists) or (AttemptCounter > 100);
end;

procedure TFormMain.ListBoxResultDblClick(Sender: TObject);
var
  Path: string;
begin
  with TListBox(Sender) do
    Path := Items[ItemIndex];
  if Fileexists(Path) then
    // ShellExecute(0, 'open', PChar(Path), nil, PChar(ExtractFilePath(Path)), SW_SHOWNORMAL);
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select, ' + ExtractFileName(Path)), PChar(ExtractFilePath(Path)),
      SW_SHOWNORMAL);
end;

procedure TFormMain.ListViewTasksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  FSelectedItem := Item;
  TimerListViewTasksChange.Enabled := true;
end;

procedure TFormMain.TimerListViewTasksChangeTimer(Sender: TObject);
var
  SaveCursor: TCursor;
  ListItem: TListItem;
begin
  // Для того чтобы при переходе на другую строку ListViewTasks, данные на вкладках Результаты и Лог загружались не сразу,
  // а с небольшой задержкой, равной TimerListViewTasksChange.Interval. Нужно чтобы уменьшить время реакции на действия пользователя.

  TimerListViewTasksChange.Enabled := false;
  ListItem := ListViewTasks.Selected;

  if Assigned(ListItem) and Assigned(TTaskUI(ListItem.Data).Results) and Assigned(TTaskUI(ListItem.Data).Log) then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    ListBoxResult.Items.BeginUpdate;
    ListBoxResult.Items.Assign(TTaskUI(ListItem.Data).Results);
    ListBoxResult.Items.EndUpdate;

    ListBoxLog.Items.BeginUpdate;
    ListBoxLog.Items.Assign(TTaskUI(ListItem.Data).Log);
    ListBoxLog.Items.EndUpdate;

    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormMain.actDeleteExecute(Sender: TObject);
var
  ListItem: TListItem;
  i: Integer;
begin
  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
  begin

    if Application.MessageBox(PChar('Вы действительно хотите удалить задачу "' + ListItem.Caption + '" ?'),
       'Подтвердите', MB_ICONQUESTION or MB_YESNO) <> IDYES then exit;

    i := ListViewTasks.Items.IndexOf(ListItem);
    if i >= 0 then ListViewTasks.Items.Delete(i);

    i := FTasks.IndexOf(ListItem.Data);
    if i >= 0 then FTasks.Delete(i);

    TTaskUI(ListItem.Data).Free;

    ListBoxResult.Clear;
    ListBoxLog.Clear;

    for i := 0 to FTasks.Count - 1 do FTasks[i].AdjustProgressBar;
  end;

end;

procedure TFormMain.actDeleteUpdate(Sender: TObject);
begin
  UpdateAction_(Sender, [tsCompleted, tsFailed, tsCancelled]);
end;

procedure TFormMain.actNewExecute(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  PopupMenuAvailableTasks.Popup(P.X, P.Y);
end;

procedure TFormMain.actStartUpdate(Sender: TObject);
begin
  UpdateAction_(Sender, [tsWaiting, tsCompleted, tsFailed, tsCancelled]);
end;

procedure TFormMain.actPauseUpdate(Sender: TObject);
begin
  UpdateAction_(Sender, [tsRunning]);
end;

procedure TFormMain.actStopUpdate(Sender: TObject);
begin
  UpdateAction_(Sender, [tsRunning, tsWaiting]);
end;

procedure TFormMain.actStopExecute(Sender: TObject);
begin
  ExecuteAction_(Sender, tcCancel);
end;

procedure TFormMain.actPauseExecute(Sender: TObject);
begin
  ExecuteAction_(Sender, tcPause);
end;

procedure TFormMain.actStartExecute(Sender: TObject);
var
  ListItem: TListItem;
  TaskUI: TTaskUI;
begin
  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
  begin
    TaskUI := ListItem.Data;
    if TaskUI.Status in [tsCompleted, tsCancelled] then
      Start(TaskUI, false)
    else if TaskUI.Status in [tsWaiting] then
      TaskUI.Command := tcResume;
  end;
end;

procedure TFormMain.LoadDLLs;
var
  DLLFiles: TStringDynArray;
  DLLHandle: THandle;
  RegisterProc: TRegisterTaskProvider;
  i: integer;
begin
  // Получаем список всех DLL в папке приложения
  DLLFiles := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)), '*.dll');
  for i := 0 to High(DLLFiles) do
  begin
    // Загружаем DLL
    DLLHandle := LoadLibrary(PChar(DLLFiles[i]));
    if DLLHandle <> 0 then
    begin
      // Ищем функцию RegisterTaskProvider
      @RegisterProc := GetProcAddress(DLLHandle, 'RegisterTaskProvider');
      if Assigned(RegisterProc) then
      begin
        RegisterProc(FTaskProviders);
      end
      else
      begin
        // Если функция не найдена - выгружаем DLL
        FreeLibrary(DLLHandle);
      end;
    end;
  end;
end;

procedure TFormMain.ExecuteTask(aTaskUI: TTaskUI);
var
  Thread: TThread;
  ListItem: TListItem;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        aTaskUI.Command := tcNone;
        aTaskUI.Task.Execute;
        TThread.Synchronize(nil,
          procedure
          begin
            CopyResults(aTaskUI);
            CopyLog(aTaskUI);
            aTaskUI.ReadDataFromTask;
          end);

      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              aTaskUI.Task.Status := tsFailed;
              aTaskUI.Task.Log.Add('Ошибка: ' + E.Message);
              CopyResults(aTaskUI);
              CopyLog(aTaskUI);
              aTaskUI.ReadDataFromTask;
            end);
        end;
      end;
    end);
  aTaskUI.Results.Clear;
  aTaskUI.Task.Results.Clear;
  aTaskUI.Log.Clear;
  aTaskUI.Task.Log.Clear;

  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) and (ListItem.Data = aTaskUI) then
  begin
    ListBoxResult.Items.Clear;
    ListBoxLog.Items.Clear;
  end;

  Thread.Start;
end;

procedure TFormMain.MenuItemNewTaskClick(Sender: TObject);
var
  i: integer;
  TaskUI: TTaskUI;
begin
  i := TMenuItem(Sender).Tag;
  if (i >= 0) and (i < FTaskProviders.Count) then
  begin
    TaskUI := TTaskUI.Create(ITaskProvider(FTaskProviders.Items[i]), ListViewTasks);
    Start(TaskUI, true);
  end;
end;

procedure TFormMain.ListViewEx1EndColumnResize(Sender: TCustomListView; aColumnIndex, aColumnWidth: integer);
var
  ListView: TListViewEx;
begin
  ListView := TListViewEx(Sender);

  for var i := 0 to ListView.Items.Count - 1 do
  begin
    if Assigned(ListView.Items[i].Data) then
      with TTaskUI(ListView.Items[i].Data).ProgressBar do
        if aColumnIndex = 0 then
          Left := aColumnWidth
        else if aColumnIndex = 1 then
          Width := aColumnWidth;
  end;
end;

procedure TFormMain.OnSynchronize(aTaskUI: pointer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      CopyResults(aTaskUI);
      CopyLog(aTaskUI);
      TTaskUI(aTaskUI).WriteCommandToTask;
      TTaskUI(aTaskUI).ReadDataFromTask;
    end);
end;

procedure TFormMain.CopyResults(aTaskUI: pointer);
begin
  GetDataFromTask(aTaskUI, ListBoxResult,  TTaskUI(aTaskUI).Task.Results, TTaskUI(aTaskUI).Results);
end;

procedure TFormMain.UpdateAction_(aAction: TObject; aStatuses: TTaskStatuses);
var
  ListItem: TListItem;
begin
  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
    TAction(aAction).Enabled := TTaskUI(ListItem.Data).Status in aStatuses
  else
    TAction(aAction).Enabled := false;
end;

procedure TFormMain.ExecuteAction_(aAction: TObject; aCommand: TTaskCommand);
var
  ListItem: TListItem;
begin
  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
    TTaskUI(ListItem.Data).Command := aCommand;
end;

procedure TFormMain.CopyLog(aTaskUI: pointer);
begin
  GetDataFromTask(aTaskUI, ListBoxLog,  TTaskUI(aTaskUI).Task.Log, TTaskUI(aTaskUI).Log);
end;

procedure TFormMain.GetDataFromTask(aTaskUI: pointer; aListBox: TListBox;  aSrc, aDest: TStrings);
var
  ListItem: TListItem;
begin
  for var i := aDest.Count to aSrc.Count - 1 do
    aDest.Add(aSrc[i]);

  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) and (ListItem.Data = aTaskUI) then
  begin
    aListBox.Items.BeginUpdate;

    for var i := aListBox.Items.Count to aSrc.Count - 1 do
      aListBox.Items.Add(aSrc[i]);

    aListBox.Items.EndUpdate;
  end;
end;

procedure TFormMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  ListItem: TListItem;
begin
  StatusBar1.Panels[0].Text := 'Получено строк - ' + IntToStr(ListBoxResult.Count);

  if not Assigned(ListViewTasks.Selected) then
    ListViewTasks.Selected := ListViewTasks.ItemFocused;

  ListItem := ListViewTasks.Selected;
  if Assigned(ListItem) and Assigned(ListItem.Data) then
  begin
    StatusBar1.Panels[1].Text := TTaskUI(ListItem.Data).Task.CurrentFile;
  end;
end;

procedure TFormMain.Start(aTaskUI: TTaskUI; aStartNew: Boolean);
begin
  if FormTaskParams.Show(aTaskUI.Task.Parameters) then
  begin
    if aStartNew then
    begin
      aTaskUI.Task.OnSynchronize := OnSynchronize;
      FTasks.Add(aTaskUI);
      aTaskUI.Show;
    end;
    ExecuteTask(aTaskUI);
  end
  else if aStartNew then
    aTaskUI.Free;
end;

{ TTaskUI }

constructor TTaskUI.Create(aTaskProvider: ITaskProvider; aListView: TListViewEx);
begin
  FTask := ITaskProvider(aTaskProvider).NewTask;
  FTask.TaskUI := Self;
  FResults := TStringList.Create;
  FLog := TStringList.Create;
  FListView := aListView;
end;

destructor TTaskUI.Destroy;
begin
  FLog.Free;
  FResults.Free;
  if Assigned(FProgressBar) then
    FProgressBar.Free;
  inherited;
end;

procedure TTaskUI.Show;
const
  pbMax = 100;
begin
  FProgressBar := TProgressBar.Create(nil);
  with FProgressBar do
  begin
    Style := pbstNormal; // pbstNormal, pbstMarquee
    Parent := FListView;
  end;

  FListItem := FListView.Items.Add;
  with FListItem do
  begin
    Caption := FTask.GetName + ' - ' + IntToStr(FListView.Items.Count);
    Data := Self;
    SubItems.Add('');
    SubItems.Add('');
  end;
  FListView.Selected := FListItem;
  FListView.ItemFocused := FListItem;

  AdjustProgressBar;

  with FProgressBar do
  begin
    Min := 1;
    Max := 100;
    Position := 1;
  end;
end;

procedure TTaskUI.AdjustProgressBar;
const
  pbColumnIndex = 1;
var
  pbRect: TRect;
begin
  pbRect := FListItem.DisplayRect(drBounds);
  with pbRect do
  begin
    Left := Left + FListView.Columns[pbColumnIndex - 1].Width;
    Right := Left + FListView.Columns[pbColumnIndex].Width;
    Top := Top + 1;
    Bottom := Bottom - 1;
  end;
  FProgressBar.BoundsRect := pbRect;
end;

function StatusToString(aTaskStatus: TTaskStatus): string;
begin
  case aTaskStatus of
    tsWaiting:
      Result := 'Ожидание';
    tsRunning:
      Result := 'Выполнение';
    tsCompleted:
      Result := 'Выполнена';
    tsFailed:
      Result := 'Прервана с ошибкой';
    tsCancelled:
      Result := 'Прервана пользователем';
  end;
end;

procedure TTaskUI.ReadDataFromTask;
begin
  FStatus := Task.Status;
  FListItem.SubItems[1] := StatusToString(FStatus);
  FProgress := Task.Progress;
  ProgressBar.Position := FProgress;
  FCurrentFile := Task.CurrentFile;
end;

procedure TTaskUI.WriteCommandToTask;
begin
  Task.Command := FCommand;
end;


end.
