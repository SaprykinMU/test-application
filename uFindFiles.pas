unit uFindFiles;

interface

uses
  Classes, SysUtils;

type
  TOnFileFound = reference to procedure(FileName: string);


  procedure FindFiles(Thread: TThread; StartPath: string; Mask: string; OnFileFound: TOnFileFound);

implementation

uses
  IOUtils, Masks;


procedure FindFiles(Thread: TThread; StartPath: string; Mask: string; OnFileFound: TOnFileFound);

  procedure ScanFolder(Path: string);
  var  sr: TSearchRec;
  begin

    if SysUtils.FindFirst(TPath.Combine(Path, '*.*'), faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory <> 0) then
        begin
          if not ((sr.Name = '.') or (sr.Name = '..')) then
            ScanFolder(TPath.Combine(Path, sr.Name))
        end else
        begin
          //if MatchesMask(ExtractFileName(dir+'\'+sr.name), '[*.txt],[?hhh*.log]') then
         // Mask := '*.txt';
          if MatchesMask(sr.Name, Mask) then
            OnFileFound(TPath.Combine(Path, sr.Name));
        end;
      until (FindNext(sr) <> 0) or Thread.CheckTerminated;
      FindClose(sr);
    end;

//    TDirectory.GetFiles


  {
    if SysUtils.FindFirst(TPath.Combine(Path, '*.*'), faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Attr and (faDirectory or faHidden) <> 0) and not ((sr.Name = '.') or (sr.Name = '..')) then
          ScanFolder(TPath.Combine(Path, sr.Name))
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;


    if SysUtils.FindFirst(TPath.Combine(Path, Mask), 0, sr) = 0 then
    begin
      repeat
         if not (sr.Attr and (faDirectory or faHidden) <> 0)  then
          OnFileFound(TPath.Combine(Path,sr.Name));
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

   }



    {
    FindFirst(TPath.Combine(Path, Mask), faAnyFile, sr);
    try
    if sr.Name <> '' then
    begin
      OnFileFound(sr.Name);
      while FindNext(sr) = 0 do
      begin
         if not (sr.Attr and (faDirectory or faHidden) <> 0)  then
          OnFileFound(TPath.Combine(Path,sr.Name));
      end;
    end;
    finally
      FindClose(sr);
    end;
    }
  end;

begin
  ScanFolder(StartPath);
end;



end.
