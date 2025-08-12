object FormTaskParams: TFormTaskParams
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1079#1072#1076#1072#1095#1080
  ClientHeight = 314
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  DesignSize = (
    384
    314)
  TextHeight = 15
  object ValueListEditor: TValueListEditor
    Left = 2
    Top = 2
    Width = 378
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TitleCaptions.Strings = (
      #1055#1072#1088#1072#1084#1077#1090#1088
      #1047#1085#1072#1095#1077#1085#1080#1077)
    OnEditButtonClick = ValueListEditorEditButtonClick
    ExplicitWidth = 370
    ExplicitHeight = 243
    ColWidths = (
      150
      222)
  end
  object ButtonOk: TButton
    Left = 193
    Top = 272
    Width = 78
    Height = 34
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 185
    ExplicitTop = 260
  end
  object ButtonCancel: TButton
    Left = 290
    Top = 272
    Width = 86
    Height = 34
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 282
    ExplicitTop = 260
  end
  object ActionList1: TActionList
    Left = 112
    Top = 272
    object actOk: TAction
      Caption = 'actOk'
      ShortCut = 13
      OnExecute = actOkExecute
    end
    object actCancel: TAction
      Caption = 'actCancel'
      ShortCut = 27
      OnExecute = actCancelExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 184
  end
end
