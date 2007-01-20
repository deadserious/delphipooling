object Form1: TForm1
  Left = 227
  Top = 109
  Width = 483
  Height = 400
  Caption = 'Object Pool Test Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Thread 1'
  end
  object Label2: TLabel
    Left = 220
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Thread 2'
  end
  object Label3: TLabel
    Left = 344
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Thread 3'
  end
  object Label4: TLabel
    Left = 96
    Top = 128
    Width = 43
    Height = 13
    Caption = 'Thread 4'
  end
  object Label5: TLabel
    Left = 220
    Top = 128
    Width = 43
    Height = 13
    Caption = 'Thread 5'
  end
  object Label6: TLabel
    Left = 344
    Top = 128
    Width = 43
    Height = 13
    Caption = 'Thread 6'
  end
  object Label7: TLabel
    Left = 96
    Top = 248
    Width = 43
    Height = 13
    Caption = 'Thread 7'
  end
  object Label8: TLabel
    Left = 220
    Top = 248
    Width = 43
    Height = 13
    Caption = 'Thread 8'
  end
  object Label9: TLabel
    Left = 344
    Top = 248
    Width = 43
    Height = 13
    Caption = 'Thread 9'
  end
  object btnStart: TButton
    Left = 12
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 12
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object lbThread1: TListBox
    Left = 96
    Top = 24
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 2
  end
  object lbThread2: TListBox
    Left = 220
    Top = 24
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 3
  end
  object lbThread3: TListBox
    Left = 344
    Top = 24
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 4
  end
  object lbThread4: TListBox
    Left = 96
    Top = 144
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 5
  end
  object lbThread5: TListBox
    Left = 220
    Top = 144
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 6
  end
  object lbThread6: TListBox
    Left = 344
    Top = 144
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 7
  end
  object lbThread7: TListBox
    Left = 96
    Top = 264
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 8
  end
  object lbThread8: TListBox
    Left = 220
    Top = 264
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 9
  end
  object lbThread9: TListBox
    Left = 344
    Top = 264
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 10
  end
  object chkAutoGrow: TCheckBox
    Left = 12
    Top = 72
    Width = 81
    Height = 17
    Caption = 'AutoGrow'
    TabOrder = 11
    OnClick = chkAutoGrowClick
  end
end
