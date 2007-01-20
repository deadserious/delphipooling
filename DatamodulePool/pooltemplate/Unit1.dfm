object Form1: TForm1
  Left = 243
  Top = 107
  Width = 441
  Height = 318
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 4
    Top = 4
    Width = 425
    Height = 277
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object IWStandAloneServer1: TIWStandAloneServer
    DebugLog = True
    HTML32 = False
    WML = False
    XHTMLMP = False
    RunIn = wbDefault
    RunSSL = False
    OnDebugLog = IWStandAloneServer1DebugLog
    OnNewSession = IWStandAloneServer1NewSession
    Left = 208
    Top = 144
  end
end
