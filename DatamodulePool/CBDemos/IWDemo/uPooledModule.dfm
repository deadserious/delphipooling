object dmPooled: TdmPooled
  OldCreateOrder = False
  Left = 173
  Top = 107
  Height = 251
  Width = 290
  object Session1: TSession
    Active = True
    AutoSessionName = True
    Left = 20
    Top = 8
  end
  object Database1: TDatabase
    AliasName = 'BCDEMOS'
    Connected = True
    DatabaseName = 'dbtest'
    LoginPrompt = False
    SessionName = 'Session1_1'
    Left = 100
    Top = 8
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'dbtest'
    SessionName = 'Session1_1'
    TableName = 'biolife.db'
    Left = 168
    Top = 8
  end
end
