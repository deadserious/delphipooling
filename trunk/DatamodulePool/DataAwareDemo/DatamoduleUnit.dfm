object DataModule1: TDataModule1
  OldCreateOrder = False
  Left = 472
  Top = 220
  Height = 334
  Width = 387
  object PoolAdapter: TArcDMPoolAdapter
    Left = 24
    Top = 8
  end
  object dbDemos: TIBDatabase
    Connected = True
    DatabaseName = 'C:\Program Files\Common Files\Borland Shared\Data\dbdemos.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = transDefault
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 176
    Top = 136
  end
  object tblBiolife: TIBTable
    Database = dbDemos
    Transaction = transDefault
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    FieldDefs = <
      item
        Name = 'SPECIES_NO'
        DataType = ftFloat
      end
      item
        Name = 'CATEGORY'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'COMMON_NAME'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'SPECIES_NAME'
        DataType = ftString
        Size = 40
      end
      item
        Name = 'LENGTH__CM_'
        DataType = ftFloat
      end
      item
        Name = 'LENGTH_IN'
        DataType = ftFloat
      end
      item
        Name = 'NOTES'
        DataType = ftMemo
        Size = 8
      end
      item
        Name = 'GRAPHIC'
        DataType = ftBlob
        Size = 8
      end>
    IndexDefs = <
      item
        Name = 'BIOLIFE0'
        Fields = 'SPECIES_NO'
        Options = [ixUnique]
      end>
    StoreDefs = True
    TableName = 'BIOLIFE'
    Left = 72
    Top = 136
  end
  object transDefault: TIBTransaction
    Active = True
    DefaultDatabase = dbDemos
    AutoStopAction = saNone
    Left = 64
    Top = 192
  end
end
