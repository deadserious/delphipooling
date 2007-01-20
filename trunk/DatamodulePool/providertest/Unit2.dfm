object DataModule2: TDataModule2
  OldCreateOrder = False
  Left = 304
  Top = 107
  Height = 219
  Width = 263
  object IBDatabase1: TIBDatabase
    Connected = True
    DatabaseName = 
      'c:\development\arcana\Websites\serviceauctions.com\db\SERVICEAUC' +
      'TIONS.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    Left = 28
    Top = 8
  end
  object IBTransaction1: TIBTransaction
    Active = True
    DefaultDatabase = IBDatabase1
    Left = 108
    Top = 8
  end
  object IBTable1: TIBTable
    Database = IBDatabase1
    Transaction = IBTransaction1
    Active = True
    FieldDefs = <
      item
        Name = 'fkLocale'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'Code'
        DataType = ftString
        Size = 10
      end>
    StoreDefs = True
    TableName = 'AreaCode'
    Left = 60
    Top = 72
  end
end
