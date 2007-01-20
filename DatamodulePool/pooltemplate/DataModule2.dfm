object dmDataTwo: TdmDataTwo
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 243
  Top = 107
  Height = 150
  Width = 215
  object connDB: TSQLConnection
    ConnectionName = 'IBLocal'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      
        'Database=c:\development\arcana\IWComponents\datamodulepool\poolt' +
        'emplate\data.gdb'
      'DriverName=Interbase'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet='
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    VendorLib = 'GDS32.DLL'
    Left = 24
    Top = 8
  end
  object tblContacts: TSQLTable
    MaxBlobSize = -1
    SQLConnection = connDB
    TableName = 'CONTACTS'
    Left = 96
    Top = 8
    object tblContactsPK: TIntegerField
      FieldName = 'PK'
      Required = True
    end
    object tblContactsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
  end
  object PoolAdapter: TArcDMPoolAdapter
    Left = 24
    Top = 64
  end
end
