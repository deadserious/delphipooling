object dmDataOne: TdmDataOne
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 137
  Top = 162
  Height = 261
  Width = 358
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
  object tblMessages: TSQLTable
    MaxBlobSize = -1
    SQLConnection = connDB
    TableName = 'MESSAGES'
    Left = 112
    Top = 8
    object tblMessagesPK: TIntegerField
      FieldName = 'PK'
      Required = True
    end
    object tblMessagesTOADDRESS: TStringField
      FieldName = 'TOADDRESS'
      Size = 255
    end
    object tblMessagesSUBJECT: TStringField
      FieldName = 'SUBJECT'
      Size = 255
    end
    object tblMessagesMESSAGETEXT: TMemoField
      FieldName = 'MESSAGETEXT'
      BlobType = ftMemo
      Size = 1
    end
  end
  object PoolAdapter: TArcDMPoolAdapter
    Left = 28
    Top = 72
  end
  object spMessagesPK: TSQLStoredProc
    MaxBlobSize = -1
    Params = <
      item
        DataType = ftInteger
        Name = 'ID'
        ParamType = ptOutput
        Size = 4
        Value = 1
      end>
    SQLConnection = connDB
    StoredProcName = 'SP_GEN_MESSAGES_ID'
    Left = 112
    Top = 72
  end
end
