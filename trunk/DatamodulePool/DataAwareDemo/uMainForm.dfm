object IWForm1: TIWForm1
  Left = 0
  Top = 0
  Width = 555
  Height = 400
  ConnectionMode = cmAny
  SupportedBrowsers = [brIE, brNetscape7, brOpera, brSafari, brNetscape6]
  BrowserSecurityCheck = True
  Background.Fixed = False
  HandleTabs = False
  LeftToRight = True
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  XPTheme = True
  DesignLeft = 441
  DesignTop = 284
  object IWDBEdit1: TIWDBEdit
    Left = 60
    Top = 48
    Width = 249
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Alignment = taLeftJustify
    BGColor = clNone
    FocusColor = clNone
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWDBEdit1'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 0
    AutoEditable = False
    DataField = 'COMMON_NAME'
    PasswordPrompt = False
    DataSource = dsBiolife
  end
  object IWDBEdit2: TIWDBEdit
    Left = 60
    Top = 80
    Width = 249
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Alignment = taLeftJustify
    BGColor = clNone
    FocusColor = clNone
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWDBEdit2'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 1
    AutoEditable = False
    DataField = 'SPECIES_NAME'
    PasswordPrompt = False
    DataSource = dsBiolife
  end
  object IWDBImage1: TIWDBImage
    Left = 60
    Top = 108
    Width = 250
    Height = 150
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    BorderOptions.Color = clNone
    BorderOptions.Width = 0
    DoSubmitValidation = True
    ScriptEvents = <>
    TabOrder = 2
    UseSize = False
    FriendlyName = 'IWDBImage1'
    DataField = 'GRAPHIC'
    DataSource = dsBiolife
  end
  object IWDBNavigator1: TIWDBNavigator
    Left = 60
    Top = 0
    Width = 300
    Height = 28
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Confirmations.Delete = 'Are you sure you want to delete this record?'
    Confirmations.Post = 'Are you sure you want to update this record?'
    Confirmations.Cancel = 'Are you sure you want to cancel your changes to this record?'
    DataSource = dsBiolife
    FriendlyName = 'IWDBNavigator1'
    ImageHeight = 24
    ImageWidth = 24
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh]
    Orientation = orHorizontal
  end
  object cdsBiolife: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    ProviderName = 'tblBiolife'
    RemoteServer = PoolConnection
    Left = 24
    Top = 236
    object cdsBiolifeSPECIES_NO: TFloatField
      FieldName = 'SPECIES_NO'
      Origin = 'BIOLIFE.SPECIES_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object cdsBiolifeCATEGORY: TStringField
      FieldName = 'CATEGORY'
      Origin = 'BIOLIFE.CATEGORY'
      Size = 15
    end
    object cdsBiolifeCOMMON_NAME: TStringField
      FieldName = 'COMMON_NAME'
      Origin = 'BIOLIFE.COMMON_NAME'
      Size = 30
    end
    object cdsBiolifeSPECIES_NAME: TStringField
      FieldName = 'SPECIES_NAME'
      Origin = 'BIOLIFE.SPECIES_NAME'
      Size = 40
    end
    object cdsBiolifeLENGTH__CM_: TFloatField
      FieldName = 'LENGTH__CM_'
      Origin = 'BIOLIFE.LENGTH__CM_'
    end
    object cdsBiolifeLENGTH_IN: TFloatField
      FieldName = 'LENGTH_IN'
      Origin = 'BIOLIFE.LENGTH_IN'
    end
    object cdsBiolifeNOTES: TMemoField
      FieldName = 'NOTES'
      Origin = 'BIOLIFE.NOTES'
      BlobType = ftMemo
      Size = 8
    end
    object cdsBiolifeGRAPHIC: TBlobField
      FieldName = 'GRAPHIC'
      Origin = 'BIOLIFE.GRAPHIC'
      Size = 8
    end
  end
  object dsBiolife: TDataSource
    DataSet = cdsBiolife
    Left = 20
    Top = 272
  end
  object PoolConnection: TArcPoolConnection
    Pool = IWServerController.Pool
    OnGetPool = PoolConnectionGetPool
    Connected = True
    Left = 24
    Top = 200
  end
end
