object FormWizard: TFormWizard
  Left = 489
  Top = 196
  BorderStyle = bsDialog
  Caption = 'Wizard Console'
  ClientHeight = 344
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Courier'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 20
  object ListViewMonsters: TListView
    Left = 8
    Top = 48
    Width = 473
    Height = 145
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'Name'
      end
      item
        Caption = 'Sym'
      end
      item
        Caption = 'OwnPos'
      end
      item
        Caption = 'DunPos'
      end
      item
        Caption = 'Energy'
      end
      item
        Caption = 'HP'
      end
      item
        Caption = 'Alive'
      end
      item
        Caption = 'Awake'
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object ButtonGainXP: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Gain XP'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonGainXPClick
  end
  object ButtonGainGold: TButton
    Left = 104
    Top = 7
    Width = 89
    Height = 25
    Caption = 'Gain Gold'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonGainGoldClick
  end
  object ButtonHeal: TButton
    Left = 200
    Top = 7
    Width = 89
    Height = 25
    Caption = 'Heal HP'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonHealClick
  end
  object ButtonGetItem: TButton
    Left = 296
    Top = 7
    Width = 89
    Height = 25
    Caption = 'Get Item'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonGetItemClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 200
    Width = 473
    Height = 129
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object ButtonMagicMap: TButton
    Left = 392
    Top = 7
    Width = 89
    Height = 25
    Caption = 'Magic Map'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = ButtonMagicMapClick
  end
end
