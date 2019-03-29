object Form1: TForm1
  Left = 214
  Top = 126
  Width = 914
  Height = 732
  Caption = 'TQProgressBarDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    906
    698)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel6: TBevel
    Left = 384
    Top = 320
    Width = 513
    Height = 329
    Shape = bsFrame
  end
  object Bevel7: TBevel
    Left = 8
    Top = 320
    Width = 361
    Height = 329
    Shape = bsFrame
  end
  object Bevel5: TBevel
    Left = 16
    Top = 512
    Width = 329
    Height = 89
    Shape = bsFrame
  end
  object Bevel1: TBevel
    Left = 8
    Top = 32
    Width = 681
    Height = 273
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 254
    Top = 96
    Width = 294
    Height = 15
    Caption = 'aBar.BlockSize := 15; aBar.SpaceSize := 4;'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 254
    Top = 216
    Width = 140
    Height = 30
    Caption = 'aBar.BlockSize := 0; aBar.SpaceSize := 0; '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 254
    Top = 160
    Width = 203
    Height = 15
    Alignment = taRightJustify
    Caption = 'aBar.ShowFullBlock := False; '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 254
    Top = 128
    Width = 196
    Height = 15
    Alignment = taRightJustify
    Caption = 'aBar.ShowFullBlock := True; '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    Left = 24
    Top = 104
    Width = 203
    Height = 45
    Caption = 
      'In order to display blocks, the two first bars have blocks and s' +
      'paces sizes > 0 :'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Bevel2: TBevel
    Left = 32
    Top = 192
    Width = 649
    Height = 9
    Shape = bsTopLine
  end
  object Label6: TLabel
    Left = 24
    Top = 528
    Width = 259
    Height = 30
    Caption = 'Bars without shape are transparent : aBar.shaped := False;'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label7: TLabel
    Left = 24
    Top = 216
    Width = 196
    Height = 60
    Caption = 
      'The third shows a continuous bar, because either blockSize or sp' +
      'aceSize (or both) is set to zero :'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label8: TLabel
    Left = 16
    Top = 40
    Width = 657
    Height = 49
    AutoSize = False
    Caption = 
      'TQProgressBars can display positions either under the form of a ' +
      'continuous bar, or by blocks. If byBlocks, blocks can be filled ' +
      'either smoothly, or full block by full block :'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label9: TLabel
    Left = 704
    Top = 256
    Width = 189
    Height = 45
    Caption = 
      'Four flat vertical bars, on a panel of the same color than their' +
      ' background one.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Bevel3: TBevel
    Left = 240
    Top = 96
    Width = 9
    Height = 81
    Shape = bsLeftLine
  end
  object Bevel4: TBevel
    Left = 240
    Top = 216
    Width = 9
    Height = 73
    Shape = bsLeftLine
  end
  object Label10: TLabel
    Left = 554
    Top = 671
    Width = 340
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = 'TQProgressBar component demo, '#169' 2004 - Olivier Touzot'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 110
    Top = 336
    Width = 133
    Height = 25
    AutoSize = False
    Caption = 'Some other examples'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = [fsUnderline]
    ParentFont = False
    WordWrap = True
  end
  object Label12: TLabel
    Left = 22
    Top = 376
    Width = 168
    Height = 15
    Caption = 'aBar.barLook := blMetal;'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label13: TLabel
    Left = 22
    Top = 440
    Width = 168
    Height = 15
    Caption = 'aBar.barLook := blGlass;'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label14: TLabel
    Left = 534
    Top = 328
    Width = 133
    Height = 25
    AutoSize = False
    Caption = 'Try some settings'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = [fsUnderline]
    ParentFont = False
    WordWrap = True
  end
  object Label15: TLabel
    Left = 640
    Top = 484
    Width = 47
    Height = 13
    Caption = 'BlockSize'
  end
  object Label16: TLabel
    Left = 640
    Top = 508
    Width = 51
    Height = 13
    Caption = 'SpaceSize'
  end
  object Bevel8: TBevel
    Left = 696
    Top = 32
    Width = 201
    Height = 273
    Shape = bsFrame
  end
  object Label17: TLabel
    Left = 720
    Top = 480
    Width = 161
    Height = 56
    Caption = 
      '(In order to display blocks, both blockSize AND spaceSize must b' +
      'e <> 0)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label18: TLabel
    Left = 688
    Top = 368
    Width = 140
    Height = 28
    Caption = '(Bars without shape are transparents)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label19: TLabel
    Left = 728
    Top = 560
    Width = 161
    Height = 56
    Caption = 
      '(This bar displays the current position'#39's value, in its hint, as' +
      ' a percentage)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object QProgressBar1: TQProgressBar
    Left = 440
    Top = 560
    width = 209
    height = 21
    orientation = boHorizontal
    barKind = bkFlat
    barLook = blMetal
    roundCorner = True
    backgroundColor = clWhite
    barColor = clBlue
    startColor = clBlue
    finalColor = clYellow
    showInactivePos = False
    invertInactPos = False
    inactivePosColor = 10337791
    shaped = True
    shapeColor = 7617536
    blockSize = 0
    spaceSize = 0
    showFullBlock = False
    maximum = 100
    position = 50
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = True
    ShowPosAsPct = True
  end
  object QProgressBar2: TQProgressBar
    Left = 464
    Top = 128
    width = 209
    height = 21
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blMetal
    roundCorner = True
    backgroundColor = clWhite
    barColor = 12710665
    startColor = 12710665
    finalColor = 12710665
    showInactivePos = False
    invertInactPos = False
    inactivePosColor = clGray
    shaped = True
    shapeColor = 7617536
    blockSize = 15
    spaceSize = 4
    showFullBlock = True
    maximum = 100
    position = 50
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = False
    ShowPosAsPct = False
  end
  object TrackBar1: TTrackBar
    Left = 456
    Top = 256
    Width = 225
    Height = 33
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object QProgressBar3: TQProgressBar
    Left = 464
    Top = 160
    width = 209
    height = 21
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blMetal
    roundCorner = True
    backgroundColor = clWhite
    barColor = 12710665
    startColor = 12710665
    finalColor = 12710665
    showInactivePos = False
    invertInactPos = False
    inactivePosColor = clGray
    shaped = True
    shapeColor = 7617536
    blockSize = 15
    spaceSize = 4
    showFullBlock = False
    maximum = 100
    position = 50
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = False
    ShowPosAsPct = False
  end
  object QProgressBar4: TQProgressBar
    Left = 464
    Top = 216
    width = 209
    height = 21
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blMetal
    roundCorner = True
    backgroundColor = clWhite
    barColor = 12710665
    startColor = 12710665
    finalColor = 12710665
    showInactivePos = False
    invertInactPos = False
    inactivePosColor = clGray
    shaped = True
    shapeColor = 7617536
    blockSize = 0
    spaceSize = 0
    showFullBlock = False
    maximum = 100
    position = 50
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = False
    ShowPosAsPct = False
  end
  object QProgressBar5: TQProgressBar
    Left = 24
    Top = 392
    width = 313
    height = 33
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blMetal
    roundCorner = False
    backgroundColor = clGray
    barColor = 12651505
    startColor = 12651505
    finalColor = 12710665
    showInactivePos = True
    invertInactPos = False
    inactivePosColor = clSilver
    shaped = True
    shapeColor = clBlack
    blockSize = 5
    spaceSize = 2
    showFullBlock = False
    maximum = 100
    position = 0
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = True
    ShowPosAsPct = True
  end
  object TrackBar2: TTrackBar
    Left = 40
    Top = 616
    Width = 241
    Height = 25
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 6
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar2Change
  end
  object Panel1: TPanel
    Left = 712
    Top = 40
    Width = 169
    Height = 193
    Color = 5308416
    TabOrder = 7
    object QProgressBar6: TQProgressBar
      Left = 24
      Top = 16
      width = 25
      height = 165
      orientation = boVertical
      barKind = bkFlat
      barLook = blMetal
      roundCorner = False
      backgroundColor = 5308416
      barColor = clRed
      startColor = clRed
      finalColor = clLime
      showInactivePos = True
      invertInactPos = False
      inactivePosColor = 6569026
      shaped = True
      shapeColor = 5308416
      blockSize = 1
      spaceSize = 1
      showFullBlock = False
      maximum = 100
      position = 0
      captionAlign = taLeftJustify
      font.Charset = DEFAULT_CHARSET
      font.Color = clWindowText
      font.Height = -11
      font.Name = 'MS Sans Serif'
      font.Style = []
      AutoCaption = False
      AutoHint = False
      ShowPosAsPct = False
    end
    object QProgressBar7: TQProgressBar
      Left = 56
      Top = 16
      width = 25
      height = 165
      orientation = boVertical
      barKind = bkFlat
      barLook = blMetal
      roundCorner = False
      backgroundColor = 5308416
      barColor = clRed
      startColor = clRed
      finalColor = clLime
      showInactivePos = True
      invertInactPos = False
      inactivePosColor = 6569026
      shaped = True
      shapeColor = 5308416
      blockSize = 1
      spaceSize = 1
      showFullBlock = False
      maximum = 100
      position = 0
      captionAlign = taLeftJustify
      font.Charset = DEFAULT_CHARSET
      font.Color = clWindowText
      font.Height = -11
      font.Name = 'MS Sans Serif'
      font.Style = []
      AutoCaption = False
      AutoHint = False
      ShowPosAsPct = False
    end
    object QProgressBar8: TQProgressBar
      Left = 88
      Top = 16
      width = 25
      height = 165
      orientation = boVertical
      barKind = bkFlat
      barLook = blMetal
      roundCorner = False
      backgroundColor = 5308416
      barColor = clRed
      startColor = clRed
      finalColor = clLime
      showInactivePos = True
      invertInactPos = False
      inactivePosColor = 6569026
      shaped = True
      shapeColor = 5308416
      blockSize = 1
      spaceSize = 1
      showFullBlock = False
      maximum = 100
      position = 0
      captionAlign = taLeftJustify
      font.Charset = DEFAULT_CHARSET
      font.Color = clWindowText
      font.Height = -11
      font.Name = 'MS Sans Serif'
      font.Style = []
      AutoCaption = False
      AutoHint = False
      ShowPosAsPct = False
    end
    object QProgressBar9: TQProgressBar
      Left = 120
      Top = 16
      width = 25
      height = 165
      orientation = boVertical
      barKind = bkFlat
      barLook = blMetal
      roundCorner = False
      backgroundColor = 5308416
      barColor = clRed
      startColor = clRed
      finalColor = clLime
      showInactivePos = True
      invertInactPos = False
      inactivePosColor = 6569026
      shaped = True
      shapeColor = 5308416
      blockSize = 1
      spaceSize = 1
      showFullBlock = False
      maximum = 100
      position = 0
      captionAlign = taLeftJustify
      font.Charset = DEFAULT_CHARSET
      font.Color = clWindowText
      font.Height = -11
      font.Name = 'MS Sans Serif'
      font.Style = []
      AutoCaption = False
      AutoHint = False
      ShowPosAsPct = False
    end
  end
  object TrackBar3: TTrackBar
    Left = 704
    Top = 232
    Width = 177
    Height = 17
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 8
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = TrackBar3Change
  end
  object QProgressBar10: TQProgressBar
    Left = 68
    Top = 568
    width = 189
    height = 17
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blMetal
    roundCorner = False
    backgroundColor = clWhite
    barColor = 3162159
    startColor = 3162159
    finalColor = 33023
    showInactivePos = False
    invertInactPos = False
    inactivePosColor = clSilver
    shaped = False
    shapeColor = 2897976
    blockSize = 12
    spaceSize = 7
    showFullBlock = False
    maximum = 100
    position = 0
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = True
    ShowPosAsPct = True
  end
  object QProgressBar11: TQProgressBar
    Left = 24
    Top = 456
    width = 313
    height = 33
    orientation = boHorizontal
    barKind = bkCylinder
    barLook = blGlass
    roundCorner = False
    backgroundColor = clGray
    barColor = 12651505
    startColor = 12651505
    finalColor = 12710665
    showInactivePos = True
    invertInactPos = False
    inactivePosColor = clSilver
    shaped = True
    shapeColor = clBlack
    blockSize = 5
    spaceSize = 2
    showFullBlock = False
    maximum = 100
    position = 0
    captionAlign = taLeftJustify
    font.Charset = DEFAULT_CHARSET
    font.Color = clWindowText
    font.Height = -11
    font.Name = 'MS Sans Serif'
    font.Style = []
    AutoCaption = False
    AutoHint = True
    ShowPosAsPct = True
  end
  object TrackBar4: TTrackBar
    Left = 424
    Top = 600
    Width = 241
    Height = 25
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 11
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar4Change
  end
  object CheckBox1: TCheckBox
    Left = 408
    Top = 368
    Width = 129
    Height = 17
    Caption = 'roundCorners'
    Checked = True
    State = cbChecked
    TabOrder = 12
    OnClick = CheckBox1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 408
    Top = 400
    Width = 129
    Height = 65
    Caption = ' barKind '
    ItemIndex = 0
    Items.Strings = (
      'bkFlat'
      'bkCylinder')
    TabOrder = 13
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 592
    Top = 400
    Width = 129
    Height = 65
    Caption = ' barLook'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'blMetal'
      'blGlass')
    TabOrder = 14
    OnClick = RadioGroup2Click
  end
  object CheckBox2: TCheckBox
    Left = 408
    Top = 480
    Width = 169
    Height = 17
    Caption = 'showInactivePositions'
    TabOrder = 15
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 408
    Top = 504
    Width = 169
    Height = 17
    Caption = 'invertInactivePositions'
    Enabled = False
    TabOrder = 16
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 592
    Top = 368
    Width = 81
    Height = 17
    Caption = 'shaped'
    Checked = True
    State = cbChecked
    TabOrder = 17
    OnClick = CheckBox4Click
  end
  object SpinEdit1: TSpinEdit
    Left = 592
    Top = 480
    Width = 41
    Height = 22
    MaxValue = 250
    MinValue = 0
    TabOrder = 18
    Value = 0
    OnChange = SpinEdit1Change
  end
  object SpinEdit2: TSpinEdit
    Left = 592
    Top = 504
    Width = 41
    Height = 22
    MaxValue = 250
    MinValue = 0
    TabOrder = 19
    Value = 0
    OnChange = SpinEdit2Change
  end
  object CheckBox5: TCheckBox
    Left = 592
    Top = 536
    Width = 97
    Height = 17
    Caption = 'showFullBlocks'
    TabOrder = 20
    OnClick = CheckBox5Click
  end
  object Panel2: TPanel
    Left = 408
    Top = 560
    Width = 21
    Height = 21
    Color = clBlue
    TabOrder = 21
    OnClick = Panel2Click
  end
  object Panel3: TPanel
    Left = 656
    Top = 560
    Width = 21
    Height = 21
    Color = clYellow
    TabOrder = 22
    OnClick = Panel3Click
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 8
  end
end
