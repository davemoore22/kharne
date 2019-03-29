{ UnitDisplay

  Copyright (c) 2007-2009 Dave Moore 

  Main Display, Display Interface and GUI-handling routines

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is all code apart where indicated.

  The Initial Developer of the Original Code is Dave Moore (starbog@gmail.com)

  Portions created by Dave Moore are Copyright (C) Dave Moore. All Rights Reserved.

  Contributor(s): 

}

unit UnitDisplay;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, KeyboardHandler, DateUtils, jpeg, ImgList, Menus,
  ComCtrls, ToolWin, Gradlabl, PathPlanner, QProgBar, AppEvnts, hotlog,
  UnitDungeon, UnitDefines, UnitConst, UnitVars, UnitFunctions, UnitItem,
  UnitCreature,  UnitMonster, UnitInit, UnitGame;

{ Main form }
type
  TFormDisplay = class(TForm)
    KeyboardHandler: TKeyboardHandler;
    ImageListPlayer: TImageList;
    ImageListTerrain: TImageList;
    Timer: TTimer;
    MainMenu: TMainMenu;
    Game1: TMenuItem;
    SaveandQuit1: TMenuItem;
    QuitSuicide1: TMenuItem;
    Hewlp1: TMenuItem;
    UseTileGraphics1: TMenuItem;
    UseMonochromeASCII1: TMenuItem;
    UseColouredASCII1: TMenuItem;
    Help1: TMenuItem;
    GameplayOptions1: TMenuItem;
    ViewGameGuide1: TMenuItem;
    ControlSummary1: TMenuItem;
    ImageListWeapons: TImageList;
    ImageListArmour: TImageList;
    ImageListAmulets: TImageList;
    ImageListRings: TImageList;
    ImageListPotions: TImageList;
    ImageListWands: TImageList;
    ImageListScrolls: TImageList;
    ImageListFood: TImageList;
    PageControlMain: TPageControl;
    TabSheetDisplay: TTabSheet;
    TabSheetInventory: TTabSheet;
    PanelDisplay: TPanel;
    PanelBackgroundInventory: TPanel;
    PanelInventory1: TPanel;
    BackPanel12: TPanel;
    LabelItem12: TLabel;
    BackPanel1: TPanel;
    LabelItem1: TLabel;
    BackPanel2: TPanel;
    LabelItem2: TLabel;
    BackPanel3: TPanel;
    LabelItem3: TLabel;
    BackPanel4: TPanel;
    LabelItem4: TLabel;
    BackPanel5: TPanel;
    LabelItem5: TLabel;
    BackPanel6: TPanel;
    LabelItem6: TLabel;
    BackPanel7: TPanel;
    LabelItem7: TLabel;
    BackPanel8: TPanel;
    LabelItem8: TLabel;
    BackPanel9: TPanel;
    LabelItem9: TLabel;
    BackPanel10: TPanel;
    LabelItem10: TLabel;
    BackPanel13: TPanel;
    LabelItem13: TLabel;
    BackPanel11: TPanel;
    LabelItem11: TLabel;
    PanelEquipped: TPanel;
    BackPanel39: TPanel;
    BackPanel32: TPanel;
    LabelItem32: TLabel;
    BackPanel33: TPanel;
    LabelItem33: TLabel;
    BackPanel31: TPanel;
    LabelItem31: TLabel;
    BackPanel30: TPanel;
    LabelItem30: TLabel;
    BackPanel29: TPanel;
    LabelItem29: TLabel;
    BackPanel28: TPanel;
    LabelItem28: TLabel;
    BackPanel27: TPanel;
    LabelItem27: TLabel;
    BackPanel34: TPanel;
    LabelItem34: TLabel;
    BackPanel35: TPanel;
    LabelItem35: TLabel;
    BackPanel38: TPanel;
    LabelItem38: TLabel;
    BackPanel36: TPanel;
    LabelItem36: TLabel;
    BackPanel37: TPanel;
    LabelItem37: TLabel;
    LabelItem39: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label14: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    TimerColorReset: TTimer;
    ScreenInventory: TPaintBox;
    GradLabelInventoryTitle: TGradLabel;
    LabelBackpack: TGradLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    TabSheetSkills: TTabSheet;
    PanelSkills: TPanel;
    GradLabelSkill0: TGradLabel;
    GradLabelSkill20: TGradLabel;
    GradLabelSkill21: TGradLabel;
    GradLabelSkill22: TGradLabel;
    GradLabelSkill23: TGradLabel;
    GradLabelSkill24: TGradLabel;
    GradLabelSkill25: TGradLabel;
    GradLabelSkill26: TGradLabel;
    GradLabelSkill27: TGradLabel;
    GradLabelSkill28: TGradLabel;
    GradLabelSkill29: TGradLabel;
    GradLabelSkill30: TGradLabel;
    GradLabelSkill12: TGradLabel;
    GradLabelSkill11: TGradLabel;
    GradLabelSkill10: TGradLabel;
    GradLabelSkill7: TGradLabel;
    GradLabelSkill6: TGradLabel;
    GradLabelSkill5: TGradLabel;
    GradLabelSkill4: TGradLabel;
    GradLabelSkill3: TGradLabel;
    GradLabelSkill2: TGradLabel;
    GradLabelSkill1: TGradLabel;
    GradLabelAC: TGradLabel;
    GradLabelEV: TGradLabel;
    GradLabelSP: TGradLabel;
    GradLabelSTR: TGradLabel;
    GradLabelAGI: TGradLabel;
    GradLabelEND: TGradLabel;
    GradLabelINT: TGradLabel;
    GradLabelRESO: TGradLabel;
    GradLabelCHA: TGradLabel;
    GradLabelName: TGradLabel;
    GradLabelBurden: TGradLabel;
    GradLabelACC: TGradLabel;
    GradLabelDam: TGradLabel;
    GradLabelBlock: TGradLabel;
    GradLabelDeflect: TGradLabel;
    GradLabelResF: TGradLabel;
    GradLabelResE: TGradLabel;
    GradLabelResA: TGradLabel;
    GradLabel34: TGradLabel;
    GradLabelResW: TGradLabel;
    GradLabel36: TGradLabel;
    GradLabelResP: TGradLabel;
    GradLabel38: TGradLabel;
    GradLabelResL: TGradLabel;
    GradLabel39: TGradLabel;
    TabSheetMagic: TTabSheet;
    PanelMagic: TPanel;
    ScreenMagic: TPaintBox;
    GradLabel10: TGradLabel;
    GradLabel26: TGradLabel;
    PanelSpellsAvailable: TPanel;
    ListViewAvailableSchools: TListView;
    GradLabel31: TGradLabel;
    PanelSpellsKnown: TPanel;
    ListViewSpellsKnown: TListView;
    PanelSpellDescription: TPanel;
    TimerCycleColours: TTimer;
    PanelDrop: TPanel;
    PanelDropFront: TPanel;
    ImageDropDragTarget: TImage;
    SimplePathPlanner: TSimplePathPlanner;
    TimerAnimate: TTimer;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    PanelCentreBack: TPanel;
    PanelInfo: TPanel;
    PanelRightSplitter: TPanel;
    PanelCentreMiddle: TPanel;
    ScreenMain: TPaintBox;
    PanelMessages: TPanel;
    PanelSplitterMessages: TPanel;
    UpdateLog: TRichEdit;
    PanelCharacter: TPanel;
    PanelStatus: TPanel;
    PanelStats: TPanel;
    Label5: TLabel;
    LabelPAC: TLabel;
    Label7: TLabel;
    LabelPEV: TLabel;
    Label8: TLabel;
    LabelPSpeed: TLabel;
    Label9: TLabel;
    LabelPGold: TLabel;
    Label11: TLabel;
    LabelPExp: TLabel;
    PanelBaseInfo: TPanel;
    Label1: TLabel;
    LabelPHealth: TLabel;
    Label3: TLabel;
    LabelPMana: TLabel;
    LabelCharacterClass: TLabel;
    LabelCharacterName: TLabel;
    Label12: TLabel;
    LabelTurns: TLabel;
    PanelMonsters: TPanel;
    PanelMap: TPanel;
    PanelLMap: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelMinimap: TPanel;
    PaintBoxMinimap: TPaintBox;
    Label51: TLabel;
    LabelMeleeWeapon: TLabel;
    LabelRangedWeapon: TLabel;
    PbHealth: TQProgressBar;
    PbMana: TQProgressBar;
    Label54: TLabel;
    LabelPTurns: TLabel;
    LabelGold: TLabel;
    Label60: TLabel;
    LabelExperience: TLabel;
    Label64: TLabel;
    Label66: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label65: TLabel;
    Label67: TLabel;
    Label70: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    GradLabelResBase: TGradLabel;
    Label89: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    PaintBox1: TPaintBox;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label56: TLabel;
    Label58: TLabel;
    GradLabelHP: TLabel;
    GradLabelMP: TLabel;
    LabelWearingWielding: TGradLabel;
    Panel6: TPanel;
    LabelItemName: TLabel;
    MemoDescription: TMemo;
    MemoSpellDescription: TRichEdit;
    GradLabel2: TGradLabel;
    LabelCurrentSpell: TLabel;
    Label109: TLabel;
    TabSheetHelp: TTabSheet;
    Panel2: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RichEditHelp: TRichEdit;
    TabSheetDump: TTabSheet;
    Panel10: TPanel;
    RichEditDump: TRichEdit;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    TabSheetVersion: TTabSheet;
    Panel17: TPanel;
    Panel16: TPanel;
    Panel15: TPanel;
    Panel14: TPanel;
    RichEditVersion: TRichEdit;
    Panel18: TPanel;
    Panel19: TPanel;
    ImageEat: TImage;
    Panel20: TPanel;
    Panel21: TPanel;
    ImageDrink: TImage;
    Panel22: TPanel;
    Panel23: TPanel;
    ImageRead: TImage;
    GradLabelSkill0Progress: TGradLabel;
    GradLabelSkill1Progress: TGradLabel;
    GradLabelSkill2Progress: TGradLabel;
    GradLabelSkill3Progress: TGradLabel;
    GradLabelSkill11Progress: TGradLabel;
    GradLabelSkill10Progress: TGradLabel;
    GradLabelSkill12Progress: TGradLabel;
    GradLabelSkill20Progress: TGradLabel;
    GradLabelSkill21Progress: TGradLabel;
    GradLabelSkill22Progress: TGradLabel;
    GradLabelSkill25Progress: TGradLabel;
    GradLabelSkill26Progress: TGradLabel;
    GradLabelSkill29Progress: TGradLabel;
    GradLabelSkill30Progress: TGradLabel;
    GradLabelSkill23Progress: TGradLabel;
    GradLabelSkill24Progress: TGradLabel;
    GradLabelSkill27Progress: TGradLabel;
    GradLabelSkill28Progress: TGradLabel;
    GradLabelSkill4Progress: TGradLabel;
    GradLabelSkill5Progress: TGradLabel;
    GradLabelSkill6Progress: TGradLabel;
    GradLabelSkill7Progress: TGradLabel;
    PageControlDetails: TPageControl;
    TabSheetVisibleMonsters: TTabSheet;
    TabSheetIntro: TTabSheet;
    Image2: TImage;
    Panel24: TPanel;
    Image1: TImage;
    VersionLabel: TLabel;
    Label111: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Panel25: TPanel;
    Label118: TLabel;
    Label119: TLabel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    LabelMenu4: TLabel;
    LabelMenu1: TLabel;
    LabelMenu2: TLabel;
    LabelMenu3: TLabel;
    Starta1: TMenuItem;
    ContinueanExistingGame1: TMenuItem;
    LabelMenu5: TLabel;
    OpenDialog: TOpenDialog;
    TabSheetHiScores: TTabSheet;
    RichEditHiScores: TRichEdit;
    Panel31: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    Label126: TLabel;
    Label128: TLabel;
    SaveDumpDialog: TSaveDialog;
    Exit1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    GradLabel1: TGradLabel;
    GradLabel3: TGradLabel;
    TimerAnimateIntro: TTimer;
    TabSheetCharacterCreate: TTabSheet;
    PanelCharCreateBackground: TPanel;
    PageControlCharacterCreate: TPageControl;
    TabSheetChar1: TTabSheet;
    GradLabel4: TGradLabel;
    EditName: TEdit;
    ImageGenerateRandomName: TImage;
    Label10: TLabel;
    TabSheetChar2: TTabSheet;
    Label49: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    GradLabel5: TGradLabel;
    TabSheetChar3: TTabSheet;
    GradLabel6: TGradLabel;
    Label132: TLabel;
    Label133: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    TabSheetChar4: TTabSheet;
    GradLabel7: TGradLabel;
    Label108: TLabel;
    Label121: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label107: TLabel;
    TabSheetChar5: TTabSheet;
    GradLabel8: TGradLabel;
    Label131: TLabel;
    MemoBackground: TMemo;
    ImageGenerateBackground: TImage;
    Label55: TLabel;
    Label63: TLabel;
    Label112: TLabel;
    Panel36: TPanel;
    Panel38: TPanel;
    BackPanel14: TPanel;
    LabelItem14: TLabel;
    Label27: TLabel;
    BackPanel15: TPanel;
    LabelItem15: TLabel;
    Label28: TLabel;
    BackPanel16: TPanel;
    LabelItem16: TLabel;
    Label29: TLabel;
    BackPanel17: TPanel;
    LabelItem17: TLabel;
    Label30: TLabel;
    BackPanel18: TPanel;
    LabelItem18: TLabel;
    Label31: TLabel;
    BackPanel19: TPanel;
    LabelItem19: TLabel;
    Label32: TLabel;
    BackPanel20: TPanel;
    LabelItem20: TLabel;
    Label33: TLabel;
    BackPanel21: TPanel;
    LabelItem21: TLabel;
    Label34: TLabel;
    BackPanel22: TPanel;
    LabelItem22: TLabel;
    Label35: TLabel;
    BackPanel23: TPanel;
    LabelItem23: TLabel;
    Label36: TLabel;
    BackPanel24: TPanel;
    LabelItem24: TLabel;
    Label37: TLabel;
    BackPanel25: TPanel;
    LabelItem25: TLabel;
    Label38: TLabel;
    BackPanel26: TPanel;
    LabelItem26: TLabel;
    Label39: TLabel;
    TabSheetItem: TTabSheet;
    PanelItemTopBorder: TPanel;
    RichEditItem: TRichEdit;
    Panel40: TPanel;
    Panel41: TPanel;
    Panel42: TPanel;
    Label140: TLabel;
    LabelKeysInventory: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    LabelDumpKeys: TLabel;
    Label145: TLabel;
    LabelKeysHiScores: TLabel;
    Label147: TLabel;
    Label57: TLabel;
    Label144: TLabel;
    Label59: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    LabelKeysDrop: TLabel;
    LabelKeysEat: TLabel;
    LabelKeysRead: TLabel;
    LabelKeysIdentify: TLabel;
    LabelKeysDrink: TLabel;
    TimerDelay: TTimer;
    PanelVisibleMonsters: TPanel;
    LabelMonster9: TLabel;
    LabelMonster8: TLabel;
    LabelMonster7: TLabel;
    LabelMonster6: TLabel;
    LabelMonster5: TLabel;
    LabelMonster4: TLabel;
    LabelMonster3: TLabel;
    LabelMonster2: TLabel;
    LabelMonster1: TLabel;
    ListViewMonsters: TListView;
    TabSheetMonster: TTabSheet;
    Panel1: TPanel;
    Label110: TLabel;
    Panel39: TPanel;
    Panel43: TPanel;
    RichEditMonster: TRichEdit;
    Panel44: TPanel;
    LabelMonsterName: TLabel;
    Panel37: TPanel;
    Label125: TLabel;
    Label117: TLabel;
    Label120: TLabel;
    Label127: TLabel;
    Label129: TLabel;
    Label130: TLabel;
    Label138: TLabel;

    { Save and quit }
    procedure ImageSaveAndExitClick(Sender: TObject);

    { Display refresh }
    procedure ScreenMainPaint(Sender: TObject);

    { Main keyboard handler - this checks keyboard for keys pressed }
    procedure TimerTimer(Sender: TObject);
    
    { Shortcut for switching to graphics mode }
    procedure ImageDisplayGraphicsClick(Sender: TObject);
    
    { Shortcut for switching to monochrome ascii mode }
    procedure ImageDisplayASCIIStandardClick(Sender: TObject);
    
    { Shortcut for switching to coloured ascii mode }
    procedure ImageDisplayASCIICrawlClick(Sender: TObject);
    
    { Form Constructor }
    procedure FormCreate(Sender: TObject);
    
    { Handle mouse clicks on the same screen }
    procedure ScreenMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      
    { Standard form destructor }
    procedure FormDestroy(Sender: TObject);
    
    { Shortcut for switching to monochrome ascii mode }
    procedure ImageMonochromeASCIIClick(Sender: TObject);
    
    { Shortcut for switching to coloured ascii mode }
    procedure ImageColouredASCIIClick(Sender: TObject);
    
    { Reset inventory panels back to default colour }   
    procedure TimerColorResetTimer(Sender: TObject);
    
    { Handle mouse-over hinting of equipped item slots }
    procedure LabelItem1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
        
    { Handle mouse-down hinting of equipped item slots }   
    procedure LabelItem1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      
    { Handle drag-and-drop of items onto the inventory }
    procedure LabelItem1DragDrop(Sender, Source: TObject; X, Y: Integer);
    
    { Drag-and-drop checking of inventory drag-and-drop }
    procedure LabelItem1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
     
    { Handle drag-and-drop of items onto the equipped items slots } 
    procedure LabelItem32DragDrop(Sender, Source: TObject; X, Y: Integer);
    
    { Drag-and-drop checking of equiped items drag-and-drop }
    procedure LabelItem32DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);

		{ Handle colouring of spell schools }
    procedure ListViewAvailableSchoolsCustomDrawSubItem(
      Sender: TCustomListView; Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; var DefaultDraw: Boolean);
      
    { Handle clicking of a spell school, which loads the spells for that 
    	school }
    procedure ListViewAvailableSchoolsClick(Sender: TObject);
    
    { Handle custom data-dependent formatting of spells in the spell list }
    procedure ListViewSpellsKnownCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
      
    { Handle custom data-dependent formatting of spells in the spell list }
    procedure ListViewSpellsKnownCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
      
    { Handle animations }
    procedure TimerCycleColoursTimer(Sender: TObject);
    
    { Drop an item by drag-dropping }
    procedure ImageDropDragTargetDragDrop(Sender, Source: TObject; X,
      Y: Integer);
      
    { Handle verification of items to drop }
    procedure ImageDropDragTargetDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
      
    { Key redirect for update log }
    procedure UpdateLogKeyPress(Sender: TObject; var Key: Char);
    
    { Click on save and quit menu item }
    procedure SaveandQuit1Click(Sender: TObject);
    
    { Click on quit (suicide) menu item }
    procedure ImageQuitClick(Sender: TObject); 
    
    { Monster AI Routines }
    procedure SimplePathPlannerBlockLocation(Sender: TObject; X,
      Y: Integer);
    procedure SimplePathPlannerMapPassability(Sender: TObject; X,
      Y: Integer; var result: Single);
    procedure SimplePathPlannerPrepareMap(Sender: TObject);
    procedure SimplePathPlannerValidLocationCheck(Sender: TObject; X,
      Y: Integer; var result: Boolean);
      
    { Main animation timer }
    procedure TimerAnimateTimer(Sender: TObject);
    
    { Handle the resizing of the main window }
    procedure FormResize(Sender: TObject); 
    
    { System call to allow automatic refresh of the minimap }
    procedure PaintBoxMinimapPaint(Sender: TObject);
    
    { Allow multiple colours in the monster listview }
    procedure ListViewMonstersCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
      
    { Compare routine for the monster listview for sorting }
    procedure ListViewMonstersCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
      
    { Selecting a spell school }
    procedure ListViewAvailableSchoolsSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
      
    { Selecting a spell }
    procedure ListViewSpellsKnownSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
      
    { Drag-and-drop checking of eating drag-and-drop }   
    procedure ImageEatDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
      
    { Handle drag-and-drop of food onto the food button } 
    procedure ImageEatDragDrop(Sender, Source: TObject; X, Y: Integer);
    
    { Handle movement of the cursor }
    procedure ScreenMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
      
    { Menu callback for starting a new game }
    procedure Starta1Click(Sender: TObject);
    
    { Menu callback for continuing a game }
    procedure ContinueanExistingGame1Click(Sender: TObject);
    
    { Menu callback for exiting }
    procedure Exit1Click(Sender: TObject);
    
    { Menu callback for selecting monochrome graphics }
    procedure UseMonochromeASCII1Click(Sender: TObject);
    
    { Menu callback for selecting coloured graphics }
    procedure UseColouredASCII1Click(Sender: TObject);
    
    { Application system start event }
    procedure ApplicationEvents1Activate(Sender: TObject);
    
    { Application system stop event }
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    
    { Menu callback for quitting a game }
    procedure QuitSuicide1Click(Sender: TObject);
    
    { Timer to animate menu on front screen }
    procedure TimerAnimateIntroTimer(Sender: TObject);
    
    { Enable/Disable menus when entering the intro screen }
    procedure TabSheetIntroShow(Sender: TObject);
    
    { Enable/Disable menus when leaving the intro screen }
    procedure TabSheetIntroHide(Sender: TObject);
    
    { Generate a random name in characfer creation }
    procedure ImageGenerateRandomNameClick(Sender: TObject);
    
    { Generate a random background in characfer creation }
    procedure ImageGenerateBackgroundClick(Sender: TObject);
    procedure TimerDelayTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Character used in character creation }
    NewCharacter: TCreature;

    { Stores Details of current game }
    Game: TGame;

    { Animation Counter used to handle the main menu animation on the intro }
    AnimateCounter: Integer;

    { Start a new game or load an old game }
    procedure Initialise(CharName: String; NewGame: Boolean = True);

    { Display a suitable welcome message }
    procedure DisplayWelcomeMessage(NewGame: Boolean = True);

    { Main Display Routine }
    function DrawDungeonToSurface(Destination: TRect; PaintBox: TPaintBox;
      Animate: Boolean = False): Boolean;

    { Draw a character to the display }
    procedure DrawCharacterToSurface(Destination: TRect; PaintBox: TPaintBox;
      Location: TPoint; Character: Char; Color: TColor);

    { Switch between different types of display }
    procedure ChangeDrawMode(NewDrawMode: Integer; RedrawDungeon:
      Boolean = True);

    { Update the character info panel }
    procedure UpdateStatus;

    { Redisplay the minimap }
    procedure RefreshMiniMap;

    { Update the active monster list }
    procedure UpdateMonsterList;

    { Kill the character }
    procedure PlayerDie(KilledBy: TMonster);

    { Draw a health bar on a monster }
    procedure DrawHealthBar(Canvas: TCanvas; X: Integer; Y: Integer; MaxHP:
      Integer; CurrentHP: Integer);

    { Draw a con-indicator on a monster }
    procedure DrawMonsterCon(Canvas: TCanvas; X: Integer; Y: Integer;
      LocalMonster: TMonster);

    { Public declarations }
  end;

var
  { Form Variable }
  FormDisplay: TFormDisplay;

  { Offscreen Surfaces used in double-buffering the display }
  BufferRect: TRect;
  MainRect: TRect;
  InventoryRect: TRect;
  SkillRect: TRect;
  SourceRect: TRect;
  MagicRect: TRect;
  SquareRect: TRect;
  Buffer: TBitmap;
  MinimapClientRect: TRect;

  { Miscellaneous variables used in display }
  Zoom: Integer;
  BaseX: Integer;
  BaseY: Integer;
  MiddleX: Integer;
  MiddleY: Integer;

  { Cursor Handling }
  CursorPosition: array[0..5] of TPoint;
  NewCursorPosition: array[0..5] of TPoint;
  CursorPos: TPoint;
  MousePos: TPoint;
  
implementation

uses UnitEngine, UnitVersion, UnitWizard, UnitSplash, UnitHiScores,
  UnitOtherFuncs;

{$R *.dfm}

{ TFormDisplay }

{ Display a suitable welcome message }
procedure TFormDisplay.DisplayWelcomeMessage(NewGame: Boolean = True);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.DisplayWelcomeMessage()');

  try
    { Display a message welcoming the player and giving instructions for help }
    if NewGame then
    begin
      UnitEngine.UpDateLog('Welcome to Kharne, ' + Game.Player.Name,
        mesDefault);
      UnitEngine.UpDateLog('Press X at any time for a summary of Commands',
        mesDefault);
    end
    else
      UnitEngine.UpDateLog('Welcome back to Kharne, ' + Game.Player.Name
        + '...', mesDefault);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Start a new game or load an old game. Until such time that we have saving and
  loading implemented, these two scenarios will be identifical }
procedure TFormDisplay.Initialise(CharName: String; NewGame: Boolean = True);
var
  SaveDirectory: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.Initialise()');

  { Reset the title bar }
  FormDisplay.Caption := ' Kharne ' + GetVersion;


  { TODO: This routine will have to be extensively rewritten when saving/loading
    is implemented }
  try
    { Check for a valid character file extension }
    if Pos('.CHR', CharName) = 0 then
      CharName := CharName + '.CHR';

    { Get the character file save directory }
    SaveDirectory := ExtractFilePath(Application.ExeName) + 'save\';

    { Clear the message log }
    FormDisplay.UpdateLog.Clear;

    { If we are starting a new game }
    if NewGame = True then
    begin
      FormDisplay.Game := TGame.Create(True, SaveDirectory + CharName);
      FormDisplay.Game.GameNew := NewGame;
    end
    else
    { Load a previous game }
    begin
      FormDisplay.Game := TGame.Create(False, SaveDirectory + CharName);
      FormDisplay.Game.GameNew := NewGame;
    end;

    { Swap to the main screen }
    PageControlMain.ActivePage := TabSheetDisplay;

    { Default mode is coloured ASCII }
    ChangeDrawMode(DASCIICOLOURED, False);

    { Set the character visibility }
    FormDisplay.Game.Dungeon.SetVisible(FormDisplay.Game.PlayerX,
      FormDisplay.Game.PlayerY, FormDisplay.Game.Player.Alertness);

    { Refresh the display }
    DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);

    { Start the keyboard timer }
    Timer.Enabled := True;

    { Start the animation timer }
    TimerAnimate.Enabled := True;

    { Display the welcome message }
    FormDisplay.DisplayWelcomeMessage(FormDisplay.Game.GameNew);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Switch between different types of display }
procedure TFormDisplay.ChangeDrawMode(NewDrawMode: Integer; RedrawDungeon: Boolean = True);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ChangeDrawMode()');

  try
    { set the drawmode }
    FormDisplay.Game.CurrentDrawMode := NewDrawMode;

    { Set the view boundaries }
    FormDisplay.Game.SetViewBoundaries(ScreenMain.Width, ScreenMain.Height);

    { Set the viewport }
    FormDisplay.Game.SetViewPort;

    { Check to make sure we have the screen visible to redraw }
    if Assigned(FormDisplay) then
      if RedrawDungeon then
        DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Save and quit }
procedure TFormDisplay.ImageSaveAndExitClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageSaveAndExitClick()');

  try
    { Stop the timers }
    Timer.Enabled := False;
    TimerAnimate.Enabled := False;

    { Close the main window }
    ModalResult := mrOK;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Display refresh }
procedure TFormDisplay.ScreenMainPaint(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.ScreenMainPaint()');

  try
    DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Main keyboard handler - this checks keyboard for keys pressed }
procedure TFormDisplay.TimerTimer(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.TimerTimer()');

  try
    { Occasionally we want to block keyboard input completely, for example,
      whenever the application is not the active window, etc }
    if not(BlockKeyBoardInput) then
      HandleKeyboardInput(KeyboardHandler);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Draw a character to the display }
procedure TFormDisplay.DrawCharacterToSurface(Destination: TRect; PaintBox:
  TPaintBox; Location: TPoint; Character: Char; Color: TColor);
var
  OutX: Integer;
  OutY: Integer;
  TextColor: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.DrawCharacterToSurface()');

  try
    { Translate the input coordinates to screen coordinates }
    OutX := (Location.X - FormDisplay.Game.topleftx) * ScalingFactorX;
    OutY := PaintBox.Height - ((Location.Y - FormDisplay.Game.bottomlefty) *
      ScalingFactorY);

    { Default colour }
    TextColor := clWhite;

    { Check to see what colour we want }
    case FormDisplay.Game.CurrentDrawMode of
      DGRAPHICS:
      begin
        { Do nothing for now }
      end;
      DASCIIStandard:
      begin
        TextColor := clWhite;
      end;
      DASCIICOLOURED:
      begin
        TextColor := Color;
      end;
    end;

    { Set the pen properties }
    PaintBox.Canvas.Font.Name := FontName;
    PaintBox.Canvas.Font.Size := FontSize;
    PaintBox.Canvas.Font.Color := TextColor;

    { Output the character }
    PaintBox.Canvas.TextOut(OutX, OutY, Character);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Main Display Routine }
function TFormDisplay.DrawDungeonToSurface(Destination: TRect; PaintBox: TPaintBox; Animate: Boolean = False): Boolean;
var
  LocalItem: TItem;
  LocalImageList: TImageList;
  LocalMonster: TMonster;
  EffectColor: TColor;
  EffectID: Integer;
  FadeRequired: Boolean;
  Loop: Integer;
  MonsterCount: Integer;
  MonsterID: Integer;
  ObjectID: Integer;
  OutX: Integer;
  OutY: Integer;
  PX: Integer;
  PY: Integer;
  X: Integer;
  Y: Integer;
  Terrain: Integer;
  TextColor: Integer;
  Theme: Integer;
  Visible: Integer;
  Walkable: Boolean;
  Zone: Integer;
  Distance: Integer;
  ColorStep: Integer;
  PLoc: TPoint;
  ElapsedTime: Double;
  Projectile: Char;
  ProjectileColour: TColor;
  XLoc: Integer;
  YLoc: Integer;
  StatusText: WideString;
  Extent: TSize;
  PercentHealth: Double;
  Quartile: Integer;
  FontColour: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.DrawDungeonToSurface()');

	{ Default result }
	Result := False;
	
  try
		{ Set the default text and effect colours }
		TextColor := clWhite;
		EffectColor := clWhite;

		{ Only update the display if we have the display displayed }
		if PageControlMain.ActivePage = TabSheetDisplay then
		begin
			{ Fade is no longer used, but the functionality has been left in }
			
			{ First of all, do we want to fade all text down if we have an inventory
			  display floating on front, for example }
			FadeRequired := PaintBox.Name <> ScreenMain.Name;

			{ Work out the gradient for brightness vs distance from the player }
			ColorStep := Trunc((FormDisplay.Game.Player.Alertness * 10 / 
				FormDisplay.Game.Player.Alertness));

			{ Initialise visible monsters array }
			MonsterCount := 0;
			for Loop := Low(FormDisplay.Game.CurrentMonsters) to 
				High(FormDisplay.Game.CurrentMonsters) do
				FormDisplay.Game.CurrentMonsters[Loop] := NO_MONSTER;

			{ Screenbuffer is the background surface that everything is drawn to
			  before being blitted onto the screen }
			  
			{ Initialise the background surface }
			ScreenBuffer.Canvas.Brush.Color := clBlack;
			ScreenBuffer.Canvas.FillRect(BufferRect);

			{ Set the surface properties }
			ScreenBuffer.Canvas.Font.Name := FontName;
			ScreenBuffer.Canvas.Font.Size := FontSize;
			ScreenBuffer.Canvas.Font.Style := [];
			
			{ For every map cell within the "viewport"... }
			for X := FormDisplay.Game.topleftx to FormDisplay.Game.toprightx do
			begin
				for Y := FormDisplay.Game.bottomlefty to FormDisplay.Game.toplefty do
				begin
					{ Paranoia checks }				
					if X < 1 then 
						continue;
					if X > DUNGEONSIZEX then 
						continue;
					if Y < 1 then 
						continue;
					if Y > DUNGEONSIZEY then 
						continue;

					{ Get the cell properties and work out what to display }
					Visible := FormDisplay.Game.Dungeon.Visible[X, Y];
					ObjectID := FormDisplay.Game.Dungeon.Objects[X, Y];
					EffectID := FormDisplay.Game.Dungeon.Effects[X, Y];
					MonsterID := FormDisplay.Game.Dungeon.Monsters[X, Y];
					Zone := FormDisplay.Game.Dungeon.Zone[X, Y];
					Terrain := FormDisplay.Game.Dungeon.Terrain[X, Y];
					Walkable := FormDisplay.Game.Dungeon.Walkable[X, Y];
          Projectile := FormDisplay.Game.Dungeon.Projectiles[X, Y];
          ProjectileColour := FormDisplay.Game.Dungeon.ProjectilesColour[X, Y];
					
					{ Distance from the character to the cell is used tor the lighting
					  gradient, and is a simple Euclidean distance }
					Distance := Dist(Point(FormDisplay.Game.PlayerX, 
						FormDisplay.Game.PlayerY), Point(X, Y));

					{ If the cell is currently visible, i.e. within the character's 
					  current FOV }
					if Visible = CURRENTLY_VISIBLE then
					begin
						{ Calculate Scaling from Dungeon Coordinates to Screen 
							Coordinates }
						OutX := (X - FormDisplay.Game.topleftx) * ScalingFactorX;
						OutY := PaintBox.Height - ((Y - FormDisplay.Game.bottomlefty) * 
							ScalingFactorY);
							
						{ Zones are like mini-instances of other types of levels so we
						  need to override the colours etc if the cell is part of a zone }
						if (Zone > 0) and (Zone < Z_VAULT) then 
							Theme := Zone 
						else 
							Theme := FormDisplay.Game.Dungeon.LevelTheme;

						{ Examine the graphics mode selected }
						case FormDisplay.Game.CurrentDrawMode of
							DGRAPHICS:
							begin
								{ Do nothing for now }
							end;
							DASCIIStandard:
							begin
								{ Monochrome ASCII - the display routine for this is much much
								  simpler than with coloured ASCII }
								  
								{ Handle fading }
								if FadeRequired then
								begin
									TextColor := FADE_COLOUR;
									EffectColor := FADE_COLOUR;
								end;
								
								{ Since we only have effectively one layer, we draw things in
								  order of priorty: Monsters then Items then Effects then
                  Terrain }
								if MonsterID > 0 then
								begin
									{ We have a monster in this cell }
									ScreenBuffer.Canvas.Font.Color := TextColor;
									
									{ Get the monster }
                  if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
                    LocalMonster := (GTownsPeopleList.Items[MonsterID] as
                      TMonster)
                  else
									  LocalMonster := (GMonsterList.Items[MonsterID] as TMonster);

									{ Handle monsters who haven't noticed the player yet }
									if not(LocalMonster.Awake) then
										ScreenBuffer.Canvas.Font.Color := clGray;

									{ Output the monster character to the screen }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, LocalMonster.Char);

									{ Return the font to the standard size and style }
									ScreenBuffer.Canvas.Font.Style := [];

									{ Add the monster we've just drawn to the current monster
										list }
									FormDisplay.Game.CurrentMonsters[MonsterCount] := MonsterID;
									Inc(MonsterCount);
								end
								else if ObjectID > 0 then
								begin
									{ We have an item here, but no monster }
									ScreenBuffer.Canvas.Font.Color := TextColor;

									{ Handle money first }
									if ObjectID >= ITEM_GOLD then
										ScreenBuffer.Canvas.TextOut(OutX, OutY, CHAR_GOLD)
									else
									{ None-currency items }
									begin
										{ Get the item }
										LocalItem := (GItemList.Items[ObjectID] as TItem);

										{ Output the item's character }
										ScreenBuffer.Canvas.TextOut(OutX, OutY,
											LocalItem.ItemSymbol);

										{ Deal with multiple items on the same tile }
										if LocalItem.NextItem <> NO_NEXT_ITEM then
										begin
											{ Add the multiple item symbol }
											ScreenBuffer.Canvas.Font.Size := FontSize div 2;
											ScreenBuffer.Canvas.Font.Color := clWhite;
											ScreenBuffer.Canvas.TextOut(OutX + ScalingFactorX div 2,
												OutY + ScalingFactorY div 2, '+');
											ScreenBuffer.Canvas.Font.Size := FontSize;
										end;
									end;
								end
								else if EffectID > 0 then
								begin
									{ We have a special effect but no items or monsters }
									if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
										ScreenBuffer.Canvas.Font.Color := Darker(EffectColor, Distance * ColorStep)
									else
										ScreenBuffer.Canvas.Font.Color := EffectColor;
										
									{ Output the effect character, which varies across dungeons }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, 
										GDungeonEffectArray[Theme]);
								end
								else
								begin
									{ No effect, no item or no monster so just drawn the 
									  terrain }
									if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
									begin
										{ Fade the terrain according to distance from the
										  character }
										ScreenBuffer.Canvas.Font.Color := Darker(TextColor, 
											Distance * ColorStep);
											
										{ Check for the presence of a zone }
										if (Zone > 0) and (Zone < Z_VAULT) and 
											((FormDisplay.Game.Dungeon.Terrain[X, Y] = 
											(T_FLOOR_ROOM)) or 
											(FormDisplay.Game.Dungeon.Terrain[X, Y] = 
											(T_FLOOR_CORRIDOR))) then
                      { Output the character }
											ScreenBuffer.Canvas.TextOut(OutX, OutY, 
												GDungeonEffectArray[Terrain])
										else
											{ Output the character }
											ScreenBuffer.Canvas.TextOut(OutX, OutY, 
												GStandardASCII[Terrain])
									end
									else
									begin
										{ With the town level, the symbols are different }
										ScreenBuffer.Canvas.TextOut(OutX, OutY, 
											GStandardTownASCII[Terrain]);
										ScreenBuffer.Canvas.Font.Color := TextColor;
									end;
								end;

                { Also draw projectiles as well }
                if Projectile <> ' ' then
                begin
                  { We have a projectile in this cell }
									ScreenBuffer.Canvas.Font.Color := clSilver;

                  	{ Output the projectile character to the screen }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, Projectile);

									{ Return the font to the standard size and style }
									ScreenBuffer.Canvas.Font.Style := [];
                end;
							end;
							DASCIICOLOURED:
							begin
								{ Coloured ASCII }
								
								{ Set the buffer properties }
								ScreenBuffer.Canvas.Font.Name := FontName;
								ScreenBuffer.Canvas.Font.Size := FontSize;
								ScreenBuffer.Canvas.Font.Color := clWhite;
								
								{ Since we only have effectively one layer, we draw things in
									order of priorty: Monsters then Items then Effects then
                  Terrain }
                if MonsterID > 0 then
								begin
									{ We have a monster in this cell }
									ScreenBuffer.Canvas.Font.Color := TextColor;
									
									{ Get the monster }
                  if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
                    LocalMonster := (GTownsPeopleList.Items[MonsterID] as
                      TMonster)
                  else
									  LocalMonster := (GMonsterList.Items[MonsterID] as TMonster);
									
									{ Set the monster icon colour }
									if FadeRequired then 
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
									else 
										ScreenBuffer.Canvas.Font.Color := LocalMonster.Colour;
										
									{ Unique monsters are larger }
									if Length(Trim(LocalMonster.UniqueName)) > 0 then
										ScreenBuffer.Canvas.Font.Size := FontSize + 2;

									{ Handle monsters who haven't noticed the player yet }
									if not(LocalMonster.Awake) then
										ScreenBuffer.Canvas.Font.Color := clGray;

									{ Output the monster character to the screen }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, LocalMonster.Char);

									{ Return the font to the standard size and style }
									ScreenBuffer.Canvas.Font.Style := [];
									ScreenBuffer.Canvas.Font.Size := FontSize;

									{ Draw a health bar above the monster - but only if
									  injured }
									DrawHealthBar(ScreenBuffer.Canvas, OutX, OutY +
                    ScalingFactorX, LocalMonster.MaxHP, LocalMonster.CurrentHP);

                  { Draw monster con indicator }
                  DrawMonsterCon(ScreenBuffer.Canvas, OutX, OutY +
                    ScalingFactorX, LocalMonster);

									{ Add the monster we've just drawn to the current monster
										list }
									FormDisplay.Game.CurrentMonsters[MonsterCount] := MonsterID;
									Inc(MonsterCount);
								end
								{ We have an item here, but no monster }
								else if ObjectID > 0 then
								begin								
									{ Handle money first }
									if ObjectID >= ITEM_GOLD then
									begin
										{ Get the colours for the different types of currency }
										if ObjectID = ITEM_GOLD then
											ScreenBuffer.Canvas.Font.Color := clYellow
										else if ObjectID = ITEM_SILVER then
											ScreenBuffer.Canvas.Font.Color := clSilver
										else if (ObjectID = ITEM_BRONZE) then
											ScreenBuffer.Canvas.Font.Color := $00004080;

										{ Handle fades }
										if FadeRequired then
											ScreenBuffer.Canvas.Font.Color := FADE_COLOUR;

										{ Output the currency symbol }
										ScreenBuffer.Canvas.TextOut(OutX, OutY, CHAR_GOLD);
									end
									else
									{ None-currency items }
									begin
										{ Get the item }
										LocalItem := (GItemList.Items[ObjectID] as TItem);

										{ Get the item's colour }
										if FadeRequired then 
											ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
										else 
											ScreenBuffer.Canvas.Font.Color := LocalItem.TextColour;

										{ Override this for Potions, Food and Scrolls }
										if LocalItem.ItemType = iPotion then
											ScreenBuffer.Canvas.Font.Color := 
												GPotionColourArray[LocalItem.Position]
										else if LocalItem.ItemType = iConsumable then
											ScreenBuffer.Canvas.Font.Color := LocalItem.ItemColour
										else if LocalItem.ItemType = iScroll then
											ScreenBuffer.Canvas.Font.Color := clWhite;
											
										{ Handle fades }	
										if FadeRequired then
											ScreenBuffer.Canvas.Font.Color := FADE_COLOUR;

										{ Output the item's character }
										ScreenBuffer.Canvas.TextOut(OutX, OutY, 
											LocalItem.ItemSymbol);

										{ Deal with multiple items on the same tile }
										if LocalItem.NextItem <> NO_NEXT_ITEM then
										begin
											{ Add the multiple item symbol }
											ScreenBuffer.Canvas.Font.Size := FontSize div 2;
											ScreenBuffer.Canvas.Font.Color := clWhite;
											ScreenBuffer.Canvas.Brush.Style := bsClear;
											ScreenBuffer.Canvas.TextOut(OutX + 2 + ScalingFactorX
                        div 2, OutY - 2, '+');
											ScreenBuffer.Canvas.Font.Size := FontSize;
										end;
									end
								end
								{ We have a special effect but no items or monsters }
								else if EffectID > 0 then
								begin
                  begin
                    { Handle fading }
                    if FadeRequired then
                      ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
                    { Handle stealth }
                    else if Game.Player.Has(STEALTHED) then
                      ScreenBuffer.Canvas.Font.Color := STEALTH_COLOUR
                    { Handle blindness }
                    else if Game.Player.Has(BLINDED) then
                      ScreenBuffer.Canvas.Font.Color := BLIND_COLOUR
                    { Handle rage }
                    else if Game.Player.Has(ENRAGED) then
                      ScreenBuffer.Canvas.Font.Color := RAGE_COLOUR;

										{ Work out what type of effect is present }
										case EffectID of
                      E_MINERAL:
                      begin
                        { Output the standard terrain character but in a
                          different colour }
                        ScreenBuffer.Canvas.Font.Color := clYellow;
												ScreenBuffer.Canvas.TextOut(OutX, OutY, 
													GColouredASCII[Terrain]);
                      end;
											E_SPECIALEFFECT:
											begin
												{ Check for animation }
												if Animate then 
													ScreenBuffer.Canvas.Font.Color :=
														GetRelatedColor(GASCIIColours[Theme, Terrain], $40)
												else 
													ScreenBuffer.Canvas.Font.Color := 
														Darker(GASCIIColours[Theme, Terrain], 
														Distance * ColorStep);
														
												{ Output the effect character, which varies across 
													dungeons }
												ScreenBuffer.Canvas.TextOut(OutX, OutY, 
													GDungeonSpecialEffectArray[Theme]);
											end;
											E_STANDARDEFFECT:
											begin
												{ Get the effect colour }
												ScreenBuffer.Canvas.Font.Color := 
													Darker(SlightlyLight(GASCIIColours[Theme, Terrain]), 
													Distance * ColorStep);
													
												{ Output the effect character, which varies across 
													dungeons }
												ScreenBuffer.Canvas.TextOut(OutX, OutY, 
													GDungeonSpecialEffectArray[Theme]);
											end;
											E_DARKEREFFECT:
											begin
												{ Get the effect colour }
												ScreenBuffer.Canvas.Font.Color := 
													Darker(GDungeonDarkerEffectColourArray[Theme], 
													Distance * ColorStep);
													
												{ Output the effect character, which varies across 
													dungeons }
												ScreenBuffer.Canvas.TextOut(OutX, OutY,
													GDungeonSpecialEffectArray[Theme]);
											end;
											E_GROUNDEFFECT:
											begin
												{ Get the effect colour }
												ScreenBuffer.Canvas.Font.Color := 
													Darker(GDungeonGroundEffectColourArray[Theme], 
													Distance * ColorStep);
													
												{ Output the standard terrain character }
												ScreenBuffer.Canvas.TextOut(OutX, OutY, 
													GDungeonGroundEffectArray[Terrain]);
											end;
											E_LIGHTSQUARE:
											begin
												{ Get the effect colour }
												ScreenBuffer.Canvas.Font.Color := 
													Darker(Light(GASCIIColours[Theme, Terrain]), 
													Distance * ColorStep);
													
												{ Output the standard terrain character }
												ScreenBuffer.Canvas.TextOut(OutX, OutY, 
													GColouredASCII[Terrain]);
											end;
										end;
									end;
								end
								else
								{ No effect, no item or no monster so just drawn the terrain }
								begin
									{ The town level uses different ASCII character }
									if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
									begin
										{ Handle fade }
										if FadeRequired then
											ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
                    { Handle stealth }
									  else if Game.Player.Has(STEALTHED) then
                      ScreenBuffer.Canvas.Font.Color := STEALTH_COLOUR
                    { Handle blindness }
									  else if Game.Player.Has(BLINDED) then
                      ScreenBuffer.Canvas.Font.Color := BLIND_COLOUR
                    { Handle rage }
                    else if Game.Player.Has(ENRAGED) then
                      ScreenBuffer.Canvas.Font.Color := RAGE_COLOUR
									  else
										begin
											{ TODO: not sure why this is here - all this code is the
											  same is it not? }
											  
											{ Fade the terrain according to distance from the
										  character }
											if FormDisplay.Game.Dungeon.LevelTheme = D_ABYSS then
												ScreenBuffer.Canvas.Font.Color := 
													Darker(GASCIIColours[Theme, Terrain], Distance * 
													ColorStep)
											else if FormDisplay.Game.Dungeon.LevelTheme = 
												D_WILDERNESS then
											begin
												if Walkable then 
													ScreenBuffer.Canvas.Font.Color := 
														Darker(GASCIIColours[Theme, Terrain], 
															Distance * ColorStep)
												else 
													ScreenBuffer.Canvas.Font.Color := 
														Darker(GASCIIColours[Theme, Terrain], 
															Distance * ColorStep);
											end
											else if (FormDisplay.Game.Dungeon.LevelTheme = D_AIR) or
												(FormDisplay.Game.Dungeon.LevelTheme = D_EARTH) or
												(FormDisplay.Game.Dungeon.LevelTheme = D_FIRE) or
												(FormDisplay.Game.Dungeon.LevelTheme = D_WATER) then
											begin
												if Walkable then 
													ScreenBuffer.Canvas.Font.Color := 
														Darker(GASCIIColours[Theme, Terrain], 
														Distance * ColorStep)
												else 
													ScreenBuffer.Canvas.Font.Color := 
														Darker(GASCIIColours[Theme, Terrain], 
														Distance * ColorStep);
											end
											else 
												ScreenBuffer.Canvas.Font.Color := 
													Darker(GASCIIColours[Theme, Terrain], 
														Distance * ColorStep);
										end;
										
										{ Check for the presence of a zone }
										if (Zone > 0) and (Zone < Z_VAULT) and 
											((Terrain = T_FLOOR_ROOM) or (Terrain = T_FLOOR_CORRIDOR))
                      then
											{ Output the effect character if we have a zone }
											ScreenBuffer.Canvas.TextOut(OutX, OutY, 
												GDungeonEffectArray[Theme])
										else
											{ Output the terrain character }
											ScreenBuffer.Canvas.TextOut(OutX, OutY, 
												GColouredASCII[Terrain]);
									end
									else
									{ Handle Town level terrain }
									begin
										{ Handle fades }
										if FadeRequired then
											ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
										{ Handle animations }
										else if Animate then
										begin
											{ Deal with animations of portals and stars }
											if (FormDisplay.Game.Dungeon.Terrain[X, Y] >
												(D_TILE_CRYPT - 1)) and
												(FormDisplay.Game.Dungeon.Terrain[X, Y] <>
												D_TILE_DO_NOT_USE) then
												ScreenBuffer.Canvas.Font.Color :=
													GetRelatedColor(GASCIITownColours[Terrain], $20)
											else
												ScreenBuffer.Canvas.Font.Color :=
													GASCIITownColours[Terrain];
										end
										else
											{ Find colour of town terrain }
											ScreenBuffer.Canvas.Font.Color :=
												GASCIITownColours[Terrain];
										{ Output the character }
										ScreenBuffer.Canvas.TextOut(OutX, OutY,
                      GColouredTownASCII[Terrain]);
									end;
								end;

                { Also draw the projectiles in all cases }
                if Projectile <> ' ' then
                begin
                  { We have a projectile in this cell }
                  if FadeRequired then
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
									else
										ScreenBuffer.Canvas.Font.Color := ProjectileColour;

                  	{ Output the projectile character to the screen }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, Projectile);

									{ Return the font to the standard size and style }
									ScreenBuffer.Canvas.Font.Style := [];
                end
							end;
						end;
					end
					{ Previously visible terrain is simplier - only output the terrain
					  character, no items, no monsters, no objects }
					else if Visible = PREVIOUSLY_VISIBLE then
					begin
						{ Calculate Scaling from Dungeon Coordinates to ScreenCoordinates }
						OutX := (X - FormDisplay.Game.topleftx) * ScalingFactorX;
						OutY := Paintbox.Height - ((Y - FormDisplay.Game.bottomlefty) *
							ScalingFactorY);

						{ Get the terrain to display for this cell }
						Terrain := FormDisplay.Game.Dungeon.Terrain[X, Y];

						{ Handle zones overriding colours }
						if (Zone > 0) and (Zone < Z_VAULT) then
							Theme := Zone
						else
							Theme := FormDisplay.Game.Dungeon.LevelTheme;

						{ Examine the graphics mode selected }
						case FormDisplay.Game.CurrentDrawMode of
							dGraphics:
							begin
								{ Do nothing for now }
							end;
							dASCIIStandard:
							begin
								{ Monochrome ASCII - the display routine for this is much much
								  simpler than with coloured ASCII }

								{ Handle fading }
								if FadeRequired then
									ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
								else
									ScreenBuffer.Canvas.Font.Color := $005B5B5B;

								{ Output the character which may be different on the town
								  level }
								if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
									ScreenBuffer.Canvas.TextOut(OutX, OutY,
										GStandardASCII[Terrain])
								else
									ScreenBuffer.Canvas.TextOut(OutX, OutY,
										GStandardTownASCII[Terrain]);
							end;
							DASCIICOLOURED:
							begin
								{ Coloured ASCII }

								{ Check for the town level }
								if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
								begin
									{ Handle fading }
									if FadeRequired then
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
                  { Handle stealth }
									else if Game.Player.Has(STEALTHED) then
                    ScreenBuffer.Canvas.Font.Color := STEALTH_COLOUR
                  { Handle blindness }
									else if Game.Player.Has(BLINDED) then
                    ScreenBuffer.Canvas.Font.Color := BLIND_COLOUR
                  { Handle rage }
                  else if Game.Player.Has(ENRAGED) then
                    ScreenBuffer.Canvas.Font.Color := RAGE_COLOUR
                  else
										ScreenBuffer.Canvas.Font.Color :=
											Dark(GASCIIColours[Theme, Terrain]);
									end;

									{ Output the character }
									ScreenBuffer.Canvas.TextOut(OutX, OutY,
										GColouredASCII[Terrain]);
								end
								else
								begin
									{ Handle fading }
									if FadeRequired then
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
									else
										ScreenBuffer.Canvas.Font.Color := $005B5B5B;

                  { Handle stealth }
                  if Game.Player.Has(STEALTHED) then
                    ScreenBuffer.Canvas.Font.Color := STEALTH_COLOUR
									else if Game.Player.Has(BLINDED) then
                    ScreenBuffer.Canvas.Font.Color := BLIND_COLOUR
                  { Handle rage }
                  else if Game.Player.Has(ENRAGED) then
                    ScreenBuffer.Canvas.Font.Color := RAGE_COLOUR;

									{ Output the character }
									ScreenBuffer.Canvas.TextOut(OutX, OutY,
										GColouredTownASCII[Terrain]);
								end;
							end;
          
					end
					{ Handle Magic Mapped cells }
					else if Visible = MAGIC_VISIBLE then
					begin
						{ Calculate Scaling from Dungeon Coordinates to Screen 
							Coordinates }
						OutX := (X - FormDisplay.Game.topleftx) * ScalingFactorX;
						OutY := Paintbox.Height - ((Y - FormDisplay.Game.bottomlefty) * 
							ScalingFactorY);
							
						{ Get the terrain to display }
						Terrain := FormDisplay.Game.Dungeon.Terrain[X, Y];

						{ Override for zones }
						if (Zone > 0) and (Zone < Z_VAULT) then 
							Theme := Zone 
						else 
							Theme := FormDisplay.Game.Dungeon.LevelTheme;

						{ Examine the graphics mode selected }
						case FormDisplay.Game.CurrentDrawMode of
							dGraphics:
							begin
								{ Do nothing for now }
							end;
							dASCIIStandard:
							begin
								{ Handle monochrome ASCII }
								
								{ Handle fading }
								if FadeRequired then 
									ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
								else 
									ScreenBuffer.Canvas.Font.Color := Dark($005B5B5B);
									
								{ Deal with the different characters on the town level }
								if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then 
									ScreenBuffer.Canvas.TextOut(OutX, OutY, 
										GStandardASCII[Terrain])
								else 
									ScreenBuffer.Canvas.TextOut(OutX, OutY, 
										GStandardTownASCII[Terrain]);
							end;
							DASCIICOLOURED:
							begin
								{ Coloured ASCII }
								
								{ Town level has different characters }
								if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
								begin
									{ Handle fading }
									if FadeRequired then
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
									else
										ScreenBuffer.Canvas.Font.Color := 
											Dark(Dark(GASCIIColours[Theme, Terrain]));
											
									{ Output the character }
									ScreenBuffer.Canvas.TextOut(OutX, OutY, 
										GColouredASCII[Terrain]);
								end
								else
								begin
									{ Handle fading }
									if FadeRequired then 
										ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
									else 
										ScreenBuffer.Canvas.Font.Color := $005B5B5B;

                  { Handle stealth }
                  if Game.Player.Has(STEALTHED) then
                    ScreenBuffer.Canvas.Font.Color := STEALTH_COLOUR
                  { Handle blindness }
									else if Game.Player.Has(BLINDED) then
                    ScreenBuffer.Canvas.Font.Color := BLIND_COLOUR
                  { Handle rage }
                  else if Game.Player.Has(ENRAGED) then
                    ScreenBuffer.Canvas.Font.Color := RAGE_COLOUR;

									{ Output the character }
									ScreenBuffer.Canvas.TextOut(OutX, OutY,
										GColouredTownASCII[Terrain]);
								end;
							end;
						end;
					end
				end;
			end;

			{ Find the location to draw the character graphic }
			PLoc := GetPlayerScreenLoc(FormDisplay.Game.CurrentDrawMode,
				Point(ScreenMain.Width, ScreenMain.Height));
			PX := PLoc.X;
			PY := PLoc.Y;
			
			{ Set the font properties for the character graphic }
			ScreenBuffer.Canvas.Brush.Style := bsClear;
			ScreenBuffer.Canvas.Font.Name := FontNamePlayer;
			ScreenBuffer.Canvas.Font.Size := FontSizePlayer;

			{ Handle the different graphic modes }
			case FormDisplay.Game.CurrentDrawMode of
				dGraphics:
				begin
					{ Do Nothing for Now }
				end;
				dASCIIStandard:
				begin
					{ Handle fading }
					if FadeRequired then 
						ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
					else 
						ScreenBuffer.Canvas.Font.Color := clWhite;
						
					{ Output the character graphic }
					ScreenBuffer.Canvas.TextOut(PX, PY, '@');
				end;
				DASCIICOLOURED:
				begin
					{ Handle fading }
					if FadeRequired then 
						ScreenBuffer.Canvas.Font.Color := FADE_COLOUR
					else 
						ScreenBuffer.Canvas.Font.Color := clWhite;
						
					{ Output the character graphic at the correct colour}

          PercentHealth := (FormDisplay.Game.Player.HP /
            FormDisplay.Game.Player.MaxHP) * 100;

          { What quartile is the monster's health currently in? }
          Quartile := Trunc(PercentHealth) div 25;


          { The colour the bar changes depending on health level }
          case Quartile of
            0: FontColour := clRed;
            1: FontColour := $000080FF;
            2: FontColour := clYellow;
            3: FontColour := clWhite;
          else
            FontColour := clWhite;
          end;

          ScreenBuffer.Canvas.Font.Color := FontColour;
					ScreenBuffer.Canvas.TextOut(PX, PY, '@');
				end;
			end;

      ScreenBuffer.Canvas.Font.Name := FontName;

      { Do the Character Statuses }
      XLoc := 8;
      YLoc := ScreenBuffer.Width - 8;
      ScreenBuffer.Canvas.Font.Size := CONDITION_FONT_SIZE;
      ScreenBuffer.Canvas.Font.Style := [fsBold];
      if Game.Player.FoodStatus <> '' then
      begin
        StatusText := Game.Player.FoodStatus;
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := Game.Player.FoodColour;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18,
          Game.Player.FoodStatus);
        Dec(YLoc, 8);
      end;
      if Game.Player.PoisonStatus <> '' then
      begin
        StatusText := Game.Player.PoisonStatus;
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := Game.Player.PoisonColour;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18,
          Game.Player.PoisonStatus);
        Dec(YLoc, 8);
      end;
      if Game.Player.Has(CONFUSED) then
      begin
        StatusText := 'Conf';
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := clYellow;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18, StatusText);
        Dec(YLoc, 8);
      end;
      if Game.Player.Has(BLINDED) then
      begin
        StatusText := 'Blin';
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := clWhite;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18, StatusText);
        Dec(YLoc, 8);
      end;
      if Game.Player.Has(DRAINED) then
      begin
        StatusText := 'Drai';
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := clFuchsia;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18, StatusText);
        Dec(YLoc, 8);
      end;
      if Game.Player.Has(HELD) then
      begin
        StatusText := 'Held';
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Dec(YLoc, Extent.cx);
        ScreenBuffer.Canvas.Font.Color := clRed;
        ScreenBuffer.Canvas.TextOut(YLoc, ScreenBuffer.Height - 18, StatusText);
        Dec(YLoc, 8);
      end;
      if Game.Player.Has(STEALTHED) then
      begin
        StatusText := 'Hide';
        ScreenBuffer.Canvas.Font.Color := clWhite;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(ENRAGED) then
      begin
        StatusText := 'Rage';
        ScreenBuffer.Canvas.Font.Color := clRed;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(REGENERATING) then
      begin
        StatusText := 'Rege';
        ScreenBuffer.Canvas.Font.Color := clLime;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(HASTED) then
      begin
        StatusText := 'Sped';
        ScreenBuffer.Canvas.Font.Color := clPurple;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(MASTERY) then
      begin
        StatusText := 'Mast';
        ScreenBuffer.Canvas.Font.Color := $000080FF;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(REFLEXES) then
      begin
        StatusText := 'Refl';
        ScreenBuffer.Canvas.Font.Color := clAqua;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(SEEINV) then
      begin
        StatusText := 'SeeI';
        ScreenBuffer.Canvas.Font.Color := clSilver;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(FREEA) then
      begin
        StatusText := 'Free';
        ScreenBuffer.Canvas.Font.Color := $00004080;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(RESISTING_FIRE) then
      begin
        StatusText := 'Fire';
        ScreenBuffer.Canvas.Font.Color := clRed;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(RESISTING_EARTH) then
      begin
        StatusText := 'Earth';
        ScreenBuffer.Canvas.Font.Color := $00004080;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(RESISTING_AIR) then
      begin
        StatusText := 'Air';
        ScreenBuffer.Canvas.Font.Color := clWhite;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      if Game.Player.Has(RESISTING_WATER) then
      begin
        StatusText := 'Water';
        ScreenBuffer.Canvas.Font.Color := clBlue;
        ScreenBuffer.Canvas.TextOut(XLoc, ScreenBuffer.Height - 18, StatusText);
        GetTextExtentPoint32W(ScreenBuffer.Canvas.Handle, PWideChar(StatusText),
          Length(StatusText), Extent);
        Inc(XLoc, Extent.cx + 8);
      end;
      ScreenBuffer.Canvas.Font.Style := [];

			{ If we are blitting to the main display }
			if PaintBox.Name = ScreenMain.Name then
			begin
				{ If the display is maximised and the status panel hidden }
				if FormDisplay.PanelInfo.Width = 0 then
				begin
					{ Output status information along the top of the screen }
					ScreenBuffer.Canvas.Font.Size := STATUS_FONT_SIZE;
					ScreenBuffer.Canvas.TextOut(2, 2,
						Format('%s HP %d/%d  MP %d/%d  AC %d  EV %d  SP %d',
						[FormDisplay.Game.Player.Name, FormDisplay.Game.Player.HP,
						FormDisplay.Game.Player.MaxHP, FormDisplay.Game.Player.MP,
						FormDisplay.Game.Player.MaxMP, FormDisplay.Game.Player.AC,
						FormDisplay.Game.Player.EV, FormDisplay.Game.Player.Speed]));
				end;
				
				{ If we are in wizard mode, then also output the current location of
				  the player and the type of terrain}
				if Assigned(FormWizard) then
				begin
					if FormWizard.Visible = True then
					begin
						ScreenBuffer.Canvas.Font.Size := STATUS_FONT_SIZE;
						ScreenBuffer.Canvas.TextOut(ScreenBuffer.Width - 140, 4,
              IntToStr(FormDisplay.Game.PlayerX) + '/' +
              IntToStr(FormDisplay.Game.PlayerY) + ':' +
              IntToStr(Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
                FormDisplay.Game.PlayerY]) + ':' +
              IntToStr(Game.Dungeon.Zone[FormDisplay.Game.PlayerX,
                FormDisplay.Game.PlayerY]));
					end;
				end;

				{ Now output a mode indicator in the top left }
				if FormDisplay.Game.CurrentTargetMode = tgDig then 
					ScreenBuffer.Canvas.TextOut(10, 10, 
						'Digging [direction keys to dig or ESC to cancel]')
				else if FormDisplay.Game.CurrentTargetMode = tgRanged then 
					ScreenBuffer.Canvas.TextOut(10,10, 'Ranged');
			end;

			{ Reset the pen colour }
			PaintBox.Canvas.Pen.Color := clWhite;

			{ Blit the output buffer to the screen }
			PaintBox.Canvas.CopyRect(Destination, ScreenBuffer.Canvas, SourceRect);

			{ Very last graphical thing we do is draw the cursor }
			PaintBox.Canvas.Pen.Width := 1;

      { Check if we're in view of the character, and change the cursor colour
        depending on this }
      if (CursorPos.X <> 0) and (CursorPos.Y <> 0) then
      begin
        if (FormDisplay.Game.Dungeon.Visible[CursorPos.X, CursorPos.Y] = 1) then
          PaintBox.Canvas.Pen.Color := clYellow
        else
          PaintBox.Canvas.Pen.Color := clRed;
      end
      else
        PaintBox.Canvas.Pen.Color := clRed;

      { Redraw the Cursor }
			PaintBox.Canvas.PolyLine([NewCursorPosition[0], NewCursorPosition[1],
				NewCursorPosition[2], NewCursorPosition[3], NewCursorPosition[4]]);

			{ Update Wizard Log Monster Display if displayed }
			if Assigned(FormWizard) then
				if FormWizard.Visible = True then
					FormWizard.RefreshDisplay;

			{ And update the status if the status panel is visible }
			if (FormDisplay.PanelInfo.Width <> 0) then
			begin
				if (FormDisplay.Visible = True) then
				begin
					{ Update the status }
					UpdateStatus;

					{ Refresh the minimap }
					RefreshMiniMap;

					{ Update the visible monsters list }
					UpdateMonsterList;
				end;
			end;

			{ Reset the buffer properties }
      ScreenBuffer.Canvas.Brush.Style := bsSolid;

			{ Flag a successful redraw }
			Result := True;


		end;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Shortcut for switching to graphics mode }
procedure TFormDisplay.ImageDisplayGraphicsClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageDisplayASCIIColouredClick()');
	
  try
  	{ Switch to Graphics }
		if FormDisplay.Game.CurrentDrawMode <> DGRAPHICS then 
			ChangeDrawMode(DGRAPHICS, True);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Shortcut for switching to monochrome ascii mode }
procedure TFormDisplay.ImageDisplayASCIIStandardClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageDisplayASCIIStandardClick()');

  try
  	{ Switch to monochrome ASCII }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIISTANDARD then 
  		ChangeDrawMode(DASCIISTANDARD, True);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Shortcut for switching to coloured ascii mode }
procedure TFormDisplay.ImageDisplayASCIICrawlClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageDisplayASCIIColouredClick()');
	
  try
  	{ Switch to coloured ASCII }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIICOLOURED then 
  		ChangeDrawMode(DASCIICOLOURED, True);
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Form Constructor }
procedure TFormDisplay.FormCreate(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.FormCreate()');
	
  try
		try
			{ Timer controls keyboard handling }
			Timer.Enabled := False;

			{ We're not yet ingame }
			InGame := False;

      { Reduce Flicker }
      // DoubleBuffered := True;

			{ Enable/Disable menus }
			UseMonochromeASCII1.Enabled := InGame;
			UseColouredASCII1.Enabled := InGame;
			Starta1.Enabled := not(InGame);
			ContinueanExistingGame1.Enabled := not(InGame);
			QuitSuicide1.Enabled := InGame;

			{ Don't block keyboard input }
			BlockKeyBoardInput := False;

			{ Reset the front screen animation }
			AnimateCounter := 0;

			{ Animation timers that should only be activated when we are in a game }
			TimerCycleColours.Enabled := False;
			TimerColorReset.Enabled := False;
			TimerAnimate.Enabled := false;
		except
			on E: Exception do MessageDlg('Error setting up timers: ' + LINE_BREAK +
        LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Set up the font }
			if Assigned(FormSplash) and (FormSplash.Visible) then
			begin
				FormSplash.LabelProgress.Caption := 'Configurating System Data....';
				Application.ProcessMessages;
			end;                 

			{ Add the veramono font }
			AddFontResource(PChar(ExtractFilePath(Application.ExeName) +
        'VeraMono.ttf'));
			PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
		except
			on E: Exception do MessageDlg('Error setting up fonts: ' + LINE_BREAK +
        LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Create the global data }
			if Assigned(FormSplash) and (FormSplash.Visible) then
			begin
				FormSplash.LabelProgress.Caption := 'Creating Global Data...';
				Application.ProcessMessages;
			end;
			CreateGlobalData;
		except
			on E: Exception do MessageDlg('Error setting up global data: ' +
        LINE_BREAK + LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Load the global data }
			if Assigned(FormSplash) and (FormSplash.Visible) then
			begin
				FormSplash.LabelProgress.Caption := 'Populating Global Data....';
				Application.ProcessMessages;
			end;
			PopulateGlobalData(True);
		except
			on E: Exception do MessageDlg('Error connecting to the database: ' +
        LINE_BREAK + LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Set up the dungeons }
			if Assigned(FormSplash) and (FormSplash.Visible) then
			begin
				FormSplash.LabelProgress.Caption := 'Configurating User Data....';
				Application.ProcessMessages;
			end;
			SetupDungeons;
		except
			on E: Exception do MessageDlg('Error setting up levels: ' + LINE_BREAK +
        LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Set up default magic }
			if Assigned(FormSplash) and (FormSplash.Visible) then
			begin
				FormSplash.LabelProgress.Caption := 'Configurating Character Data....';
				Application.ProcessMessages;
			end;
			SetupMagic;
		except
			on E: Exception do MessageDlg('Error setting up magic: ' + LINE_BREAK +
        LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Create the wizard form }
			FormWizard := TFormWizard.Create(Self);
			
			{ Don't show it by default }
			FormWizard.Hide;
		except
			on E: Exception do MessageDlg('Error setting up forms: ' + LINE_BREAK +
        LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		try
			{ Set up the window title }
			FormDisplay.Caption := ' Kharne';
			PageControlMain.ActivePage := TabSheetIntro;
			VersionLabel.Caption := 'Alpha Release 24';

			{ Initialise the buffers used for double-buffering the display }
			ScreenBuffer := TBitmap.Create;
			ScreenBuffer.Width := ScreenMain.Width;
			ScreenBuffer.Height := ScreenMain.Height;

			{ Initialise the character display buffer }
			SquareBuffer := TBitmap.Create;
			SquareBuffer.Width := ScalingFactorX;
			SquareBuffer.Height := ScalingFactorY;

			{ Set up the multidrop item list }
			ItemsToDrop := TStringList.Create;

			{ Set up the minimap }
			Buffer := TBitmap.Create;
			Buffer.Width := PaintBoxMiniMap.Width;
			Buffer.Height := PaintBoxMiniMap.Height;
			MinimapClientRect := Rect(0, 0, PaintBoxMiniMap.Width, 
				PaintBoxMiniMap.Height);
			MiddleX := PaintBoxMiniMap.Width div 2;
			MiddleY := PaintBoxMiniMap.Height div 2;
			Zoom := 2;
		except
			on E: Exception do MessageDlg('Error setting up graphics: ' + 
				#10#13#10#13 + E.Message, mtError, [mbOK], 0);
		end;
			
		try
			{ Setup the help screen }
			RichEditHelp.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 
				'info\keys.rtf');
		except
			on E: Exception do
			begin
			 RichEditHelp.Lines.Text := e.Message;
			end;
		end;

		try
			{ Set up the version screen }
			RichEditVersion.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) +
				'docs\version.txt');
		except
			on E: Exception do
			begin
			 RichEditVersion.Lines.Text := e.Message;
			end;
		end;

		try
			{ Setup the display buffering }
			MainRect := Rect(0, 0, ScreenMain.Width, ScreenMain.Height);
			SquareRect := Rect(0, 0, SquareBuffer.Width, SquareBuffer.Height);
			SourceRect := Rect(0, 0, ScreenBuffer.Width, ScreenBuffer.Height);
			BufferRect := Rect(0, 0, ScreenBuffer.Width, ScreenBuffer.Height);
		except
			on E: Exception do MessageDlg('Error configuraing graphics: ' + LINE_BREAK
        + LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		
		try
			{ Set up the image lists for the graphics }
			LoadImageList(ImageListPlayer, 'p_icons.bmp', -1, True);
			LoadImageList(ImageListWeapons, 'i_weapons.bmp', -1, True);
			LoadImageList(ImageListArmour, 'i_armour.bmp', -1, True);
			LoadImageList(ImageListRings, 'i_rings.bmp', -1, True);
			LoadImageList(ImageListAmulets, 'i_amulets.bmp', -1, True);
			LoadImageList(ImageListPotions, 'i_potions.bmp', -1, True);
			LoadImageList(ImageListWands, 'i_wands.bmp', -1, True);
			LoadImageList(ImageListScrolls, 'i_scrolls.bmp', -1, True);
			LoadImageList(ImageListFood, 'i_food.bmp', -1, True);
		except
			on E: Exception do MessageDlg('Error setting up image lists: ' +
        LINE_BREAK + LINE_BREAK + E.Message, mtError, [mbOK], 0);
		end;

		{ Timer controls keyboard handling }
		Timer.Enabled := True;
  except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

 { Handle mouse clicks on the same screen }
procedure TFormDisplay.ScreenMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ScreenMainMouseDown()');
	
  try
  	{ Handle mouse clicks }
  	HandleScreenInput(X, Y, Button);
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Update the character info panel }
procedure TFormDisplay.UpdateStatus;
var
  Temp: String;
  LocalItem: TItem;
  ItemName: String;
  XLoc: Integer;
  Extent: TSize;
  StatusText: WideString;
  SpellSchool: Integer;
  Loop: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.UpdateStatus()');
	
  try
  	{ Update the character name }
		LabelCharacterName.Caption := Game.Player.Name;

		{ Get character race }
		case (Ord(Game.Player.SubRace)) of
			0: Temp := '';
			1: Temp := 'Human ';
			2: Temp := 'Halfling ';
			3: Temp := 'Orc ';
			4: Temp := 'Dwarf ';
			5: Temp := 'Elf ';
		end;
    
    { Update the character short description }
    LabelCharacterClass.Caption := 'L' + IntToStr(Game.Player.Levels) + ' ' +
      Temp + Game.Player.FriendlyClassName;

		{ Setup form caption }
		if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
			Temp := 'Currently exploring Level ' + 
				IntToStr(FormDisplay.Game.Dungeon.LevelDepth) + ' of ' + 
				Trim(FormDisplay.Game.Dungeon.Name)
		else
			Temp := 'Currently exploring ' + Trim(FormDisplay.Game.Dungeon.Name);
			
		{ Add the gametime }
		Temp := Temp + ' (Game Time: ' + GetElapsedGameTime + ')';
		FormDisplay.Caption := Temp;

		{ Display wielded mainhand weapon, if any }
		if Game.Player.Inventory[S_MAINHAND] > 0 then
		begin
			{ Get the item }
			LocalItem := GItemList[Game.Player.Inventory[S_MAINHAND]] as TItem;
			
			{ Get the short name }
			ItemName := Trim(LocalItem.ShortName);
			
			{ Trim the name }
			if Length(ItemName) > MAXIMUM_ITEM_LENGTH then 
				ItemName := Copy(ItemName, 1, MAXIMUM_ITEM_LENGTH) + '...';
				
			{ Set the appropriate caption }	
			LabelMeleeWeapon.Caption := ItemName;
			
			{ Get the item colour. which is dependent upon the item quality }
			if LocalItem.Cursed then 
				LabelMeleeWeapon.Font.Color := clRed
			else
			begin
				{ Wielded items are always identified }
				case (LocalItem.ItemQuality) of
					iWorthless: LabelMeleeWeapon.Font.Color := clSilver;
					iCommon: LabelMeleeWeapon.Font.Color := clWhite;
					iSuperb: LabelMeleeWeapon.Font.Color := clGreen;
					iLegendary: LabelMeleeWeapon.Font.Color := clblue;
					iEpic: LabelMeleeWeapon.Font.Color := $00FF0080;
					iArtifact: LabelMeleeWeapon.Font.Color := $000080FF;
				else
					LabelMeleeWeapon.Font.Color := clSilver;
				end;
			end;
		end
		else
		{ No weapon wielded }
		begin
			LabelMeleeWeapon.Caption := '';
		end;

    { Recalculate spell costs }
    for SpellSchool := SK_FIRE to SK_TRAVEL do
      for Loop := 1 to 8 do
      begin
        { Work out the cost}
        GMagicCost[SpellSchool - SK_FIRE, Loop] :=
          Game.Player.GetSpellCost(Game.Player.Skills[SK_MAGIC],
          Game.Player.Skills[SpellSchool], Loop, Game.Player.Intelligence);                          ;

        { Work out thecasting probability }
        GMagicProbability[SpellSchool - SK_FIRE, Loop] :=
          Game.Player.GetSpellProbability(Game.Player.Skills[SK_MAGIC],
          Game.Player.Skills[SpellSchool], Loop, Game.Player.Intelligence);
      end;

		{ Display wielded mainhand weapon, if any }
		if Game.Player.Inventory[S_RANGED] > 0 then
		begin
			{ Get the item }
			LocalItem := GItemList[Game.Player.Inventory[S_RANGED]] as TItem;
			
			{ Get the short name }
			ItemName := Trim(LocalItem.ShortName);
			
			{ Trim the name }
			if Length(ItemName) > MAXIMUM_ITEM_LENGTH then
				ItemName := Copy(ItemName, 1, MAXIMUM_ITEM_LENGTH) + '...';
				
			{ Set the appropriate caption }		
			LabelRangedWeapon.Caption := ItemName;
			
			{ Get the item colour. which is dependent upon the item quality }
			if LocalItem.Cursed then 
				LabelRangedWeapon.Font.Color := clRed
			else
			begin
				{ Wielded items are always identified }
				case (LocalItem.ItemQuality) of
					iWorthless: LabelRangedWeapon.Font.Color := clSilver;
					iCommon: LabelRangedWeapon.Font.Color := clWhite;
					iSuperb: LabelRangedWeapon.Font.Color := clGreen;
					iLegendary: LabelRangedWeapon.Font.Color := clblue;
					iEpic: LabelRangedWeapon.Font.Color := $00FF0080;
					iArtifact: LabelRangedWeapon.Font.Color := $000080FF;
				else
					LabelRangedWeapon.Font.Color := clSilver;
				end;
			end;
		end
		{ No weapon wielded }
		else
			LabelRangedWeapon.Caption := '';
			
		{ Update turn count caption }
		LabelTurns.Caption := IntToStr(FormDisplay.Game.Turns);

		
		{ Update health bars/mana bars }
		PBHealth.maximum := Game.Player.MaxHP;
		PBHealth.position := Game.Player.HP;
		if Game.Player.MaxMP > 0 then
		begin
			PBMana.maximum := Game.Player.MaxMP;
			PBMana.position := Game.Player.MP;
		end
		else
		begin
			PBMana.maximum := 1;
			PBMana.position := 0;
		end;

		{ Update character attributes/abiilities }
		LabelPHealth.Caption := IntToStr(Game.Player.HP) + '/' + 
			IntToStr(Game.Player.MaxHP);
		LabelPMana.Caption := IntToStr(Game.Player.MP) + '/' + 
			IntToStr(Game.Player.MaxMP);
		LabelPAC.Caption := IntToStr(Game.Player.AC);
		LabelPEV.Caption := IntToStr(Game.Player.EV);
		LabelPSpeed.Caption := IntToStr(Game.Player.Speed);
		LabelPGold.Caption := IntToStr(Game.Player.Gold);
		LabelPExp.Caption := IntToStr(Game.Player.XP) + '/' + 
			IntToStr(Game.Player.NextLevel);

		{ Display selected magic spell, along with cost in mana }
		if Game.Player.SelectedSpellSchool > -1 then
			LabelCurrentSpell.Caption := GMagic[Game.Player.SelectedSpellSchool, 
				Game.Player.SelectedSpell] + ' (' + IntToStr(Game.Player.
				GetSpellCost(Game.Player.Skills[SK_MAGIC], 
				Game.Player.Skills[Game.Player.SelectedSpellSchool + SK_FIRE],
				Game.Player.SelectedSpell, Game.Player.Intelligence)) + ' MP)'
		else
			LabelCurrentSpell.Caption := '';

		{ Display hunger status }
		//LabelHunger.Caption := Game.Player.FoodStatus;
		//LabelHunger.Font.Color := Game.Player.FoodColour;

    { Display poison status }
		//LabelPoison.Caption := Game.Player.PoisonStatus;
		//LabelPoison.Font.Color := Game.Player.PoisonColour;

    { Display other statuses }
    {LabelStealth.Visible := Game.Player.Has(STEALTHED);
    LabelEnraged.Visible := Game.Player.Has(ENRAGED);
    LabelRegenerating.Visible := Game.Player.Has(REGENERATING);
    LabelHasted.Visible := Game.Player.Has(HASTED);
    LabelCombatMastery.Visible := Game.Player.Has(MASTERY);
    LabelDodge.Visible := Game.Player.Has(REFLEXES);
    LabelSeeInvis.Visible := Game.Player.Has(SEEINV);
    LabelFreedom.Visible := Game.Player.Has(FREEA);

    LabelConfusion.Visible := Game.Player.Has(CONFUSED);
    LabelBlinded.Visible := Game.Player.Has(BLINDED);
    LabelDrained.Visible := Game.Player.Has(DRAINED);
    LabelHeld.Visible := Game.Player.Has(HELD);}

  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Standard form destructor }
procedure TFormDisplay.FormDestroy(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.FormDestroy()');
	
  try
		{ Free anything we've created }
		FormWizard.Hide;
		FormWizard.Free;
		FormWizard := nil;
		Buffer.Free;
		Buffer := nil;
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Shortcut for switching to monochrome ascii mode }
procedure TFormDisplay.ImageMonochromeASCIIClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageMonochromeASCIIClick()');
	
  try
  	{ If we're not in monochrome ascii mode switch to it }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIISTANDARD then 
  		ChangeDrawMode(DASCIISTANDARD, True);
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Shortcut for switching to coloured ascii mode }
procedure TFormDisplay.ImageColouredASCIIClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageColouredASCIIClick()');
	
  try
  	{ If we're not in monochrome ascii mode switch to it }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIICOLOURED then 
  		ChangeDrawMode(DASCIICOLOURED, True);
  except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Reset inventory panels back to default colour } 
procedure TFormDisplay.TimerColorResetTimer(Sender: TObject);
var
  ComponentName: String;
  Loop: Integer;
  PanelPicture: TComponent;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.TimerColorResetTimer()');
	
  try
  	{ For every inventory slot }
		for Loop := S_FEET to S_BACK do
		begin
			{ Find the relevant component }
			ComponentName := 'BackPanel' + IntToStr(Loop);
			PanelPicture := FindComponent(ComponentName);
			
			{ Reset the colour back to black }
			(PanelPicture as TPanel).Color := clBlack;
		end;
  
  	{ And disable the timer }
  	TimerColorReset.Enabled := False;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle mouse-over hinting of equipped item slots }
procedure TFormDisplay.LabelItem1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LocalItem: TItem;
  LocalItemIndex: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem1MouseMove()');
	
  try
		{ Highlight the appropriate place we can put this item }
		LocalItemIndex := FormDisplay.Game.Player.
			Inventory[(Sender as TComponent).Tag];
			
		{ Check we have an item }
		if LocalItemIndex > 0 then
		begin
			{ If we have an item, get it }
			LocalItem := GItemList[LocalItemIndex] as TItem;
			
			{ Highlight the destination slot }
			InventoryHighlightDestination(LocalItem);
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle mouse-down hinting of equipped item slots }   
procedure TFormDisplay.LabelItem1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LocalItemIndex: Integer;
  LocalItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem1MouseMove()');
	
  try
  	{ Only respond to the left-mouse button }
		if Button = mbLeft then
		begin
			{ Highlight the appropriate place we can put this item }
			LocalItemIndex := FormDisplay.Game.Player.
				Inventory[(Sender as TComponent).Tag];
				
			{ Check we have an item }	
			if LocalItemIndex > 0 then
			begin
				{ If we have an item, get it }
				LocalItem := GItemList[LocalItemIndex] as TItem;
				
				{ Highlight the destination slot }
				InventoryHighlightDestination(LocalItem);
			end;
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle drag-and-drop of items onto the inventory }
procedure TFormDisplay.LabelItem1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  EndItem: TItem;
  EndItemIndex: Integer;
  EndSlot: Integer;
  SourceItem: TItem;
  SourceItemIndex: Integer;
  SourceSlot: Integer;
  Temp: String;
  TempItem: Integer;
  TempCount: Integer;
  TempLocation: crItemSlot;
  UpdateLogText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem1DragDrop()');
	
  try
  	{ Get the slot of the item we are dragging }
		SourceSlot := (Source as TComponent).Tag;
		
		{ Get the slot of the item we are dragging onto, i.e. the one we want to
		  replace }
		EndSlot := (Sender as TComponent).Tag;
		
		{ Get the IDs of the items }
		SourceItemIndex := FormDisplay.Game.Player.Inventory[SourceSlot];
		EndItemIndex := FormDisplay.Game.Player.Inventory[EndSlot];
		
		{ In case we're doing a swap, temporarily store the end item }
		TempItem := FormDisplay.Game.Player.Inventory[EndSlot];
		TempCount := FormDisplay.Game.Player.InventoryCount[EndSlot];
		
		{ Swap the two items }
		FormDisplay.Game.Player.Inventory[EndSlot] := 
			FormDisplay.Game.Player.Inventory[SourceSlot];
		FormDisplay.Game.Player.InventoryCount[EndSlot] := 
			FormDisplay.Game.Player.InventoryCount[SourceSlot];
		FormDisplay.Game.Player.Inventory[SourceSlot] := TempItem;
		FormDisplay.Game.Player.InventoryCount[SourceSlot] := TempCount;

		{ Refresh the inventory details }
		LoadInventoryDetails(FormDisplay.Game.Player);

		{ Now handle user-feedback on what we've just done }
		if SourceSlot > S_INVENTORY_END then
		begin
			{ If we are taking off a equipped item add a message }
			if (SourceSlot = S_MAINHAND) or (SourceSlot = S_OFFHAND) or
				(SourceSlot = S_RANGED) then
				UpdateLogText := 'You unwield %s'
			else
				UpdateLogText := 'You take off %s';

			{ Get the item we moved }
			SourceItem := GItemList[SourceItemIndex] as TItem;
			
			{ Output an appropriate message }
			Temp := Format(UpdateLogText, [Lower(SourceItem.Name)]);
			UnitEngine.UpdateLog(Temp, mesItemManipulation);

			{ Do unwear effects }
			UnitEngine.UpdateLog(SourceItem.RemoveEffect, mesDefault);

			{ If we have done a swap, then we need to do the same with the other
			  item that we're now wearing/wielding }
			if TempItem > 0 then
			begin	
				{ Get the item we moved }
				EndItem := GItemList[EndItemIndex] as TItem;
				if (SourceSlot = S_MAINHAND) or (SourceSlot = S_OFFHAND) or
				 (SourceSlot = S_RANGED) then
					UpdateLogText := 'You wield %s'
				else
					UpdateLogText := 'You put on %s';
					
				{ Output an appropriate message }	
				Temp := Format(UpdateLogText, [Lower(EndItem.ShortName)]);
				UnitEngine.UpdateLog(Temp, mesItemManipulation);

				{ Do wear effects }
				UnitEngine.UpdateLog(EndItem.WearEffect, mesDefault);
			end;

			{ Swap the locations of the items about }
			if EndItemIndex > 0 then
			begin
				TempLocation := EndItem.Location;
				EndItem.Location := SourceItem.Location;
				SourceItem.Location := EndItem.Location;
			end
			else
				SourceItem.Location := iInventory;
		end
		else
		begin
			{ Deal with moving items between inventory slots }
			if SourceSlot <> EndSlot then
				 UnitEngine.UpdateLog('You reorder some items in your backpack', 
				 	mesItemManipulation);
		end;
		
		{ Force a check of the temporary HP in case the player has died of his 
			own volition by removing a health-giving amulet whilst at low health.}
		Game.Player.TakeDamage(nil, 0);
		
		{ Handle death in these circumstances }
		if Game.Player.HP < 1 then 
			PlayerDie(nil);
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Drag-and-drop checking of inventory drag-and-drop }
procedure TFormDisplay.LabelItem1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  EndItemSlot: crItemSlot;
  EndItemIndex: Integer;
  EndSlot: Integer;
  SourceItemSlot: crItemSlot;
  SourceItemIndex: Integer;
  SourceSlot: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem1DragOver()');
	
  try
		{ Find the original item we want to move }
		SourceSlot := (Source as TComponent).Tag;
		
		{ Check if we have an item already present in the destination slot }
		if FormDisplay.Game.Player.Inventory[SourceSlot] > 0 then
		begin
			{ Check if we are moving an equipped item }
			if SourceSlot > S_INVENTORY_END then
			begin
				{ Get the item }
				SourceItemIndex := FormDisplay.Game.Player.Inventory[SourceSlot];
				
				{ Don't allow cursed items to be unequiped }
				if (GItemList[SourceItemIndex] as TItem).Cursed then
					Accept := False
				else
				begin
					{ Check for moving to an incompatible slot in case of swapping }
					EndSlot := (Sender as TComponent).Tag;
					if FormDisplay.Game.Player.Inventory[EndSlot] > 0 then
					begin
						{ Get the item details }
						EndItemIndex := FormDisplay.Game.Player.Inventory[EndSlot];
						SourceItemSlot := (GItemList[SourceItemIndex] as TItem).ItemSlot;
						EndItemSlot := (GItemList[EndItemIndex] as TItem).ItemSlot;
						
						{ Check that the two items being swapped are the same type }
						if (SourceItemSlot <> EndItemSlot) then
							 Accept := False
						else
							Accept := True;
					end
					else
						{ We're dragging an equipped item to an empty slot so just accept }
						Accept := True;
				end;
			end
			else
				{ We're dragging an inventory item onto another empty slot so accept }
				Accept := True;
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle drag-and-drop of items onto the equipped items slots } 
procedure TFormDisplay.LabelItem32DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  DragSuccess: Boolean;
  EndSlot: Integer;
  EndItem: TItem;
  EndItemCount: Integer;
  EndItemIndex: Integer;
  OtherItem: TItem;
  OtherItemIndex: Integer;
  SourceSlot: Integer;
  SourceItem: TItem;
  SourceItemIndex: Integer;
  Temp: String;
  UpdateLogText: String;
  UpdateLogTextOther: String;
  TempLocation: crItemSlot;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem32DragDrop()');
	
  try
  	{ Setup a flag to indicate if we've been succesful }
		DragSuccess := False;
		
		{ Get the slot of the item we are dragging }
		SourceSlot := (Source as TComponent).Tag;
		
		{ Get the slot of the item we are dragging onto, i.e. the one we want to
		  replace }
		EndSlot := (Sender as TComponent).Tag;
		
		{ Get the IDs of the items }
		SourceItemIndex := FormDisplay.Game.Player.Inventory[SourceSlot];
		EndItemIndex := FormDisplay.Game.Player.Inventory[EndSlot];
		
		{ Get the source item }
		SourceItem := GItemList[SourceItemIndex] as TItem;
		
		{ Get the destination item count }
		EndItemCount := FormDisplay.Game.Player.InventoryCount[EndSlot];

		{ Get the destination item if we are swapping item }
		if EndItemIndex > 0 then
			EndItem := GItemList[EndItemIndex] as TItem
		else
			EndItem := nil;
			
		{ Now handle dragging to each different slot }				
		case EndSlot of
			S_FEET:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iFeet then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_LEGS:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iLegs then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_HANDS:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iHands then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
					
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_ARMS:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iArms then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_CHEST:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iChest then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_HEAD:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iHead then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_NECK:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iNeck then
				begin
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_MAINHAND:
			begin	
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iMainhand then
				begin
					{ If we are trying to wield a 2-handed weapon then check if we have 
					  nothing in the offhand }
					if SourceItem.ItemHandedNess = TWO_HANDED then
					begin
						if FormDisplay.Game.Player.Inventory[S_OFFHAND] = 0 then
						begin
							{ Swap the items }
							FormDisplay.Game.Player.Inventory[EndSlot] := 
								FormDisplay.Game.Player.Inventory[SourceSlot];
							FormDisplay.Game.Player.InventoryCount[EndSlot] := 
								FormDisplay.Game.Player.InventoryCount[SourceSlot];
								
							{ Flag the swap as a success }
							DragSuccess := True;
							
							{ Add a suitable message }
							UpdateLogText := 'You wield %s';
						end;
						{ Else we've not succeeded }
					end
					else
					begin
						{ Swap the items }
						FormDisplay.Game.Player.Inventory[EndSlot] := 
							FormDisplay.Game.Player.Inventory[SourceSlot];
						FormDisplay.Game.Player.InventoryCount[EndSlot] := 
							FormDisplay.Game.Player.InventoryCount[SourceSlot];
							
						{ Flag the swap as a success }
						DragSuccess := True;
						
						{ Add a suitable message }
						UpdateLogText := 'You wield %s';
					end;					
				end;
			end;
			S_OFFHAND:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iOffhand then
				begin
					{ Check we can put something into this slot - we can't if the 
						mainhand has a 2-handed weapon in it }
					OtherItemIndex := 0;
					OtherItem := nil;
					if FormDisplay.Game.Player.Inventory[S_MAINHAND] > 0 then
					begin
						{ Get the item in the mainhand }
						OtherItemIndex := FormDisplay.Game.Player.Inventory[S_MAINHAND];
						OtherItem := GItemList[OtherItemIndex] as TItem;
					end;
					
					{ If there is an item in the mainhand }
					if OtherItemIndex > 0 then
					begin
						{ We're ok to equip if the item in the mainhand is one-handed }
						if OtherItem.ItemHandedNess = ONE_HANDED then
						begin
							{ Swap the items }
							FormDisplay.Game.Player.Inventory[EndSlot] := 
								FormDisplay.Game.Player.Inventory[SourceSlot];
							FormDisplay.Game.Player.InventoryCount[EndSlot] := 
								FormDisplay.Game.Player.InventoryCount[SourceSlot];
								
							{ Flag the swap as a success }
							DragSuccess := True;
						end;
					end
					else
					begin
						{ Swap the items }
						FormDisplay.Game.Player.Inventory[EndSlot] := 
							FormDisplay.Game.Player.Inventory[SourceSlot];
						FormDisplay.Game.Player.InventoryCount[EndSlot] := 
							FormDisplay.Game.Player.InventoryCount[SourceSlot];
							
						{ Flag the swap as a success }
						DragSuccess := True;
					end;
					
					{ Add a suitable message }
					UpdateLogText := 'You wield %s';
				end;
			end;
			S_LEFTFINGER:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iFinger then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_RIGHTFINGER:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iFinger then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
			S_RANGED:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iRanged then
				begin			
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You wield %s';
				end;
			end;
			S_BACK:
			begin
				{ Make sure we have the right item type }
				if SourceItem.ItemSlot = iBack then
				begin
					{ Swap the items }
					FormDisplay.Game.Player.Inventory[EndSlot] := 
						FormDisplay.Game.Player.Inventory[SourceSlot];
					FormDisplay.Game.Player.InventoryCount[EndSlot] := 
						FormDisplay.Game.Player.InventoryCount[SourceSlot];
						
					{ Flag the swap as a success }
					DragSuccess := True;
					
					{ Add a suitable message }
					UpdateLogText := 'You put on %s';
				end;
			end;
		end;
		
		{ If we have equipped an item successfully }
		if DragSuccess = True then
		begin
			{ If this was a swap }
			if EndItem <> nil then
			begin
				{ Generate a message about the item we've removed so this one can be
				  equipped }
				if (EndSlot = S_MAINHAND) or (EndSlot = S_OFFHAND) or 
					(EndSlot = S_RANGED) then
					UpdateLogTextOther := 'You unwield %s'
				else
					UpdateLogTextOther := 'You take off %s';

				{ Add the message }
				Temp := Format(UpdateLogTextOther, [Lower(EndItem.Name)]);
				UnitEngine.UpdateLog(Temp, mesItemManipulation);

				{ Do unwear effects }
				UnitEngine.UpdateLog(EndItem.RemoveEffect, mesDefault);
			end;

			{ Update the inventory }
			FormDisplay.Game.Player.Inventory[SourceSlot] := EndItemIndex;
			FormDisplay.Game.Player.InventoryCount[SourceSlot] := EndItemCount;

			{ If we have a swap, update the items concerned }
			if Assigned(EndItem) then
			begin
				TempLocation := SourceItem.Location;
				SourceItem.Location := EndItem.Location;
				EndItem.Location := TempLocation;
			end
			else
				SourceItem.Location := SourceItem.ItemSlot;

			{ Generate a message about this }
			Temp := Format(UpdateLogText, [Lower(SourceItem.Name)]);
			UnitEngine.UpdateLog(Temp, mesItemManipulation);

			{ Do wear effects }
			UnitEngine.UpdateLog(SourceItem.WearEffect, mesDefault);

			{ Handle on-equip identification }
			if SourceItem.Known = False then
			begin
				SourceItem.Known := True;
				Temp := Format('This is %s!', [Lower(SourceItem.Name)]);
				UnitEngine.UpdateLog(Temp, mesDiscoverMagicItem);
				
				{ If we equip an artifact, then make a note of this }
				if SourceItem.ItemQuality = iArtifact then
					Game.Player.TakeNote(FormDisplay.Game.Turns, SourceItem.Name, nItem, 
						FormDisplay.Game.Dungeon);
			end;
		end
		else
			{ Invalid drag-and-drop }
			Beep;
			
		{ Refresh the inventory }
		LoadInventoryDetails(Game.Player);
		
		
		{ Force a check of the temporary HP in case the player has died of his 
					own volition by removing a health-giving amulet whilst at low health.}
		Game.Player.TakeDamage(nil, 0);
				
		{ Handle death in these circumstances }
		if Game.Player.HP < 1 then 
			PlayerDie(nil);
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Drag-and-drop checking of equiped items drag-and-drop }
procedure TFormDisplay.LabelItem32DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  EndSlot: Integer;
  Slot: Integer;
  EndItemIndex: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.LabelItem32DragOver()');
	
  try
		{ Find the slot we're sending the item to }
		Slot := (Source as TComponent).Tag;
		
		{ If the destination slot is empty then accept }
		if FormDisplay.Game.Player.Inventory[Slot] > 0 then
			Accept := True;

		{ Get the source slot, i.e. the item we're dragging }
		EndSlot := (Sender as TComponent).Tag;
		
		{ If we have a item in the source slot }
		if FormDisplay.Game.Player.Inventory[EndSlot] > 0 then
		begin
			{ Get the item }
			EndItemIndex := FormDisplay.Game.Player.Inventory[EndSlot];
			
			{ Don't allow dragging of cursed iteme }
			if (GItemList[EndItemIndex] as TItem).Cursed then
			 	Accept := False;
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle colouring of spell schools }
procedure TFormDisplay.ListViewAvailableSchoolsCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewAvailableSchoolsCustomDraw' +
  	'SubItem()');
	
  try
  	{ Draw columns different colour depending on the column }
		if SubItem = 0 then
		begin
			ListViewAvailableSchools.Canvas.Font.Color := clRed;
		end
		else if SubItem = 1 then
		begin
			{ TODO: why is this commented out? }
			{SpellIndex := StrToIntDef(Item.SubItems[1], -1);
			if (SpellIndex = -1) then 
				ListViewAvailableSchools.Canvas.Font.Color := clWhite
			else 
				ListViewAvailableSchools.Canvas.Font.Color := 
					GMagicColour[SpellIndex];}
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle clicking of a spell school, which loads the spells for that school }
procedure TFormDisplay.ListViewAvailableSchoolsClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewAvailableSchoolsClick()');
	
  try
  	{ If we have selected a school, display the spells }
		if (Sender as TListView).SelCount <> 0 then
			ShowSchoolSpells(FormDisplay.Game.Player, 
				(Sender as TListView).Selected.SubItems[1]);
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle custom data-dependent formatting of spells in the spell list }
procedure TFormDisplay.ListViewSpellsKnownCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Cost: Integer;
  LoopColor: TColor;
  SpellLevel: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewSpellsKnownCustomDrawSubItem()');
	
  try
  	{ Get the spell cost }
		Cost := StrToIntDef(Item.SubItems[1], -1);
		
		{ If the spell cost is greater than current MP then highlight it 
		  differently than if it costs less }
		if Cost > Game.Player.MP then
		begin
			{ Get the spell level }
			SpellLevel := StrToIntDef(Item.SubItems[3], -1);
			
			{ Change the formatting on a column-by-column basis }
			case SubItem of
				0: LoopColor := clWhite;
				1: LoopColor := clRed;
				2: LoopColor := clRed;
				3: LoopColor := clWhite;
				4: LoopColor := clWhite;
			else
				LoopColor := clRed;
			end;                                            
			ListViewSpellsKnown.Canvas.Font.Color := LoopColor;
		end
		else
		begin
			{ Get the spell level }		
			SpellLevel := StrToIntDef(Item.SubItems[3], -1);
			
			{ Change the formatting on a column-by-column basis }
			case SubItem of
				0: LoopColor := clWhite;
				1: LoopColor := clLime;
				2: LoopColor := clFuchsia;
				3: LoopColor := clWhite;
			else
				LoopColor := clRed;
			end;
			ListViewSpellsKnown.Canvas.Font.Color := LoopColor;
		end;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle custom data-dependent formatting of spells in the spell list }
procedure TFormDisplay.ListViewSpellsKnownCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  Cost: Integer;
  Loop: Integer;
  LoopColor: TColor;
  SpellLevel: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewSpellsKnownCustomDrawItem()');
	
  try
  	{ Get the spell cost }
		Cost := StrToIntDef(Item.SubItems[1], -1);
		
		{ Deal with spell costs }
		if Cost <= Game.Player.MP then
		begin
			{ Get the spell level }	
			SpellLevel := StrToIntDef(Item.SubItems[3], -1);
			
			{ Castable spells are flagged in green }
			LoopColor := clLime;
			for Loop := 0 to SpellLevel do
				LoopColor := Darker(LoopColor, 5);
			ListViewSpellsKnown.Canvas.Font.Color := LoopColor;
		end
		else
		begin
			{ Get the spell level }	
			SpellLevel := StrToIntDef(Item.SubItems[3], -1);
			
			{ Uncastable spells are flagged in red }
			LoopColor := clRed;
			for Loop := 0 to SpellLevel do
			begin
				LoopColor := Darker(LoopColor, 5);
			end;
			ListViewSpellsKnown.Canvas.Font.Color := LoopColor;
		end;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle animations }
procedure TFormDisplay.TimerCycleColoursTimer(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.TimerCycleColoursTimer()');
	
  try
  	{ Switch off the timer }
  	TimerCycleColours.Enabled := False;
  	
  	{ Refresh the display }
  	FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end;  
end;

{ Drop an item by drag-dropping }
procedure TFormDisplay.ImageDropDragTargetDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  DragSuccess: Boolean;
  SourceSlot: Integer;
  SourceItem: TItem;
  SourceItemIndex: Integer;
  Temp: String;
  UpdateLogText: String;
  FloorItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageDropDragTargetDragDrop()');
	
  try
  	{ If we are standing in a shop, we can't drop an item }
		if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) and 
			(FormDisplay.Game.Dungeon.Terrain[FormDisplay.Game.PlayerX,
      FormDisplay.Game.PlayerY] = D_TILE_SHOP) then
		begin
			{ Add an appropriate message }
			DragSuccess := False;
			UpdateLogText := 'The Shopkeeper does not want this item!';
		end
		else if FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
			FormDisplay.Game.PlayerY] > 0 then
		begin
			{ There is already an item on the floor, so get the last item }
			FloorItem := FormDisplay.Game.Dungeon.GetLastItemOnTile(Point
				(FormDisplay.Game.PlayerX, FormDisplay.Game.PlayerY));

			{ We've dropped an item }
			DragSuccess := True;
			
			{ Get the item we're dropping }
			SourceSlot := (Source as TComponent).Tag;
			SourceItemIndex := FormDisplay.Game.Player.Inventory[SourceSlot];
			SourceItem := GItemList[SourceItemIndex] as TItem;
			
			{ Set the item location to the floor }
			(GItemList[SourceItemIndex] as TItem).Location := iFloor;

			{ Reestablish the pointers for the multiple items on the floor }
			FloorItem.NextItem := SourceItemIndex;

			{ Reset any pseudoID progress }
			(GItemList[SourceItemIndex] as TItem).IDCounter := 0;

			{ Remove the item from the inventory }
			FormDisplay.Game.Player.InventoryCount[SourceSlot] := 0;
			FormDisplay.Game.Player.Inventory[SourceSlot] := 0;

			{ Add a suitable message }
			UpdateLogText := 'You drop %s';
			Temp := Format(UpdateLogText, [Lower(SourceItem.Name)]);
			UnitEngine.UpdateLog(Temp, mesItemManipulation);
			
			{ Refresh the inventory }
			LoadInventoryDetails(Game.Player);
			
			{ Pass a turn }
			PassATurn;
			
			{ Go back to the main display }
			FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
			FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
		end
		else
		begin
			{ We've dropped an item }
			DragSuccess := True;
			
			{ Get the item we're dropping }
			SourceSlot := (Source as TComponent).Tag;
			SourceItemIndex := FormDisplay.Game.Player.Inventory[SourceSlot];
			SourceItem := GItemList[SourceItemIndex] as TItem;
			
			{ Set the item location to the floor }
			(GItemList[SourceItemIndex] as TItem).Location := iFloor;

			{ Reset any pseudoID progress }
			(GItemList[SourceItemIndex] as TItem).IDCounter := 0;

			{ Place the item on the floor }
			FormDisplay.Game.Dungeon.Objects[FormDisplay.Game.PlayerX, 
				FormDisplay.Game.PlayerY] := SourceItemIndex;

			{ Remove the item from the inventory }
			FormDisplay.Game.Player.InventoryCount[SourceSlot] := 0;
			FormDisplay.Game.Player.Inventory[SourceSlot] := 0;
			
			{ Add a suitable message }
			UpdateLogText := 'You drop %s';
			Temp := Format(UpdateLogText, [Lower(SourceItem.Name)]);
			UnitEngine.UpdateLog(Temp, mesItemManipulation);
			
			{ Refresh the inventory }
			LoadInventoryDetails(Game.Player);
			
			{ Pass a turn }
			PassATurn;
			
			{ Go back to the main display }
			FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
			FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
		end; 
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end; 
end;

{ Handle verification of items to drop }
procedure TFormDisplay.ImageDropDragTargetDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageDropDragTargetDragOver()');
	
  try
  	{ Automatically allow items to be drag-and-dropped }
  	Accept := True;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end; 
end;

{ Key redirect for update log }
procedure TFormDisplay.UpdateLogKeyPress(Sender: TObject; var Key: Char);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.UpdateLogKeyPress()');
	
  try
		{ Automatically redirect keypresses from the updatelog into \dev\null }
		if PageControlMain.ActivePage =  TabSheetDisplay then 
			PanelDisplay.SetFocus;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end; 
end;

{ Click on save and quit menu item }
procedure TFormDisplay.SaveandQuit1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.SaveandQuit1Click()');
	
  try
  	{ TODO: save the game here obviously }
  
  	{ Close the display window }
  	ModalResult := mrOK;
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end; 
end;

{ Click on quit (suicide) menu item }
procedure TFormDisplay.ImageQuitClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageQuitClick()');
	
  try
  	{ TODO: quit the game here obviously }
  
  	{ Close the display window }
  	ModalResult := mrOK;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end; 
end;

{ Monster AI Routines }
procedure TFormDisplay.SimplePathPlannerBlockLocation(Sender: TObject; X,
  Y: Integer);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.SimplePathPlannerBlockLocation()');
	
  try
  	{ We hold inside the AI component a list of visited cells }
	 	FormDisplay.Game.Visited[X + DUNGEONSIZEX * Y] := True;
	 	
	 	{ Set the cell to visited }
		if not (((FormDisplay.Game.StartCell.X = X) and 
			(FormDisplay.Game.StartCell.Y = Y)) or 
			((FormDisplay.Game.EndCell.X = X) and 
			(FormDisplay.Game.EndCell.Y = Y))) then
			FormDisplay.Game.Dungeon.TerrainCost[X, Y ] := TC_VISITED;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Monster AI Routines }
procedure TFormDisplay.SimplePathPlannerMapPassability(Sender: TObject; X,
  Y: Integer; var result: Single);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.SimplePathPlannerMapPassability()');
	
  try
  	{ Convert a cell cost or cell type into a status value for the aI }
		if (X < 0) or (X >= DUNGEONSIZEX) or (Y < 0) or (Y >= DUNGEONSIZEY) then
			result := 0
		else if (FormDisplay.Game.Dungeon.TerrainCost[X, Y] = TC_VISITED) or 
			(FormDisplay.Game.Visited[X + DUNGEONSIZEY * Y]) then
			result := 0
		else
			result := 1;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Monster AI Routines }
procedure TFormDisplay.SimplePathPlannerPrepareMap(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.SimplePathPlannerPrepareMap()');
	
  try
  	{ Prepare the list of cells used for the AI Pathing }
		if FormDisplay.Game.Visited <> nil then
		begin
			FormDisplay.Game.Visited.Free;
			FormDisplay.Game.Visited := nil;
		end;
		FormDisplay.Game.Visited := TBits.Create;
		FormDisplay.Game.Visited.Size := DUNGEONSIZEX * DUNGEONSIZEY;
  except
		{ in case of error, log the Exception }
		on E: Exception do hLog.AddException(E);
  end;
end;

{ Monster AI Routines }
procedure TFormDisplay.SimplePathPlannerValidLocationCheck(Sender: TObject;
  X, Y: Integer; var result: Boolean);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.SimplePathPlannerValidLocationCheck()');
	
  try
  	{ Check if the cell visited is a valid location }
		if (X < 0) or (X >= DUNGEONSIZEX) or (Y < 0) or (Y >= DUNGEONSIZEY) then
			result := False
		else
			result := not(FormDisplay.Game.Visited[X + DUNGEONSIZEY * Y]);
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Main animation timer }
procedure TFormDisplay.TimerAnimateTimer(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.TimerAnimateTimer()');
	
  try
		{ If we haven't created the main display, disable the timer }
  	if not(Assigned(FormDisplay)) then
			TimerAnimate.Enabled := False
		{ If we are in coloured ASCII mode }
		else  if (FormDisplay.PageControlMain.ActivePageIndex = MAINDISPLAY) and 
			(FormDisplay.Game.CurrentDrawMode = DASCIICOLOURED) then
			{ Redraw the screen }
    	DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain, True);  
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle the resizing of the main window }
procedure TFormDisplay.FormResize(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.FormResize()');
	
  try
  	{ Make sure we can only do this when the main display is visible }
		if Assigned(FormDisplay) then
		begin
			{ Resize the graphics buffer }
			ScreenBuffer.Width := ScreenMain.Width;
			ScreenBuffer.Height := ScreenMain.Height;

			{ Reset the drawing frames to the new dimensions }
			MainRect := Rect(0, 0, ScreenMain.Width, ScreenMain.Height);
			SourceRect := Rect(0, 0, ScreenBuffer.Width, ScreenBuffer.Height);
			BufferRect := Rect(0, 0, ScreenBuffer.Width, ScreenBuffer.Height);
	
			{ Resize and redraw the viewport and the screen if we have a game }
			if Assigned(FormDisplay.Game) then
				ChangeDrawMode(FormDisplay.Game.CurrentDrawMode, True);
		end;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Refresh the minimap }
procedure TFormDisplay.RefreshMiniMap;
var
  X: Integer;
  Y: Integer;
  RectX: Integer;
  RectY: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.RefreshMiniMap()');
	
  try
  	{ Get the player location }
		BaseX := FormDisplay.Game.PlayerX;
		BaseY := FormDisplay.Game.PlayerY;
		
		{ Set the default background of the buffer }
		Buffer.Canvas.Brush.Color := clBlack;
		Buffer.Canvas.FillRect(MinimapClientRect);
		
		{ Don't bother displaying the map whilst in town }
		if FormDisplay.Game.Dungeon.LevelTheme <> D_TOWN then
		begin
			{ TODO: optimise this so it doesn't iterate through the entire dungeon
			  every time }
			for X := 1 to DUNGEONSIZEX do
			begin
				for Y := 1 to DUNGEONSIZEY do
				begin
					{ Check if the cell has been visited or is currently visible }
					if (FormDisplay.Game.Dungeon.Visible[X, Y] <> 0) then
					begin
						{ Select an appropriate colour to represent the terrain }
						case FormDisplay.Game.Dungeon.Terrain[X, Y] of
							T_FLOOR_ROOM: Buffer.Canvas.Brush.Color := ClGray;
							T_FLOOR_CORRIDOR: Buffer.Canvas.Brush.Color := ClGray;
							T_HARDWALL: Buffer.Canvas.Brush.Color := clWhite;
							T_SOFTWALL: Buffer.Canvas.Brush.Color := clWhite;
							T_DOOR_OPEN: Buffer.Canvas.Brush.Color := ClGray;
							T_DOOR_CLOSED: Buffer.Canvas.Brush.Color := clBlue;
							T_STAIRS_DOWN: Buffer.Canvas.Brush.Color := clLime;
							T_STAIRS_UP: Buffer.Canvas.Brush.Color := clAqua;
							T_TERRAIN: Buffer.Canvas.Brush.Color := ClGray;
							T_ARCH: Buffer.Canvas.Brush.Color := ClGray;
							T_SPECIAL: Buffer.Canvas.Brush.Color := ClGray;
						else 
							Buffer.Canvas.Brush.Color := ClBlack;
						end;
						
						{ Show monsters }
						if FormDisplay.Game.Dungeon.Monsters[X, Y] > 0 then 
							Buffer.Canvas.Brush.Color := ClRed;

						{ Work out screen co-ordinates }	
						RectX := Zoom * (X - BaseX) + MiddleX;
						RectY := Buffer.Height - (Zoom * 2 * (Y - BaseY) + MiddleY);

						{ Write the pixels to the minimap }
						Buffer.Canvas.Pen.Color := Buffer.Canvas.Brush.Color;
						Buffer.Canvas.Rectangle(RectX, RectY, RectX + Zoom, RectY + Zoom 
							* 2);
					end;
				end;
			end;

			{ Write the character icon }
			Buffer.Canvas.Brush.Color := clPurple;
			Buffer.Canvas.FillRect(Rect(MiddleX, MiddleY, MiddleX + Zoom, MiddleY + 
				Zoom * 2));

			{ Blit the buffer to the minimap }
			Buffer.Canvas.CopyMode := cmSrcCopy;
			PaintBoxMiniMap.Canvas.Draw(0, 0 ,Buffer);		
		end;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ System call to allow automatic refresh of the minimap }
procedure TFormDisplay.PaintBoxMinimapPaint(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.PaintBoxMinimapPaint()');
	
  try
		{ Refresh the minimap }
		RefreshMiniMap;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Update the active monster list }
procedure TFormDisplay.UpdateMonsterList;
var
  CheckLoop: Integer;
  DuplicateFound: Boolean;
  Loop: Integer;
  MonListItem: TListItem;
  LocalMonster: TMonster;
  MonCount: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.UpdateMonsterList()');
	
  try
  	{ If we don't have the mainform displayed then exit }
		if not(FormDisplay.Visible) then
			Exit;
			
		{ If we don't have the main display visible then exit }
		if FormDisplay.PageControlMain.ActivePageIndex <> MAINDISPLAY then
    begin
      { Unfreeze the listview }
      PanelMonsters.Visible := True;
			Exit;
    end;
			
		{ TODO: also add a check in for maximised view }

		{ Freeze the graphical representation to allow faster and flicker-free
      updating }
    PanelMonsters.Visible := False;
    try
      ListViewMonsters.Items.Clear;

      { Empty the monster view }
      LabelMonster1.Caption := '';
      LabelMonster2.Caption := '';
      LabelMonster3.Caption := '';
      LabelMonster4.Caption := '';
      LabelMonster5.Caption := '';
      LabelMonster6.Caption := '';
      LabelMonster7.Caption := '';
      LabelMonster8.Caption := '';
      LabelMonster9.Caption := '';

      { Have a fake listview in memory for purposes of building this up }
      ListViewMonsters.Items.Clear;
      Loop := 0;

      { If we're on the town level, exit }
      if (FormDisplay.Game.Dungeon.LevelTheme = D_TOWN) then
        Exit;

      { Iterate through the active monsters list and add each unique one found
        as a new listitem }
      repeat
        { By default, the CurrentMonsters array is filled with -1s }
        if FormDisplay.Game.CurrentMonsters[Loop] <> NO_MONSTER then
        begin
          { Get the monster information }
          LocalMonster := (GMonsterList.Items[FormDisplay.Game.
            CurrentMonsters[Loop]] as TMonster);

          { Set a flag to indicate if we've found a duplicate }
          DuplicateFound := False;

          { Check if we already have a monster of this type in the list }
          for CheckLoop := 0 to ListViewMonsters.Items.Count - 1 do
          begin
            { Monster Archetype IDs are stored in a hidden field in the 
              TListItem and we use this for comparison purposes }
            if LocalMonster.ID = 
              StrToInt(ListViewMonsters.Items[CheckLoop].SubItems[2]) then
            begin
              { Duplicate found, so increase the count for this type of monster
                (the default is 1) }
              MonCount := 
                StrToIntDef(ListViewMonsters.Items[CheckLoop].SubItems[3], 0);
              Inc(MonCount);
              ListViewMonsters.Items[CheckLoop].SubItems[3] := 
                IntToStr(MonCount);
              DuplicateFound := True;
            end;
          end;

          { No duplicate found for this monster type }
          if not(DuplicateFound) then
          begin
            { Only display the first nine types of monsters visible to avoid
              scroll bars appearing and spoiling the view }
            if ListViewMonsters.Items.Count < 10 then
            begin
              { Change the listview type temporarily to avoid trimming of the
                listitem captions }
              ListViewMonsters.ViewStyle := vsSmallIcon;
						
              { Add a new listitem }
              MonListItem := ListViewMonsters.Items.Add;

              { At this point we don't add the caption as we don't know if we
                have more than 1 monster of that type visible and hence we don't
                know if we have to use the singular or plural form of the name }
						  
              { SubItem 0 - Colour of Monster }  
              if LocalMonster.Colour <> clBlack then
                MonListItem.SubItems.Add(ColorToString(LocalMonster.Colour))
              else
                { Since some monsters are invisible, i.e. their display colour
                  is set to black, we really need to make sure that they appear
                  as non-black in the list }
                MonListItem.SubItems.Add(ColorToString(clWhite));
						
              { SubItem 1 - Monster Number }
              MonListItem.SubItems.Add(IntToStr(FormDisplay.Game.
                CurrentMonsters[Loop]));  
							
              { SubItem 2 - Monster ID }	
              MonListItem.SubItems.Add(IntToStr(LocalMonster.ID));
						
              { SubItem 3 - Monster count }
              MonListItem.SubItems.Add('1');  
						
              { SubItem 4 - Monster character }
              MonListItem.SubItems.Add(LocalMonster.Char);  
						
              { SubItem 5 - Monster singular name }
              MonListItem.SubItems.Add(LocalMonster.Name);
						
              { SubItem 6 - Monster level - this is what the list is sorted by }
              MonListItem.SubItems.Add(IntToStr(LocalMonster.Level)); 

              { SubItem 7 - Monster plural name }
              MonListItem.SubItems.Add(LocalMonster.PluralName);

              { Reset the list view type }
              ListViewMonsters.ViewStyle := vsList;
            end;
          end;
          Inc(Loop);
        end;
      until (FormDisplay.Game.CurrentMonsters[Loop] = NO_MONSTER) or
        (Loop = High(FormDisplay.Game.CurrentMonsters));

      { Now loop through the list setting the caption }
      for Loop := 0 to ListViewMonsters.Items.Count - 1 do
      begin
        { Get the count of monsters }
        MonCount := StrToIntDef(ListViewMonsters.Items[Loop].SubItems[3], 0);

        { Reset the caption }
        ListViewMonsters.Items[Loop].Caption := '';

        { For each monster of a type visible, add the monster glyph }
        for CheckLoop := 1 to MonCount do
          ListViewMonsters.Items[Loop].Caption :=
            ListViewMonsters.Items[Loop].Caption + 
            ListViewMonsters.Items[Loop].SubItems[4];

        { Then deal with singular and plural names }
        if MonCount > 1 then
          ListViewMonsters.Items[Loop].Caption := 
            ListViewMonsters.Items[Loop].Caption + ' ' + 
            ListViewMonsters.Items[Loop].SubItems[7]
        else
          ListViewMonsters.Items[Loop].Caption :=
            ListViewMonsters.Items[Loop].Caption + ' ' +
            ListViewMonsters.Items[Loop].SubItems[5];
      end;

      { Finally populate the visible list - not the most optimal way of doing
      { this but good enough }
      for Loop := 0 to ListViewMonsters.Items.Count - 1 do
      begin
        if Loop = 0 then
        begin
          LabelMonster1.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster1.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 1 then
        begin
          LabelMonster2.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster2.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 2 then
        begin
          LabelMonster3.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster3.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 3 then
        begin
          LabelMonster4.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster4.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 4 then
        begin
          LabelMonster5.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster5.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 5 then
        begin
          LabelMonster6.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster6.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 6 then
        begin
          LabelMonster7.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster7.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 7 then
        begin
          LabelMonster8.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster8.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end
        else if Loop = 8 then
        begin
          LabelMonster9.Caption := ListViewMonsters.Items[Loop].Caption;
          LabelMonster9.Font.Color :=
            StringToColor(ListViewMonsters.Items[Loop].SubItems[0]);
        end;
      end;

      { Unfreeze the view }
    finally
      PanelMonsters.Visible := True;
    end;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Allow multiple colours in the monster listview }
procedure TFormDisplay.ListViewMonstersCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  FontColor: TColor;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.ListViewMonstersCustomDrawItem()');

	try
		{ Set the font colour to the monster colour }
		FontColor := StringToColor(Item.SubItems[0]);
		Sender.Canvas.Font.Color := FontColor;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Compare routine for the monster listview for sorting }
procedure TFormDisplay.ListViewMonstersCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.ListViewMonstersCompare()'); 
  
	try
		{ Sort according to level }
		if (Sender as TListView).Items.Count = 0 then 
			Compare := 0
		else
		begin
			{ Uses the standard Compare routine definitions as defined in the Delphi
			  help files }
			if (StrToInt(Item1.SubItems[6])) > (StrToInt(Item1.SubItems[6])) then 
				Compare := 1
			else if (StrToInt(Item1.SubItems[6])) = 
				(StrToInt(Item1.SubItems[6])) then 
				Compare := 0
			else 
				Compare := -1;
		end;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Selecting a spell school }
procedure TFormDisplay.ListViewAvailableSchoolsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewAvailableSchoolsSelectItem()');  
  
	try
		{ If we've selected a spell school }
		if (Sender as TListView).SelCount <> 0 then
		begin
			{ Display the spell for this school }
			ShowSchoolSpells(FormDisplay.Game.Player, 
				(Sender as TListView).Selected.SubItems[1]);
			MemoSpellDescription.Text := 'The fabled ' + 
				(Sender as TListView).Selected.SubItems[0] + ' contains ' + 
				GMagicDescription[StrToInt((Sender as TListView).Selected.SubItems[1]),
				0];
		end
		else
			{ Don't display anything }
			MemoSpellDescription.Text := '';
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Selecting a spell }
procedure TFormDisplay.ListViewSpellsKnownSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ListViewSpellsKnownSelectItem()');  
  
	try
		{ If we've chosen a spellschool and a spell }
		if ((Sender as TListView).SelCount <> 0) and 
			(ListViewAvailableSchools.SelCount <> 0) then
		begin
			{ Set the spell school }
			Game.Player.SelectedSpellSchool := 
				StrToInt(ListViewAvailableSchools.Selected.SubItems[1]);
				
			{ Set the spell }
			Game.Player.SelectedSpell :=  
				StrToInt((Sender as TListView).Selected.Caption);
				
			{ Clear the spell description }
			MemoSpellDescription.Text := '';
			
			{ Add the spell title to the selected spell details }
			AddText(GMagic[StrToInt(ListViewAvailableSchools.Selected.SubItems[1]), 
				StrToInt((Sender as TListView).Selected.Caption)], clLime, 
				MemoSpellDescription);
				
			{ Add the spell description to the selected spell details }	
			AddText(LINE_BREAK + GMagicDescription[StrToInt(ListViewAvailableSchools.
				Selected.SubItems[1]), 
				StrToInt((Sender as TListView).Selected.Caption)], clWhite, 
				MemoSpellDescription);
				
			{ Check to see if the spell selected can be cast }	
			if StrToInt((Sender as TListView).Selected.SubItems[1]) > 
				Game.Player.MaxMP then
				AddText(LINE_BREAK + 'You cannot cast this spell yet', clRed, 
					MemoSpellDescription)
			else
			begin
				{ Do an additional check to check if the spell can't currently be
				  cast }
				if StrToInt((Sender as TListView).Selected.SubItems[1]) > 
					Game.Player.MP then
					AddText(LINE_BREAK + 'You cannot cast this spell at this time',
						$000080FF, MemoSpellDescription)
				else
				{ Display spell casting details }
					AddText(LINE_BREAK + 'With your current skill, your chance of casting'
						+ ' this spell is ' + (Sender as TListView).Selected.SubItems[2] +
						' and it will cost you ' + 
						(Sender as TListView).Selected.SubItems[1] + 
						' MP out of your ' + IntToStr(Game.Player.MaxMP) + 
						' MP to do so', clWhite, MemoSpellDescription);
			end;
		end
		else
			{ Clear the spell description }
			MemoSpellDescription.Text := '';
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Kill the character }
procedure TFormDisplay.PlayerDie(KilledBy: TMonster);
var
  GameID: String;
  DumpBuffer: TStringList;
begin
	{ TODO: this should really be part of Player.TakeDamage, and triggered only
	  if HP < 0 }

  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.PlayerDie()');  
  
	try
		{ Check to make sure the player isn't dead }
		if not(Game.Player.Dead) then
		begin
			{ Mark the Player as dead! }
			Game.Player.Dead := True;

			{ Check what killed him/her }
			if Killedby = nil then
			begin
				{ In case of suicide/quitting }
			 	Game.Killed := 'You decided life was not worth living any more ';
			 	Game.Player.KilledBy := 'Quit';
			end
			else
			begin
				{ See if he/she was killed by a unique }
				if KilledBy.UniqueName <> '' then
				begin
					Game.Killed := 'You were killed by ' + KilledBy.Name;
					Game.Player.KilledBy := 'Killed by ' + KilledBy.Name;
				end
				else
				begin
					Game.Killed := 'You were killed by ' + KilledBy.SinglePrefix +  KilledBy.Name;
					Game.Player.KilledBy := 'Killed by ' + KilledBy.SinglePrefix +  KilledBy.Name;
				end;
			end;

			{ Output what killed him/her }
			UnitEngine.UpdateLog(Game.Killed, mesYouKill);
			UnitEngine.UpdateLog('You are dead...', mesError);
			
			{ Make a note of player death }
			Game.Player.TakeNote(FormDisplay.Game.Turns, Game.Player.KilledBy, nDeath, FormDisplay.Game.Dungeon);
			
			{ Pause to allow the death to sink in }
			Sleep(2000);
			
			{ Calculate the score and write it to the highscore table }
			GameID := WriteHiScores;
			
			{ Switch to the highscores tab and display them, with the just finished
			  game highlighted }
			RichEditHiScores.Clear;
			PageControlMain.ActivePage := TabSheetHiScores;
			DisplayHiScores(RichEditHiScores, GameID, True);

			{ Now save the character log into a morgue file and save the character 
			  dump and then delete the original save file }
			Game.Player.Save(ExtractFilePath(Application.ExeName) + 'morgue\');
			DumpBuffer := TStringList.Create;
			try
				DumpBuffer.Text := CharDump(FormDisplay.Game.Player.Dead);
				DumpBuffer.SaveToFile(ExtractFilePath(Application.ExeName) + 'morgue\' +
					Game.Player.Name + '.TXT');
				if FileExists(ExtractFilePath(Application.ExeName) + 'save\' +
					Game.Player.Name + '.CHR') then
					DeleteFile(ExtractFilePath(Application.ExeName) + 'save\' +
					Game.Player.Name + '.CHR');
			finally
				DumpBuffer.Free;
			end;
		end;
  except
 		{ in case of error, log the Exception }
 		on E: Exception do hLog.AddException(E);
  end;
end;

{ Drag-and-drop checking of eating drag-and-drop } 
procedure TFormDisplay.ImageEatDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Slot: Integer;
  SourceItem: TItem;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageEatDragOver()');
	
  try
		{ Find the original item we want to move }
		Slot := (Source as TComponent).Tag;
		
		{ Check we have an item }
		if FormDisplay.Game.Player.Inventory[Slot] > 0 then
		begin
			{ Get the item }
			SourceItem := GItemList[Game.Player.Inventory[Slot]] as TItem;
			
			{ Make sure its food }
			if SourceItem.ItemType = iConsumable then 
				Accept := True 
			else 
				Accept := False;
  end;
  except
	   { in case of error, log the Exception }
	   on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle drag-and-drop of food onto the food button } 
procedure TFormDisplay.ImageEatDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Slot: Integer;
  SourceItem: TItem;
  Temp: String;
  UpdateLogText: String;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageEatDragDrop()');
	
  try
		{ Find the original item we want to move }
		Slot := (Source as TComponent).Tag;

		{ Check we're dragging an actual item }
		if FormDisplay.Game.Player.Inventory[Slot] > 0 then
		begin
			{ Get the source item }
			SourceItem := GItemList[Game.Player.Inventory[Slot]] as TItem;

			{ If the source item is stacked, reduce the count by 1 }
			if SourceItem.Count > 1 then
				SourceItem.Count := SourceItem.Count - 1;      
			Dec(Game.Player.InventoryCount[Slot]);

			{ Clear the inventory slot }
			if Game.Player.InventoryCount[Slot] = 0 then
				Game.Player.Inventory[Slot] := 0;

			{ Feed the character }
			Game.Player.Feed(FOOD_NUTRITION_VALUE);

			{ Add a message to the message log }
			UpdateLogText := 'You eat %s - ' + GetFoodDescription + '!';
			Temp := Format(UpdateLogText, [SourceItem.SingleName]);
			UnitEngine.UpdateLog(Temp, mesItemManipulation);

			{ Pass a turn }
			PassATurn;

			{ Refresh the inventory }
			LoadInventoryDetails(Game.Player);

			{ Switch back to the main screen }
			FormDisplay.PageControlMain.ActivePageIndex := MAINDISPLAY;
			FormDisplay.DrawDungeonToSurface(MainRect, FormDisplay.ScreenMain);
		end;
 	except
     { in case of error, log the Exception }
     on E: Exception do hLog.AddException(E);
  end;
end;

{ Handle movement of the cursor }
procedure TFormDisplay.ScreenMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PointX: Double;
  PointY: Double;
  InX: Integer;
  InY: Integer;
  LocalMonster: TMonster;
  LocalItem: TItem;
  Offset: Integer;
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.ScreenMainMouseMove()');
	
  try
  	{ This is somewhat annoying - in order to convert screen coordinates back
			to map co-ordinates we have to take into account any changes in the size
			of the paintbox where we are responding to the mouse move }
		Offset := (Sender as TPaintBox).Height - 
			(Sender as TPaintBox).Constraints.MinHeight;

		{ Convert screen coordinates to map coordinates }
		PointX := (X / ScalingFactorX) + FormDisplay.Game.TopLeftX;
		PointY := FormDisplay.Game.BottomLeftY + (((Sender as TPaintBox).Height 
			- (Y - OffSet)) / ScalingFactorY);
		
		{ Round, not truncase }
		InX := Round(PointX);
		InY := Round(PointY);
		MousePos := Point(X, Y);

    { Reset the tooltip }
    Game.Tooltip := '';
		
		{ Get rid of the existing cursor }
		if (InX <> CursorPos.X) or (InY <> CursorPos.Y) then
		begin			
			ScreenMain.Canvas.Pen.Color := clBlack;
			ScreenMain.Canvas.PolyLine([NewCursorPosition[0], 
				NewCursorPosition[1], NewCursorPosition[2], NewCursorPosition[3], 
				NewCursorPosition[4]]);
			CursorPos := Point(InX, InY);
		end;

		{ Define the new cursor position }
		NewCursorPosition[0] := Point(Trunc((InX - FormDisplay.Game.TopLeftX) * 
			ScalingFactorX), Round((FormDisplay.Game.TopLeftY - InY - 2) * 
			ScalingFactorY + (OffSet + (ScalingFactorY * 3/4))));
		NewCursorPosition[1] := Point(Trunc((InX - FormDisplay.Game.TopLeftX) * 
			ScalingFactorX), Round((FormDisplay.Game.TopLeftY - InY - 2) * 
			ScalingFactorY + (OffSet + ScalingFactorY +  (ScalingFactorY div 2))));
		NewCursorPosition[2] := Point(Trunc((InX - FormDisplay.Game.TopLeftX) * 
			ScalingFactorX + ScalingFactorX), Round((FormDisplay.Game.TopLeftY - InY
			- 2) * ScalingFactorY + (OffSet + ScalingFactorY + 
			(ScalingFactorY div 2))));
		NewCursorPosition[3] := Point(Trunc((InX - FormDisplay.Game.TopLeftX) * 
			ScalingFactorX + ScalingFactorX),  Round((FormDisplay.Game.TopLeftY - InY 
			- 2) * ScalingFactorY + (OffSet + (ScalingFactorY * 3/4))));
		NewCursorPosition[4] := NewCursorPosition[0];

		{ Check if we're in view of the character, and change the cursor colour
      depending on this }
    if (FormDisplay.Game.Dungeon.Visible[Inx, InY] = 1) then
		  ScreenMain.Canvas.Pen.Color := clYellow
    else
      ScreenMain.Canvas.Pen.Color := clRed;

    { Redraw the cursor }
		ScreenMain.Canvas.PolyLine([NewCursorPosition[0], NewCursorPosition[1],
      NewCursorPosition[2], NewCursorPosition[3], NewCursorPosition[4]]);

		{ Do mouse-over events }
		if FormDisplay.Game.Dungeon.Visible[Inx, InY] = 1 then
		begin
			if FormDisplay.Game.Dungeon.Monsters[InX, InY] > 0 then
			begin
				{ If we have a monster }
        if FormDisplay.Game.Dungeon.LevelTheme = D_TOWN then
          LocalMonster :=
            (GTownsPeopleList.Items[FormDisplay.Game.Dungeon.Monsters[InX, InY]]
            as TMonster)
        else
				  LocalMonster := (GMonsterList.Items[FormDisplay.Game.Dungeon.Monsters
					  [InX, InY]] as TMonster);

        { Get the monster name }
        Game.Tooltip := LocalMonster.Name;

        { Now add a status depending on the monster status }
        if not(LocalMonster.Awake) then
          Game.Tooltip := Game.Tooltip + ' (Unaware)';
			end
			else if (FormDisplay.Game.Dungeon.Objects[InX, InY] > 0) and 
				(FormDisplay.Game.Dungeon.Objects[InX, InY] < ITEM_GOLD) then
			begin
				{ If we have an item }
				if FormDisplay.Game.Dungeon.GetNumberofItemsOnTile(Point(InX, InY)) = 1
          then
				begin
					{ Only a single item here }
					LocalItem := 
						(GItemList.Items[FormDisplay.Game.Dungeon.Objects[InX, InY]] as 
						TItem);
						
					{ Get the item name }
					Game.Tooltip := LocalItem.Name;
				end
				else
					{ Multiple items here }
          Game.Tooltip := 'A Pile of Items';
			end
      { Handle gold }
			else if FormDisplay.Game.Dungeon.Objects[InX, InY] >= ITEM_GOLD then
        Game.Tooltip := 'A pile of coins'
      { Handle special terrain effects }
      else if FormDisplay.Game.Dungeon.Effects[InX, InY] in
        [E_SPECIALEFFECT, E_STANDARDEFFECT] then
        { Handle special terrain inside of a zone }
        if FormDisplay.Game.Dungeon.Zone[InX, InY] > 0 then
          Game.Tooltip :=
            GDungeonEffectDescriptionArray[FormDisplay.Game.Dungeon.Zone
            [InX, InY]]
        { Handle special terrain not in zones }
        else
          Game.Tooltip :=
            GDungeonEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
      else if FormDisplay.Game.Dungeon.Effects[InX, InY] = E_GROUNDEFFECT then
        Game.Tooltip :=
          GDungeonBackgroundEffectDescriptionArray[FormDisplay.Game.Dungeon.LevelTheme]
      { Handle special terrain effects }
      else if FormDisplay.Game.Dungeon.Effects[InX, InY] in
        [E_DARKEREFFECT] then
        { Handle special terrain inside of a zone }
        if FormDisplay.Game.Dungeon.Zone[InX, InY] > 0 then
          Game.Tooltip :=
            GDungeonDarkerEffectDescriptionArray[
            FormDisplay.Game.Dungeon.Zone[InX, InY]]
        { Handle special terrain not in zones }
        else
          Game.Tooltip :=
            GDungeonDarkerEffectDescriptionArray[
            FormDisplay.Game.Dungeon.LevelTheme]
      else
      { Handle normal terrain }
      begin
        case FormDisplay.Game.Dungeon.Terrain[InX, InY] of
          T_BLANK: Game.Tooltip := '';
          T_HARDWALL: Game.Tooltip := 'Impassable Wall';
          T_SOFTWALL: Game.Tooltip := 'Wall';
          T_FLOOR_ROOM: Game.Tooltip := '';
          T_FLOOR_CORRIDOR: Game.Tooltip := '';
          T_DOOR_OPEN: Game.Tooltip := 'Open Door';
          T_DOOR_CLOSED: Game.Tooltip := 'Closed Door';
          T_STAIRS_DOWN: Game.Tooltip := 'Stairs Down';
          T_STAIRS_UP: Game.Tooltip := 'Stairs Up';
        end;
      end;
		end;

  { Draw the tooltip }
  if Game.Tooltip <> '' then
  begin
    ScreenMain.Canvas.Font.Name := FontNameSmall;
    ScreenMain.Canvas.Font.Size := 7;
    ScreenMain.Canvas.Font.Color := clWhite;
    ScreenMain.Canvas.TextOut(NewCursorPosition[0].X, NewCursorPosition[0].Y - 14,
      Game.Tooltip);
    ScreenMain.Canvas.Font.Name := FontName;
  end;

 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for starting a new game }
procedure TFormDisplay.Starta1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.Starta1Click()');
	
  try
  	{ Disable the keyboard timer }
		FormDisplay.Timer.Enabled := False;
		
		{ Go to the first page of character creation }
		FormDisplay.PageControlMain.ActivePage :=
      FormDisplay.TabSheetCharacterCreate;
		FormDisplay.PageControlCharacterCreate.ActivePage :=
			FormDisplay.TabSheetChar1;

		{ Wipe the character object used in character generation }
		if Assigned(FormDisplay.NewCharacter) then
			FormDisplay.NewCharacter.Free;

		{ Create the character object to hold the generated character }
		FormDisplay.NewCharacter := TCreature.Create;

		{ Blank the character creation screens }
		EditName.Text := '';
		
		{ Restart keyboard timer }
		EmptyKeyQueue;

    { Sleep to avoid keypress reptition }
    Screen.Cursor := crHourGlass;
    Sleep(500);
    Screen.Cursor := crDefault;
    
		FormDisplay.Timer.Enabled := True;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for continuing a game }
procedure TFormDisplay.ContinueanExistingGame1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ContinueanExistingGame1Click()');
	
  try
	 	{ Load an existing game }
		OpenDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'save\';
		
		{ Use a system open dialog }
		if OpenDialog.Execute then
		begin

		{ TODO: we need to write the code for a full load here }
		
		{ And restart the game }
		Initialise(ExtractFileName(OpenDialog.FileName), False);
  end;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for exiting }
procedure TFormDisplay.Exit1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.Exit1Click()');
	
  try
  	{ TODO: handle saving of the game }
  
  	{ Quit }
  	ModalResult := mrOK;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Draw a health bar on a monster }
procedure TFormDisplay.DrawHealthBar(Canvas: TCanvas; X: Integer; Y: Integer;
  MaxHP: Integer; CurrentHP: Integer);
var
  PercentHealth: Double;
  BarColour: TColor;
  Quartile: Integer;
  HealthBarWidth: Integer;
  CurrentPen: TColor;
  CurrentBrush: TColor;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.DrawHealthBar()');
	
  try
		{ Push Current Colours }
		CurrentPen := Canvas.Pen.Color;
		CurrentBrush := Canvas.Brush.Color;

		{ OffSet, i.e. the healthbar is a number of pixels above the monster }
		Inc(X, 1);
		Inc(Y, 8);

		{ To speed things up, don't bother displaying health bars for healthy 
			monsters }
		if CurrentHP <> MaxHP then 
		begin;
			{ Work out how injured the monster is }
			PercentHealth := (CurrentHP/MaxHP) * 100;

			{ What quartile is the monster's health currently in? }
			Quartile := Trunc(PercentHealth) div 25;
			
			{ Scale the size of the health bar - to allow future zooming }
			HealthBarWidth := Trunc(PercentHealth / 100 * ScalingFactorX);

			{ The colour the bar changes depending on health level }
			case Quartile of
				0: BarColour := clRed;
				1: BarColour := $000080FF;
				2: BarColour := clYellow;
				3: BarColour := clLime;
			end;

			{ Set the bar colours and draw it in two stages, first for the HP it
			  currently has }
			Canvas.Brush.Color := BarColour;
			Canvas.Pen.Color := BarColour;
			Canvas.Rectangle(X, Y, X + HealthBarWidth, Y + 3);
			
			{ Then the portion to show it's not been injured }
			Canvas.Brush.Color := clMaroon;
			Canvas.Pen.Color := clMaroon;
			Canvas.Rectangle(X + HealthBarWidth, Y, X + ScalingFactorX, Y + 3);

			{ Pop Current Pens }
			Canvas.Pen.Color := CurrentPen;
			Canvas.Brush.Color := CurrentBrush;
		end;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for selecting monochrome graphics }
procedure TFormDisplay.UseMonochromeASCII1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.UseMonochromeASCII1Click()');
	
  try
  	{ Switch to monochrome ASCII }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIISTANDARD then 
  		ChangeDrawMode(DASCIISTANDARD, True);
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for selecting coloured graphics }
procedure TFormDisplay.UseColouredASCII1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.UseColouredASCII1Click()');
	
  try
  	{ Switch to coloured ASCII }
  	if FormDisplay.Game.CurrentDrawMode <> DASCIICOLOURED then 
  		ChangeDrawMode(DASCIICOLOURED, True);
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Application system start event }
procedure TFormDisplay.ApplicationEvents1Activate(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ApplicationEvents1Activate()');
	
  try
  	{ Enable keyboard timer }
  	Timer.Enabled := True;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Application system stop event }
procedure TFormDisplay.ApplicationEvents1Deactivate(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ApplicationEvents1Deactivate()');
	
  try
  	{ Disable keyboard timer }
  	Timer.Enabled := False;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Menu callback for quitting a game }
procedure TFormDisplay.QuitSuicide1Click(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.QuitSuicide1Click()');
	
  try
  	{ If we're in a game then quit }
  	if (InGame) then
    	FormDisplay.PlayerDie(nil);
  except
	  { in case of error, log the Exception }
	  on E: Exception do hLog.AddException(E);
  end;
end;

{ Timer to animate menu on front screen }
procedure TFormDisplay.TimerAnimateIntroTimer(Sender: TObject);
begin
  { Logging }
  // hLog.Add('{now} {lNum} TFormDisplay.TimerAnimateIntroTimer()');
	
  try
  	{ Only try to animate the menu when we're on the front screen }
		if PageControlMain.ActivePage = TabSheetIntro then 
		begin
			{ Cycle the colours }
			LabelMenu1.Font.Color := MenuColors[(AnimateCounter + 0) mod 5];
			LabelMenu2.Font.Color := MenuColors[(AnimateCounter + 1) mod 5];
			LabelMenu3.Font.Color := MenuColors[(AnimateCounter + 2) mod 5];
			LabelMenu4.Font.Color := MenuColors[(AnimateCounter + 3) mod 5];
			LabelMenu5.Font.Color := MenuColors[(AnimateCounter + 4) mod 5];

			{ Increase the counter we use to animate the menu }
			inc(AnimateCounter);
		end;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Enable/Disable menus when entering the intro screen }
procedure TFormDisplay.TabSheetIntroShow(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.TabSheetIntroShow()');
	
  try
  	{ Enable/Disable menu options appropriately }
		UseMonochromeASCII1.Enabled := InGame;
		UseColouredASCII1.Enabled := InGame;
		Starta1.Enabled := not(InGame);
		ContinueanExistingGame1.Enabled := not(InGame);
		QuitSuicide1.Enabled := InGame;
		TimerAnimateIntro.Enabled := True;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Enable/Disable menus when leaving the intro screen }
procedure TFormDisplay.TabSheetIntroHide(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.TabSheetIntroHide()');
	
  try
  	{ Enable/Disable menu options appropriately }
		TimerAnimateIntro.Enabled := False;
		UseMonochromeASCII1.Enabled := InGame;
		UseColouredASCII1.Enabled := InGame;
		Starta1.Enabled := not(InGame);
		ContinueanExistingGame1.Enabled := not(InGame);
		QuitSuicide1.Enabled := InGame;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate a random name in characfer creation }
procedure TFormDisplay.ImageGenerateRandomNameClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageGenerateRandomNameClick()');
	
  try
  	{ Generate a random character name }
  	EditName.Text := GenerateName;  
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Generate a random background in characfer creation }
procedure TFormDisplay.ImageGenerateBackgroundClick(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.ImageGenerateBackgroundClick()');
	
  try
  	{ Generate a random background }
  	MemoBackGround.Text := GenerateBackground;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ General purpose blocking timer, which only allows screen animation to happen }
procedure TFormDisplay.TimerDelayTimer(Sender: TObject);
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.TimerDelayTimer()');
	
  try
    { Switch off the timer }
    Game.Block := False;
    TimerDelay.Enabled := False;
 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

{ Draw the monster con rating }
procedure TFormDisplay.DrawMonsterCon(Canvas: TCanvas; X, Y: Integer;
  LocalMonster: TMonster);
var
  BarColour: TColor;
  CurrentPen: TColor;
  CurrentBrush: TColor;
  EffectiveDungeonLevel: Integer;
begin
  { Logging }
  hLog.Add('{now} {lNum} TFormDisplay.DrawMonsterCon()');
	
  try
    { Work out monster relative difficulty }
    EffectiveDungeonLevel := FormDisplay.Game.Dungeon.LevelDepth div 2 +
      FormDisplay.Game.Dungeon.LevelOffset;
    if EffectiveDungeonLevel < MINDUNGEONLEVEL then
      EffectiveDungeonLevel := MINDUNGEONLEVEL;
    if EffectiveDungeonLevel > MAXDUNGEONLEVEL then
      EffectiveDungeonLevel := MAXDUNGEONLEVEL;

    { Work out monster con }
    if LocalMonster.UniqueName <> '' then
    begin
      { Push Current Colours }
		  CurrentPen := Canvas.Pen.Color;
		  CurrentBrush := Canvas.Brush.Color;

      { OffSet, i.e. the consider status is a number of pixels beneath the monster }
		  Inc(X, 1);
		  Dec(Y, 8);

      { Draw the indicator }
      BarColour := clGray;
      Canvas.Brush.Color := BarColour;
			Canvas.Pen.Color := BarColour;
			Canvas.Rectangle(X, Y, X + 8, Y - 1);
      Canvas.Rectangle(X, Y - 3, X + 8, Y - 4);

      { Pop Current Pens }
			Canvas.Pen.Color := CurrentPen;
			Canvas.Brush.Color := CurrentBrush;
    end
    else if LocalMonster.Level > (EffectiveDungeonLevel + 1) then
    begin
      { Push Current Colours }
		  CurrentPen := Canvas.Pen.Color;
		  CurrentBrush := Canvas.Brush.Color;

      { OffSet, i.e. the consider status is a number of pixels beneath the monster }
		  Inc(X, 1);
		  Dec(Y, 8);

      { Draw the indicator }
      BarColour := clGray;
      Canvas.Brush.Color := BarColour;
			Canvas.Pen.Color := BarColour;
			Canvas.Rectangle(X, Y, X + 8, Y - 1);

      { Pop Current Pens }
			Canvas.Pen.Color := CurrentPen;
			Canvas.Brush.Color := CurrentBrush;
    end;

 	except
    { in case of error, log the Exception }
    on E: Exception do hLog.AddException(E);
  end;
end;

end.
