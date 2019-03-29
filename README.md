**Kharne**

*2019 NOTE: this is the very first roguelike I attempted to write, back in the early/mid 2000s. It never got finished - what was missing was spells/magic, and an endgame, and I ran out of steam, frankly. I've not coded in Delphi/Pascal for a decade now and I don't even have a Windows machine anymore, so I can't compile it, but I present the source code here for Roguelike Archaeological Purposes. The contents of this game are presented as found on an old hard disc. The last blog entry relevant to Kharne was <https://kharne-rl.blogspot.com/2011/07/new-fonts.html>*

This is an Alpha release of my roguelike, Kharne. 

Kharne is still a work in progress and you should be warned - it is definitely not finished. This release is merely to give you a flavour of what to expect in the completed game. Many features (such as a magic system and saving/loading) aren't implemented yet and many others are incomplete.

The latest changes can be found in VERSION.TXT (alternatively, press V when in game)

Although Kharne makes use of the mouse much more than most other roguelikes, where possible, I've tried to implement many of the standard Roguelike key-bindings. Pressing X will give you a help screen with all the available commands. Some of them are:

- Control your '@' with the numeric-keyboard (the 'vi' keys should work as well). 
- < and > are used to navigate through portals or stairs.
- 'I' displays the inventory. Pressing the key appropriate to the item displays information (if known) about the item.
- 'D' displays the inventory and allows you to drop items.
- 'E' displays the inventory and allows you to eat items.
- 'R' displays the inventory and allows you to read scrolls.

Or you can use drag-and-drop whilst within the inventory. To drop an item, simply drag it onto the open hand at the lower right of the inventory screen. To eat an item, again, drag it onto the food icon at the lower right of the inventory screen.

To wear/wield and also remove items, just drag them to the appropriate slot to and from the backpack at the top of the screen to the equipped slots at the bottom.

- 'Z' brings up the cast spell interface (currently kinda pointless, but it demonstrates some of the magic that will be implemented),
- 'O' opens any nearby doors.
- 'C' closes any nearby doors.
- 'M' toggles the Player Information display. 
- 'Q' drinks from a fountain.
- 'W' shows a wizard mode screen that allows you to cheat or explore the game.
- 'T' then a direction tunnels in the direction you specify (ESC to cancel this)
- Left-clicking, if you have a ranged weapon, will fire a projectile at your target (for now, you have infinite generic ammunition)

- F1/F2 minimises/maximises the game view.

When you begin the game, you begin in the Nexus, which is, following Roguelike Convention, the Town level. Use the portals to access the various dungeons. They are, in order of intended difficuly:

Levels 1 to 5: The Wilderlands, The Fortress
Levels 6 to 10: The Mausoleum, The Keep
Levels 11 to 15: The Four Elemental Planes
Levels 16 to 20: The Abyss

Eventually, the aim of the game will be to return a magic item from each of these level bands to the surface. Currently, there is no win condition.

For now, to run properly Kharne really needs a resolution of 1024x768 or more. This will probably change in later releases. The main window can be resized.

And to answer the other couple of inevitable questions:

- eventually I do hope to produce a multi-platform version one day. It depends whither or not I can port Kharne to use Lazarus or when Codegear release Delphi X (which will cross-compile to Mac and Linux)

- Kharne is released under the Mozilla Public License v1.1. The source code is available from the development blog at kharne-rl.blogspot.com

Feedback is welcomed and encouraged - feel free to send it to davemoore22@gmail.com

*Please install the "VeraMono.ttf" (usually by copying to C:\WINDOWS\FONTS) font before playing Kharne if you do not have it already installed*