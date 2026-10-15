# The FS Hockey League

A retro-style ice hockey game for Nu game engine, inspired by Solar Hockey (1990-1992), written in F#. 
There is also available [Windows Forms version](https://github.com/Thorium/FsHockey/tree/WinForms), 
and [MonoGame version](https://github.com/Thorium/FsHockey/tree/MonoGame).

## Controls

| Action        | Player 1            | Player 2    |
|---------------|---------------------|-------------|
| Move          | Arrow keys          | WASD        |
| Shoot / Pass  | Right Shift / Enter | Space / Tab |

Hold the shoot key longer for a harder shot. A quick tap gives a weaker pass.

On Windows the accessibility shortcuts (Shift pressed five times for Sticky Keys,
right Shift held for Filter Keys, Num Lock held for Toggle Keys) are off while
the game window is in front, so their dialogs can't interrupt a match; they come
back when you switch away or quit. Players who have the feature itself turned on
keep it. No admin rights are needed, and nothing is saved to your settings.

## Menu

| Key       | Action                                    |
|-----------|-------------------------------------------|
| Up / Down | Select team                               |
| Tab       | Switch between Team 1 and Team 2 columns  |
| Enter     | Start exhibition match                    |
| L         | Start league tournament                   |
| F         | Toggle fast human players                 |
| H         | Toggle hard mode (stronger CPU)           |
| 5         | Toggle 3v3 / 5v5 player mode             |
| Esc       | Quit                                      |

Set a team to **HUMAN PLAYER** for keyboard control. Both teams can be human or CPU.

The menu is also clickable: click a team in either list to pick it, click
**EXHIBITION** or **LEAGUE** to start, and the toggles below them switch
3v3 / 6v6, fast human, hard CPU and gamepads. Every control shows its
keyboard shortcut, and the keys above keep working.


## Credits

By Tuomas Hietanen. Influenced by Wayne Gretzky Hockey (1988) and Solar Hockey (c) 1990-1992 Galifir Developments.

<img width="1191" height="527" alt="image" src="https://github.com/user-attachments/assets/01e121cb-1d32-4087-96b6-ff4b9200188e" />
