/// Windows accessibility shortcuts, switched off while the game has focus.
/// Pressing Shift five times (Sticky Keys), holding right Shift for eight
/// seconds (Filter Keys) or holding Num Lock (Toggle Keys) opens a Windows
/// dialog that steals focus mid-round. While the game window is active the
/// shortcuts are disabled — unless the feature itself is on, which means the
/// player uses it — and the saved settings come back when the window loses
/// focus or the game exits. The change is never written to the user profile,
/// so even a killed process leaves it only until the next sign-in.
/// On other operating systems every call does nothing.
module FsHockey.ShortcutKeys

open System
open System.Runtime.InteropServices

[<DllImport("user32.dll", EntryPoint = "SystemParametersInfoW")>]
extern bool private SystemParametersInfo(uint32 action, uint32 param, nativeint data, uint32 winIni)

[<DllImport("user32.dll")>]
extern nativeint private GetForegroundWindow()

[<DllImport("user32.dll")>]
extern uint32 private GetWindowThreadProcessId(nativeint window, uint32& processId)

/// One accessibility feature: its get/set actions and its struct size (the
/// struct starts with cbSize, then dwFlags).
[<Struct>]
type private Feature = { Get: uint32; Set: uint32; Size: int }

let private features =
    [ { Get = 0x003Au; Set = 0x003Bu; Size = 8 }    // SPI_GET/SETSTICKYKEYS, STICKYKEYS
      { Get = 0x0032u; Set = 0x0033u; Size = 24 }   // SPI_GET/SETFILTERKEYS, FILTERKEYS
      { Get = 0x0034u; Set = 0x0035u; Size = 8 } ]  // SPI_GET/SETTOGGLEKEYS, TOGGLEKEYS

/// The feature is on (SKF_STICKYKEYSON / FKF_FILTERKEYSON / TKF_TOGGLEKEYSON).
[<Literal>]
let private FeatureOn = 0x1
/// The shortcut is enabled, and asks for confirmation (the *_HOTKEYACTIVE and
/// *_CONFIRMHOTKEY flags).
[<Literal>]
let private HotkeyFlags = 0xC

/// Read a feature's struct; None when Windows refuses.
let private read (f: Feature) : byte array option =
    let buf = Marshal.AllocHGlobal f.Size
    try
        Marshal.Copy(Array.zeroCreate<byte> f.Size, 0, buf, f.Size)
        Marshal.WriteInt32(buf, 0, f.Size)
        if SystemParametersInfo(f.Get, uint32 f.Size, buf, 0u) then
            let data = Array.zeroCreate<byte> f.Size
            Marshal.Copy(buf, data, 0, f.Size)
            Some data
        else None
    finally
        Marshal.FreeHGlobal buf

/// Write a feature's struct for this session only (no SPIF_UPDATEINIFILE).
let private write (f: Feature) (data: byte array) =
    let buf = Marshal.AllocHGlobal f.Size
    try
        Marshal.Copy(data, 0, buf, f.Size)
        SystemParametersInfo(f.Set, uint32 f.Size, buf, 0u) |> ignore
    finally
        Marshal.FreeHGlobal buf

/// The settings as they were before disable, while the shortcuts are off.
let mutable private saved: (Feature * byte array) list = []
let mutable private disabled = false

/// Switch the shortcuts off.
let private disable () =
    if not disabled then
        disabled <- true
        saved <-
            features
            |> List.choose (fun f ->
                read f
                |> Option.bind (fun data ->
                    let flags = BitConverter.ToInt32(data, 4)
                    if flags &&& FeatureOn = 0 && flags &&& HotkeyFlags <> 0 then
                        let off = Array.copy data
                        BitConverter.GetBytes(flags &&& ~~~HotkeyFlags).CopyTo(off, 4)
                        write f off
                        Some(f, data)
                    else None))

/// Put the saved settings back (game window in the background, game exiting).
let restore () =
    if OperatingSystem.IsWindows() && disabled then
        disabled <- false
        for f, data in saved do
            write f data
        saved <- []

/// Once per frame: shortcuts off while the foreground window is the game's
/// (this process's), back on while it is not.
let sync () =
    if OperatingSystem.IsWindows() then
        let mutable pid = 0u
        GetWindowThreadProcessId(GetForegroundWindow(), &pid) |> ignore
        if int pid = Environment.ProcessId then disable () else restore ()
