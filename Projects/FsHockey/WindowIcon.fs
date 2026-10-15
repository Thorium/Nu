/// The game window's icon: Nu leaves SDL's default, so the faceoff icon is
/// set once the window exists, from Icon.bmp next to the executable.
module FsHockey.WindowIcon

#nowarn "9" "51" // native pointers to SDL's window list

open System
open System.IO
open Microsoft.FSharp.NativeInterop
open SDL

let mutable private isSet = false

/// Once per frame until it succeeds: set the icon of the app's (only) window.
let trySet () =
    if not isSet then
        let path = Path.Combine(AppContext.BaseDirectory, "Icon.bmp")
        let mutable count = 0
        let windows = SDL3.SDL_GetWindows(&&count)
        if count > 0 && File.Exists path then
            let utf8Path = Text.Encoding.UTF8.GetBytes(path + "\000")
            use pathPtr = fixed utf8Path
            let icon = SDL3.SDL_LoadBMP pathPtr
            if not (NativePtr.isNullPtr icon) then
                SDL3.SDL_SetWindowIcon(NativePtr.read windows, icon) |> ignore
                SDL3.SDL_DestroySurface icon
            isSet <- true
        if not (NativePtr.isNullPtr windows) then SDL3.SDL_free(NativePtr.toVoidPtr windows)
