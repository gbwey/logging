let Level = < Debug | Info | Warn | Error >
let ScreenType = < StdOut | StdErr >
let Screen = { ScreenType : ScreenType, Level : Level}
let Email = { SmtpServer : Text, SmtpTo : Text, SmtpFrom : Text }
let File = {  Prefix : Text, LongName : Bool, Level : Level, Dir : Text }
in { debug = Level.Debug
   , info = Level.Info
   , warn = Level.Warn
   , error = Level.Error
   , stdout = ScreenType.StdOut
   , stderr = ScreenType.StdErr
   , screen = Screen
   , email = Email
   , file = File
   , noscreen = None Screen
   , nofile = None File
   , noemail = None Email
   , nodir = "."
   , Type = { File : Optional File, Screen : Optional Screen, Email : Optional Email, Debug : Bool }
   , default = { File = Some { Prefix = "def", LongName = True, Level = Level.Debug, Dir = "." }
               , Screen = Some { ScreenType = ScreenType.StdOut, Level = Level.Info }
               , Email = None Email
               , Debug = False
               }
  }
