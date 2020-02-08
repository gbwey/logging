let x = ./corelog.dhall
in {
     File = Some { Prefix = "def", LongName = True, Level = x.debug, Dir = "testdir" }
   , Screen = Some { ScreenType = x.stdout, Level = x.info }
   , Email = Some { SmtpServer = "testsmtpserver"
                      , SmtpTo = "testsmtpto"
                      , SmtpFrom = "testsmtpfrom"
                      }
   , Debug = True
}

