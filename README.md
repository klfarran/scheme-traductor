# scheme traductor
Official project for scheme-traductor: write in scheme, but in Spanish! 
Designed for use with Dr. Racket graphical interface


## Install and Configure Dr Racket with _scheme traductor_ for Windows: 
Navigate to [Download Dr Racket](download.racket-lang.org  "Download Dr Racket") 
    Download the version appropriate for your computer and operating system (Windows)
    Click on the “racket-...exe” button
    In the opening/saving dialog that follows choose to save the file racket....exe
    Execute the racket....exe file that you saved 

Download the scheme-traductor folder from the main branch in this repository
    Copy the scheme-traductor folder into the Racket/collects folder 
    For Windows, assuming that the C: drive is the main drive on your computer, this is usually located at: C:\Program Files\Racket
    
Delete the 'Racket' folder located at C:\Users\_user name_\AppData\Roaming\Racket
    Create a new folder named 'Racket' inside the C:\Users\_user name_\AppData\Roaming folder 
    Copy the racket-prefs.rktd from the main branch of this repository into this 'Racket' folder 
    
Open Dr Racket, hit 'Run' in the upper right hand corner, and you're all set! 
    Note that all files should (automatically) start with the following two lines in order to use scheme traductor
        #lang scheme
        (require scheme-traductor/scheme-traductor)
  
## Install and Configure Dr Racket with _scheme traductor_ for Mac: 
Navigate to [Download Dr Racket](download.racket-lang.org  "Download Dr Racket") 
    Download the version appropriate for your computer and operating system (MacOS)
    Click on the “racket-...dmg” button
    In the opening/saving dialog that follows choose to save the file racket....dmg
    Double-click on the racket....dmg disk-image file that you saved 
    From the disk folder that opens, drag the Racket vXXXX folder (where XXXX is the number of the current version of DrRacket) to the Applications folder


Download the scheme-traductor folder from the main branch in this repository
    Copy the scheme-traductor folder into the Racket/collects folder 
    The 'collects' folder should be located at _hard drive_/Applications/Racket vXXXX, where XXXX is the number of the current version of Dr Racket

Open the Terminal application and make sure that the entire /Applications/Racket vXXXX/collects hierarchy is world-readable and world-executable, but not world-writeable by executing the command
    chmod -R go+rx-w /Applications/Racket\ vXXXX/collects

 If they exist, delete the files
    hard drive/Users/_user name_/Library/Preferences/org.plt￾scheme.DrScheme.plist
    hard drive/Users/_user name_/Library/Preferences/org.plt￾scheme.prefs.ss
    hard drive/Users/_user name_/Library/Preferences/org.racket￾lang.DrRacket.plist
    hard drive/Users/_user name_/Library/Preferences/org.racket￾lang.prefs.rktd
    hard drive/Users/_user name_/Library/Preferences/PLT-autosave-toc
    hard drive/Users/_user name_/Library/Preferences/PLT-autosave￾toc-save
    hard drive/Users/_user name_/Library/Preferences/PLT-autosave￾toc.rktd
    hard drive/Users/_user name_/Library/Preferences/PLT-autosave￾toc-save.rktd
    
 Copy the file org.racket-lang.prefs.rktd from the main branch of this repository into the folder 
    hard drive/Users/user name/Library/Preferences

Open Dr Racket, hit 'Run' in the upper right hand corner, and you're all set! 
    Note that all files should (automatically) start with the following two lines in order to use scheme traductor
        #lang scheme
        (require scheme-traductor/scheme-traductor)
