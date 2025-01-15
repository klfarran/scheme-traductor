# esquemático!
### Scroll down for English
- #### Proyecto oficial para _esquemático_: escribir programas en el lenguaje de programación _Scheme_, ¡pero en español!
- #### Para ser utilizado con la interfaz gráfica de [Dr Racket](https://www.racket-lang.org/) 
- #### Diseñado por Kelly Farran bajo la asesoría del Dr. Sean McCulloch del departamento de matemáticas e informática en la Universidad de Ohio Wesleyan, EE.UU 

---

## Instrucciones de instalación para el sistema operativo Windows
- Navega a [Descargar Dr Racket](https://download.racket-lang.org)
  - En el menú desplegable 'Platform', selecciona la versión de Windows adecuada para tu computadora
  - Haz clic en el botón "racket-...exe"
  - Ejecuta el archivo "racket....exe"

- Descarga la carpeta _scheme-traductor_ de la rama 'main' de este repositorio
  - Copia la carpeta _scheme-traductor_ en la carpeta Racket/collects
  - Suponiendo que la unidad C: sea la unidad principal de tu computadora, generalmente se encuentra _Racket/colects_ en C:\Archivos de programa\Racket

- Elimina la carpeta 'Racket' ubicada en C:\Usuarios\*nombre de usuario*\AppData\Roaming\Racket
  - Crea una carpeta nueva llamada 'Racket' dentro de la carpeta C:\Usuarios\*nombre de usuario*\AppData\Roaming
  - Descarga el archivo _racket-prefs.rktd_ de la rama 'main' de este repositorio
  - Pon el archivo _racket-prefs.rktd_ dentro de la carpeta 'Racket' que creaste
 
- Abre Dr Racket, haz clic en _Run_ en la esquina superior derecha, ¡y ya está!
    - Ten en cuenta que todos los archivos deben comenzar (automáticamente) con las siguientes dos líneas para usar _esquemático_
        - #lang scheme
        - (require scheme-traductor/scheme-traductor)

## Instrucciones de instalación para el sistema operativo Mac
- Navega a [Descargar Dr Racket](https://download.racket-lang.org)
  - En el menú desplegable 'Platform', selecciona la versión de MacOS adecuada para tu computadora
  - Haz clic en el botón “racket-...dmg”
  - En el cuadro de diálogo de abrir/guardar que aparece, elige guardar el archivo "racket-...dmg"
  - Haz doble clic en el archivo de imagen de disco racket....dmg que guardaste
  - Desde la carpeta del disco que se abre, arrastra la carpeta Racket vXXXX (donde XXXX es el número de la versión actual de DrRacket) a la carpeta _Aplicaciones_
 
- Descarga la carpeta _scheme-traductor_ de la rama 'main' de este repositorio
  - Copia la carpeta _scheme-traductor_ en la carpeta Racket/collects
  - La carpeta _collects_ deber estar ubicada en _disco duro_/Aplicaciones/Racket vXXXX, donde XXXX es el número de la versión actual de Dr Racket

- Abre la aplicación _Terminal_ y asegúrate de que toda la jerarquía /Applications/Racket vXXXX/collects sea legible y ejecutable por todos, pero no escribible por todos, ejecutando el comando:
  - chmod -R go+rx-w /Applications/Racket\ vXXXX/collects
 
- Si existen, elimina los archivos:
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/org.plt-scheme.DrScheme.plist
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/org.plt-scheme.prefs.ss
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/org.racket-lang.DrRacket.plist
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/org.racket-lang.prefs.rktd
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/PLT-autosave-toc
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/PLT-autosave-toc-save
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/PLT-autosave-toc.rktd
  - _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias/PLT-autosave-toc-save.rktd
 
- Descarga el archivo _org.racket-lang.prefs.rktd_ de la rama 'main' de este repositorio
   - Copia el archivo _org.racket-lang.prefs.rktd_ en la carpeta _disco duro_/Usuarios/_nombre de usuario_/Biblioteca/Preferencias

- Abre Dr Racket, haz clic en _Run_ en la esquina superior derecha, ¡y ya está!
    - Ten en cuenta que todos los archivos deben comenzar (automáticamente) con las siguientes dos líneas para usar _esquemático_
         - #lang scheme
        - (require scheme-traductor/scheme-traductor)

---        

# esquemático! 
- #### Official project for _esquemático_: write in Scheme, but in Spanish! 
- #### For use with [Dr Racket](https://www.racket-lang.org/) graphical interface
- #### Designed by Kelly Farran under the advisement of Dr. Sean McCulloch of the Department of Mathematics and Computer Science at Ohio Wesleyan University


## Install and Configure Dr Racket with _scheme traductor_ for Windows: 
- Navigate to [Download Dr Racket](https://download.racket-lang.org) 
    - Download the version appropriate for your computer and operating system (Windows)
    - Click on the “racket-...exe” button
    - Execute the "racket....exe" file 

- Download the _scheme-traductor_ folder from the main branch in this repository
    - Copy the _scheme-traductor_ folder into the Racket/collects folder 
    - Assuming that the C: drive is the main drive on your computer, this is usually located at: C:\Program Files\Racket
    
- Delete the 'Racket' folder located at C:\Users\*user name*\AppData\Roaming\Racket
    - Create a new folder named 'Racket' inside the C:\Users\*user name*\AppData\Roaming folder 
    - Download the _racket-prefs.rktd_ file from the main branch of this repository
    - Place the _racket-prefs.rktd_ file inside of the 'Racket' folder you created  
    
- Open Dr Racket, hit _Run_ in the upper right hand corner, and you're all set! 
    - Note that all files should (automatically) start with the following two lines in order to use _scheme traductor_
        - #lang scheme
        - (require scheme-traductor/scheme-traductor)
  
## Install and Configure Dr Racket with _esquemático_ for Mac: 
- Navigate to [Download Dr Racket](https://download.racket-lang.org) 
    - Download the version appropriate for your computer and operating system (MacOS)
    - Click on the “racket-...dmg” button
    - In the opening/saving dialog that follows choose to save the file "racket....dmg"
    - Double-click on the "racket....dmg" disk-image file that you saved 
    - From the disk folder that opens, drag the Racket vXXXX folder (where XXXX is the number of the current version of DrRacket) to the _Applications_ folder

- Download the scheme-traductor folder from the main branch in this repository
    - Copy the scheme-traductor folder into the Racket/collects folder 
    - The 'collects' folder should be located at _hard drive_/Applications/Racket vXXXX, where XXXX is the number of the current version of Dr Racket

- Open the Terminal application and make sure that the entire /Applications/Racket vXXXX/collects hierarchy is world-readable and world-executable, but not world-writeable by executing the command
    - chmod -R go+rx-w /Applications/Racket\ vXXXX/collects

 - If they exist, delete the files
    - _hard drive_/Users/_user name_/Library/Preferences/org.plt-scheme.DrScheme.plist
    - _hard drive_/Users/_user name_/Library/Preferences/org.plt-scheme.prefs.ss
    - _hard drive_/Users/_user name_/Library/Preferences/org.racket-lang.DrRacket.plist
    - _hard drive_/Users/_user name_/Library/Preferences/org.racket-lang.prefs.rktd
    - _hard drive_/Users/_user name_/Library/Preferences/PLT-autosave-toc
    - _hard drive_/Users/_user name_/Library/Preferences/PLT-autosave-toc-save
    - _hard drive_/Users/_user name_/Library/Preferences/PLT-autosave-toc.rktd
    - _hard drive_/Users/_user name_/Library/Preferences/PLT-autosave-toc-save.rktd
    
 - Copy the file org.racket-lang.prefs.rktd from the main branch of this repository into the folder 
    - _hard drive_/Users/user name/Library/Preferences

- Open Dr Racket, hit 'Run' in the upper right hand corner, and you're all set! 
    - Note that all files should (automatically) start with the following two lines in order to use esquemático
        - #lang scheme
        - (require scheme-traductor/scheme-traductor)
