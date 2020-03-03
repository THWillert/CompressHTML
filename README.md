> Backup von 1999 - Weiterentwicklung eingestellt.

Short:        HTML (De)Compressor/Preprozessor (German)  
Author:       thorsten.willert@gmx.de Thorsten Willert  
Uploader:     thorsten.willert@gmx.de (Thorsten Willert)  
Version:      0.606b  
Type:         comm/www  
Requires:     WB3.x, tritonrexx.library, rexxMOOS.library, rexxlocaldates.library  
Kurz:         HTML (De)Kompressor/Preprozessor  

![CompressHTML](/Images/CompressHTML.png)


Allgemeines:
- Triton- Oberfläche
- Integrierte Projektverwaltung
- Die Hauptfunktionen sind als Plugins realisiert
- Plugins können mit internen Funktionen über eine Funktionsliste beliebig kombiniert werden (Batchprozessor)
- Dokumentation in HTML


benötigt:
- dev/gui/tri20b2usr.lha        (Triton)
- util/rexx/TritonRexx376.lha   (TritonRexx)
- util/rexx/MOOS.lha            (rexxMOOS)
- util/rexx/RexxLocalDates.lha  (RexxLocalDates)

___

Hauptprogramm:

Neues:
- Über das Tooltype TEMPLATE können nun bis zu zehn Vorlagen ins Menü integriert werden.
- Schriften für die Oberfläche und die Listen sind jetzt einstellbar.
- Statt einem bestimmten Projekt, kann nun auch das zuletzt verwendete, bei Programmstart geöffnet werden.
- Alle Fenster die beim Beenden offen waren, werden beim Starten auch wieder geöffnet.
- Shortcuts für Menüs und die Oberfläche eingebaut.
- Die Gadgets Ein und Aus, für Funktionen, durch eines ersetzt.
- Mehrere interne Änderungen.
- Dokumentation und Beispiele überarbeitet.

Korrekturen:
- Das Tooltype TOFRONT funktioniert wieder.
- Fehler in Funktion BUFFER_TO_CACHE behoben.

___

Plugins:

· Zwei Startscripts für RexxOpt zum Komprimieren und Dekomprimieren von ARexx- Programmen.

· AmigaGuide-Kompressor:
· entfernt überflüssige Zeilen vor dem ersten und zwischen den @nodes

· HTML-Compressor(en) mit folgenden Funktionen:
· entfernen von Kommentaren
· löschen von Leerzeilen
· löschen von Zeilenumbrüchen
· entfernen von überflüssigen Leerzeichen
· PRE-formatierter Text und Scripts werden berücksichtigt

· HTML-Decompressor:
· Dekomprimiert Dateien wieder

· HTML-Preprozessor, mit folgenden Befehlen:
\#\#          Preprozessor- Kommentar, wird von diesem gelöscht.

#INCLUDE:   Einbinden einer Datei ab dieser Zeile (angegebene Datei oder über mehrere Standardsuchpfaden)

#DEFINE:    Definieren von Macros, diese dürfen wiederrum Befehle enhalten. An Macros können bis zu 100 Variablen übergeben werden.

Alles zwischen  
#DEFINE #BLOCK und  
#ENDDEF:    kann als Macro definiert werden. Länge bis 65k, mit bis zu 100 Variablen.

#UNDEF:     Löschen von Macro-Definitionen

Kontrollstruktur mit:  
#IFDEF:     Abfrage eines mit #DEFINE erstellten Bezeichners  
#IFNDEF:    Negation der vorherigen Abfrage  
#ELSE:      Alternativzweig  
#ENDIF:     Ende der #IFDEF- Struktur  
Keine Schachtelung möglich!

#TIME:      Einfügen der aktuellen Uhrzeit, in wahlfreiem Format.

#DATE:      Einfügen des aktuellen Datums/Tages, in wahlfreiem Format.

#FILETIME:  Einfügen der Speicherzeit der Quelldatei oder der angegebenen Datei, in wahlfreiem Format.

#FILEDATE:  Einfügen des Speicherdatums/Tages der Quelldatei oder der angegebenen Datei, in wahlfreiem Format.

#FILENAME:  Einfügen des Namens der Quelldatei mit, ohne oder nur Pfadangabe.

#FILESIZE:  Fügt die Größe der angegebenen Datei ein, mit optionaler automatischer Bezeichung (MB usw.).

#IMGSIZE:   Fügt die Dimensionen des angegebenen Bildes ein.

#CLI        Ausgabe des angegebenen CLI-Commandos wird ab dieser Stelle einfügt, wahlweise PRE-formatiert.

Alles zwischen  
#HTMLCODE   und  
#ENDHTMLCODE wird in sichtbaren HTML-Quelltext umgewandelt (optional: PRE-formatiert, Sonderzeichenwandlung)

Zeilen zwischen  
#SORT       und  
#ENSORT     können einfach, nach URL oder dem dazugehörigen LINK sortiert werden (auch Case-Sensitiv).

Mit vorgefertigten #INCLUDE-Dateien.

Pfadangaben relativ (wie in HTML) möglich.
