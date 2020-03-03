/** $VER: Version 0.01 (23 Feb 1999), by Thorsten Willert
******************************************************************************
** VERSION    : 0.01
** PROGRAMM   : Vorlage_Modul.rexx
** AUTHOR     : Thorsten Willert
** DISCRIPTION: Vorlage f�r CompressHTML- Module
** DATE       : 23 Feb. 1999
** STATUS     : FREEWARE
** REQUIRES   : ARexx ;-)
**
** Erstellt mit Hilfe des ARexxWizards 0.2
** � 1998, by Thorsten Willert
*****************************************************************************/

SIGNAL ON ERROR
SIGNAL ON SYNTAX

PARSE ARG In,Out,Data,Path,Prefs
/* Eingabedatei,Ausgabedatei,Modulpfad (f�r evtl.Moduldaten),Ursprungspfad der Dateien,Voreinstellungen */

/* !! Es werden noch keine Voreinstellungen �bergeben !! */

IF In = "INFO" THEN
DO
    RETURN 100","0 /* Typ,Art

                              Typ:
                              0 = HTML
                              1 = ARexx
                              2 = AmigaGuide
                            100 = ASCII ;-) Einfach f�r allgemeine Plugins

                              Art:
                              0 = Compressor
                              1 = Decompressor
                              5 = Preprozessor

                              !! Diese Informationen braucht CompressHTML um die
                              Dateien VOR der �bergabe an das Modul, richtig
                              behandlen/testen zu k�nnen !! */
END
ELSE
DO

    CALL OPEN( InDatei,  In ,"R")
    CALL OPEN( OutDatei, Out,"W")

        /* Hier steht dann die Bearbeitungsroutine */

        /* Beispiel f�r einen "ASCII-Compressor" ;-))
        (er entfernt einfach alle Leerzeichen am Zeilenende) */
        CALL WRITELN( OutDatei , STRIP( READLN( InDatei ), 'E' )

    CALL CLOSE( InDatei)
    CALL CLOSE( OutDatei)

    RETURN 1 /* Bearbeitung war erfolgreich, mu� zur�ckgegeben werden! */
END

RETURN 0

/*****************************************************************************/

ERROR:
SYNTAX:

RETURN RC","SIGL

/* Fehlermeldung wird dann von CompressHTML ausgegeben!

CompressHTML bricht dann nicht ab, sondern gibt die entsprechende
Fehlermeldung aus, und zeigt die Zeile des Plugins, mit dem Fehler an. */
