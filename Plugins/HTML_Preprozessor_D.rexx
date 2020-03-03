/** $VER: Version 0.41_D (02 Jan 2000), by Thorsten Willert
**/
/*****************************************************************************
** VERSION    : 0.41_D
** PROGRAMM   : HTML_Preprozessor_D.rexx
** AUTHOR     : Thorsten Willert
** DISCRIPTION: HTML- Preprozessor.
** DATE       : 02 Dez 2000
** STATUS     : FREEWARE
** REQUIRES   : ARexx ;-), rexxMOOS.library
**
** Erstellt mit Hilfe des ARexxWizards 0.2
** © 1998, by Thorsten Willert
*****************************************************************************/

SIGNAL ON ERROR
SIGNAL ON SYNTAX


PARSE ARG In,Out,Data,Pfad,Prefs .   /* Eingabedatei,Ausgabedatei,Modulpfad,Ursprungspfad,Voreinstellungen */

IF In = "INFO" THEN
DO
    RETURN 0","5 /* Typ (Preprozessor),Art (HTML)*/
END
ELSE
DO

    Ib = OpenLocale()
    CALL GetLocaleVars(Ib,LVars)

    TmpDatei = 'T:' || PRAGMA( 'ID' ) || '.tmp'

    /* Dateidaten abfragen */
    CALL EXAMINE( In , 'InFileData.' )

    /* Dateigröße speichern */
    Save.Size  = InFileData.Size

    /* Dateinamen */
    CALL SPLITPATH( In, 'Out1.' )

    st       = 1
    Includes = 1
    DatumOK  = 0
    TimeOK   = 0

   /* Include-Dateien und CLI-Ausgaben einsetzen */
   DO WHILE Includes >= 1

      CALL OPEN( InDatei,  In ,'R')
      CALL OPEN( OutDatei, Out,'W')

      DO WHILE ~EOF( InDatei )
         Zeile = READLN( InDatei )

           IF (INDEX( Zeile , '<!--#INC' ) ~= 0) | (INDEX( Zeile , '<!--#CLI' ) ~== 0 ) THEN
           DO
               PARSE VAR Zeile '<!--#INCLUDE' Datei '-->'
               Datei = STRIP( Datei )

               IF Datei ~= '' THEN
               DO
                   Ind = WORDINDEX(Zeile,1)
                   IF Ind == 1 THEN Indent = ''
                   ELSE Indent = COPIES(' ',Ind-1)

                   Includes = Includes + 1
                   Zeile = ''
                   CALL InsertINCLUDE()
                   ITERATE
               END

               /* Ausgabe von CLI einsetzen */
               PARSE VAR Zeile '<!--#CLI' "'"Commando"'" Option '-->'
               Commando = STRIP( Commando )

               IF Commando ~= '' THEN
               DO
                  Indent = ''
                  Zeile = ''
                  CALL InsertCLI()
               END

               CALL WRITELN( OutDatei, Zeile )
           END
           ELSE IF INDEX( Zeile , '<!--#SORT' ) ~== 0 THEN
           DO
               PARSE VAR Zeile '<!--#SORT' Option '-->'
               CALL LineSort(Option)

           END
           ELSE CALL WRITELN( OutDatei, Zeile )
           IF Includes > 25 THEN LEAVE
      END

      Includes = Includes - 1

      IF Includes > 1 THEN Includes = 1

      CALL CLOSE( InDatei)
      CALL CLOSE( OutDatei)

      CALL EXECUTE( 'COPY' Out 'TO' In ' CLONE')
   END

/* Wenn alle Include-Dateien eingebunden sind, Ersetz-Befehle anwenden */

   CALL OPEN( InDatei,  In ,'R')
   CALL OPEN( OutDatei, Out,'W')

   HTMLCode = 0
   HTMLOut  = 1
   Def.0    = 0
   Bez.0    = 0

   IFDef.   = 0
   Ifs      = 0
   EndIfs   = 0

   DO WHILE ~EOF( InDatei )
      Zeile = READLN( InDatei )

      /* Preprozessor-Kommentare löschen */
      IF INDEX(Zeile,'<!--##' ) ~== 0 THEN Zeile = DelCom( Zeile )

      IF STRIP(Zeile) ~== '' THEN /* Leerzeilen überspringen */
      DO
         /* Definition löschen */
         IF INDEX( Zeile, '<!--#UNDEF' ) ~==0 THEN
         DO
            CALL DelDefine(Zeile)
            ITERATE
         END

         /* Funktionsblöcke ausführen */
         IF IFDEFs() == 1 THEN ITERATE

         /* Definition auslesen und speichern (nur wenn auch IFDEF wahr ist) */
         IF (INDEX( Zeile , '<!--#DEFINE' ) ~== 0) & (HTMLOut == 1) THEN
         DO
            CALL MakeDefine(Zeile)
            ITERATE
         END

         /* DEFINE ersetzen */
         IF INDEX( Zeile , '<!--#IFDEF' ) == 0 THEN
         DO i = 1 TO Def.0
            IF INDEX(Zeile,Def.i.1) ~== 0 THEN
            DO
               Zeile = REPLACE( Zeile, Def.i.1, Def.i.2 )
               ITERATE
            END
            IF INDEX(Zeile,Def.i.3) ~== 0 THEN Zeile = ReplaceMacro( Zeile,i )
         END

         /* ENDDEF darf keines mehr vorkommen */
         IF INDEX( Zeile , '<!--#ENDDEF-->' ) ~== 0 THEN
         CALL REQ('Ungültige #ENDDEF Anweisung.',,'HTML_Preprozessor_D:')

         IF INDEX( Zeile , '<!--#' ) ~== 0 THEN
         DO
            IF INDEX( Zeile , '-->' ) == 0 THEN CALL Fehler(Zeile)
            ELSE
            DO
               /* HTML-Code lesbar machen */
               IF INDEX(Zeile,'<!--#HTMLCODE') ~== 0 THEN
               DO
                  PARSE VAR Zeile '<!--#HTMLCODE' HTMLCodeOption '-->'
                  HTMLCodeOption = UPPER(HTMLCodeOption)
                  IF HTMLCode == 0 THEN
                  DO
                     Zeile = ''
                     IF INDEX(HTMLCodeOption,'PRE') ~== 0 THEN Zeile = '<PRE>'
                     HTMLCode = 1
                  END
                  ELSE ITERATE
               END

               /* HTML-Code wieder übersetzbar machen */
               IF INDEX(Zeile,'<!--#ENDHTMLCODE-->') ~== 0 THEN
               DO
                  IF HTMLCode == 1 THEN
                  DO
                     Zeile = ''
                     IF INDEX(HTMLCodeOption, 'PRE') ~== 0 THEN Zeile = '</PRE>'
                     HTMLCode = 0
                  END
               END

               /* Datum einsetzen */
               IF INDEX(Zeile,'<!--#DATE' ) ~== 0 THEN
               DO
                  IF DatumOK == 0 THEN DatumOK = MakeDate()
                  Zeile = ReplaceDate( Zeile )
               END
               IF INDEX(Zeile,'<!--#FILEDATE' ) ~== 0 THEN Zeile = ReplaceFileDate( Zeile )

               /* Zeit einsetzen */
               IF INDEX(Zeile,'<!--#TIME') ~== 0 THEN
               DO
                  IF TimeOK == 0 THEN TimeOK = MakeTime()
                  Zeile = ReplaceTime( Zeile )
               END
               IF INDEX(Zeile,'<!--#FILETIME' ) ~== 0 THEN Zeile = ReplaceFileTime( Zeile )

               /* Dateiname einsetzen */
               IF INDEX(Zeile,'<!--#FILENAME') ~== 0 THEN Zeile = ReplaceFileName( Zeile )

               /* Dateigröße einsetzen */
               IF INDEX(Zeile,'<!--#FILESIZE') ~== 0 THEN Zeile = ReplaceSize( Zeile )

               /* Grafik Ausmase ermitteln */
               IF INDEX(Zeile,'<!--#IMGSIZE') ~== 0 THEN Zeile = ReplaceImg( Zeile )

               IF HTMLOut == 1 THEN CALL WRITELN( OutDatei, Zeile )
            END
         END
         ELSE
         DO
            IF HTMLCode == 1 THEN Zeile = MakeHTMLCode(Zeile,HTMLCodeOption)
            IF HTMLOut == 1 THEN CALL WRITELN( OutDatei, Zeile )
         END

      END
      END

      IF Ifs ~== 0 | EndIfs ~== 0 THEN
      DO
         CALL REQ('Falsche Anzahl #IFDEF/#IFNDEF #ENDIF  Anweisungen.',,'HTML_Preprozessor_D:')
         Ifs = 0
         EndIfs = 0
      END

      CALL CLOSE( InDatei)
      CALL CLOSE( OutDatei)
   END

   RETURN 1

   CALL CloseLocale()

END

RETURN 0

/*****************************************************************************/

InsertINCLUDE:

IF INDEX( Datei, '../' ) ~== 0 THEN Datei  = MakePath( Datei )

SELECT
   WHEN EXISTS( Datei )           THEN CALL InsertData( Datei )
   WHEN EXISTS( 'Include/'Datei ) THEN CALL InsertData( 'Include/'Datei )
   WHEN EXISTS( 'Inc/'Datei )     THEN CALL InsertData( 'Inc/' Datei )
   WHEN EXISTS( AddPart(Pfad,'Include/'Datei) ) THEN CALL InsertData( AddPart(Pfad,'Include/'Datei) )
   WHEN EXISTS( AddPart(Pfad,'Inc/'Datei) )     THEN CALL InsertData( AddPart(Pfad,'Inc/'Datei) )
   OTHERWISE
   DO FOREVER
      IF REQ('Include-Datei nicht gefunden:' Datei ,'_Auswählen|_Ignorieren','HTML_Preprozessor_D:') == 1 THEN
      DO
         Datei = RqtFileReq('','SYS:','TITLE "Include-Datei auswählen:"')
         IF Datei ~== '' & EXISTS( Datei ) THEN
         DO
            CALL InsertData( Datei )
            LEAVE
         END
      END
      ELSE LEAVE
   END
END

RETURN

/*****************************************************************************/

InsertCLI:

    CALL EXECUTE( Commando '>' || TmpDatei , 'dummy.' )

    IF UPPER(Option) = 'PRE' THEN CALL WRITELN( OutDatei , '<PRE>' )
    CALL InsertData( TmpDatei )
    IF UPPER(Option) = 'PRE' THEN CALL WRITELN( OutDatei , '</PRE>' )

    CALL EXECUTE( 'DELETE' TmpDatei, 'dummy.' )

RETURN

/*****************************************************************************/

InsertData: PROCEDURE EXPOSE Indent
PARSE ARG Datei

    IF OPEN( IncDatei, Datei ) THEN
    DO
      DO WHILE ~EOF( IncDatei )
         CALL WRITELN( OutDatei , Indent || READLN( IncDatei ) )
      END
      CALL CLOSE( IncDatei )
    END

RETURN

/*****************************************************************************/

LineSort: PROCEDURE
PARSE UPPER ARG Sort

n = 1
Sortierung = 0

   DO WHILE ~EOF( InDatei )
      Zeile = READLN( InDatei )
      IF INDEX( Zeile , '<!--#ENDSORT' ) ~== 0 THEN LEAVE
      PARSE VAR Zeile CAnfang '<A' CHref '>' CLink '</A>' CEnde

      Code.n.Anfang = CAnfang || '00'X || n
      Code.n.HREF   = CHref || '00'X || n
      Code.n.LINK   = CLink || '00'X || n
      Code.n.Rest   = CEnde || '00'X || n
      n = n + 1
   END

   n = n -1
   Code.0 = n

   IF (INDEX(Sort,'HREF') ~== 0) | (INDEX(Sort,'URL') ~==0) THEN
   DO i = 0 TO Code.0
      In.i = Code.i.HREF
   END
   ELSE IF INDEX(Sort,'LINK') ~== 0 THEN
   DO i = 0 TO Code.0
      In.i = Code.i.LINK
   END
   ELSE
   DO i = 0 TO Code.0
      In.i = Code.i.Anfang
   END

   In.0 = n

   IF INDEX(Sort,'CASE') ~== 0 THEN Sortierung = StemSort("In.* out. CASE")
   ELSE Sortierung = StemSort("In.* out.")

   IF Sortierung == 1 THEN
   DO j = 0 FOR out.count

      PARSE VAR out.j . '00'X Nr

      DO k = 1 TO out.count

         PARSE VAR Code.k.Anfang Anf '00'X AnfNr
         IF AnfNr == Nr THEN AnfangOut = Anf

         PARSE VAR Code.k.HREF Rf '00'X RfNr
         IF RfNr == Nr THEN RefOut = Rf

         PARSE VAR Code.k.Link Lnk '00'X LinkNr
         IF LinkNr == Nr THEN LinkOut = Lnk

         PARSE VAR Code.k.Rest Rst '00'X RestNr
         IF RestNr == Nr THEN RestOut = Rst
      END

      IF (RefOut ~== '') & (LinkOut ~== '') THEN CALL WRITELN( OutDatei,  AnfangOut || '<A' || RefOut || '>' || LinkOut || '</A>' || RestOut )
      ELSE CALL WRITELN( OutDatei,  AnfangOut || RestOut )
   END



RETURN

/****************************************************************************/

IFDEFs:

SELECT
   WHEN INDEX( Zeile , '<!--#IFDEF' ) ~== 0 THEN
   DO
      PARSE VAR Zeile '<!--#IFDEF' IFDefin '-->'
      IFDefin = STRIP( IFDefin )

      HTMLOut = 0
      IFDef = 0
      Ifs = Ifs + 1
      EndIfs = EndIfs + 1

      IF Ifs > 1 THEN CALL REQ('Achtung Datei enthält'||'0A'X||'geschachtelte #IFDEF Anweisungen!',,'HTML_Preprozessor_D:')

      DO u = 1 TO Bez.0
         IF IFDefin == Bez.u THEN
         DO
            HTMLOut = 1
            IFDef = 1
            LEAVE
         END
      END
      RETURN 1
   END
   WHEN INDEX( Zeile , '<!--#IFNDEF' ) ~== 0 THEN
   DO
      PARSE VAR Zeile '<!--#IFNDEF' IFDefin '-->'
      IFDefin = STRIP( IFDefin )

      HTMLOut = 1
      IFDef = 1
      Ifs = Ifs + 1
      EndIfs = EndIfs + 1

      DO u = 1 TO Bez.0
         IF IFDefin == Bez.u THEN
         DO
            HTMLOut = 0
            IFDef = 0
            LEAVE
         END
      END
      RETURN 1
   END
   WHEN INDEX( Zeile , '<!--#ELSE' ) ~== 0 THEN
   DO
      IF Ifs == 0 THEN CALL REQ('#ELSE oder #ELSEDEF Anweisung ohne #IFDEF.',,'HTML_Preprozessor_D:')
      IF IFDef == 0 THEN HTMLOut = 1
      ELSE HTMLOut = 0
      RETURN 1
   END
   WHEN INDEX( Zeile , '<!--#ENDIF-->' ) ~== 0 THEN
   DO
      HTMLOut = 1
      IFDef = 0
      Ifs = Ifs - 1
      EndIfs = EndIfs - 1
      RETURN 1
   END
   OTHERWISE NOP
END

RETURN 0

/*****************************************************************************/

MakeDefine: PROCEDURE EXPOSE Def. Bez. InDatei
PARSE ARG Zeile

EndDef = 0

PARSE VAR Zeile '<!--#DEFINE' Arg1 "'"Arg2

IF INDEX(UPPER(Zeile),'#BLOCK') == 0 THEN
    DO
        /* Normale Definition */
        Arg2 = STRIP(Arg2)
        IF (INDEX( Arg1, '-->' ) ~== 0) | (INDEX( Arg2, '-->') ~== 0) THEN
        DO
            IF Arg2 ~== '' THEN
            DO
               Arg2 = STRIP(SUBSTR(Arg2,1,LASTPOS('-->',Arg2 )-1))
               Arg2 = STRIP(Arg2,'T',"'")
            END
            ELSE Arg1 = SUBSTR( Arg1,1,LASTPOS('-->',Arg1)-1)

            /* Bezeichner */
            IF Arg2 == '' THEN
            DO
               Bez.0 = Bez.0 + 1
               BezNr = Bez.0
               Bez.BezNr = STRIP(Arg1)
               RETURN /* Bezeichner braucht nicht mehr darum RETURN !!!!*/
            END
         END
         ELSE CALL Fehler(Zeile)
    END
ELSE /* Definitionsblock */
DO
    DO WHILE ~EOF( InDatei )
        Zeile = READLN( InDatei )
        IF INDEX( Zeile, '<!--#ENDDEF-->' ) ~== 0 THEN
        DO
            PARSE VAR Arg1 Arg1 .
            EndDef = 1
            LEAVE
        END
        ELSE
        DO
            Arg2 = Arg2 || Zeile || '0A'X
            IF LENGTH(Arg2) > 65000 THEN LEAVE
        END
    END

    IF EndDef ~== 1 THEN CALL REQ('#DEFINE- #BLOCK ohne #ENDDEF!',,'HTML_Preprozessor_D:')
END

IF Arg1 ~== '' THEN
DO

   Def.0 = Def.0 + 1
   Nr = Def.0

   /* Variablen Parser */
   IF INDEX(Arg1,'(') ~== 0 & INDEX(Arg1,')') ~== 0 THEN
   DO
       PARSE VAR Arg1 Macro'(' Variablen ')'MacroEnd
       /* Variablen speichern */
       z = 0
       DO z = 1 TO 100
          PARSE VAR Variablen Variable','Variablen
          IF Variable = '' THEN LEAVE
          Def.Nr.Vars.z = STRIP(Variable)
       END
       Def.Nr.Vars.0 = z

       Def.Nr.3 = STRIP(Macro)||'('
       Def.Nr.4 = ')'||STRIP(MacroEnd)
   END

   /* Definition Speichern */
   Def.Nr.1 = STRIP(Arg1)

   /* In Definition \n durch Zeilenumbruch und
                    \t durch Tabulator ersetzen */
   Def.Nr.2 = REPLACE(STRIP(Arg2),'\n','0A'X)
   Def.Nr.2 = REPLACE(Def.Nr.2,'\t','09'X)

END

RETURN

/****************************************************************************/

ReplaceMacro: PROCEDURE EXPOSE Def. InDatei
PARSE ARG Zeile,Nr

i=0
z=0

Macr = Def.Nr.3
MacrEnd = Def.Nr.4

DO WHILE INDEX(Zeile,Def.Nr.3) ~== 0

    ZeileAlt = Zeile

    NewArg = Def.Nr.2

    PARSE VAR Zeile (Macr)Argumente(MacrEnd)

    IF Argumente ~== '' THEN
    DO
      ArgumenteAlt = Argumente
      DO z = 1 TO 100
      PARSE VAR Argumente Variable"','"Argumente
         IF (Variable == '') & (Argumente == '') THEN LEAVE
         Args.z = STRIP(STRIP(Variable),'B',"'")
      END
      Args.0 = z

      IF Def.Nr.Undef ~== 1 THEN
      DO
          IF Args.0 == Def.Nr.Vars.0 THEN
          DO z = Args.0 -1 TO 1 BY -1
             NewArg = REPLACE( NewArg , Def.Nr.Vars.z , STRIP(STRIP( Args.z ) ,'B',"'"))
          END
          ELSE
          DO
             CALL REQ('Falsche Anzahl an Argumenten bei Macroaufruf:'||'0A'X||Macr||SUBSTR(ArgumenteAlt,1,60)'...'MacrEnd,,'HTML_Preprozessor_D:')
             LEAVE
          END
      END

      Zeile = REPLACE(Zeile,Macr||ArgumenteAlt||MacrEnd,NewArg)

      IF Zeile == ZeileAlt THEN LEAVE

    END
END

RETURN Zeile

/****************************************************************************/

DelDefine: PROCEDURE EXPOSE Def. Bez. i
PARSE ARG '<!--#UNDEF' Arg Option '-->'

Arg = STRIP(Arg)
Option = UPPER(STRIP(Option))

IF Arg ~== '' THEN
DO
    DO i = 1 TO Def.0
       IF Def.i.1 = Arg THEN
       DO
          IF Option == 'CLEAR' THEN Def.i.2 = ''
          ELSE Def.i.2 = Def.i.1
          Def.i.Undef = 1
       END
       ELSE IF INDEX(Arg,Def.i.3) ~== 0 THEN
       DO
          IF Option == 'CLEAR' THEN Def.i.2 = ''
          ELSE Def.i.2 = Def.i.3 Def.i.4
          Def.i.Undef = 1
       END
    END

    DO z = 1 TO Bez.0
       IF Bez.z = Arg THEN Bez.z = ''
    END
END

RETURN

/****************************************************************************/
/* Preprozessor- Kommentare löschen */

DelCom: PROCEDURE
PARSE ARG Zeile

DO WHILE Kommentar ~== ''
   PARSE VAR Zeile Teil1 '<!--##'Kommentar'-->' Teil2
   Zeile = Teil1||Teil2
END

RETURN Zeile
/****************************************************************************/
/* HTML Code sichtbar machen */

MakeHTMLCode: PROCEDURE
PARSE ARG Zeile,Option

IF INDEX(Option, '&') ~== 0 THEN Zeile = REPLACE(Zeile,'&','&amp;')
Zeile = REPLACE(Zeile,'<','&lt;' )
Zeile = REPLACE(Zeile,'>','&gt;' )
Zeile = REPLACE(Zeile,'"','&quot;')

RETURN Zeile

/****************************************************************************/
/* Dateinamen ersetzen */

ReplaceFileName: PROCEDURE EXPOSE Pfad Out1.
PARSE ARG Zeile

DO WHILE INDEX(Zeile,'<!--#FILENAME') ~== 0
   PARSE VAR Zeile '<!--#FILENAME'Opt'-->'

   NewOpt = UPPER(Opt)
   IF Opt ~== '' THEN
   DO
      IF INDEX(NewOpt,'PATH') ~== 0 THEN
      NewOpt = Pfad
      ELSE IF INDEX(NewOpt,'FULL') ~== 0 THEN
      NewOpt = AddPart(Pfad,Out1.FilePart)

      Zeile = REPLACE( Zeile, '<!--#FILENAME'||Opt||'-->',NewOpt)
   END
   ELSE
   Zeile = REPLACE( Zeile,'<!--#FILENAME-->',Out1.FilePart)
END

RETURN Zeile

/****************************************************************************/
/* Aktuelles Datum ersetzen */

ReplaceDate: PROCEDURE EXPOSE Akt.
PARSE ARG Zeile

DO WHILE INDEX(Zeile,'<!--#DATE') ~== 0
    PARSE VAR Zeile '<!--#DATE'Opt'-->'

    IF Opt ~= '' THEN
    DO
       NewOpt = UPPER(Opt)
       NewOpt = REPLACE(NewOpt,'JJJJ' ,Akt.YYYY )
       NewOpt = REPLACE(NewOpt,'YYYY' ,Akt.YYYY )
       NewOpt = REPLACE(NewOpt,'JJ'   ,Akt.YY   )
       NewOpt = REPLACE(NewOpt,'YY'   ,Akt.YY   )
       NewOpt = REPLACE(NewOpt,'DAY'  ,Akt.Day  )
       NewOpt = REPLACE(NewOpt,'DDD'  ,Akt.ddd  )
       NewOpt = REPLACE(NewOpt,'TT'   ,Akt.dd   )
       NewOpt = REPLACE(NewOpt,'DD'   ,Akt.dd   )
       NewOpt = REPLACE(NewOpt,'*D'   ,Akt.d    )
       NewOpt = REPLACE(NewOpt,'MONTH',Akt.Month)
       NewOpt = REPLACE(NewOpt,'MMM'  ,Akt.mmm  )
       NewOpt = REPLACE(NewOpt,'MM'   ,Akt.mm   )
       NewOpt = REPLACE(NewOpt,'*M'   ,Akt.m    )
    END
    ELSE NewOpt = Akt.FullDate

    NewOpt = STRIP(NewOpt)
    NewOpt = REPLACE( SPACE(NewOpt,1), ' ','&nbsp;')
    /* Wegen März */
    NewOpt = REPLACE(NewOpt,'ä','&auml;')

    Zeile = REPLACE( Zeile, '<!--#DATE'||Opt||'-->',NewOpt )

END


RETURN Zeile

/****************************************************************************/
/* Aktuelle Uhrzeit ersetzen */

ReplaceTime: PROCEDURE EXPOSE Akt.
PARSE ARG Zeile

DO WHILE INDEX(Zeile,'<!--#TIME') ~== 0
    PARSE VAR Zeile '<!--#TIME'Opt'-->'

    IF Opt ~= '' THEN
    DO
       NewOpt = UPPER(Opt)
       NewOpt = REPLACE(NewOpt,'HH',Akt.TimeH )
       NewOpt = REPLACE(NewOpt,'MM',Akt.TimeM )
       NewOpt = REPLACE(NewOpt,'SS',Akt.TimeS )
    END
    ELSE NewOpt = Akt.Time

    NewOpt = STRIP(NewOpt)
    Zeile = REPLACE( Zeile, '<!--#TIME'||Opt||'-->',NewOpt )
END

RETURN Zeile

/***************************************************************************/
/* Speicherdatum einer Datei ersetzen */

ReplaceFileDate: PROCEDURE EXPOSE InFileData. Pfad Ib LVars.
PARSE ARG Zeile

DO WHILE INDEX(Zeile,'<!--#FILEDATE') ~== 0
    PARSE VAR Zeile '<!--#FILEDATE'Optionen'-->'
    PARSE VAR Optionen "'"Datei"'" Opt

    IF Datei ~== '' THEN Datei = GetFile(Datei)
    ELSE FileData.Date = InFileData.Date

    IF EXISTS(Datei) THEN CALL Examine(Datei,'FileData.')
    ELSE FileData.Date = InFileData.Date

    IF Datei == '' & Opt == '' THEN Opt = Optionen

    FDate = TRANSLATE( FileData.Date , ' ', '-' )
    CALL ParseDate(Ib,FileDate,'%d %b %y',FDate)

    IF Opt ~= '' THEN
    DO
      NewOpt = UPPER(Opt)
      NewOpt = REPLACE(NewOpt,'JJJJ' ,'%Y' )
      NewOpt = REPLACE(NewOpt,'YYYY' ,'%Y' )
      NewOpt = REPLACE(NewOpt,'JJ'   ,'%y' )
      NewOpt = REPLACE(NewOpt,'YY'   ,'%y' )
      NewOpt = REPLACE(NewOpt,'DAY'  ,'%A' )
      NewOpt = REPLACE(NewOpt,'DDD'  ,'%a' )
      NewOpt = REPLACE(NewOpt,'TT'   ,'%d' )
      NewOpt = REPLACE(NewOpt,'MONTH','%B' )
      NewOpt = REPLACE(NewOpt,'MMM'  ,'%b' )

      Save.mm  = FormatDate(Ib,FileDate,'%m')
      NewOpt = REPLACE(NewOpt,'MM','%m' )
      IF Save.mm < 10 THEN Save.m = SUBSTR(Save.mm,2,1)
      ELSE Save.m = Save.mm
      NewOpt = REPLACE(NewOpt,'*M',Save.m )

      Save.dd = FormatDate(Ib,FileDate,'%d')
      NewOpt = REPLACE(NewOpt,'DD','%d' )
      IF Save.dd < 10 THEN Save.d = SUBSTR(Save.dd,2,1)
      ELSE Save.d = Save.dd
      NewOpt = REPLACE(NewOpt,'*D',Save.d )

    END
    ELSE NewOpt = '%d. %b. %y'

    NewOpt = STRIP(NewOpt)
    NewOpt = FormatDate(Ib,FileDate,NewOpt)
    NewOpt = REPLACE( SPACE(NewOpt,1), ' ','&nbsp;')
    /* Wegen März */
    NewOpt = REPLACE(NewOpt,'ä','&auml;')

    Zeile  = REPLACE( Zeile, '<!--#FILEDATE'||Optionen||'-->',NewOpt )
END

RETURN Zeile

/****************************************************************************/
/* Speicherzeit einer Datei ersetzen */

ReplaceFileTime: PROCEDURE EXPOSE InFileData. Pfad Ib
PARSE ARG Zeile

DO WHILE INDEX(Zeile,'<!--#FILETIME') ~== 0
    PARSE VAR Zeile '<!--#FILETIME'Optionen'-->'
    PARSE VAR Optionen "'"Datei"'" Opt

    IF Datei == '' & Opt == '' THEN Opt = Optionen

    IF Datei ~== '' THEN Datei = GetFile(Datei)
    ELSE FileData.Time = InFileData.Time

    IF EXISTS(Datei) THEN CALL Examine(Datei,'FileData.')
    ELSE FileData.Time = InFileData.Time

    CALL MakeFileTime()

    IF Opt ~= '' THEN
    DO
       NewOpt = UPPER(Opt)
       NewOpt = REPLACE(NewOpt,'HH',Save.TimeH )
       NewOpt = REPLACE(NewOpt,'MM',Save.TimeM )
       NewOpt = REPLACE(NewOpt,'SS',Save.TimeS )
    END
    ELSE NewOpt = Save.Time

    NewOpt = STRIP(NewOpt)
    Zeile = REPLACE( Zeile, '<!--#FILETIME'||Optionen||'-->',NewOpt )

END

RETURN Zeile

/****************************************************************************/

ReplaceIMG: PROCEDURE EXPOSE Pfad
PARSE ARG Zeile

   NewOpt = ''
   ImgSrc = ''

DO WHILE INDEX(Zeile,'<!--#IMGSIZE') ~== 0
   PARSE VAR Zeile '<!--#IMGSIZE'Optionen'-->'
   PARSE VAR Optionen "'"Datei"'" Opt

   ImgSrc = GetFile(Datei)

   IF ImgSrc ~== '' THEN
   DO
      IF CreateImage(grafik,ImgSrc,'TRXIM_PubScreenName Default') == 1 THEN
      DO
         Breite = ImageWidth(grafik)
         Hoehe  = ImageHeight(grafik)
         CALL DeleteImage(grafik)

         Opt = STRIP(Opt)

         IF Opt ~== '' THEN
         DO
            NewOpt = UPPER(Opt)
            NewOpt = REPLACE(NewOpt,'HEIGHT', Hoehe )
            NewOpt = REPLACE(NewOpt,'WIDTH' , Breite )
         END
         ELSE NewOpt = 'WIDTH=' || Breite 'HEIGHT=' || Hoehe

      END
   END
   ELSE LEAVE

   Zeile = REPLACE( Zeile, '<!--#IMGSIZE'||Optionen||'-->',NewOpt )

END

RETURN Zeile

/***************************************************************************/

ReplaceSize: PROCEDURE EXPOSE Save. Pfad ZeileNr
PARSE Arg Zeile

FileSize = 0
DIM = ''

DO WHILE INDEX(Zeile, '<!--#FILESIZE' ) ~== 0
    PARSE VAR Zeile '<!--#FILESIZE'Opt'-->'
    PARSE VAR Opt "'"Datei"'" Option

    IF Datei ~== '' THEN
    DO
       IF INDEX(Datei,'../') ~== 0 THEN Datei = MakePath( Datei )
       ELSE IF ~EXISTS(Datei) THEN Datei = AddPart( Pfad , Datei )

       IF EXISTS(Datei) THEN
       DO
          CALL EXAMINE(Datei,'Out.')
          FileSize = Out.Size
       END
    END
    ELSE FileSize = Save.Size

       NewOpt = UPPER( Opt )

       /* Bezeichner evtl. automatisch bilden */
       IF INDEX( NewOpt , 'AUTO' ) ~== 0 THEN
       DO
         IF FileSize < 1000 THEN DIM = 'Byte'
         ELSE IF FileSize >= 1000000 THEN
         DO
            FileSize = FileSize +  500000
            FileSize = FileSize / 1000000
            DIM = 'MB'
         END
         ELSE IF FileSize >= 1000 THEN
         DO
            FileSize = FileSize +  500
            FileSize = FileSize / 1000
            DIM = 'kB'
         END
       END
       ELSE
       DO
         IF INDEX( NewOpt , 'KB' ) ~== 0 THEN
         DO
            FileSize = FileSize +  500
            FileSize = FileSize / 1000
         END
         ELSE IF INDEX( NewOpt , 'MB' ) ~== 0 THEN
         DO
            FileSize = FileSize +  500000
            FileSize = FileSize / 1000000
         END
       END

       FileSize = TRUNC(FileSize,0)

    Zeile = REPLACE( Zeile , '<!--#FILESIZE'Opt'-->', FileSize'&nbsp;'DIM )
END


RETURN Zeile

/***************************************************************************/

GetFile: PROCEDURE EXPOSE Pfad
PARSE ARG Datei

IF INDEX( Datei , '../' ) ~== 0 THEN File = MakePath( Datei )
ELSE
DO
   IF EXISTS( Datei ) == 1 THEN File = Datei
   ELSE IF EXISTS( AddPart( Pfad, Datei )) == 1 THEN File = AddPart( Pfad, Datei )
END

RETURN File

/****************************************************************************/

MakePath: PROCEDURE EXPOSE Pfad
PARSE ARG Datei

Dir=Pfad
i=0

DO WHILE INDEX(Datei,'../') ~== 0

   Datei = SUBSTR(Datei,4)

   IF INDEX(Dir,'/') ~== 0 THEN
      Dir = STRIP(SUBSTR(Dir,1,LASTPOS('/',Dir)),'T','/')
   ELSE
      Dir = SUBSTR(Dir,1,POS(':',Dir))

   i=i+1
   IF i>10 THEN LEAVE

END

RETURN AddPart(Dir,Datei)

/****************************************************************************/

Fehler: PROCEDURE EXPOSE ZeileNr
PARSE ARG line
CALL REQ('Ungültige Anweisung :'||'0A'X||STRIP(SUBSTR(line,INDEX(line,'<!--#') ,70 ) ),,'HTML_Preprozessor_D:')
RETURN

REQ: PROCEDURE
PARSE ARG txt,gads,title
args=''
IF gads~=='' THEN args=args 'GADS' gads
IF title~=='' THEN args=args 'TITLE' title
RETURN RqtEasyReq(txt,args)

REPLACE: PROCEDURE
PARSE ARG src,old,new
str=''
DO WHILE ''~=src
loc=POS(old,src)
PARSE VAR src sub (old) src
str=str||sub
IF loc~=0 THEN str=str||new
END
RETURN str

/*
REPLACEWORD: PROCEDURE
PARSE ARG src,old,new
loc=1;i=0
DO WHILE loc ~= 0
loc=FIND(src,old)
loc1=WORDINDEX(src,loc)
IF loc~==0 THEN src=DELWORD(src,loc,1)
IF loc1~==0 THEN src=INSERT(new' ',src,loc1-1)
i=i+1
IF i>=50 THEN LEAVE
END
RETURN src
*/

/****************************************************************************/

MakeTime: PROCEDURE EXPOSE Akt. Ib

    Akt.Time  = FormatDate(Ib,,'%H:%M:%S')
    Akt.TimeH = FormatDate(Ib,,'%H')
    Akt.TimeM = FormatDate(Ib,,'%M')
    Akt.TimeS = FormatDate(Ib,,'%S')

RETURN 1

/****************************************************************************/

MakeFileTime: PROCEDURE EXPOSE Save. Ib FileData.

    FTime = FileData.Time
    CALL ParseDate(Ib,FileTime,'%H:%M:%S',FTime)

    Save.Time  = FormatDate(Ib,FileTime,'%H:%M:%S')
    Save.TimeH = FormatDate(Ib,FileTime,'%H')
    Save.TimeM = FormatDate(Ib,FileTime,'%M')
    Save.TimeS = FormatDate(Ib,FileTime,'%S')

RETURN

/****************************************************************************/

MakeDate: PROCEDURE EXPOSE Akt. Ib LVars.

    Akt.FullDate = FormatDate(Ib,,'%d. %b. %y')

    Akt.YYYY  = FormatDate(Ib,,'%Y')
    Akt.YY    = FormatDate(Ib,,'%y')
    Akt.Month = FormatDate(Ib,,'%B')
    Akt.mmm   = FormatDate(Ib,,'%b')
    Akt.mm    = FormatDate(Ib,,'%m')

    IF Akt.mm < 10 THEN Akt.m = SUBSTR(Akt.mm,2,1)
    ELSE Akt.m = Akt.mm

    Akt.Day   = FormatDate(Ib,,'%A')
    Akt.ddd   = FormatDate(Ib,,'%a')
    Akt.dd    = FormatDate(Ib,,'%d')

    IF Akt.dd < 10 THEN Akt.d = SUBSTR(Akt.dd,2,1)
    ELSE Akt.d = Akt.dd

RETURN 1

/****************************************************************************/

ERROR:
SYNTAX:

EXIT RC","SIGL
