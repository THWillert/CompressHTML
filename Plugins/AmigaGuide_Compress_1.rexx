/** $VER: Version 0.01 (07 Nov 1999), by Thorsten Willert
******************************************************************************
** VERSION    : 0.01
** PROGRAMM   : AmigaGuide_Compress_1.rexx
** AUTHOR     : Thorsten Willert
** DISCRIPTION: AmigaGuide - Compressor
** DATE       : 07 Nov. 1999
** STATUS     : FREEWARE
** REQUIRES   : ARexx ;-)
**
** Erstellt mit Hilfe des ARexxWizards 0.2
** © 1998, by Thorsten Willert
*****************************************************************************/
;SIGNAL ON ERROR;SIGNAL ON SYNTAX;PARSE ARG In,Out,Data,Path,Prefs;IF In="INFO" THEN;DO;RETURN 2","0;END;ELSE;DO;CALL OPEN(InDatei,In,"R");CALL OPEN(OutDatei,Out,"W");FirstNode=0;Node=0;DO WHILE ~EOF(InDatei);Zeile=STRIP(READLN(InDatei),'T');ZeileU=UPPER(Zeile);IF INDEX(ZeileU,'@NODE') ~==0&Node==0 THEN FirstNode=1;IF INDEX(ZeileU,'@NODE') ~==0&FirstNode==1 THEN Node=1;IF INDEX(ZeileU,'@ENDNODE') ~==0&Node=1 THEN;DO;Node=0;CALL WRITELN(OutDatei,Zeile);END;IF FirstNode==0 THEN;DO;IF INDEX(Zeile,'@') ~==0 THEN;CALL WRITELN(OutDatei,Zeile);END;ELSE IF Node==1 THEN;DO;CALL WRITELN(OutDatei,Zeile);END;END;CALL CLOSE(InDatei);CALL CLOSE(OutDatei);RETURN 1;END;RETURN 0;ERROR:;SYNTAX:;RETURN RC","SIGL