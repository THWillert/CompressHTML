/** $VER: Version 0.01 (05 Dez 1999), by Thorsten Willert
******************************************************************************
** VERSION    : 0.01
** PROGRAMM   : Arexx_Compress_1.rexx
** AUTHOR     : Thorsten Willert
** DISCRIPTION: ARexx - Compressor
** DATE       : 05 Dez. 1999
** STATUS     : FREEWARE
** REQUIRES   : ARexx ;-)
**
** Erstellt mit Hilfe des ARexxWizards 0.2
** � 1998, by Thorsten Willert
*****************************************************************************/
;SIGNAL ON ERROR;SIGNAL ON SYNTAX;PARSE ARG In,Out,Data,Path,Prefs;IF In="INFO" THEN;DO;RETURN 1","0;END;ELSE;DO;CALL EXECUTE('rexxopt' in out 'KBL FC NSM','dummy.*');RETURN 1;END;RETURN 0;ERROR:;SYNTAX:;RETURN RC","SIGL
