/* rx-patcher V1.0  */
/* Patch den rx-Befehl so, daß er kein Ausgabefenster mehr
   öffnet und legt das Ergebnis in SYS:Rexxc/RXNIL ab
*/

OPEN(in,"SYS:Rexxc/RX", "read")
OPEN(out,"SYS:Rexxc/RXNIL", "write")

daten=READCH(in,5000)
P=POS("CON:", daten)
IF P>0 THEN DO
  daten=LEFT(daten,p-1) || 'NIL:' || X2c('00') || SUBSTR(daten,p+5)
END
WRITECH(out,daten)

CLOSE(in)
CLOSE(out)

Say "RX ist gepatcht, der neue Befehl heisst RXNIL!"


