# somekindofpuzzle
by s6makano and s6jogrun

Beschreibung:  
Es soll ein Rätselspiel werden, bei dem man das gesamte Spielfeld inklusive Start- und Endfeld sieht. Ein gültiger Weg ist eine Linie vom Start zum Ziel. Dabei ist das Spielfeld diskret, vermutlich in Form von dargestellten Plättchen. Jedes einzelne dieser soll interaktiv angeklickt werden können, wodurch es in den Weg eingebunden bzw. wieder aus diesem entfernt wird.  
Die Schwierigkeit besteht darin, dass verschiedene Elemente Voraussetzungen an den Weg stellen. Beginnend mit einfachen Blockaden wird es später auch an Felder geben, die x Teile vom Weg in der direkten Umgebung des Feldes fordern. Bleibt Zeit über, könnten auch interaktive Elemente wie plättchenbewegende Schalter umgesetzt werden.  
Alle Elementideen müssen natürlich auf Praktikabilität untersucht werden. Mit fortschreitendem Level gibt es mehr Elemente und damit weniger Möglichkeiten für einen gültigen Zug, wodurch der Schwierigkeitsgrad ansteigt.  

Der Quellcode wird zwei Schwerpunkte haben. Da die Lösung nicht eindeutig sein muss, wird es eine Routine geben, die überprüft, ob ein abgegebener Zug eine Lösung darstellt.  
Der Hauptfokus wird jedoch auf einer zufälligen Erstellung der Rätsel liegen, indem mit einem leeren Spielfeld, einem Weg darauf und einer Liste von einzubauenden Elementen ein Level erstellt wird. Dabei werden nach und nach Elemente aus der Liste in das Spielfeld integriert, sodass der vorliegende Weg noch immer eine Lösung darstellt (um Lösbarkeit zu garantieren), bis gemäß des Schwierigkeitsgrades genug Elemente integriert wurden.  
Die Elemente sollen dabei ein Datentyp sein, sodass der eigentliche Algorithmus nur abstrakt mit einem Datentyp Element arbeitet. Dadurch können, wenn der Kern des Projektes steht, in der verbliebenen Zeit weitere Elemente ausgedacht und integriert werden.  
Eine Umsetzung in reinem Haskell wird uns den Vorteil bringen, dass wir die Möglichkeiten unter anderem von lazy evaluation (für ein Terminieren in praktikabler Zeit) und Monaden nutzen können.

Für das Interface wollen wird wx-package benutzen, wie es beispielsweise nachfolgendes Spiel benutzt:  
https://hackage.haskell.org/package/babylon  
Dadurch kann das Spiel komplett haskellintern umgesetzt werden und wir lernen direkt am praktischen Beispiel, wie man ein interaktives Interface umsetzt.  

Entwicklungsplan:  
21.6. Absegnung des Projektes ✓  
30.6. Gestaltung eines Interfaces mit wx ✓  
8.7.  Algorithmus zur Kontrolle eines gültigen Weges  
      dafür bereits Umsetzung von Blockade ✓  
13.7. Algorithmus zu Erstellung einer Spielinstanz  
      dafür weitere Elemente  
bis zu Projektabgabe: Tuning an Schwierigkeit und Erstellung weiterer Elemente  

Versionhistory:  
0.1 – Wall  
0.2 - Wall, Timemachine  
