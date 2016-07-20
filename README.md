# somekindofpuzzle
by s6makano and x432ph

Beschreibung:  
Somekindofpuzzle ist ein Rätselspiel, bei dem man einen Weg vom Startfeld zum Zielfeld finden muss. Jedes Level wird zufällig erstellt, besitzt aber eine garantierte Lösung, Eindeutigkeit ist nicht gegeben.  

Anleitung:  
Klicke auf ein Nachbarfeld, um dorthin zu gehen. Klicke auf einen Teil deines Weges, um zu diesem Zeitpunkt zurückzuspringen.  
Es gibt diverse Sonderfelder, welche die Bewegungsfreiheit modifizieren oder zusätzliche Siegbedingungen aufstellen. Deren genaue Funktionsweise herauszufinden ist Teil des schier endlosen Spielspaßes!

Technische Hilfe:  
Das Spiel wurde mit dem wx-package von Haskell umgesetzt. Zur Installation sei folgende Hilfsseite ans Herz gelegt: https://wiki.haskell.org/WxHaskell/Windows#Installing_the_easy_way durchführen. Neustarten am Ende nicht vergessen, sonst geht es nicht!  
Das Spiel wird dann wie auch in der „Installationsanleitung“ beschrieben mittel cabal configure, build, install kompiliert.  

Entwicklungsplan:  
21.6. Absegnung des Projektes ✓  
30.6. Gestaltung eines Interfaces mit wx ✓  
8.7.  Algorithmus zur Kontrolle eines gültigen Weges  
      dafür bereits Umsetzung von Blockade ✓  
13.7. Algorithmus zu Erstellung einer Spielinstanz  
      dafür weitere Elemente ✓  
bis zu Projektabgabe: Tuning an Schwierigkeit und Erstellung weiterer Elemente ✓  

Versionhistory:  
0.1 – Wall  
{- 0.2 - Wall, Timemachine -}  
0.3 Wall, Portals, Minesweepterthingys  
0.4 Wall, Portals, Minesweepterthingys, Key
