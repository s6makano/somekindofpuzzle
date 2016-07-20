# somekindofpuzzle
by s6makano and s6jogrun

Beschreibung:  
Somekindofpuzzle ist ein Rätselspiel, bei dem man einen Weg vom Startfeld zum Zielfeld finden muss. Jedes Level wird zufällig erstellt, besitzt aber eine garantierte Lösung, Eindeutigkeit ist nicht gegeben.  

Anleitung:  
Klicke auf ein Nachbarfeld, um dorthin zu gehen. Klicke auf einen Teil deines Weges, um zu diesen Zeitpunkt zurückzuspringen.  
Wälle sind nicht begehbar, Portale erlauben einem, zu einem Portal der anderen Farbe zu springen. Steht auf einem Feld eine Zahl, so musst du genau so viele der neun Umgebungsfelder dieses Feldes auf deinem finalen Weg begangen haben.  
Manchmal ist deine Haustür verschlossen (nicht offensichtlich); versuche, den Schlüssel in deinen Weg zu integrieren!  

Technische Hilfe:  
Das Spiel wurde mit dem wx-package von Haskell umgesetzt. Zur Installation sei folgende Hilfsseite ans Herz gelegt: https://wiki.haskell.org/WxHaskell/Windows#Installing_the_easy_way durchführen. Neustarten am Ende nicht vergessen, sonst geht es nicht!  
Das Spiel wird dann wie auch in der „Installationsanleitung“ beschrieben mittel cabal configure, build, install kompiliert.  
Im Ordner somekindofpuzzle finden sich alle benötigten Dateien, picturesets beinhaltet andere Settings für die Bilder (momentan noch nicht). Obsolet kann gekonnt ignoriert werden.  

Entwicklungsplan:  
21.6. Absegnung des Projektes ✓  
30.6. Gestaltung eines Interfaces mit wx ✓  
8.7.  Algorithmus zur Kontrolle eines gültigen Weges  
      dafür bereits Umsetzung von Blockade ✓  
13.7. Algorithmus zu Erstellung einer Spielinstanz  
      dafür weitere Elemente ✓  
bis zu Projektabgabe: Tuning an Schwierigkeit und Erstellung weiterer Elemente  

Versionhistory:  
0.1 – Wall  
{- 0.2 - Wall, Timemachine -}  
0.3 Wall, Portals, Minesweepterthingys