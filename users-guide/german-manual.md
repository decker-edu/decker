# Installation

In diesem Kapitel finden Sie alle Informationen zur Installation und Einrichtung des Programms `decker`.

## Windows

Zur Installation unter Windows wird ein Installationsprogramm bereitgestellt.

### Installation mit Installationsprogramm

Laden Sie dieses herunter und starten es. Das Installationsprogramm ist nicht signiert. Es kann daher sein, dass es vom Betriebssystem als unsicher eingestuft wird. Sie können die Warnmeldung jedoch getrost umgehen und es trotzdem ausführen.

Das Installationsprogramm nimmt zwei Änderungen an Ihrem System vor:

Zum einen kopiert es das Programm `decker` in das gewählte Installationsverzeichnis.

Zum anderen fügt das Installationsprogramm den Pfad zum Installationsverzeichnis zum Suchpfad für Programme hinzu, damit das Programm `decker` unter eben diesem Namen auch von anderen Programmen gefunden werden kann.

### Überprüfung der Installation

Wenn das Programm erfolgreich installiert wurde können Sie `decker` nun entweder manuell über die Windows PowerShell ausführen oder es von anderen Programmen wie z.B. den `decker`-Plugins von Visual Studio Code bedienen lassen.

Die Visual Studio Code Plugins werden Sie darüber informieren, wenn Sie das Programm `decker` nicht finden können.

Zum direkten Überprüfen, ob die Installation erfolgreich war, können Sie in der Windows PowerShell das Programm `decker` ausführen.

Des weiteren können Sie selbst überprüfen, ob das Programm `decker` im angegebenen Installationsverzeichnis zu finden ist.

Ist dem der Fall können Sie überprüfen, ob der Suchpfad korrekt angepasst wurde, indem Sie die Umgebungsvariablen des Systems begutachten:

::: {.details summary="Anleitung mit Bildbeispielen"}

Suchen Sie im Startmenu von Windows nach dem Begriff "Umgebungsvariablen":

![](../data/umgebungsvariablen-startmenueintrag.png)

Klicken Sie im Systemeigenschaften Dialog auf "Umgebungsvariablen":

![](../data/umgebungsvariablen-systemmenu.png)

Selektieren Sie anschließend die Variable "Path" entweder für den Nutzer oder das System:

![](../data/umgebungsvariablen-menu.png)

Überprüfen Sie in der angezeigten Liste, ob der Installationspfad von Decker vorhanden ist:

![](../data/umgebungsvariablen-eintrag.png)

:::

### Installation per Hand

Sie können `decker` auch ohne das Installationsprogramm einrichten.

Downloaden Sie dazu das alleinstehende Programm `decker.exe`.

Legen Sie das Programm in ein Verzeichnis Ihrer Wahl.

Fügen Sie anschließend den Pfad zu diesem Verzeichnis dem Suchpfad des Systems hinzu, indem Sie einen Eintrag im im vorherigen Abschnitt beschriebenen Menu hinzufügen.

### Installation von optionalen Abhängigkeiten

Zusätzlich zu `decker` müssen Sie zum Nutzen aller Funktionalitäten folgende Programme separat installieren:

- [ffmpeg](https://ffmpeg.org/) zum Konveriteren von Videoaufnahmen, die mithilfe der Präsentationen angefertigt werden.
- [pdf2svg](https://github.com/jalios/pdf2svg-windows) zum Konvertieren von PDF-Dateien, die durch `pdflatex` beim Übersetzen von LaTeX-Quellcode generiert werden.
- [cwrsync](https://itefix.net/cwrsync) als Implementierung von `rsync` für Windows zur Verwendung von `decker publish`. Dieses setzt voraus, dass seine eigene Implementierung von `ssh` im Suchpfad vor der `OpenSSH` Implementierung von Windows liegt. Wenn Sie also andere Programme nutzen, die `ssh` verwenden überprüfen Sie nach Installation, ob diese mit der Cygwin-Version von `cwrsync` kompatibel sind. Das `rsync`-Programm von cwrsync funktioniert **nicht** mit der vorinstallierten `ssh`-Implementierung von Windows.

`decker` verwendet zur Umsetzung einzelner Funktionalitäten externe Programme.

`decker publish` benötigt `rsync` zum Synchronisieren Ihres Projektes auf einem externen Webserver.

`decker crunch` benötigt `ffmpeg` zum Konvertieren und Komprimieren von Videoaufnahmen.

`decker`, `decker html`, `decker decks`, `decker pages` und `decker handouts` benötigen zum Übersetzen von LaTeX Quellcodefragmenten eine installierte LaTeX-Distribution, die das Programm `pdflatex` zur Verfügung stellt. Zusätzlich wird `pdf2svg` genutzt, um die generierten PDFs in SVGs umzuwandeln, die dann in die Präsentation eingebettet werden.

Das selbe gilt für Quellcodefragmente für die Programme `gnuplot` und `plantuml`. Die entsprechenden Programme müssen ebenso separat installiert werden. `plantuml` wird für Windows jedoch nur als Java-Archiv ausgeliefert und kann daher nicht direkt ausgeführt werden. Sie benötigen zusätzlich eine Java-Installation und müssen ein Skript schreiben, welches als `plantuml` auf Ihrem Suchpfad liegt und die übergebenen Programmargumente an die Java-Ausführung von `plantuml.jar` weiterleitet:

```
... Batch-Skript für plantuml ...
```

## Linux

Die Installation von `decker` für Linux geschieht am schnellsten per Hand:

Zuerst laden Sie das ausführbare `decker`-Programm herunter.

Anschließend kontrollieren Sie, ob das Programm als **ausführbar** markiert ist und setzen es gegebenenfalls als solches: `chmod +x decker`.

Im Anschluss legen Sie das Programm in einem Verzeichnis ab, das auf Ihrem Suchpfad liegt.

Dieses ist für den einzelnen Benutzer für gewöhnlich `~/.local/bin/` oder Systemweit `/usr/local/bin/`.

### Installation optionaler Abhängigkeiten

Die optionalen Abhängigkeiten

## MacOS

Für MacOS wird ein Installationspaket bereitgestellt.

Dieses ist nicht signiert, daher müssen Sie bei Installation bestätigen, dass Sie die Sicherheitswarnung ignorieren wollen.

# Anwendungsfunktionen

`decker` ist eine Anwendung, welche Sie für gewöhnlich über die Kommandozeile ihres Systems bedienen sollten. Sie können dem Aufruf von `decker` unterschiedliche zusätzliche Argumente übergeben, um die auszuführende Funktion zu ändern. Im Folgenden werden diese Funktionen im Detail beschrieben.

Sie können die relevantesten Funktionalitäten von `decker` auch über das Visual Studio Code Plugin `decker server` mithilfe eines Rechtsklicks auf ihr Projektverzeichnis in der Dateiübersicht von Visual Studio Code aufrufen.

## Verzeichnis als Projektverzeichnis markieren

Damit das Programm `decker` das aktuelle Arbeitsverzeichnis als ein Projektverzeichnis erkennt muss im aktuellen Arbeitsverzeichnis eine Datei mit dem Namem `decker.yaml` enthalten sein. Die Datei muss selbst keinen Inhalt besitzen. In ihr können Sie Konfigurationsoptionen für ihr gesamtes Projekt hinterlegen. Näheres dazu können Sie im Kapitel für Konfigurationsoptionen erfahren.

## Grundlagen

Die Hauptaufgabe des Programms `decker` ist es Markdowndateien in Foliensätze oder Webseiten zu übersetzen. Die übersetzten Dateien, sowie alle Dateien, die zum Anzeigen der jeweilig erzeugten Dateien im Browser notwendig sind, werden in einem Verzeichnis mit dem Namen `public` abgelegt. Dieses Verzeichnis kann als solches von einem Webserver ausgeliefert werden, um die Präsentation auf dem lokalen Rechner oder auf ihrem eigenen Webhost zur Verfügung zu stellen.

Markdownquelltextdateien, die auf `-deck.md` enden werden in Foliensätze mit der Dateiendung `-deck.html` übersetzt. Markdownquelltextdateien, die auf `-page.md` enden werden in Webseiten mit der Dateiendung `-page.html` übersetzt.

Für jedes Projekt wird zusätzlich mindestens eine Datei mit dem Namen `index.html` generiert. Den Inhalt dieser Datei können Sie durch anpassen einer Datei `index.md` im Wurzelverzeichnis ihres Projektes ändern. Näheres dazu im Kapitel zur Indexdatei.

Zusätzlich können für `-deck.md`-Dateien auch Handouts mit der Dateiendung `-handout.html` erzeugt werden. Näheres dazu im Abschnitt `decker handouts`.

Für bereits übersetzte Foliensätze können mithilfe des Browsers Google Chrome PDF-Dateien der Foliensätze erzeugt werden. Diese werden in Dateien mit der Dateiendung `-deck.pdf` abgelegt. Näheres dazu im Abschnitt `decker pdf`.

### Beispiel

Ihr Projektverzeichnis beinhalte folgende Dateien.

```
decker.yaml
presentation-deck.md
supplementary-page.md
```

Ein Aufruf des Programms `decker`, gefolgt von `decker handouts` und `decker pdf` erzeugt folgende Resultate in Ihrem `public`-Verzeichnis:

```
index.html
presentation-deck.html
presentation-handout.html
pesentation-deck.pdf
supplementary-page.html
```

Zusätzlich werden Sie weitere Dateien in dem `public`-Verzeichnis vorfinden: `.json`-Dateien, ein `.decker`-Verzeichnis und das `support`-Verzeichnis. Deren Zweck wird im Kapitel für Technische Details und Ressourcenpakete behandelt und ist an dieser Stelle erstmal nicht relevant.

## Grundausführung

Rufen Sie das Programm `decker` ohne weitere Argumente auf werden aus allen im Projektverzeichnis und seinen Unterverzeichnissen befindlichen `-deck.md`- und `-page.md`-Dateien Foliensätze bzw. Webseiten generiert. Weitere Funktionalitäten können dieser Grundausführung in Zukunft hinzugefügt werden. Bisher ist dieser Aufruf identisch zum Aufruf `decker html`.

## HTML-Dateien generieren

Rufen Sie das Programm `decker` mit dem Argument `html` als `decker html` auf werden aus allen im Projektverzeichnis und seinen Unterverzeichnissen befindlichen `-deck.md`- und `-page.md`-Dateien Foliensätze bzw. Webseiten generiert. Dieser Aufruf ist zum expliziten Beschränken der Übersetzung auf diese Ergebnisse gedacht. Aktuell ist er identisch zum Aufruf von `decker` ohne Argumente.

## Foliensätze generieren

Rufen Sie das Programm `decker` mit dem Argument `decks` als `decker decks` auf werden aus allen im Projektverzeichnis und seinen Unterverzeichnissen befindlichen `-deck.md`-Dateien Foliensätze generiert. Dieser Aufruf beschränkt sich anders als der Aufruf von `decker html` auf die Foliensätze.

## Webseiten generieren

Rufen Sie das Programm `decker` mit dem Argument `pages` als `decker pages` auf werden aus allen im Projektverzeichnis und seinen Unterverzeichnissen befindlichen `-page.md`-Dateien Webseiten generiert. Dieser Aufruf beschränkt sich anders als der Aufruf von `decker html` auf die Webseiten.

## Handouts generieren

Rufen Sie das Programm `decker` mit dem Argument `handouts` als `decker handouts` auf werden aus allen im Projektverzeichnis und seinen Unterverzeichnissen befindlichen `-deck.md`-Dateien Handouts generiert. Diese repräsentieren den textuellen Inhalt des Foliensatzes und sind zum Nachlesen der Präsentation oder als Alternative für Nutzer von assistiven Technologien gedacht.

## PDFs generieren

Das Generieren von PDF-Dateien aus den Foliensätzen verwendet das Programm Google Chrome. Die Funktionalitäten von Google Chrome, die das Generieren von PDF-Dateien aus Webseiten erlaubt funktioniert nicht in der Version, die für Windowssysteme ausgeliefert wird. Entsprechend ist diese Funktionalität von `decker` nur unter Linuxsystemen und MacOS verfügbar. Wenn Sie unter Windows PDF-Dateien Ihrer Foliensätze haben möchten müssen Sie die Foliensätze mithilfe von `decker --server` in ihrem Browser selbst aufrufen und über das Navigationsmenu das Drucken des Foliensatzes als PDF manuell durchführen.

Das Aufrufen von `decker` mit dem Argument `pdf` benötigt bereits gebaute Foliensatzdateien, die mit Google Chrome geöffnet und mithilfe dieses Programms in ihre entsprechenden PDF-Dateien gedruckt werden. Entsprechend werden wie beim Aufruf von `decker decks` die Foliensätze übersetzt, bevor die eigentliche Funktionalität dieses Aufrufs umgesetzt wird.

## Projektverzeichnis aufräumen

Rufen Sie `decker` mit dem Argument `clean` auf wird das `public`-Verzeichnis gelöscht. Dies dient zum aufräumen und entfernen aller generierten Dateien, damit ein erneuter Übersetzungsvorgang alle Dateien neu übersetzen muss.

## Projektverzeichnis vollständig bereinigen

Neben dem `public`-Verzeichnis wird auch zusätzlich ein Verzeichnis namens `.decker` angelegt, in dem für den Übersetzungsprozess wichtige Metadaten zwischengespeichert werden. Diese müssen für gewöhnlich nicht erneut angelegt werden, wenn ein Projekt wiederholt übersetzt wird.

Möchten Sie jedoch das Projektverzeichnis von absolut allen von `decker` generierten Dateien bereinigen können Sie mithilfe von `decker purge` alles entfernen lassen.

## Projektverzeichnis veröffentlichen

Rufen Sie `decker` mit dem Argument `publish` als `decker publish` auf wird das Programm angewiesen mithilfe des Programms `rsync` ihr `public`-Verzeichnis auf einem anderen Rechner zu veröffentlichen. Das Programm `rsync` muss entsprechend auf Ihrem Rechner zur Verfügung stehen. Unter Windows bedeutet dies meist die Installation von Linux-Werkzeugen mithilfe der Umgebungen CygWin oder MinGW.

Die Konfiguration von `rsync` durch `decker publish` wird im Kapitel Konfigurationsoptionen behandelt.

## Dateien beobachten

Mithilfe der Programmoption `-w` bzw. `--watch` können Sie das programm `decker` dazu anweisen die Quelldateien in Ihrem Projektverzeichnis zu beobachten. Der Aufruf von Decker mit dieser Option muss manuell beendet werden, wenn Sie die Beobachtung beenden wollen. Wenn Änderungen an Ihren Quelldateien festgestellt werden baut `decker` die davon betroffenen Zieldateien sofort neu.

## Webserver starten

Der Aufruf von `decker` mit dem Argument `serve` als `decker serve` startet einen lokalen Webserver auf ihrem Rechner. Sie können mit der Option `-p` können Sie den Port, auf dem der Webserver horcht ändern. Der Standardport ist `8888`. Entsprechend erreichen Sie den Webserver mithilfe Ihres Browsers gewöhnlicherweise unter der Adresse [http://localhost:8888](http://localhost:8888).

Wenn Sie `decker` mit der Option `-S` oder `--server` starten wird wie beim Aufruf von `serve` ein Webserver gestartet. Zusätzlich werden alle Quelltextdateien beobachtet und neu gebaut, wenn sie geändert werden wie beim Aufruf von `decker --watch`. Zusätzlich werden angezeigte Foliensätze und Webseiten in Ihrem Browser dazu angewiesen ihren Inhalt neu zu laden, wenn ihre Quelldateien neu übersetzt wurden. Dadurch können Sie Änderungen an Ihrem Quelltext sofort in Ihrem Browser begutachten und nachvollziehen. Entsprechend ist der Aufruf von `decker --server` dem Aufruf von `decker serve` im Allgemeinen zu bevorzugen.

## Videodateien konvertieren

Videodateien, die sie mithilfe der Webpräsentationen von `decker` erstellen werden im `.webm`-Format abgespeichert. Zum Ausliefern der Videos mit ihrer Präsentation ist das Verwenden von `.mp4`-Dateien notwendig.

Rufen Sie `decker` mit dem Argument `crunch` als `decker crunch` auf wird das Programm dazu angewiesen mithilfe des Programms `ffmpeg` alle `-recording.webm`-Dateien, die zu ihren Präsentationen gehören, in entsprechende `-recording.mp4`-Dateien umzuwandeln. Videos, die Sie in mehreren Teilen aufgenommen haben werden ebenfalls durch diesen Befehl zusammengeführt.

## Optionale Abhängigkeiten überprüfen

Der Aufruf von `decker` mit dem Argument `check` als `decker check` weist das Programm dazu an zu überprüfen welche optionalen Abhängigkeiten installiert sind und in einer Liste aufzuzählen.

# Markdown

## Grundlagen

Markdown ist eine Auszeichnungssprache die dazu gedacht ist direkt in HTML-Dokumente überführt werden zu können. Ihr Ziel ist es dabei im besonderen den auf der übersetzten Webseite repräsentierten Text im Quelltext so ähnlich wie möglich wiederzuspiegeln.

Als solche besitzt Markdown keine wirklichen Möglichkeiten zum definieren von Layout oder Design des repräsentierten Textes. Dies ist Aufgabe der Webseite, in die der aus dem Markdown-Quelltext übersetzte HTML-Quellcode eingebettet wird.

Decker verwendet zur Übersetzung von Markdown nach HTML die Bibliothek `pandoc`. Diese erlaubt es den Übersetzungsprozess von Markdown nach HTML anzupassen und um viele zusätzliche Funktionalitäten zu erweitern. Im Folgenden werden daher sowohl die weiter verbreiteten Markdowngrundlagen erklärt, als auch die von Decker verwendeten Anpassungen.

In Markdown werden alle Textbausteine semantisch durch eine Leerzeile (zwei aufeinander folgende Zeilenumbrüche) voneinander getrennt. Dies erlaubt es im Quelltext einzelne Bausteine schnell voneinander zu unterscheiden. Im übersetzten Dokument werden einzelne Textbausteine für gewöhnlich untereinander angeordnet. Dies macht sich im Fließtext meist durch einen sichtbaren Zeilenunbruch und einen kleinen Abstand zwischen den Textbausteinen bemerkbar.

### Überschriften

In Markdown werden Überschriften mithilfe von `#`-Rauten-Symbolen eingeleitet. Die Anzahl an `#`-Rauten-Symbolen bestimmt den Rang der Überschrift.

Überschriften der Ebene 1 haben für `decker` eine besondere Bedeutung, denn sie leiten eine neue Folie in einem Foliensatz ein. Die verwendete Überschrift ist dann der Titel der entsprechenden Folie.

Wenn Sie eine neue Folie ohne Titel einleiten wollen können Sie dies mit einer `#`-Raute ohne weiteren Text umsetzen.

#### Beispiele

Überschriften der Ebene 1 leiten neue Folien in Ihrem Foliensatz ein:

```
# Meine erste Folie

Dies ist meine erste Folie.

# Meine zweite Folie

Dies ist meine zweite Folie.

#

Diese Folie hat keinen Titel.
```

Überschriften anderer Ebenen können Sie benutzen um den Inhalt einer Folie zu strukturieren:

```
# Folientitel

Text

## Themenüberschrift

Mehr Text

## Weiteres Thema

Noch mehr Text

### Unterunterüberschrift

Viel mehr Text
```

### Paragraph

Einen einzelnen Textparagraphen können Sie in Markdown durch notieren des Textinhalts im Quelltext beschreiben. Wie alle Quelltextbausteine in Markdown werden zwei Paragraphen von einer Leerzeile voneinander getrennt.

#### Beispiel

Wenn Sie zwei Zeilen im Quelltext folgendermaßen notieren werden Sie als zwei Teile ein und des selben Paragraphen verstanden. Zeilenumbrüche, die im Quelltext vorkommen werden im Resultat nicht übernommen:

```
Diese Zeile gehört zu einem Paragraphen.
Diese Zeile gehört zum selben Paragraphen. Das Resultat wird keinen Zeilenumbruch besitzen.
```

Möchten Sie einen Zeilenumbruch innerhalb eines Paragraphen erzwingen müssen Sie die Zeile, nach der umgebrochen werden soll, mit zwei Leerzeichen beenden.

```
Diese Zeile ist Teil eines Paragraphen. Nach ihr wird umgebrochen.  
Diese Zeile ist Teil des selben Paragraphen, vor ihr wurde jedoch umgebrochen.
```

Möchten Sie zwei Paragraphen voneinander trennen müssen Sie diese im Quelltext mit einer Leerzeile voneinander trennen:

```
Diese Zeile gehört zu einem Paragraphen.

Diese Zeile gehört zu einem anderen Paragraphen.
```

### Textdekoration

In Markdown können Sie Fließtext mit Sonderzeichen umranden. Der Text zwischen den Sonderzeichen wird besonders dargestellt.

Wenn Sie Text mit einzelnen `*`-Asterisk-Symbolen umranden wird dieser *emphasiert*. Dies geschieht für gewöhnlich durch *kursiven Text*.

Wenn Sie Text mit zwei `**`-Asterisk-Symbolen umranden wird dieser **gestärkt**. Dies geschieht für gewöhnlich durch **Fettdruck**.

Das verwenden von drei `***`-Asterisk-Symbolen wendet beide ***Textdekorationen*** auf den Text an.

Weitergehend können Sie Text mit zwei `~~`-Tilden umranden, um ihn ~~durchzustreichen~~.

Quelltext können Sie im Fließtext mithilfe von einzelnen `` ` ``-Backtick-Symbolen markieren. Dieser wird für gewöhnlich in einem `Monospace-Font` dargestellt. Soll Ihr Quelltext selbst auch einzelne `` ` ``-Backtick-Symbole beinhalten können Sie auch zwei ` `` ` zur Umrandung verwenden.

Sie können keinen Text unterstreichen, da er auf Webseiten ansonsten mit Links verwechselt wird.

#### Beispiel

Im Quelltext würden Sie die Textdekorationen folgendermaßen verwenden:

``` markdown
Dies ist ein Fließtext. Ich möchte *diesen Begriff* hervorheben.  
Diesen **anderen Begriff** möchte ich durch Fettdruck hervorheben.  
Andere Details möchte is mit ***beiden Methoden*** markieren.  
Diese ~~Tatsache~~ möchte ich deutlich als falsch markieren.
Dies ist mein `Quelltextfragment`, welches ich im Fließtext verwenden möchte.
```

### Blockzitate

Sie können einen Paragraphen als Blockzitat kennzeichnen, indem Sie ihn mit einem `>`-Kleiner-Symbol einleiten. Sie können weitere Zeilen hinter dem Blockzitat mit weiteren `>` einrücken, müssen dies jedoch nicht tun solange Sie die Inhalte nicht mit Leerzeilen voneinander trennen. Erst wenn Sie das Zitat mit zwei Zeilenumbrüchen vom nächsten Textbaustein trennen wird es beendet. Im Fließtext des Blockzitats gelten die selben Regeln für Zeilenumbrüche und Textdekoration wie für Paragraphen.

`decker` erlaubt es zusätzlich eine Quellenangabe oder sonstige Unterschrift dem Blockzitat hinzufügen. Sie können dies durch Notieren eines neuen Paragraphen direkt hinter dem Zitat umsetzen, der mit der Zeichenfolge `Caption:` beginnt.

#### Beispiel

Im Quelltext können Sie ein Blockzitat folgendermaßen angeben:

``` markdown
> Dies ist in Blockzitat.  
> Dieser Text gehört *weiterhin* zum Blockzitat.
Dieser Text ebenso.
> Auch diese Zeile setzt das Zitat fort, obwohl die Zeile über ihr nicht mit einem `>` beginnt.

Caption: Dies wird zur Zitatunterschrift des Blockzitats.

Erst dieser Text ist wieder sein eigener Paragraph.
```

### Listen

Eine ungeordnete Liste können Sie im Markdownquelltext notieren, indem Sie einen neuen Textbaustein mit einem Bindestrich (`-`) einleiten. Einen neuen Eintrag der Liste leiten Sie durch eine neue Zeile, die mit einem weiteren Bindestrich beginnt, ein. Wie auch zuvor wird eine Liste erst durch eine Leerzeile beendet. Auch Zeilenumbrüche und Textdekoration innerhalb der Listeneinträge folgt den selben Regeln für Fließtext wie bisherige Textbausteine.

Möchten Sie eine Aufzählung bzw. geordnete Liste notieren so müssen Sie die Zeilen der Listeneinträge nicht mit einem Bindestrich, sondern mit der gewünschten Zahl der Aufzählung, gefolgt von einem Punkt (`.`) einleiten.

Sie können die einleitenden Symbole eines Listeneintrags einrücken, um die Lesbarkeit im Quelltext zu verbessern.

Möchten Sie innerhalb eines Listeneintrags mehrere Paragraphen haben, so müssen Sie (mindestens) die erste Zeile des neuen Paragraphen um eine Ebene (ein Tab-Symbol oder vier Leerzeichen) einrücken.

#### Beispiel

Eine ungeordnete Liste im Markdownquelltext wird folgendermaßen notiert:

``` markdown
 - Mein erster Listeneintrag
 - Mein zweiter Listeneintrag
 Dieser Text ist weiterhin Teil des zweiten Listeneintrags
 - Mein dritter Listeneintrag mit zwei Leerzeichen am Ende  
 Dieser Text gehört zum dritten Listeneintrag, steht aber in seiner eigenen Zeile
 -  Mein vierter Listeneintrag mit mehreren Paragraphen
    
    Dieser Paragraph ist Teil des vierten Eintrags.
Nur die erste Zeile muss eingerückt werden, um die Zugehörigkeit zu kennzeichnen.
```

Die selbe Liste kann als geordnete Aufzählung folgendermaßen notiert werden:

``` markdown
 1. Mein erster Listeneintrag
 2. Mein zweiter Listeneintrag
 Dieser Text ist weiterhin Teil des zweiten Listeneintrags
 3. Mein dritter Listeneintrag mit zwei Leerzeichen am Ende  
 Dieser Text gehört zum dritten Listeneintrag, steht aber in seiner eigenen Zeile
 4. Mein vierter Listeneintrag mit mehreren Paragraphen
    
    Dieser Paragraph ist Teil des vierten Eintrags.
Nur die erste Zeile muss eingerückt werden, um die Zugehörigkeit zu kennzeichnen.
```

### Codeblöcke

Möchten Sie einen Textbaustein als vorformatierten Quelltext darstellen müssen Sie ihn mit mindestens drei ` ``` `-Backtick-Symbolen umgeben. Da hier davon ausgegangen wird, dass Sie einen mehrzeiligen Textbaustein notieren wollen sollten die einleitenden und beendenden Symbole in ihrer eigenen Zeile stehen. Hinter den einleitenden Symbolen können Sie den Namen der verwendeten Programmier- oder Auszeichnungssprache angeben, um der Darstellung des Textes die Möglichkeit zu geben die Schlüsselworte des Textes entsprechend hervorzuheben.

Sie können die Anzahl der einleitenden und beendenden Backtick Symbole varriieren, um im Quelltext selbst Backtick-Symbole verwenden zu können.

`decker` erlaubt es zusätzlich eine Quellenangabe oder sonstige Unterschrift dem Quelltext hinzufügen. Sie können dies durch Notieren eines Paragraphen direkt hinter dem Codeblock umsetzen, der mit der Zeichenfolge `Caption:` beginnt. Anders als bei Blockzitaten muss hier keine Leerzeile zwischen Unterschrift und Quelltext existieren.

#### Beispiel

Im Folgenden sehen Sie, wie man z.B. Markdownquelltext in das Dokument einbetten würde.

```` markdown
``` markdown
Dieser Text wird als vorformatierter Markdownquelltext angezeigt.
  Vorvormatierter Text berücksichtigt alle Leerzeichen, die notiert werden.
```
Caption: Vorformatierter Quelltext.
````

Möchten Sie eine Gruppe von drei `` ` ``-Backticks selbst im Quelltext anzeigen, so müssen Sie den Quellcode mit vier `` ` ``-Backticks einleiten.

````` markdown
```` markdown
``` markdown
Dies ist ein Beispiel, wie man im Quelltext selbst Backtick-Blöcke verwenden kann,
ohne dabei ausversehen die Quelltextumgebung zu schließen.
```
````
`````

Sie können eine breite Auswahl an Programmiersprachen für das Hervorheben von Syntaxelementen wählen:

```` markdown
``` c 
int main(int argc, char** argc) {
  printf("Hello World.\n");
  return 0;
}
```
`````

### Links

Ein Link besteht im Markdownquelltext aus zwei Teilen: Einem Linktext in eckigen Klammern und einer Ziel-URL in runden Klammern:

``` markdown
[Linktext](URL)
```

Ein Link kann so jederzeit im Fließtext eingebettet werden. Möchten Sie eine Kurzinformation angeben, die eingeblendet wird, wenn der Mauszeiger über dem Link schwebt, so können Sie diese in doppelten Anführungsstrichen innerhalb der URL-Klammern angeben:

``` markdown
[Linktext](URL "Kurzinformation")
```

Möchten Sie eine klickbare Link-URL als solche im Fließtext angeben können Sie die URL in spitzen Klammern im Quelltext notieren:

``` markdown
<http://example.org>
```

Sie können zudem Links zum späteren Referenzieren im Quelltext angeben. Diese haben keine textuelle Repräsentation, können aber im späteren Verlauf des Dokuments verwendet werden, um Links einfacher zu notieren und ihre Ziel-URL einheitlich zu halten, sollte sie sich nachträglich ändern. Die Auszeichnung dieser Linkdefinitionen im Quelltext ist auf unterschiedliche Arten und Weise möglich, beinhaltet aber immer die Informationen in der Reihenfolge: Linkdefinitionsname, URL, Kurzinformation

``` markdown
[Linkdefinition]: <http://example.org> (Kurzinformation)
[Linkdefinition]: http://example.org (Kurzinformation)
[Linkdefinition]: http://example.org 'Kurzinformation'
[Linkdefinition]: http://example.org "Kurzinformation"
```
Caption: All diese Definitionen sind gleichbedeutend, benutzen aber unterschiedliche Sonderzeichen.

Die so angelegte Linkdefinition können Sie anschließend im Dokument verwenden, indem Sie einfach den Namen der Linkdefinition in eckigen Klammern angeben:

``` markdown
[Linkdefinition]
```

Der Linktext ist ohne weitere Angabe der Name der Linkdefinition. Wenn Sie einen alternativen Linktext verwenden möchten, so können Sie den Linktext auch in einem weiteren Paar eckiger Klammern hinter dem Linkdefinitionsnamen angeben:

``` markdown
[Linktext][Linkdefinition]
```

#### Beispiel

Hier sehen Sie eine Möglichkeit einen Link im Fließtext zu verwenden:

``` markdown
Wenn Sie weitere Informationen haben möchten so klicken Sie [hier](http://example.org "Beispielwebseite").
```

Eine Linkdefinition würden Sie an beliebiger Stelle in Ihrem Dokument folgendermaßen notieren:

``` markdown
[Beispielwebseite]: http://example.org "Hier geht es zum Beispiel des Internets"
```

Die Linkdefinition können Sie dann im Fließtext folgendermaßen verwenden:

``` markdown
Sie können unsere [Beispielwebseite] besuchen, um mehr zu erfahren.
```

Möchten Sie einen anderen Linktext für die Referenz benutzen, können Sie diesen folgendermaßen angeben:

``` markdown
In unserem [Beispiel][Beispielwebseite] sehen Sie, wie man einen Referenzlink verwenden kann.
```

### Bilder

Ein Bild wird in Markdown ähnlich wie ein Link notiert, beginnt jedoch mit einem Ausrufezeichen:

```
![Bildunterschrift](URL oder Pfad)
```

Für gewöhnlich sind Bilder nicht Teil eines Fließtextes.

Sie können entweder Bilder aus dem Web mit einer URL referenzieren oder den Pfad zu einem Bild in ihrem Projektverzeichnis angeben. Das Verzeichnis, in dem Sie Ihre Bilder aufbewahren sollte in der Liste der `static-resource-dirs` stehen. Ein Bild in einem solchen Ordner können Sie dann vom Wurzelverzeichnis aus referenzieren, indem Sie den Pfad mit einem `/` beginnen lassen. Näheres dazu finden Sie im Kapitel zu Konfigurationsoptionen.

#### Beispiel

Ein externes Bild können Sie folgendermaßen einbetten:

```
![Logo der Wikipedia](https://www.wikipedia.org/portal/wikipedia.org/assets/img/Wikipedia-logo-v2.png)
```

Ein Bild aus Ihrem Projektverzeichnis würden Sie folgendermaßen referenzieren:

```
![Fotografie des Labors](/bilder/laboratory.png)
```

## Erweiterte Syntax

Die Programmbibliothek `pandoc` erlaubt es die Syntax von Markdown um viele verschiedene Mechanismen zu erweitern. In den folgenden Abschnitten werden die Menchanismen beschriben, die `decker` verwendet, um Ihnen mehr Möglichkeiten zum Gestalten des übersetzten HTML-Codes zu geben.

### Textabschnitte

Sie können im Fließtext einzelne Textabschnitte mit Attributen, CSS-Klassen oder Identifikatoren versehen, indem Sie den Textabschnitt mit eckigen Klammern umranden und hinter diesen die geschweiften Klammern zum zuweisen von Attributen (siehe kommender Abschnitt) setzen:

``` markdown
[Textinhalt]{ attribute }
```

#### Beispiel

Wenn Sie einen Textabschnitt zum Beispiel einfärben möchten würde man das folgendermaßen umsetzen:

``` markdown
In diesem Paragraphen ist [dieser Text]{style="color: red;"} rot.
```

### Abgegrenzte Bereiche

Ein abgegrenzter Bereich (engl. *fenced div*) ist dazu da, um Abschnitte in ihrem Dokument zu gruppieren und mithilfe von CSS-Klassen für besondere Funktionalitäten von `decker` zu markieren.

Einen Bereich können Sie abgrenzen, indem Sie ihn mit (mindestens) drei `:::`-Doppelpunkten umranden. Die Anzahl der Doppelpunkte, die den Anfang und das Ende eines Bereiches markieren ist unerheblich. Eine Gruppe von Doppelpunkten, die auf eine beliebige öffnende Gruppe von Doppelpunkten folgen wird immer als schließende Gruppe interpretiert. Wenn Sie Bereiche ineinander schachteln wollen, so müssen Sie die Gruppe von Doppelpunkten, die den verschachtelten Bereich markieren sollen mit Attributen versehen. Am Besten verwenden Sie dafür eine Platzhalter-CSS-Klasse, der Sie keine Semantik geben, um eine solche Gruppe von Doppelpunkten zu markieren, wenn sie einfach nur Bereiche ineinander schachteln wollen.

#### Beispiele

Ein einfacher abgegrenzter Bereich wird folgendermaßen notiert:

``` markdown
:::

Gruppierter Inhalt des Bereiches

:::
```

Möchten Sie einen Bereich mit einer einzelnen CSS-Klasse markieren, so können Sie das folgendermaßen umsetzen:

``` markdown
::: klassenname

Gruppierter Inhalt des Bereiches

:::
```

Wenn Sie Bereiche ineinander schachteln möchten müssen Sie dies folgendermaßen umsetzen. Die "klammernden" Gruppen von Doppelpunkten des äußeren und der inneren Bereiche besitzen in diesem Beispiel die selbe Anzahl an Doppelpunkten.

``` markdown
::::: aussen

::: innen

Gruppierter Inhalt eines inneren Bereiches

:::

Inhalt des äußeren Bereiches

::: innen

Gruppierter Inhalt eines inneren Bereiches

:::

:::::
```

Wie bereits erwähnt ist die Anzahl an Doppelpunkten nicht relevant, um öffnende und schließende Gruppen von Doppelpunkten zu erkennen. In diesem Beispiel ist die Anzahl der jeweiligen Doppelpunkte bewusst verwirrend gewählt:

``` markdown
:::: aussen

Teil des äußeren Bereiches.

::::: innen

Gruppierter Inhalt des inneren Bereiches

::::

(Der Innere Bereich hört hier auf).
Weiterer Teil des äußeren Bereiches.

:::
```

### Attribute, CSS-Klassen und Identifikatoren

Sie können hinter bestimmten Textbausteinen geschweifte Klammern setzen, um das HTML-Element, was durch die Übersetzung erzeugt wird mit zusätzlichen HTML-Attributen, CSS-Klassen oder Identifikatoren zu versehen.

In erster Linie sind diese Attributzuweisungen da, um den betroffenen Elementen einen eigenen Stil zuzuweisen oder sie für weitere Funktionalitäten von `decker` zu markieren. Welche CSS-Klassen besonderen Einfluss auf Elemente haben wird in den kommenden Abschnitten erklärt.

Die Textbausteine für die `decker` eine Attributzuweisung erlaubt sind Medienelemente, Quelltextblöcke, Folienüberschriften, direkte Unterüberschriften, Links, Textabschnitte und abgegrenzte Bereiche.

Innerhalb der geschweiften Klammern werden zuzuweisende CSS-Klassen mit `.klassenname` spezifiziert. Möchten Sie dem Objekt einen Identifikator zuweisen, so müssen Sie dies mit `#identifikator` umsetzen. Attribute werden als Schlüssel-Wert-Paare angegeben: `attribut=wert`. Manche Textbausteine erlauben auch die direkte Zuweisung einer einzelnen CSS-Klasse durch alleinige Angabe des Namens der Klasse.

#### Folien mit Attributen

Eine Folie kann mit Attributen versehen werden, indem hinter dem Folientitel geschweifte Klammern gesetzt werden:

``` markdown
# Folienüberschrift { ... }
```

#### Unterüberschriften mit Attributen

Unterüberschriften der Ebene 2, die direkte Kinder von Folienüberschriften sind, gruppieren den Inhalt hinter ihnen sowie sich selbst in einem HTML-Blockelement, dem Sie Attribute zuweisen können, indem Sie hinter die Unterüberschrift geschweifte Klammern setzen:

``` markdonw
# Folienüberschfit

Text

## Unterüberschrift { ... }

Text

## Unterüberschrift { ... }

Text
```

#### Quelltext mit Attributen

Ein Quelltextblock kann mit Attributen versehen werden, indem hinter den öffnenden `` ` ``-Backticks geschweifte Klammern gesetzt werden. Soll dem Quelltextblock nur eine einzelne CSS-Klasse zugewiesen werden, so können die Klammern auch weggelassen werden. Das Syntaxhighlighting wird über eine solche CSS-Klasse spezifiziert:

```` markdown
``` { ... }
Quelltext
```
````

Wenn nur eine einzelne CSS-Klasse angegeben werden soll:

```` markdown
``` klassenname
Quelltext
```
````

#### Medienelemente mit Attributen

Einem Medienelement können Attribute zugewiesen werden, indem hinter den runden Klammern, in denen die URL bzw. der Pfad angegeben wird, die geschweiften Klammern gesetzt werden:

```` markdown
![Unterschrift](URL){ ... }
````

#### Textabschnitte mit Attributen

Einem Textabschnitt können Attribute zugewiesen werden, indem hinter der eckigen Klammer, die den Text beinhaltet, die geschweiften Klammern gesetzt werden:

```` markdown
[Textinhalt]{ ... }
````

#### Links mit Attributen

Einem Link können Attribute zugewiesen werden, indem hinter der runden Klammer, die die URL beinhaltet, die geschweiften Klammern gesetzt werden:

```` markdown
[Linktext](URL){ ... }
````

#### Abgegrenzte Bereiche mit Attributen

Einem Bereich können Attribute zugewiesen werden, indem hinter den öffnenden Doppelpunkten die geschweiften Klammern gesetzt werden.

```` markdown
::: { ... }

Text

:::
````

Möchten Sie dem Bereich nur eine einzelne CSS-Klasse zuweisen, so können Sie diese durch alleinige Angabe des Namens zuweisen.

```` markdown
::: klassenname

Text

:::
````

### Medienelemente

Bilder sind nicht die einzige Form von Medienelement, die von `decker` mithilfe der `![]()`-Syntax eingebettet werden können.

Anhand der Dateiendung der referenzierten Datei können unterschiedliche Medienelemente in ihren Foliensatz eingebettet werden.

#### Bilder

Das Einbetten von Bildern wurde in den Grundlagen bereits erklärt.

```
![Bildunterschrift](Pfad oder URL){ Attribute }
```

#### Vektorgrafiken

Wenn Sie eine Vektorgrafik in Ihr Dokument einbetten wollen können Sie dies entweder als Bild tun oder den `.svg`-Quelltext der referenzierten Datei direkt als HTML-Elemente in ihr Dokument einbetten. Letzteres können Sie umsetzen, indem Sie dem Medienelement die CSS-Klasse `.embed` zuweisen:

```
![Bildunterschrift](/pfad/bild.svg){ .embed }
```

Der Vorteil einer direkt eingebetteten Vektorgrafik ist, dass die SVG-Elemente auch als Fragmente (siehe kommendes Kapitel) markiert werden können, um so sich schrittweise aufbauende, animierte Vektorgrafiken zu erzeugen.

#### Videos

Ein Video wird anhand der Dateiendung der referenzierten Datei identifiziert. Das Element wird in dem Falle in ein natives HTML-Video-Element übersetzt. Sie können dem Videoelement bis zu drei besondere CSS-Klassen zuweisen, um das Verhalten des Videos auf der Folie zu kontrollieren:

- `.autoplay`: Weisen Sie diese CSS-Klasse zu, wenn Sie möchten, dass das Video beim Aufruf der beinhaltenden Folie sofort abgespielt wird.
- `.muted`: Weisen Sie diese CSS-Klasse zu, wenn Sie möchten, dass das Video initial stumm geschaltet sein soll.
- `.controls`: Weisen Sie diese CSS-Klasse zu, wenn Sie möchten, dass das Video die nativen Steuerelemente beinhalten soll.

Zum Beispiel können Sie ein Video folgendermaßen einbetten:

``` markdown
![Videobeschreibung](/pfad/video.mp4){ .autoplay .muted .controls }
```

#### Externe Webseiten

Sie können eine externe Webseite in Ihr Dokument einbetten, indem Sie das Medienelement mit der CSS-Klasse `.iframe` versehen.

Die verlinkte Webseite wird dann als *Iframe* in Ihr Dokument eingebettet:

``` markdown
![Webseitenbeschreibung](http://example.org) { .iframe }
```

#### PDF-Dateien

Wenn die Dateiendung der verlinkten Datei `.pdf` ist, so wird bettet das Medienelement das PDF-Dokument in ihr Dokument ein. Die PDF-Datei kann vom Betrachter der Präsentation oder Webseite vollständig durchsucht werden.

``` markdown
![Dokumentenbeschreibung](/pfad/datei.pdf)
```

#### 3D-Modelle

Wenn Sie eine `.off`-Datei als Medienelement verlinken, so wird eine Applikation in Ihr Dokument eingebettet, die das 3D-Modell zum Betrachten mit der Maus anzeigt:

``` markdown
![Modellbeschreibung](/pfad/modell.off)
```

#### Geogebra

Geogebra-Projekte mit der Dateiendung `.ggb` können Sie auch als Medienelement einbetten. In Ihr Dokument wird dann an der Stelle des Medienelements eine Applikation in Ihr Dokument eingebettet, das die Interaktion mit der Geogebra-Anwendung erlaubt:

``` markdown
![Anwendungsbeschreibung](/pfad/anwendung.ggb)
```

#### Größenattribute für Medienelemente

Die übliche interne Größe der Anzeigefläche einer `decker` Präsentation ist 1280x720 Pixel groß. Medienelemente verwenden üblicherweise so viel Platz auf ihrer Folie wie sie benötigen. Dies führt häufig dazu, dass Medienelemente den gesamten Platz einer Folie einnehmen und sogar über die Grenzen der Seite hinaus ragen. Sie können die Größe eines Medienelements mit den Schlüssel-Wert-Paar `width=` und `height=` auf eine von Ihnen wählbare Dimension begrenzen. Sie können den Attributen `width` und `height` beliebige gültige CSS-Größen zuweisen:

```
![Webseite der Wikipedia](https://wikipedia.org/){ .iframe width=512px height=512px }
```

Wenn Sie nur einen der beiden Werte angeben wird versucht das Seitenverhältnis des Medienelements beizubehalten.

### Folienfragmente

In Foliensätzen möchten Sie unter Umständen die Folie während einer Präsentation schrittweise aufbauen.

Sie können jedem beliebigen Folienelement, dem Sie CSS-Klassen und Attribute zuweisen können die CSS-Klasse `.fragment` zuweisen.

Ein Fragment ist auf einer Folie solange nicht sichtbar, bis es durch Fortschreiten der Präsentation angezeigt wird. Die Reihenfolge, in der Fragmente aufgedeckt werden ist üblicherweise die Reihenfolge, inder sie im Quelltext notiert werden.

Möchten Sie die Reihenfolge, in der Fragmente aufgedeckt werden ändern, müssen Sie den Fragmenten das Attribut `data-fragment-index=` zuweisen. Die Zahl dieses Attributs bestimmt die Position in der Aufdeckreihenfolge.

#### Beispiele

Im Folgenden wird eine Folie beschrieben, die vier Fragmente besitzt:

```
# Fragmentierte Folie

Dieser Text ist immer sichtbar. [Dieser Text wird zuletzt aufgedeckt]{ .fragment data-fragment-index=4 }

::: fragment

Dieser Text wird zuerst aufgedeckt.

:::

::: fragment

Dieser Text wird als Zweites aufgedeckt.

:::

![Dieses Bild wird als Drittes aufgedeckt.](/pfad/bild.png){ .fragment }

```

#### Inkrementelle Listen

Möchten Sie eine Liste inkrementell aufdecken, so müssen Sie nicht den einzelnen Listenelementen die `.fragment`-Klasse zuweisen.

Wenn Sie die Liste in einen abgegrenzten Bereich notieren, der die Klasse `.incremental` besitzt, so wird die Liste schrittweise, Element für Element aufgedeckt:

``` markdown

::: incremental

- Erstes Listenelement
- Zweites Listenelement
- Drittes Listenelement

:::

```

### Statistiken mit chart.js

In Foliensätzen können Sie animierte Graphen und Statistiken einbetten. Zum Darstellen der Statistiken wir die Javascript-Bibliothek `chart.js` in den Foliensätzen eingebettet. Die Daten der Statistik werden dafür in einen Quelltextblock angegeben. Die CSS-Klasse des Quelltextblocks bestimmt dabei den Typ des Graphen.

Es sind folgende Graphtypen erlaubt:

- `.bar-chart`
- `.horizontalBar-chart`
- `.line-chart`
- `.radar-chart`
- `.doughnut-chart`
- `.pie-chart`
- `.polarArea-chart`

Im Quelltext werden die Daten folgendermaßen strukturiert:

````
``` { .chart-typ }
Label Datum 1, Label Datum 2, Label Datum 3, ...
Datensatzname, Datum 1, Datum 2, Datum 3, ...
Datensatzname, Datum 1, Datum 2, Datum 3, ...
```
````

Beispielsweise können Sie ein Liniendiagramm folgendermaßen gestalten:

````
``` {.line-chart width="512px" }
January, February, March, April, May, June, July, August, September, October, November, December
James Smith,-52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0
Derek Jones, 98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0
```
````

### Vorübersetzte Inhalte

`decker` erlaubt es LaTeX, Gnuplot und Plantuml Quellcode direkt an die entsprechenden Übersetzer zu übergeben und entsprechend markierte Quellcodebereiche durch das Ergebnis der Übersetzung zu ersetzen. Dies geschieht während dem Übersetzungsprozess des Dokuments.

Für das Übersetzen von LaTeX-Code muss auf Ihrem System eine LaTeX-Distribution, die `pdflatex` zur Verfügung stellt installiert sein.

Für das Übersetzen von Gnuplot und Dot-Code muss auf Ihrem System `gnuplot` installiert sein.

Für das Übersetzen von plantuml-Code muss auf Ihrem System `plantuml` installiert sein. Die Windows-Version von `plantuml` wird nur als Java-Applet ohne ausführendes Script ausgeliefert. Daher sind `plantuml`-Texte nicht ohne erweiterte technische Kenntnisse auf Windows Systemen übersetzbar.

Einen Quellcodeblock, den Sie während des Übersetzungsprozesses durch das entsprechende Ergebnis ersetzen möchten müssen Sie mit der CSS-Klasse `.render` und der entsprechenden Quellcodeklasse der Quellsprache versehen.

#### LaTeX Beispiel

Wenn Sie mit LaTeX/TikZ ein Bild erzeugen möchten, können Sie das zum Beispiel folgendermaßen umsetzen:

````
``` { .tex .render }
\documentclass{standalone}
\usepackage{tikz}
\usepackage{verbatim}
\begin{document}
\pagestyle{empty}
\begin{tikzpicture}[scale=3,cap=round]
  % Local definitions
  \def\costhirty{0.8660256}

  % Colors
  \colorlet{anglecolor}{green!50!black}
  \colorlet{sincolor}{red}
  \colorlet{tancolor}{orange!80!black}
  \colorlet{coscolor}{blue}

  % Styles 
  \tikzstyle{axes}=[]
  \tikzstyle{important line}=[very thick]
  \tikzstyle{information text}=[rounded corners,fill=red!10,inner sep=1ex]

  % The graphic
  \draw[style=help lines,step=0.5cm] (-1.4,-1.4) grid (1.4,1.4);

  \draw (0,0) circle (1cm);

  \begin{scope}[style=axes]
    \draw[->] (-1.5,0) -- (1.5,0) node[right] {$x$};
    \draw[->] (0,-1.5) -- (0,1.5) node[above] {$y$};

    \foreach \x/\xtext in {-1, -.5/-\frac{1}{2}, 1}
      \draw[xshift=\x cm] (0pt,1pt) -- (0pt,-1pt) node[below,fill=white]
            {$\xtext$};

    \foreach \y/\ytext in {-1, -.5/-\frac{1}{2}, .5/\frac{1}{2}, 1}
      \draw[yshift=\y cm] (1pt,0pt) -- (-1pt,0pt) node[left,fill=white]
            {$\ytext$};
  \end{scope}

  \filldraw[fill=green!20,draw=anglecolor] (0,0) -- (3mm,0pt) arc(0:30:3mm);
  \draw (15:2mm) node[anglecolor] {$\alpha$};

  \draw[style=important line,sincolor]
    (30:1cm) -- node[left=1pt,fill=white] {$\sin \alpha$} +(0,-.5);

  \draw[style=important line,coscolor]
    (0,0) -- node[below=2pt,fill=white] {$\cos \alpha$} (\costhirty,0);

  \draw[style=important line,tancolor] (1,0) --
    node [right=1pt,fill=white]
    {
      $\displaystyle \tan \alpha \color{black}=
      \frac{ {\color{sincolor}\sin \alpha} }{\color{coscolor}\cos \alpha}$
    } (intersection of 0,0--30:1cm and 1,0--1,1) coordinate (t);

  \draw (0,0) -- (t);
\end{tikzpicture}
\end{document}
```
````

Einen gnuplot Graphen können Sie folgendermaßen einbetten:

````
``` {.gnuplot .render height=500px }
set samples 20, 20
set isosamples 20, 20
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set style data lines
set xrange [ -3.00000 : 3.00000 ] noreverse nowriteback
set yrange [ -2.00000 : 2.00000 ] noreverse nowriteback
DEBUG_TERM_HTIC = 119
DEBUG_TERM_VTIC = 119
splot 1 / (x*x + y*y + 1)
```
````

### Interaktive Quizzes

Sie können abgegrenzte Bereiche als Quizbereiche deklarieren, indem Sie ihnen eine der folgenden CSS-Klassen zuweisen:

- `.quiz` - Antwort-Wahl Quiz
- `.quiz-mi` - Zuordnungsaufgaben
- `.quiz-ft` - Freitextaufgaben
- `.quiz-ic` - Auswahlboxen

#### Antwort-Wahl Quiz

Antwort-Wahl Quizzes werden durch eine Todo-Liste repräsentiert. Angekreuzte Antwortmöglichkeiten markieren richtige Antworten, unangekreuzte Antwortmöglichkeiten falsche Antworten. Sie können einen Hinweistext den Antworten hinzufügen, indem Sie einen einzelnen Listeneintrag als verschachtelte Liste dem Eintrag der Todo-Liste hinzufügen.

``` markdown
::: quiz

- [x] Correct
    - Yes
- [ ] False
    - No

:::
```

Folien, die ein solches Quiz haben können das Quiz als Zuschauerumfrage präsentieren. Näheres dazu im Präsentationskapitel.

#### Zuordnungsaufgaben

Zurodnungsaufgaben werden notiert, indem Sie den entsprechenden Lösungsfeldern mit einem einzelnen Doppelpunkt aufgelistet die Antwortmöglichkeiten zuweisen. Bei Aufruf der Folie mit dem Quiz werden alle Antwortmöglichkeiten gemischt und außerhalb der Lösungszone platziert. Die Antworten können per Drag-&-Drop den Lösungsfeldern zugewiesen werden. Einer Lösung können mehrere Objekte zugewiesen werden.

Möchten Sie Objekte im Quiz haben, die keinem Lösungsfeld zugeordnet werden sollen so können Sie das Objekt der Lösungsmöglichkeit `!` zuweisen.

``` markdown
::: quiz-mi
Lösung A
: Objekt A

Lösung B
: Objekt B1
: Objekt B2

Lösung C
: Objekt C

!
: Nicht Zuweisen
:::
```

#### Freitextaufgaben

Freitextaufgaben werden notiert, indem eine To-Do-Liste mit den korrekten Antworten aufgelistet wird. Sie können den Text, der zur Aufgabe gehört vor oder in den abgegrenzten Bereich notieren.

``` markdown
::: quiz-ft

Freitextfrage

- [x] Richtig
- [x] Auch
:::
```

#### Auswahlboxaufgaben

Auswahlboxaufgaben können mehrere To-Do-Listen beinhalten. Die Auswahlboxen werden in den Fließtext als "Lückenfüller" eingesetzt.

``` markdown
::: quiz-ic

Dieser Fließtext möchte eine 

- [ ] Antwortmöglichkeit
- [x] Richtige Antwortmöglichkeit
- [ ] Antwortmöglichkeit

von Ihnen erhalten. Sie können mehrere solcher Boxen zum Geben von

- [ ] Antwortmöglichkeit
- [ ] Antwortmöglichkeit
- [x] Richtigen Antworten

in den Text einbetten.

:::
```

### Hintergrundbilder und -videos

Sie können einer Folie ein Hintergrundbild zuweisen, indem Sie es entweder explizit als Hintergrundbild in den Attributen der Folie mit dem Attribut `data-background-image=/pfad/bild.png` verknüpfen oder es als Bild im Titeltext der Folie angeben: `# Folientitel ![](/pfad/bild.png)`.

Sie können die Positionierung und das Verhalten des Bildes mithilfe der Attribute `size=`, `repeat=` und `position=` bestimmen.

- `size` kann die Werte `contain`, `cover` oder einen `%`-Wert annehmen, um das Füllverhalten des Bildes anzupassen.
- `repeat` kann die Werte `repeat` und `no-repeat` annehmen, um einzustellen ob das Hintergrundbild wie gekachelt wiederholt werden soll.
- `position` kann die Position des Hintergrundbildes im Hintergrund anpassen. Der Wert wird als "x y"-Koordinatenpaar angegeben. Ist `repeat="repeat"` gesetzt beschreibt dies die Position der ersten Kachel.

Wenn Sie stattdessen ein Video als Hintergrund verwenden wollen können Sie dies entweder explizit durch Angabe des Attributs `data-background-video=/pfad/video.mp4` machen oder es im Folientitel als Medienelement angeben. Sie können das Video in einer Endlossschleife spielen lassen, indem Sie dem Video bzw. der Folie das Attribut `loop="1"` geben. Die CSS-Klassen `.autoplay` und `.controls` ergeben hier keinen Sinn, da Sie mit dem Hintergrundvideo nicht interagieren können sollen.

#### Beispiel

Ein Hintergrundbild würden Sie folgendermaßen einbetten:

```
# Folientitel ![](/path/image.png){size=20% repeat="repeat" position="128px 128px"}
```

### Ausklappbarer Bereich

Sie können einen abgegrenzten Bereich in Ihrem Dokument ausklappbar machen, indem Sie ihn mit der CSS-Klasse `details` versehen.

Ein `details`-Bereich besteht aus zwei Teilen: Der Zusammenfassung oder *Klappentext* `summary` und dem auszuklappenden Inhalt.

Sie können den *Klappentext* des Bereiches definieren, indem Sie dem Bereich das Attribut `summary=Klappentex` zuweisen.

Die Inhalte des Bereiches werden nicht angezeigt bis der Nutzer auf die Zusammenfas

#### Beispiel

Ein ausklappbarer Bereich mit dem Klappentext "Beweis" können Sie folgendermaßen notieren:

````
::: {.details summary="Beweis"}

Jeglicher Inhalt innerhalb dieses Bereiches wird erst durch Interaktion des Nutzers mit dem Klappentext angezeigt.

:::
````

### Spaltenlayout

Üblicherweise werden alle Textbausteine einer Folie untereinander angeordnet.

Wenn Sie zwei Bausteine nebeneinander anordnen wollen müssen Sie einen abgegrenzten Bereich mit einer `columns-`-CSS-Klasse einfügen. Hinter dem `columns-`-Begriff können Sie die Größenverhältnisse der Spalten zueinander angeben:

`columns-1-1` wäre ein Bereich mit zwei gleich großen Spalten.

`columns-1-2` wäre ein Bereich mit zwei Spalten von denen die rechte doppelt so viel Platz einnimmt wie die linke.

`columns-2-3-2` wäre ein Bereich mit drei Spalten, von denen die mittlere ein Drittel größer ist als die beiden äußeren.

Sie können als Größenverhältnisse nur ganze Zahlen angeben.

Innerhalb des abgegrenzten Bereiches mit `columns-`-Klasse können Sie nun weitere Textbausteine notieren, die im Folienlayout von links nach rechts nebeneinander anstatt untereinander angeordnet werden.

Wenn Sie mehrere Textbausteine in einer gemeinsamen Spalte gruppieren möchten müssen Sie einen neuen, inneren Bereich deklarieren, in dem Sie die anderen Textbausteine notieren.

Wenn Sie mehr Elemente einfügen als Spalten deklariert wurden werden die weiteren Elemente wieder beginnend bei der linkesten Spalte eingefügt.

#### Beispiele

Ein Spaltenlayout mit zwei nebeneinander angeordneten Paragraphen:

````
::: columns-1-1

Dieser Paragraph wird in der linken Spalte einsortiert.

Dieser Paragraph wird in der rechten Spalte einsortiert.

:::
````

Mehrere Textbausteine, die gruppiert werden:

````
::: columns-1-1

::: platzhalter

Dieser Paragraph wird mit dem folgenden Paragraphen in der linken Spalte einsortiert.

Dieser Paragraph steht ebenso in der linken Spalte, da sie durch den platzhalter Gruppiert werden.

:::

::: platzhalter

Dieser Paragraph wird mit dem folgenden Paragraphen in der rechten Spalte einsortiert.

Dieser Paragraph steht ebenso in der rechten Spalte, da sie durch den platzhalter Gruppiert werden.

:::

:::
````

Wenn Sie mehr Bausteine einfügen als Spalten vorhanden sind beginnt die Einsortieren wieder in der linkesten Spalte:

````
::: columns-1-1-1

![Dieses Bild ist links](/pfad/bild1.png)

![Dieses Bild ist in der Mitte](/pfad/bild2.png)

![Dieses Bild ist rechts](/pfad/bild3.png)

![Dieses Bild ist erneut links](/pfad/bild4.png)

:::
````

# Präsentieren

Foliensätze, die mit `decker` erzeugt werden, können in einem Webbrowser betrachtet werden. Zum Präsentieren dieser Folien wird kein separates Werkzeug benötigt.

Die im Browser betrachteten Foliensätze bieten mehrere Funktionalitäten, die das Präsentieren und spätere Aufbereiten und zur Verfügung stellen von Inhalten vereinfachen. Zugriff auf diese Funktionalitäten erhalten Sie durch das Betätigen von Tastaturkürzeln innerhalb des Foliensatzes.

## Präsentationsmodus

Drücken Sie drei Mal die **P**-Taste auf Ihrer Tastatur, um in den Präsentationsmodus zu wechseln. Es wird eine kurze Meldung angezeigt, die den Zustand des Präsentationsmodus anzeigt: An oder Aus.

Ist der Präsentationsmodus an werden zusätzliche Bedienelemente für Präsentationen angezeigt.

Darunter befindet sich insbesondere das Whiteboardmenu in der unteren linken Ecke, mit der Sie den Whiteboardmodus an- und ausschalten können.

Zudem werden auf Folien, in denen sich ein Antwort-Wahl Quiz befindet im unteren Bildschirmrand Bedienelemente angezeigt, mit denen Sie ihren Zuschauern die Möglichkeit geben können über einen QR-Code an einer Umfrage zum Quiz auf der Folie teilzunehmen.

## Whiteboardmodus

Im Präsentationsmodus können Sie in der unteren linken Ecke das Whiteboardmenu finden. Ein Klick auf den Knopf des Whiteboardmenus startet den Whiteboardmodus. Wenn Sie im Whiteboardmodus erneut auf den Whiteboardknopf klicken öffnet sich das Whiteboardmenu.

Im Whiteboardmodus können Sie nicht mit den Folien interagieren und schreiben mithilfe Ihrer Maus oder eines anderen Eingabegerätes auf den Folien Annotationen.

Sie können den Whiteboardmodus auch durch Drücken der **W**-Taste an- und ausschalten.

### Whiteboardmenu

Im Whiteboardmenu können Sie Änderungen am Verhalten des Whiteboardmodus vornehmen.

Sie können die Stiftgröße und -farbe ändern, den Radiermodus an- und ausschalten, zwischen Stift und Laserpointer umschalten.

Sie können der aktuellen Folie ein Notizblatt hinzufügen. Zudem können Sie ein Raster für das Notizblatt an- und ausschalten.

![Whitboardmenufunktionen](./images/whiteboard-menu-annot.png)

Im Whiteboardmodus können Sie die Tasten 1 - 7 dazu verwenden, um zwischen verschiedenen Stiftfarben umzuschalten.

Die Tasten 8, 9 und 0 ändern die Stiftgröße.

Mit der **L**-Taste können Sie zwischen Stift und Laserpointer umschalten. Zeichnungen, die Sie mit dem Laserpointer anfertigen verschwinden nach kurzer Zeit wieder.

Mit der **W**-Taste können Sie den Whiteboardmodus wieder beenden.

### Speichern der Annotationen

Das Speichern der Annotationen geschieht auf zwei Wegen:

Zum einen wird versucht die Annotationen an den im Hintergrund laufenden Webserver zu senden, wenn Sie die Folien lokal präsentieren. Dieser wird die Annotationen als `[Präsentationsname]-annot.json` an passender Stelle in Ihrem Projektverzeichnis ablegen. Die gespeicherten Annotationen werden beim Wiederaufrufen der Folien im Browser direkt in die Folien eingebettet.

Zum anderen werden die Annotationen als Datei heruntergeladen. Der Download dieser Sicherungskopie geschieht vor dem Uploadversuch.

Der Up- und Download der Annotationen geschieht automatisch, wenn Sie eine Videoaufnahme beenden oder die Webseite, in denen die Folien präsentiert werden, verlassen. Letzteres vertraut darauf, dass Ihr Browser der Webseite genug Zeit lässt diesen Prozess vor Verlassen der Webseite durchzuführen. Dies kann sich von Version zu Version des Browsers unterscheiden als vom verwendeten Betriebssystem abhängen. Zu diesem Zweck gibt es einen Knopf im Whiteboardmenu, der es Ihnen erlaubt den Up- und Download manuell auszulösen.

Wenn Ihnen nur die Sicherungskopie vorliegt und der Upload der Daten nicht vollendet werden konnte können Sie die Sicherungskopie (`[Präsentationsname]-annot.json`) aus Ihrem Downloadverzeichnis in Ihr Projektverzeichnis an die selbe Stelle kopieren, an der die korrespondierende `[Präsentationsname]-deck.md`-Datei liegt. Achten Sie darauf, dass der Name der Sicherungskopie mit einer Zahl enden kann wenn mehrere Solcher in Ihrem Downloadverzeichnis abgelegt wurden. Sie müssen die Datei dann selbstverständlich noch passend umbenennen. Denken Sie daran die Datei mit dem aktuellsten Änderungsdatum zu verwenden.

Wenn Sie die Präsentation nicht lokal, sondern von einem Webserver aus durchführen, der den Upload der Annotationen nicht entgegen nimmt, müssen Sie den oben beschriebenen manuellen Schritt durchführen, wenn Sie die Annotationen später in Ihr Projekt übernehmen möchten.

## Videoaufnahme

Die Folien bieten die Möglichkeit an eine Videoaufnahme Ihrer Präsentation direkt im Browser durchzuführen.

### Aufnahme vorbereiten

Mit einem Druck auf die **R**-Taste können Sie die Aufnahme vorbereiten. Zur Durchführung der Aufnahme benötigt die Webseite die Erlaubnis auf Mikrofon, Kamera und Bildschirm zugreifen zu dürfen.

Für gewöhnlich wird die Erlaubnis für Mikrofon und Kamera in Ihrem Browser gespeichert, wodurch Sie dies nur ein Mal erlauben müssen.

Die Frage, welchen Bildschrim, welches Fenster oder welchen Browsertab Sie aufnehmen möchten, wird ihnen jedoch jedes Mal gestellt.

Nachdem Sie die Freigaben genehmigt haben erscheint in der oberen Mitte des Browserfensters das Aufnahmemenu.

### Aufnahmemenu

Sie können auf die Menulasche oder den kleinen schwarzen Pfeil klicken, um das Aufnahmemenu zu öffnen.

Im Aufnahmemenu können Sie Einstellungen an der Aufnahme vornehmen, Informationen über die Aufnahmedauer, Mikrofon- und Desktoplautstärke einsehen sowie die Aufnahme selbst steuern:

![Aufnahmemenu](./images/recording-menu-annot.png)

Bevor Sie die Aufnahme starten, stellen Sie sicher, dass die richtigen Geräte ausgewählt sind.

Bei der ersten Freigabe für Mikrofon und Kamera kann es vorkommen, dass die Gerätenamen nicht erkannt werden.
Sollten keine Gerätenamen angezeigt werden, laden Sie bitte die Webseite einmal neu.

Sie können am Geräuschpegel erkennen, ob sie die Aufnahmelautstärke Ihres Mikrofons dämpfen oder verstärken müssen. Nutzen Sie dazu die Lautstärkeregler.

Der Aufnahmeindikator zeigt einen leeren Kreis an, wenn die Aufnahme bereit ist. Wenn die Aufnahme läuft wird ein ausgefüllter Kreis angezeigt. Ist die Aufnahme pausiert zeigt er zwei parallele *Pause*-Striche an.

Neben dem kleinen Uhr-Symbol wird während der Aufnahme eine Digitaluhr angezeigt, die Ihnen die bisher verstrichene Aufnahmezeit angibt.

### Aufnahme steuern

Mithilfe der oberen drei Knöpfe im Aufnahmemenu können Sie die Aufnahme steuern:

Ist die Aufnahme bereit, können Sie sie mit Klick auf das Startsymbol starten. Diese Funktion kann auch über die Tastatur mit einem erneuten Druck auf die **R**-Taste genutzt werden.

Während der Aufnahme können Sie die Aufnahme pausieren. Dies ist über die Tastatur mit einem Druck auf die **R**-Taste bei laufender Aufnahme möglich.

Möchten Sie die Aufnahme beenden, drücken Sie auf den Stopp-Knopf. Dies ist über die Tastatur mit dreimaligen schnellen Drücken der **Z**-Taste möglich.

Das Ein- und Ausblenden Ihres Kamerabildes ist nur über die Tastatur mit der **V**-Taste möglich. Sie können in den Konfigurationseinstellungen Ihres Foliensatzes angeben, ob Sie die Aufnahme vor einem Greenscreen tätigen und Ihr Bild entsprechend maskiert werden soll. Näheres dazu im Kapitel zu Konfigurationseinstellungen.

### Aufnahme speichern

Ihre Aufnahme wird nur gespeichert, wenn sie sie ordentlich durch betätigen des Stopp-Knopfes oder durch dreimaliges Drücken der **Z**-Taste beenden.

Wie auch bei den Whiteboardannotationen werden die generierten Daten sowohl an den lokal laufenden Webserver gesendet als auch in Ihrem Downloadverzeichnis gesichert.

Der Sicherungsprozess kann auf schwächeren Systemen bei langer Aufnahme ein bisschen dauern. Warten Sie also darauf, dass alle Daten übertragen und abgespeichert werden.

Sollten Sie bei laufender Aufnahme die Webseite verlassen wollen sollte Ihr Browser Sie über nicht gespeicherte Änderungen warnen und die Navigation erst bei nachträglicher Bestätigung durchführen. Verlassen Sie die Webseite während der Aufnahme wird sie abgebrochen und **nicht** gespeichert.

Eine Aufnahme erzeugt zwei Dateien: Eine Datei mit dem Namen `[Präsentationsname]-times.json` und eine Datei mit dem Namen `[Präsentationsname]-recording.webm`. In der ersteren Datei werden die Informationen über die Folienübergänge hinterlegt, damit das Video später mit den Folien synchronisiert werden kann. Letztere Datei ist eine Rohversion Ihres Videos.

### Aufnahme anhängen oder ersetzen

Wenn Sie bereits eine Aufnahme zu Ihrer Präsentation vorliegen haben werden Sie beim Starten der Aufnahme gefragt, ob Sie die bereits existierende Aufnahme ersetzen oder an sie anhängen wollen. In letzterem Falle werden zusätzliche Daten an die `-times.json` angehangen während die `-recording.webm`-Videos von 1 an durchnummeriert werden.

Wählen Sie die Option "Ersetzen" werden alle bisher vorliegenden Dateien beim Beenden der neuen Aufnahme **gelöscht** und mit der neuen Aufnahme ersetzt.

### Aufname konvertieren

Bevor es in die Folien eingebettet werden kann müssen Sie das Video mit `decker crunch` in eine `-recording.mp4`-Datei umwandeln.

Sollten Sie Ihre Aufnahmen in mehreren Teilen getätigt haben fügt der Aufruf von `decker crunch` alle Aufnahmen zu einer einzelnen `-recording.mp4`-Datei zusammen.

Der Aufruf von `decker crunch` verlangt, dass sie das Programm `ffmpeg` auf Ihrem Rechner installiert haben.

## Quizumfragen

Wenn Sie sich auf einer Folie mit Antwort-Wahl-Quiz befinden und im Präsentationsmodus sind werden Ihnen folgende Bedienelemente angezeigt:

![Quizmenu](./images/quiz-menu.png)

Mit einem Klick auf den linken Knopf können Sie einen QR-Code anzeigen lassen, den Ihre Zuschauer mit ihrem Mobilgerät einscannen können, um an einer Umfrage der Antwort-Wahl-Frage teilzunehmen.

Mit einem Klick auf den rechten Knopf können Sie die Umfrage starten.

Es wird Ihnen nach Start eine Statistik angezeigt, wie viele Teilnehmer Sie haben und wie viele Teilnehmer bereits eine Antwort abgegeben haben.

Mit einem erneuten Klick auf den rechten Knopf können Sie die Umfrage beenden und eine Umfragestatistik einblenden lassen.

Zur Nutzung dieser Umfragefunktion müssen Sie in Ihren Folien einen Quizserver konfiguriert haben, mit dem während der Umfrage kommuniziert werden soll. Näheres dazu erfahren Sie im Kapitel über Konfigurationseinstellungen.

## Tastaturkürzelübersicht

Im folgenden finden Sie eine Übersicht über alle Tastaturkürzel und ihre Funktion. Sie können diese Übersicht auf den Folien auch durch Drücken der **?**-Taste auf Ihrer Tastatur einblenden:

| Taste  | Aktion |
| :----  | ------ |
| Strg + F | Textsuche öffnen |
| N oder Leertaste | Nächste Folie |
| ← , H | Nach Links navigieren (vorherige Folie / Fragment) |
| → , L | Nach Rechts navigieren (nächste Folie / Fragment) |
| ↑ , K | Nach Oben navigieren (vorherige Unterfolie / Fragment) |
| ↓ , J | Nach Unten navigieren (nächste Unterfolie / Fragment) |
| Pos1 , Umschalt + ← | Zur ersten Folie navigieren |
| Ende, Umschalt + → | Zur letzten Folie navigieren |
| B, . | Pause (Präsentation abdunkeln) |
| F | Vollbildmodus aktivieren |
| ESC, O | Folienübersicht anzeigen (wenn erlaubt) |
| P,P,P | Präsentationsmodus anschalten |
| W | Whiteboardmodus an- oder ausschalten (im Präsentationsmodus) |
| Entf | Whiteboard Annotationen aus aktueller Folie löschen |
| 0 - 7 | Stiftfarbe wechseln |
| 8, 9 | Stiftgröße wechseln |
| L | Laserpointer auswählen |
| R | Aufnahme einrichten |
| R nach Einrichtung | Aufnahme starten oder pausieren |
| V | Kamerafenster ein- oder ausblenden |
| Z,Z,Z | Aufnahme beenden und finalisieren |
| S | Sprechernotizfenster einblenden |

**Anmerkung**: Die **P**-Taste hat für gewöhnlich auch die Funktion zur vorherigen Folie zu navigieren. Diese Funktion wurde jedoch durch das Umschalten des Präsentationsmodus überschrieben, wird jedoch noch in der Übersicht auf den Folien angezeigt.