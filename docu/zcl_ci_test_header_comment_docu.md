# Dokumentation der Prüfklasse ZCL_CI_TEST_HEADER_COMMENT

## English Version

### MEANING
Check existence of a header comment in every 
* program
* function group
* global class
* global interface
### HINTS
Because the content can not be checked by the system, it is only checked that there are five consecutive comment lines, beginning with an asterisk. These lines have to be at the following positions:
* For normal programs, first line of frame program (before the statement `REPORT`)
* For global classes, in the constructor method of the class, after the line `METHOD constructor`.
In many cases, you will have to define a constructor just for this purpose. If you are in a subclass, you have to call `super->constructor( )`. at the end of the method.
* For global exception classes, the `CLASS_CONSTRUCTOR` has to be used
* For global interfaces, in the line directly after the `INTERFACE` statement
* For function pools, in TOP-include, starting in the first line (i.e. in front of the `FUNCTION-POOL` statement)

## Deutsche Verion
### MEANING
Es wird geprüft, ob
* jedes Programm
* jede Funktionsgruppe
* jede globale Klasse
*  jedes globale Interface

einen Kopfkommentar hat.

### HINTS
Da keine automatische inhaltliche Prüfung erfolgen kann, wird lediglich geprüft, dass es fünf aufeinander folgende Kommentarzeilen gibt, die mit einem Sternchen beginnen. Diese fünf Zeilen müssen an folgenden Positionen stehen:
* Bei normalen Programmen, ab der ersten Zeile des Rahmenprogramms (vor der Anweisung `REPORT`).
* Bei globalen Klassen, im Konstruktor hinter der Zeile `METHOD constructor`.
In vielen Fällen muss der Konstruktor nur zu diesem Zweck deklariert werden. 
* Falls es sich um eine abgeleitete Klasse handelt, muss am Ende der Methode `super->constructor( ).` aufgerufen werden.
* Bei globalen Ausnahmeklassen muss entsprechend der `CLASS_CONSTRUCTOR` verwendet werden
* Bei globalen Schnittstellen, in der Zeile hinter der `INTERFACE` Anweisung
* Bei Funktionsgruppen, im TOP-Include, beginnend mit der ersten Zeile (also direkt vor der `FUNCTION-POOL` Anweisung)