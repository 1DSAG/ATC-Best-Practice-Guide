# ABAP Test Cockpit Best Practice Guide

Dieses Repository beinhaltet Beispielcode als Begleitmaterial zum  ABAP Test Cockpit (ATC) Leitfaden der [DSAG](https://dsag.de/).

## Folder "src"
Der Folder beinhaltet Codebeispiele für Prüfklassen. Die Kategorie aller Prüfklassen ist `ZCL_CI_CATEGORY_DSAG`.  

### ZCL_CI_TEST_DSAG_PRETTY_PRINT
Klasse zur Überprüfung ob auf den Source Code Pretty Print angewendet wurde.

### ZCL_CI_TEST_COMP_PROCS
Klasse zur Ausführung verschiedener Code-basierter Prüfungen (Details siehe docu).

### ZCL_CI_TEST_HEADER_COMMENT
Klasse zur Überprüfung, ob Kopfkommentare in Programmen, Funktionsgruppen, globalen Klassen und globalen Interfaces existieren.

### ZCL_CI_TEST_TRANSLATIONS
Klasse zur Überprüfung ob für die wichtigsten Objektarten alle Texte sowohl in Deutsch als auch in Englisch vorliegen.

## Folder "snippets"
Der Folder "snippets" beinhaltet Code Snippets, die Sie bei Ihrer Arbeit mit dem ATC unterstützen.  

### Z_SCI_VARIANT_*
Sourcen zur Erstellen eines Reports für die übersichtliche Anzeige von Prüfvarianten.

## Folder "docu"
Der Folder "docu" beinhaltet zusätzliche Dokumentation zu den Prüfklassen, die nicht über den Mechanismus von abapGit übertragen werden kann. Sie können die Dokumentation manuell in das System übertragen. Die Namenskonvention für die Dokumentation ist:
<Name der Prüfklasse>_docu.MD