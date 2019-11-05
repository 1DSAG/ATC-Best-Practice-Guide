# Dokumentation der Prüfklasse ZCL_CI_TEST_COMP_PROCS

## English Version

### MEANING
Various Code-based checks

### HINTS
* `CALL TRANSACTION`
** always with explicit `WITH` or `WITHOUT AUTHORITY-CHECK`
** `WITHOUT CHECK` only in combination with `USING bdc` (or via exemption)
** `WITH AUTH` requires `CATCH` in following lines of procedure
* Obsolete or unwanted language elements:
** `DEFINE *`
** `BEGIN OF COMMON PART *`
* `FORMs` not allowed in new includes (new: as specified in constant `con_forms_allowed_until`)

## Deutsche Version
### MEANING
Verschiedene Code-basierte Prüfungen.

### HINTS
* `CALL TRANSACTION`
** immer mit explizitem `WITH` oder `WITHOUT AUTHORITY-CHECK`
** `WITHOUT CHECK` nur bei Verwendung mit `USING bdc` (oder Befreiung)
** `WITH AUTH` benötigt `CATCH` in den Folgezeilen
* Obsolete oder unerwünschte Sprachelemente:
** `DEFINE *`
** `BEGIN OF COMMON PART *`
* `FORMs` in neuen Includes nicht erlaubt (neu: gemäß der Konstanten `con_forms_allowed_until`)
