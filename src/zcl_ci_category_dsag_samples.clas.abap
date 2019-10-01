class ZCL_CI_CATEGORY_DSAG_SAMPLES definition
  public
  inheriting from CL_CI_CATEGORY_ROOT
  final
  create public .

public section.

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CI_CATEGORY_DSAG_SAMPLES IMPLEMENTATION.


  METHOD CONSTRUCTOR.
************************************************************************
* ATC/Code Inspector  Check Category
* Corresponds to a "folder" in the view
* for setting up check variants.
************************************************************************

    super->constructor( ).
    description = 'DSAG Beispiele'(001).
    category    = 'CL_CI_CATEGORY_TOP' ##NO_TEXT.
    position    = '010'.

  ENDMETHOD.
ENDCLASS.
