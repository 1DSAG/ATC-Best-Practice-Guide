class ZCL_CI_TEST_DSAG_PRETTY_PRINT definition
  public
  inheriting from CL_CI_TEST_SCAN
  final
  create public .

public section.

  methods CONSTRUCTOR .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.

  types:
    BEGIN OF ty_pp_settings,
           lower     TYPE flag,
           upper     TYPE flag,
           key_upper TYPE flag,
           key_lower TYPE flag,
         END OF ty_pp_settings .

  data MS_PP_SETTINGS type TY_PP_SETTINGS .
  data MV_ONE_FINDING type FLAG .

  methods FILL_MESSAGE .
  methods SET_DEFAULTS .
  methods GET_MODE
    returning
      value(RV_MODE) type CHAR5 .
private section.

  constants C_PP_ERROR type SCI_ERRC value '001'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_CI_TEST_DSAG_PRETTY_PRINT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description    = 'Überprüfung ob Pretty Printer benutzt wurde'(001).
    category       = 'ZCL_CI_CATEGORY_DSAG_SAMPLES'.

    has_documentation = abap_true.  "Es existiert eine Dokumentation dazu
    has_attributes    = abap_true.  "Gibt an, dass Attribute vorhanden sind
    attributes_ok     = abap_true.  "Angegebene Attribute sind ok

    fill_message( ).  "mögliche Meldungen füllen, die bei einem Fund
*                     "ausgegeben werden können

    set_defaults( ).  "Default-Werte für Attribut-Popup setzen

  ENDMETHOD.


  METHOD fill_message.

    DATA: ls_scimessage TYPE scimessage.


*   Inkorrekter Pretty-Print-Zustand
    ls_scimessage-test     = myname.
    ls_scimessage-code     = c_pp_error.  "pro Fehler eigenen Code definieren
    ls_scimessage-kind     = c_warning.
    ls_scimessage-text     = 'Inkorrekter Pretty-Print-Zustand'(002).
    ls_scimessage-pcom     = space.
    ls_scimessage-pcom_alt = space.
    INSERT ls_scimessage INTO TABLE scimessages.

*   hier ggf. weitere Meldungen nach o.a. Muster hinzufügen...

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT ms_pp_settings = ms_pp_settings
           mv_one_finding = mv_one_finding
           TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_mode.

    IF ms_pp_settings-lower EQ abap_true.
      rv_mode = 'LOWER'.
    ELSEIF ms_pp_settings-upper EQ abap_true.
      rv_mode = 'UPPER'.
    ELSEIF ms_pp_settings-key_upper EQ abap_true.
      rv_mode = 'HIKEY'.
    ELSE. "KEY_LOWER
      rv_mode = 'LOKEY'.
    ENDIF.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    DATA: lt_attributes TYPE sci_atttab,
          ls_attribute  LIKE LINE OF lt_attributes.


    DEFINE fill_att.
      CLEAR: ls_attribute.
      GET REFERENCE OF &1 INTO ls_attribute-ref.
      ls_attribute-text         = &2.
      ls_attribute-kind         = &3.
      APPEND ls_attribute TO lt_attributes.
    END-OF-DEFINITION.


    DEFINE fill_att_rb.
      CLEAR: ls_attribute.
      GET REFERENCE OF &1 INTO ls_attribute-ref.
      ls_attribute-text         = &2.
      ls_attribute-kind         = &3.
      ls_attribute-button_group = &4.
      APPEND ls_attribute TO lt_attributes.
    END-OF-DEFINITION.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*   Kind:
*   G = Gruppe / Selection-Screen Block
*   R = Radio-Button
*   C = Checkbox
*   L = Listbox
*   S = Select-Option
*   space = Parameter
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    fill_att mv_one_finding 'Nur 1 Befund pro Incl. melden'(007)  'C'.

    fill_att sy-index 'Groß/Kleinkonvertierung'(008) 'G'.
    fill_att_rb ms_pp_settings-lower     'Kleinschreibung'(003)     'R' 'PP1'.
    fill_att_rb ms_pp_settings-upper     'Großschreibung'(004)      'R' 'PP1'.
    fill_att_rb ms_pp_settings-key_upper 'Schlüsselwort groß'(005)  'R' 'PP1'.
    fill_att_rb ms_pp_settings-key_lower 'Schlüsselwort klein'(006) 'R' 'PP1'.

    cl_ci_query_attributes=>generic( p_name       = myname
                                     p_title      = 'Einstellungen der Prüfung'(009)
                                     p_attributes = lt_attributes
                                     p_display    = p_display ).

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT ms_pp_settings = ms_pp_settings
           mv_one_finding = mv_one_finding
           FROM DATA BUFFER p_attributes.

    ASSERT sy-subrc = 0.

  ENDMETHOD.


METHOD run.

  DATA: lt_includes  TYPE STANDARD TABLE OF program,
        lt_source_in TYPE swbse_max_line_tab,
        lt_pretty    TYPE rswsourcet,
        lv_tabix     TYPE sy-tabix.

  FIELD-SYMBOLS: <ls_level>     TYPE slevel,
                 <lv_include>   TYPE program,
                 <lv_source_in> TYPE swbse_max_line,
                 <lv_pretty>    TYPE string.


  IF get( ) <> 'X'.
    RETURN.
  ENDIF.

  LOOP AT ref_scan->levels ASSIGNING <ls_level> WHERE type = 'P'. "#EC CI_STDSEQ
    CHECK <ls_level>-name+30(2) <> 'CP'.
    CHECK <ls_level>-name+30(2) <> 'CU'.
    CHECK <ls_level>-name+30(2) <> 'CO'.
    CHECK <ls_level>-name+30(2) <> 'CI'.
    CHECK <ls_level>-name+30(2) <> 'IP'.
    CHECK <ls_level>-name+30(2) <> 'IU'.
    CHECK <ls_level>-name NP 'L*UXX'.
    INSERT <ls_level>-name INTO TABLE lt_includes.
  ENDLOOP.

  SORT lt_includes.
  DELETE ADJACENT DUPLICATES FROM lt_includes.

  LOOP AT lt_includes ASSIGNING <lv_include>.
    CLEAR: lt_source_in.
    READ REPORT <lv_include> INTO lt_source_in.

    lt_pretty = lt_source_in.

    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
      EXPORTING
        mode          = get_mode( )
      TABLES
        source        = lt_pretty
      EXCEPTIONS
        syntax_errors = 1
        OTHERS        = 2.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF lt_source_in <> lt_pretty.
      LOOP AT lt_source_in ASSIGNING <lv_source_in>.
        lv_tabix = sy-tabix.
        READ TABLE lt_pretty INDEX lv_tabix ASSIGNING <lv_pretty>.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        IF <lv_source_in> <> <lv_pretty>.

          inform( p_sub_obj_type = 'PROG'
                  p_sub_obj_name = <lv_include>
                  p_line         = lv_tabix
                  p_column       = 0
                  p_kind         = c_warning
                  p_test         = myname
                  p_code         = c_pp_error ).

          IF mv_one_finding EQ abap_true.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


  METHOD set_defaults.

    ms_pp_settings-key_upper = abap_true.
    mv_one_finding           = abap_true.

  ENDMETHOD.
ENDCLASS.
