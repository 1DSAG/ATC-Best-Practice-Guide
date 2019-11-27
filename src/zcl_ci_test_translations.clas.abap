CLASS zcl_ci_test_translations DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
    METHODS run REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_my_name TYPE seoclsname VALUE 'ZCL_CI_TEST_TRANSLATIONS' ##NO_TEXT.
    TYPES: BEGIN OF tp
         , key TYPE syst_msgv  " 50 characters. We run into problems with MESSAGE if we use more than that
         , END OF tp
         , ttp TYPE SORTED TABLE OF tp WITH UNIQUE KEY key
         .
    DATA: m_spras TYPE spras      " 1 character
        , m_laiso TYPE t002-laiso " ISO language (2 characters)
              .

    METHODS: check_program_texts
           , check_screen_texts
           , check_single_generic
               IMPORTING i_check_code TYPE sci_errc
                         i_screen     TYPE d020s-dnum OPTIONAL " only to be filled for screen checks
           , check_list_generic
               IMPORTING i_check_code TYPE sci_errc
                         i_screen     TYPE d020s-dnum OPTIONAL " only to be filled for screen checks
           , set_language_from_index
               IMPORTING i_index TYPE sy-index
           , is_master_language_missing
               RETURNING VALUE(r) TYPE abap_bool,
             remove_screen_ddic_fields
               IMPORTING i_program_name TYPE progname
                         i_screen TYPE d020s-dnum
               CHANGING  c_fields TYPE zcl_ci_test_translations=>ttp.

ENDCLASS.



CLASS zcl_ci_test_translations IMPLEMENTATION.


  METHOD check_list_generic.
    DATA: it_key TYPE ttp
        , it_key_de TYPE ttp
        , it_key_en LIKE it_key_de
        , it_key_missing LIKE it_key_de
        , pattern TYPE char40
        .

    DO 2 TIMES.
      set_language_from_index( sy-index ).
      CASE i_check_code.
        WHEN '0004'.
          SELECT msgnr FROM t100
            WHERE arbgb = @object_name "#ec ci_conv_ok
              AND sprsl = @m_spras
            INTO TABLE @it_key.                           "#EC CI_SUBRC
        WHEN '0008'.
          SELECT domvalue_l FROM dd07t
            WHERE domname    = @object_name "#ec ci_conv_ok
              AND ddlanguage = @m_spras
              AND as4local   = 'A'
              AND ddtext    <> ''
            INTO TABLE @it_key.                           "#EC CI_SUBRC
        WHEN '0009'.
          pattern = |{ object_name }___|. " followed by 3 digits
          SELECT object FROM doktl
            WHERE id = 'NA'  " message
              AND object LIKE @pattern "#ec ci_conv_ok
              AND langu = @m_spras                      "#EC CI_NOORDER
            INTO TABLE @DATA(it_key_duplicates).          "#EC CI_SUBRC
          SORT it_key_duplicates BY table_line. "#EC CI_SORTLOOP . Cannot use DISTINCT because doktl is a cluster table
          DELETE ADJACENT DUPLICATES FROM it_key_duplicates COMPARING table_line.
          it_key = it_key_duplicates.
        WHEN '0010'.
          SELECT SINGLE @abap_true
            FROM d342l
            WHERE progname = @program_name
            INTO @data(dummy).
          IF sy-subrc = 0. " if all GUI status have been deleted (no entry in table d342l), the texts still remain as zombies, but we do not want to check them.
            " However, the entry in d342l also sometimes remains, even though the GUI status has been deleted. So in some cases there will still be false (zombie) findings
            DATA(separator) = '|'.
            SELECT DISTINCT obj_type && @separator && texttype && @separator && obj_code FROM rsmptexts
              WHERE progname = @program_name "#ec ci_conv_ok
                " do not check technical texts:
                AND ( texttype <> 'T'      " various texts of GUI-Status
                      OR obj_type = 'T' )  " GUI-Title
                AND sprsl = @m_spras       " #EC CI_CMPLX_WHERE
              INTO TABLE @it_key.                         "#EC CI_SUBRC
          ENDIF.
        WHEN '0012'.
          SELECT fldn FROM d021t
            WHERE prog = @program_name "#ec ci_conv_ok
              AND dynr = @i_screen
              AND lang = @m_spras
            INTO TABLE @it_key.                           "#EC CI_SUBRC
        WHEN '0014'.
          pattern = |{ object_name WIDTH = 30 }%|. " first 30 chars = componentname + spaces (followed by viewname from pos 31)
          SELECT CASE sotr_head~alias_name
                   WHEN ' ' THEN sotr_head~concept
                   ELSE sotr_head~alias_name
                 END AS key
            FROM sotr_use INNER JOIN sotr_head ON sotr_use~concept = sotr_head~concept
                          INNER JOIN sotr_text ON sotr_use~concept = sotr_text~concept
            WHERE sotr_use~pgmid  = 'LIMU'
              AND sotr_use~object = 'WDYV'
              AND sotr_use~obj_name LIKE @pattern
              AND sotr_text~langu = @m_spras
            INTO TABLE @it_key.                           "#EC CI_SUBRC
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

      CASE sy-index.
        WHEN 1. MOVE-CORRESPONDING it_key TO it_key_de.
        WHEN 2. MOVE-CORRESPONDING it_key TO it_key_en.
      ENDCASE.
    ENDDO.

    " need to synchronize at this point, otherwise the filtering cannot be done both ways
    DO 2 TIMES.
      set_language_from_index( sy-index ).
      CASE sy-index.
        WHEN 1. it_key_missing = FILTER #( it_key_en EXCEPT IN it_key_de WHERE key = key ).
        WHEN 2. it_key_missing = FILTER #( it_key_de EXCEPT IN it_key_en WHERE key = key ).
      ENDCASE.
      IF i_check_code = '0012'.
        remove_screen_ddic_fields( EXPORTING i_program_name = program_name
                                             i_screen       = i_screen
                                   CHANGING  c_fields       = it_key_missing ).
      ENDIF.
      LOOP AT it_key_missing ASSIGNING FIELD-SYMBOL(<wa>).
        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_test     = c_my_name
                p_code     = i_check_code
                " in case 0012 (Screen [dynpro] field),
                " if the master language entry is missing (i.e. field has been deleted, but translation was not deleted),
                " ignore, because of very low risk
                " (I also verified that the Autotext numbers are not reused - EvG)
                " This case still occurs after I implemented the remove_screen_ddic_fields() method - 08.05.2019 with SAPMZE32 / 9090
                p_kind     = COND #( WHEN i_check_code = '0012' AND is_master_language_missing( ) THEN c_note
                                                                                                  ELSE c_warning )
                p_param_1  = COND #( WHEN i_check_code = '0012' THEN i_screen && '|' && <wa>-key
                                     ELSE <wa>-key )
                p_param_2  = m_laiso ).
      ENDLOOP.
    ENDDO.
  ENDMETHOD.


  METHOD check_program_texts.
    DATA: it_tpool TYPE textpool_table
        , it_de TYPE SORTED TABLE OF textpool WITH UNIQUE KEY id key
        , it_en LIKE it_de
        , it_missing LIKE it_de
        , check_code TYPE sci_errc
        .
    CHECK program_name IS NOT INITIAL.
    DO 2 TIMES.
      set_language_from_index( sy-index ).
      CLEAR it_tpool.
      READ TEXTPOOL program_name LANGUAGE m_spras INTO it_tpool.
      SORT it_tpool BY id key.                         "#EC CI_SORTLOOP
      DELETE ADJACENT DUPLICATES FROM it_tpool COMPARING id key. " we do not want to crash because of inconsistent data (e.g. report ZKFVBMTASNEU)

      CASE sy-index.
        WHEN 1. MOVE-CORRESPONDING it_tpool TO it_de.
        WHEN 2. MOVE-CORRESPONDING it_tpool TO it_en.
      ENDCASE.
    ENDDO.

    " need to synchronize at this point, otherwise the filtering cannot be done both ways
    DO 2 TIMES.
      " first iteration: m_spras = D. it_missing = texts missing in DE (i.e. existing only in EN)
      set_language_from_index( sy-index ).
      CASE sy-index.
        WHEN 1. it_missing = FILTER #( it_en EXCEPT IN it_de WHERE id = id AND key = key ).
        WHEN 2. it_missing = FILTER #( it_de EXCEPT IN it_en WHERE id = id AND key = key ).
      ENDCASE.

      " We observed once there was an ID=R Length=0 entry for a function group. Therefore:
      DELETE it_missing WHERE length = 0.               "#EC CI_SORTSEQ

      LOOP AT it_missing ASSIGNING FIELD-SYMBOL(<wa>).
        CASE <wa>-id.
          WHEN 'S'.
            check_code = '0001'.
            IF <wa>-entry(1) = 'D'.
              " if the DDIC-Flag is set in the original language, texts are taken from DDIC.
              " But a "dummy" translation "." to other languages is required for technical reasons
              " see note https://launchpad.support.sap.com/#/notes/2287306
              " ATTENTION: the logic here has to be checked when we add a further languages (besides DE and EN)
              check_code = '0016'. " Dummy-Text "." missing   --- Note that in this case there only p_param_2 / &2 is used
            ENDIF.
            " in case 0001 (Parameters),
            " if the master language entry is missing (i.e. parameter has been removed, but translation was not deleted),
            " ignore, because of very low risk
            IF is_master_language_missing( ).
              DATA(prio3_only) = abap_true.
            ENDIF.
          WHEN 'I'. check_code = '0002'.
          WHEN 'R'. check_code = '0003'.
          WHEN OTHERS. " check for list headings not supported
        ENDCASE.
        CHECK check_code IS NOT INITIAL. " correction EvG 20180509 after Upgrade
        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_test     = c_my_name
                p_code     = check_code
                p_kind     = COND #( WHEN prio3_only = abap_false THEN c_warning
                                                                  ELSE c_note )
                p_param_1  = <wa>-key
                p_param_2  = m_laiso ).
      ENDLOOP.
    ENDDO.

    DATA(it_both) = FILTER #( it_en IN it_de WHERE id = id AND key = key ).
    LOOP AT it_both ASSIGNING <wa> WHERE id = 'S'.
      DATA(wa_de) = it_de[ id = <wa>-id key = <wa>-key ].
      DATA(wa_en) = it_en[ id = <wa>-id key = <wa>-key ].
      IF    ( wa_de-entry(1) = 'D' AND wa_en-entry(1) = ' ' )
         OR ( wa_de-entry(1) = ' ' AND wa_en-entry(1) = 'D' ).
        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_test     = c_my_name
                p_code     = '0006'
                p_param_1  = <wa>-key ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_screen_texts.
    SELECT dnum FROM d020s
      WHERE type = '' " exclude generated screens
        AND prog = @program_name
      INTO TABLE @DATA(screens).                          "#EC CI_SUBRC
    LOOP AT screens ASSIGNING FIELD-SYMBOL(<screen>).
      check_single_generic( i_check_code = '0011'
                            i_screen = CONV #( <screen> ) ).
      check_list_generic( i_check_code = '0012'
                          i_screen = CONV #( <screen> ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD check_single_generic.
    DATA: missing TYPE abap_bool
        , exists TYPE abap_bool
        .

    DO 2 TIMES.
      set_language_from_index( sy-index ).
      exists = abap_false.
      CASE i_check_code.
        WHEN '0005'.
          SELECT SINGLE @abap_true FROM dd04t
            WHERE rollname = @object_name "#ec ci_conv_ok
              AND ddlanguage = @m_spras
            INTO @exists.                                 "#EC CI_SUBRC
        WHEN '0007'.
          SELECT SINGLE @abap_true FROM tstct
            WHERE tcode = @object_name "#ec ci_conv_ok
              AND sprsl = @m_spras
            INTO @exists.                                 "#EC CI_SUBRC
        WHEN '0011'.
          SELECT SINGLE @abap_true FROM d020t
            WHERE prog = @program_name "#ec ci_conv_ok
              AND dynr = @i_screen
              AND lang = @m_spras
            INTO @exists.                                 "#EC CI_SUBRC
        WHEN '0013'.
          SELECT SINGLE @abap_true FROM tparat
            WHERE paramid = @object_name "#ec ci_conv_ok
              AND sprache = @m_spras
            INTO @exists.                                 "#EC CI_SUBRC
        WHEN '0015'.
          SELECT SINGLE @abap_true FROM tdevct
            WHERE devclass = @object_name "#ec ci_conv_ok
              AND spras = @m_spras
            INTO @exists.                                 "#EC CI_SUBRC
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.
      CASE sy-index.
        WHEN 1. DATA(de_exists) = exists.
        WHEN 2. DATA(en_exists) = exists.
      ENDCASE.
    ENDDO.

    " need to synchronize at this point, otherwise the filtering cannot be done both ways
    DO 2 TIMES.
      set_language_from_index( sy-index ).
      CASE sy-index.
        WHEN 1. missing = xsdbool( de_exists = abap_false AND en_exists = abap_true  ).
        WHEN 2. missing = xsdbool( de_exists = abap_true  AND en_exists = abap_false ).
      ENDCASE.
      IF missing = abap_true.
        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_test         = c_my_name
                p_code         = i_check_code
                " in case 0005 (Data Element),
                " if the master language entry is missing (i.e. Data element has been deleted, but translation was not deleted),
                " ignore, because of very low risk
                p_kind         = COND #( WHEN i_check_code = '0005' AND is_master_language_missing( ) THEN c_note
                                                                                                      ELSE c_warning )
                p_param_1      = i_screen    " this is only filled for check 0011. All other texts here should not make use of &1
                p_param_2      = m_laiso ).  " PARAM1 should be something key-like, for the filtering of Z_DELTA check variant
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD constructor .
************************************************************************
* ATC/Code Inspector Test for missing translations.
* Checks for various object types, that texts are maintained in DE and EN.
* Author: Edo von Glan, www.draeger.com
*
* Known limitations of this check
* - Navigation to objects (translated texts) is not possible
* - If there are several GUI status for one program, the message does not reveal
*   in which status the text is missing (because info is not directly available in
*   RSMPTEXTS, would need to evaluate output of function RS_CUA_INTERNAL_FETCH
*   for various object types)
*
* Possible additional object types / checks
*    DD01T R/3 DD: domain texts - not sure if this is ever visible for the user
*    DD02T SAP DD: SAP Table Texts - not sure if this is ever visible for the user
*    DD30T Search help texts - not sure if this is ever visible for the user
*    SE61-Texts in general
*
* Note concerning other languages:
*   Currently, DE and EN are checked.
*   It would be difficult to check more than 2 languages (big change of logic / code necessary).
*   However, it would be easily possible to replace EN with a different language as second language, maybe dependent
*   on the check variant (e.g. Z_TRANSLATION_FR might replace EN with FR)
************************************************************************
* MIT License
*
* Copyright (c) 2019 DSAG
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
************************************************************************

    DEFINE fill_message.
      CLEAR smsg.
      smsg-test = c_my_name.
      smsg-code = &1. "message code
      smsg-kind = &2. "message priority
      smsg-text = &3. "message text
      smsg-pcom = &4. "pseudocomment
      INSERT smsg INTO TABLE scimessages.
    END-OF-DEFINITION.

    super->constructor( ).

    description    = TEXT-000.
    category       = 'ZCL_CI_CATEGORY_DSAG' ##NO_TEXT.
    position       = 1.
    has_attributes = abap_false.
    attributes_ok  = abap_false.
    has_documentation = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).
    add_obj_type( 'MSAG' ).
    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TRAN' ).
    add_obj_type( 'PARA' ).
    add_obj_type( 'WDYN' ).
    add_obj_type( 'DEVC' ).

    fill_message '0001' c_warning TEXT-001 c_exceptn_imposibl . " Selektionstext &1 fehlt in Sprache &2  (TEXTPOOL-ID = S)
    fill_message '0002' c_warning TEXT-002 c_exceptn_imposibl . " Textsymbol &1 fehlt in Sprache &2 (TEXTPOOL-ID = I)
    fill_message '0003' c_warning TEXT-003 c_exceptn_imposibl . " Programmtitel fehlt in Sprache &2 (TEXTPOOL-ID = R)
    fill_message '0004' c_warning TEXT-004 c_exceptn_imposibl . " Nachricht &1 fehlt in Sprache &2
    fill_message '0005' c_warning TEXT-005 c_exceptn_imposibl . " Text zu Datenelement fehlt in Sprache &2
    fill_message '0006' c_note    TEXT-006 c_exceptn_imposibl . " Selektionstext &1 hat inkonsistenten DDIC-Bezug (DE/EN)
    fill_message '0007' c_warning TEXT-007 c_exceptn_imposibl . " Text zu Transaktion fehlt in Sprache &2
    fill_message '0008' c_warning TEXT-008 c_exceptn_imposibl . " Text zu Festwert &1 fehlt in Sprache &2
    fill_message '0009' c_warning TEXT-009 c_exceptn_imposibl . " Langtext zu Nachricht &1 fehlt in Sprache &2
    fill_message '0010' c_warning TEXT-010 c_exceptn_imposibl . " GUI-Text &1 fehlt in Sprache &2
    fill_message '0011' c_warning TEXT-011 c_exceptn_imposibl . " Text zu Dynpro &1 fehlt in Sprache &2
    fill_message '0012' c_warning TEXT-012 c_exceptn_imposibl . " Text zu Dynpro-Feld &1 fehlt in Sprache &2
    fill_message '0013' c_warning TEXT-013 c_exceptn_imposibl . " Text zu GET/SET-Parameter fehlt in Sprache &2
    fill_message '0014' c_warning TEXT-014 c_exceptn_imposibl . " OTR-Text zu Konzept &1 fehlt in Sprache &2
    fill_message '0015' c_warning TEXT-015 c_exceptn_imposibl . " Kurzbeschreibung zu Paket &1 fehlt in Sprache &2
    fill_message '0016' c_warning TEXT-016 c_exceptn_imposibl . " Dummy-Text "." zu DDIC-Selektionsparameter &1 fehlt in Sprache &2

  ENDMETHOD.


  METHOD is_master_language_missing.
    SELECT SINGLE masterlang
      FROM tadir
      WHERE pgmid     = 'R3TR'
        AND object   = @object_type
        AND obj_name = @object_name
      INTO @DATA(master_language).
    IF sy-subrc <> 0 OR master_language IS INITIAL.
      SELECT SINGLE rload
        FROM reposrc
        WHERE progname = @program_name
          AND r3state = 'A'
        INTO @master_language.
      IF sy-subrc <> 0 OR master_language IS INITIAL.
        LOG-POINT ID zlog FIELDS object_type object_name program_name.
        r = abap_false. " like this, there may be an error on the side of giving too many warnings (which we prefer to too few)
        RETURN.
      ENDIF.
    ENDIF.

    r = xsdbool( m_spras = master_language ).

  ENDMETHOD.


  METHOD run.
    CHECK object_type IN typelist.
    check_program_texts( ).
    check_screen_texts( ).
    CASE object_type.
      WHEN 'DTEL'.
        check_single_generic( '0005' ).
      WHEN 'DOMA'.
        check_list_generic( '0008' ).
      WHEN 'MSAG'.
        check_list_generic( '0004' ).
        check_list_generic( '0009' ).
      WHEN 'PROG' OR 'FUGR'.
        check_list_generic( '0010' ).
      WHEN 'TRAN'.
        check_single_generic( '0007' ).
      WHEN 'PARA'.
        check_single_generic( '0013' ).
      WHEN 'WDYN'.  " R3TR WDYN --> LIMU WDYV --> OTR text
        check_list_generic( '0014' ).
      WHEN 'DEVC'.
        check_single_generic( '0015' ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_language_from_index.
    CASE i_index.
      WHEN 1. m_spras = 'D'.
      WHEN 2. m_spras = 'E'.
    ENDCASE.
    SELECT SINGLE laiso FROM t002
      WHERE spras = @m_spras
      INTO @m_laiso.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD remove_screen_ddic_fields.
    DATA: screen_fields TYPE dyfatc_tab.
    CHECK c_fields IS NOT INITIAL.
    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname              = i_program_name
        dynnr                 = i_screen
        suppress_exist_checks = 'X'                  " Unterdrücken Existenzcheck des Dynpros
        suppress_corr_checks  = 'X'                  " Unterdrücken Verprobung Anzeigeberechtigung
      TABLES
        fields_to_containers  = screen_fields
      EXCEPTIONS
        cancelled             = 1                    " Abbruch durch Nutzer im Korrektur-popup
        not_found             = 2                    " Dynpro nicht gefunden
        permission_error      = 3                    " keine Berechtigung im Importieren (Lesen)
        OTHERS                = 4.
    ASSERT ID zlog FIELDS i_program_name i_screen CONDITION sy-subrc = 0.
    LOOP AT c_fields REFERENCE INTO DATA(check_field).

      IF NOT line_exists( screen_fields[ name = check_field->key type = 'TEXT' ] )
         OR               screen_fields[ name = check_field->key type = 'TEXT' ]-from_dict = 'X'.
        DELETE c_fields USING KEY loop_key.
      ENDIF.
    ENDLOOP.
    " Alternative implementation:
    " function DYNPRO_IMPORT, then
    " data FLG1DDF type X length 1 value '20'.
    " from_dict = <field>-FLG1 BIT_AND FLG1DDF (in SAP code, the obsolete operator 'o' is used)
  ENDMETHOD.

ENDCLASS.
