CLASS zcl_ci_test_comp_procs DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_abap_comp_procs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
    METHODS analyze_proc REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      check_call_transaction IMPORTING i_stmt TYPE cl_abap_comp_procs=>t_stmt,
      check_unwanted_statement IMPORTING i_stmt TYPE cl_abap_comp_procs=>t_stmt,
      check_auth_exception IMPORTING i_stmt TYPE cl_abap_comp_procs=>t_stmt,
      check_new_subroutines IMPORTING i_stmt TYPE cl_abap_comp_procs=>t_stmt,
      is_catch IMPORTING i_stmt   TYPE cl_abap_comp_procs=>t_stmt
               RETURNING VALUE(r) TYPE abap_bool,
      report_missing_catch.
    CONSTANTS: con_forms_allowed_until TYPE endda VALUE '20190101'.
    DATA: m_call_ta_with_auth_check_stmt TYPE cl_abap_comp_procs=>t_stmt
        .
ENDCLASS.



CLASS zcl_ci_test_comp_procs IMPLEMENTATION.


  METHOD analyze_proc.
    TRY.
        LOOP AT p_proc-stmts REFERENCE INTO DATA(stmt).
          IF m_call_ta_with_auth_check_stmt IS NOT INITIAL.
            IF is_catch( stmt->* ).
              CLEAR m_call_ta_with_auth_check_stmt.
            ENDIF.
          ENDIF.
          CASE stmt->keyword.
            WHEN 'CALL'.
              check_call_transaction( stmt->* ).
            WHEN 'DEFINE' OR 'DATA'.
              check_unwanted_statement( stmt->* ).
            WHEN 'FORM'.
              check_new_subroutines( stmt->* ).
            WHEN 'ENDFORM' OR 'ENDFUNCTION' OR 'ENDMETHOD'.
              IF m_call_ta_with_auth_check_stmt IS NOT INITIAL.
                report_missing_catch(  ).
                CLEAR m_call_ta_with_auth_check_stmt.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        LOG-POINT ID zlog_p FIELDS stmt->include stmt->line. " unexpected statement pattern
    ENDTRY.
  ENDMETHOD.


  METHOD check_auth_exception.
    IF i_stmt-keyword <> 'CATCH' OR i_stmt-tokens[ 2 ]-str <> |CX_SY_AUTHORIZATION_ERROR|.
      add_info( p_include = m_call_ta_with_auth_check_stmt-include
                p_line    = m_call_ta_with_auth_check_stmt-line
                p_column  = m_call_ta_with_auth_check_stmt-column
                p_kind    = '0004' ).
    ENDIF.
  ENDMETHOD.

  METHOD is_catch.
    r = xsdbool( i_stmt-keyword = 'CATCH' AND i_stmt-tokens[ 2 ]-str CP |CX_*| ).
  ENDMETHOD.

  METHOD report_missing_catch.
    add_info( p_include = m_call_ta_with_auth_check_stmt-include
              p_line    = m_call_ta_with_auth_check_stmt-line
              p_column  = m_call_ta_with_auth_check_stmt-column
              p_kind    = '0004'
              p_comments = m_call_ta_with_auth_check_stmt-comments ).
  ENDMETHOD.

  METHOD check_call_transaction.
    IF i_stmt-tokens[ 2 ]-str = |TRANSACTION|.
      IF lines( i_stmt-tokens ) < 5 OR i_stmt-tokens[ 4 ]-str NP |WITH*| OR i_stmt-tokens[ 5 ]-str <> |AUTHORITY-CHECK|.
        add_info( p_include = i_stmt-include
                  p_line    = i_stmt-line
                  p_column  = i_stmt-column
                  p_kind    = '0001' ).
        RETURN.
      ENDIF.
      IF i_stmt-tokens[ 4 ]-str = |WITH|.
        m_call_ta_with_auth_check_stmt = i_stmt.  " check whether there is a catch CX_SY_AUTHORIZATION_ERROR statement in the following lines
      ELSE.
        ASSERT ID zlog_p FIELDS i_stmt-include i_stmt-line CONDITION i_stmt-tokens[ 4 ]-str = |WITHOUT|. " unexpected statement pattern (neither WITH nor WITHOUT)
        IF lines( i_stmt-tokens ) < 6 OR i_stmt-tokens[ 6 ]-str <> |USING|.  " WITHOUT AUTH-CHECK USING bdc is allowed
          add_info( p_include = i_stmt-include
                    p_line    = i_stmt-line
                    p_column  = i_stmt-column
                    p_kind    = '0002' ).
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_new_subroutines.
    SELECT SINGLE @abap_true FROM reposrc
      WHERE progname = @i_stmt-include
        AND cdat > @con_forms_allowed_until
        INTO @data(dummy).
    CHECK sy-subrc = 0.
    add_info( p_include = i_stmt-include
              p_line    = i_stmt-line
              p_column  = i_stmt-column
              p_kind    = '0005' ).
  ENDMETHOD.


  METHOD check_unwanted_statement.
    IF i_stmt-keyword = 'DEFINE'.
      DATA(param1) = 'DEFINE macro'.
    ENDIF.
    IF i_stmt-keyword = 'DATA' AND lines( i_stmt-tokens ) >= 5
                               AND i_stmt-tokens[ 2 ]-str = |BEGIN|
                               AND i_stmt-tokens[ 3 ]-str = |OF|
                               AND i_stmt-tokens[ 4 ]-str = |COMMON|
                               AND i_stmt-tokens[ 5 ]-str = |PART|.
      param1 = 'COMMON PART'.
    ENDIF.
    IF param1 IS NOT INITIAL.
      add_info( p_include = i_stmt-include
                p_line    = i_stmt-line
                p_column  = i_stmt-column
                p_kind    = '0003'
                p_name    = param1 ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
************************************************************************
* ATC/Code Inspector Test for various topics, based on code
* (where inheritance from cl_ci_test_abap_comp_procs is useful).
* Author: Edo von Glan, www.draeger.com
*
* - CALL TRANSACTION:
*   - always with WITH/WITHOUT AUTH-CHECK
*   (is possible with standard ATC check, but has no helpful text info)
*   - WITHOUT CHECK only with USING bdc or via exemption
*   - WITH AUTH needs CATCH in following lines
* - Obsolete and unwanted Language features
*   DEFINE *
*   BEGIN OF COMMON PART *
*   (is possible in standard, but has no helpful text info)
* - FORMs not allowed in new includes (after con_forms_allowed_until),
*   no pragma possible
*
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
    super->constructor( ).

    description    = 'Verschiedene Prüfungen (Code)'(000).
    myname         = 'ZCL_CI_TEST_COMP_PROCS'.
    category       = 'ZCL_CI_CATEGORY_DSAG' ##NO_TEXT.
    position       = 2.
    has_attributes = abap_false.
    attributes_ok  = abap_false.
    has_documentation = abap_true.
    remote_enabled = abap_false.
    remote_rfc_enabled = abap_false.

    INSERT VALUE scimessage( test = myname
                             code = '0001'
                             kind = c_error
                             text = 'CALL TRANSACTION ohne AUTHORITY-CHECK Angabe'(001) ) INTO TABLE scimessages .
    INSERT VALUE scimessage( test = myname
                             code = '0002'
                             kind = c_error
                             text = 'CALL TA WITHOUT AUTH-CHECK - benötigt Genehmigung'(002) ) INTO TABLE scimessages .
    INSERT VALUE scimessage( test = myname
                             code = '0003'
                             kind = c_error
                             text = 'Obsoleter und nicht erwünschter Befehl &1'(003) ) INTO TABLE scimessages .
    INSERT VALUE scimessage( test = myname
                             code = '0004'
                             kind = c_error
                             text = 'Fehlendes CATCH CX_SY_AUTHORIZATION_ERROR (Dump wenn unberechtigt)'(004)
" does not work                            pcom = 'CI_NOCATCH'
                             ) INTO TABLE scimessages .
    INSERT VALUE scimessage( test = myname
                             code = '0005'
                             kind = c_error
                             text = 'Subroutinen (FORMs) sind obsolet'(005) ) INTO TABLE scimessages .
  ENDMETHOD.


  METHOD run.
    CHECK get( ).
    analyze_start( ref_check ).
    LOOP AT infos REFERENCE INTO DATA(info).  " TODO SAP Docu: include this part
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = info->include
              p_line         = info->line
              p_column       = info->column
              p_test         = myname
              p_code         = info->kind
              p_param_1      = info->name ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
