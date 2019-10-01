CLASS zcl_ci_test_header_comment DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_scan
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor .
    METHODS run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: con_my_name TYPE seoclsname VALUE 'ZCL_CI_TEST_HEADER_COMMENT' ##NO_TEXT
             .
    METHODS:
      failed
    , get_nonclass_1st_relevt_token
        RETURNING VALUE(r_token_index) TYPE sytabix.

ENDCLASS.



CLASS ZCL_CI_TEST_HEADER_COMMENT IMPLEMENTATION.


  METHOD constructor .
************************************************************************
* ATC/Code Inspector Test for header comments.
* Checks for PROG, FUGR, CLAS and INTF, that a header comment was created.
* Author: Edo von Glan, www.draeger.com
*
* Required position of the comment:
* - For normal programs, first line of frame program
* - Header comments for global classes have to be placed in the constructor
*   method of the class, after the line
*     METHOD constructor.
*   (in most cases, you will have to define a constructor just for this purpose.
*   If you are in a subclass, you have to call
*     super->constructor( ).
*   at the end of the method.).
* - For global exception classes, the CLASS_CONSTRUCTOR has to be used
* - For global interfaces, in the line directly after the INTERFACE statement
* - For function pools, in TOP-include, starting in the first line (i.e. in front of the FUNCTION-POOL statement)
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

    description    = TEXT-000.
    category       = |ZCL_CI_CATEGORY_DSAG|.
    position       = 2.
    has_attributes = abap_false.
    attributes_ok  = abap_false.
    has_documentation = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).
    " enhancement implementations cannot be tested here - this class is not called by the framework even if ENHO is added as relevant object type
    " (ZCL_CI_TEST_TRANSLATIONS is called, presumably because it only inherits from CL_CI_TEST_ROOT)

    scimessages = VALUE #( ( test = con_my_name code = '0001' kind = c_warning text = TEXT-001 pcom = c_exceptn_imposibl ) ). " Kopfkommentar fehlt in &1 &2

  ENDMETHOD.


  METHOD failed.
    inform( p_sub_obj_type = object_type
            p_sub_obj_name = object_name
            p_test         = con_my_name
            p_code         = '0001'
            p_param_1      = object_type
            p_param_2      = object_name ).
  ENDMETHOD.


  METHOD get_nonclass_1st_relevt_token.
    CASE object_type.
      WHEN 'PROG'.
        r_token_index = 1.
      WHEN 'FUGR'.
        DATA(top_include) = ref_scan->levels[ 2 ].
        ASSERT ID zlog FIELDS object_name CONDITION top_include-name CP 'L*TOP'. " FUGR TOP include should always be in second line of scanned levels
        r_token_index = ref_scan->statements[ top_include-from ]-from.
      WHEN 'INTF'.
        LOOP AT ref_scan->statements REFERENCE INTO DATA(stmt) WHERE type = 'K'.
          IF ref_scan->tokens[ stmt->from ]-str = 'INTERFACE'.
            r_token_index = stmt->to + 1.
            RETURN.
          ENDIF.
        ENDLOOP.
        ASSERT ID zlog FIELDS object_name CONDITION lines( ref_scan->levels ) = 2. " INTF should contain an INTERFACE statement
    ENDCASE.
  ENDMETHOD.


  METHOD run.
    CHECK object_type IN typelist.
    CHECK get( ).

    IF object_type = 'CLAS'.
      DATA(parse_status) = |0 CLAS Init|.
    ELSE.
      DATA(nonclass_first_relevant_token) = get_nonclass_1st_relevt_token( ). " first relevant token, for objects other than CLAS
    ENDIF.
    DATA(lines) = lines( ref_scan->tokens ).
    DATA(comment_lines_found) = 0.

    LOOP AT ref_scan->tokens FROM nonclass_first_relevant_token REFERENCE INTO DATA(token).
      DATA(index) = sy-tabix.
      " If the current token is a potential comment
      IF    parse_status = '2 CLAS Constr Impl'
         OR object_type <> 'CLAS' AND index >= nonclass_first_relevant_token.
        " Check that the first five lines are comments (for comments, one token = one line)
        FIND REGEX '^\*' IN token->str. " alternativ: token->type = 'C'
        IF sy-subrc <> 0.
          parse_status = '- No Comment Found'.
          EXIT.
        ELSE.
          ADD 1 TO comment_lines_found.
          IF comment_lines_found >= 5.
            parse_status = '+ Comment Found'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      IF object_type = 'CLAS'.
        " Check whether we have progressed to a new position/status
        CASE token->str.
          WHEN 'CLASS'.
            IF     parse_status = '0 CLAS Init' AND lines >= index + 2
               AND ref_scan->tokens[ index + 1 ]-str = object_name
               AND ref_scan->tokens[ index + 2 ]-str = 'IMPLEMENTATION'.
              parse_status = '1 CLAS Impl'.
            ENDIF.
          WHEN 'CONSTRUCTOR'.
            IF object_name CP 'ZCL*'
               AND parse_status = '1 CLAS Impl' AND index > 1
               AND ref_scan->tokens[ index - 1 ]-str = 'METHOD'.
              parse_status = '2 CLAS Constr Impl'.
            ENDIF.
          WHEN 'CLASS_CONSTRUCTOR'.
            IF object_name CP 'ZCX*'  " for exception classes, the constructor is generated and cannot be modified
               AND parse_status = '1 CLAS Impl' AND index > 1
               AND ref_scan->tokens[ index - 1 ]-str = 'METHOD'.
              parse_status = '2 CLAS Constr Impl'.
            ENDIF.
          WHEN 'ENDMETHOD'.
            IF parse_status = '2 CLAS Constr Impl'.
              parse_status = '- No Comment Found'.
              EXIT.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.
    IF parse_status <> '+ Comment Found'.
      failed( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
