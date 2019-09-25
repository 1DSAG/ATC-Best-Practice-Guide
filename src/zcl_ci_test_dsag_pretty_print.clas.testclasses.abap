CLASS lth_verif_pp DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS: ci_check_single IMPORTING iv_variant            TYPE sci_chkv
                                       iv_obj_type           TYPE trobjtype
                                       iv_obj_name           TYPE sobj_name
                             RETURNING VALUE(rt_result_list) TYPE scit_rest
                             RAISING   cx_ci_invalid_variant
                                       cx_ci_invalid_object
                                       cx_ci_check_error.

ENDCLASS.


CLASS lth_verif_pp IMPLEMENTATION.

  METHOD ci_check_single.

    DATA: lo_result TYPE REF TO  cl_ci_check_result,
          lv_ok     TYPE sychar01.


    cl_ci_check=>single( EXPORTING p_variant_user = ' '
                                   p_variant      = iv_variant
                                   p_obj_type     = iv_obj_type
                                   p_obj_name     = iv_obj_name
                         IMPORTING p_ok           = lv_ok
                                   p_result       = lo_result ).

    lo_result->get_result( IMPORTING p_result = rt_result_list ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_test DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    METHODS pp_ok FOR TESTING.
    METHODS pp_nok FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.
    DATA: mo_helper TYPE REF TO lth_verif_pp.

ENDCLASS.

CLASS ltc_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_helper.
  ENDMETHOD.

  METHOD pp_ok.

    DATA: lt_result_list TYPE scit_rest,
          lv_lines       TYPE sy-index.

    TRY.
        lt_result_list = mo_helper->ci_check_single( EXPORTING iv_variant  = 'ZVERIF_PRETTY_PRINTER'
                                                               iv_obj_type = 'CLAS'
                                                               iv_obj_name = 'ZCL_CI_TEST_DSAG_PRETTY_PRINT' ).
      CATCH cx_ci_invalid_variant.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_INVALID_VARIANT' ). "#EC NOTEXT
      CATCH cx_ci_invalid_object.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_INVALID_OBJECT' ). "#EC NOTEXT
      CATCH cx_ci_check_error.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_CHECK_ERROR' ). "#EC NOTEXT
    ENDTRY.

    DESCRIBE TABLE lt_result_list LINES lv_lines.

    cl_abap_unit_assert=>assert_equals( act = lv_lines
                                        exp = 0
                                        msg = 'Falsche Anzahl von Meldungen' ).

  ENDMETHOD.

  METHOD pp_nok.

    DATA: lt_result_list TYPE scit_rest,
          lv_lines       TYPE sy-index.


    TRY.
        lt_result_list = mo_helper->ci_check_single( EXPORTING iv_variant  = 'ZVERIF_PRETTY_PRINTER_N'
                                                               iv_obj_type = 'CLAS'
                                                               iv_obj_name = 'ZCL_CI_TEST_DSAG_PRETTY_PRINT' ).
      CATCH cx_ci_invalid_variant.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_INVALID_VARIANT' ). "#EC NOTEXT
      CATCH cx_ci_invalid_object.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_INVALID_OBJECT' ). "#EC NOTEXT
      CATCH cx_ci_check_error.
        cl_abap_unit_assert=>fail( 'Exception CX_CI_CHECK_ERROR' ). "#EC NOTEXT
    ENDTRY.

    DESCRIBE TABLE lt_result_list LINES lv_lines.

    cl_abap_unit_assert=>assert_equals( act = lv_lines
                                        exp = 9
                                        msg = 'Falsche Anzahl von Meldungen' ).

  ENDMETHOD.

ENDCLASS.
