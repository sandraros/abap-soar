**********************************************************************
*
* https://github.com/sandraros/abap-generate-srp
*
**********************************************************************
*
* All code from https://github.com/sandraros/abap-generate-srp has been
* repackaged automatically by program ZSOAR_GENSRP_PACKAGING into these objects:
*   - Include ZSOAR_ABAPGENSRP
*   - ...
*
**********************************************************************
*
* MIT License
*
* Copyright (c) 2023 sandraros
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
*
**********************************************************************

CLASS lcl_gensrp DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_soar_gensrp.

    "! Instantiation is done by the method ZIF_SOAR_GENSRP~GENERATE_SUBROUTINE_POOL.
    METHODS constructor
      IMPORTING
        abap_code TYPE zif_soar_gensrp=>ty_abap_source_code
      RAISING
        zcx_soar_gensrp.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_gensrp IMPLEMENTATION.

  METHOD constructor.

    GENERATE SUBROUTINE POOL abap_code
        NAME         zif_soar_gensrp~srp_name
        MESSAGE      zif_soar_gensrp~message
        LINE         zif_soar_gensrp~line
        WORD         zif_soar_gensrp~word
        INCLUDE      zif_soar_gensrp~include
        MESSAGE-ID   zif_soar_gensrp~message_id
        OFFSET       zif_soar_gensrp~offset
        SHORTDUMP-ID zif_soar_gensrp~shortdump_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_soar_gensrp
        EXPORTING
          text  = 'Generation error &1 at line &2: &3'(003)
          msgv1 = |{ sy-subrc }|
          msgv2 = |{ zif_soar_gensrp~line }|
          msgv3 = zif_soar_gensrp~message.

    ENDIF.

  ENDMETHOD.


  METHOD zif_soar_gensrp~call_static_method.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    "=============
    " Preparation
    "=============
    IF result_object-parameter_name IS NOT INITIAL
        AND NOT line_exists( parameters[ name = result_object-parameter_name ] ).
      DATA(parameters_local) = VALUE abap_parmbind_tab(
                      BASE parameters
                      ( name = result_object-parameter_name value = NEW dba_object_ref( ) ) ).
      ASSIGN parameters_local TO <parameters>.
    ELSE.
      ASSIGN parameters TO <parameters>.
    ENDIF.

    "=============
    " Execution
    "=============
    PERFORM call_static_method IN PROGRAM (zif_soar_gensrp~srp_name)
      USING class_name
            method_name
            <parameters>.

    "=============
    " Follow-up
    "=============
    IF result_object-parameter_name IS NOT INITIAL.

      DATA(ref_returning_parameter) = REF #( <parameters>[ name = result_object-parameter_name ] OPTIONAL ).
      ASSERT ref_returning_parameter IS BOUND.

      ASSIGN ref_returning_parameter->value->* TO FIELD-SYMBOL(<object>).
      TRY.
          result = <object>.
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_soar_gensrp
            EXPORTING
              text  = 'Receiving variable is of type &1 (must be REF TO OBJECT)'(001)
              msgv1 = cl_abap_typedescr=>describe_by_data( <object> )->absolute_name.
      ENDTRY.

      IF result_object-bound_optional = abap_false
            AND result IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_soar_gensrp
          EXPORTING
            text = 'Result is not bound'(002).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_soar_gensrp~create_object.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    IF via_perform = abap_false.

      DATA(absolute_name) = |\\PROGRAM={ zif_soar_gensrp~srp_name }\\CLASS={ class_name }|.
      " EXCEPTION-TABLE is required by kernels before note 2866213, even if no specific need.
      " NB: note 2866213 - ABAP short dump RUNT_ILLEGAL_SWITCH at CREATE OBJECT ... PARAMETER-TABLE
      " at https://me.sap.com/notes/2866213/E.
      DATA(dummy_exception_table) = VALUE abap_excpbind_tab( ).
      CREATE OBJECT result TYPE (absolute_name)
                            PARAMETER-TABLE parameters
                            EXCEPTION-TABLE dummy_exception_table.

    ELSE.

      PERFORM create_object IN PROGRAM (zif_soar_gensrp~srp_name)
        USING    class_name
                 parameters
        CHANGING result.

    ENDIF.

    IF result IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_soar_gensrp
        EXPORTING
          text = 'Result is not bound'(002).
    ENDIF.

  ENDMETHOD.


  METHOD zif_soar_gensrp~generate_subroutine_pool.

    result = NEW lcl_gensrp( abap_source_code ).

  ENDMETHOD.

ENDCLASS.
