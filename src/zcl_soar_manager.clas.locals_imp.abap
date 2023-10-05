*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INCLUDE zsoar_gensrp.


CLASS lcl_soar_srpoo DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_soar_gensrp.

    CLASS-METHODS create
      IMPORTING
        subroutine_pool_name TYPE syrepid
      RETURNING
        VALUE(result)        TYPE REF TO zif_soar_gensrp.

ENDCLASS.


CLASS lcl_soar_srpoo IMPLEMENTATION.

  METHOD create.

    data(srpoo) = NEW lcl_soar_srpoo( ).
    srpoo->zif_soar_gensrp~srp_name = subroutine_pool_name.

    result = srpoo.

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

  ENDMETHOD.

ENDCLASS.
