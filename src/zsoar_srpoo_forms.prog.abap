*&---------------------------------------------------------------------*
*& Include zsoar_srpoo_forms
*&---------------------------------------------------------------------*

* The methods ZIF_SOAR_SRPOO~CALL_STATIC_METHOD and ZIF_SOAR_SRPOO~CREATE_OBJECT of class ZCL_SOAR_MANAGER
* propose two ways to work with a subroutine pool, either the traditional way with PERFORM, or
* with the absolute type name of a local class. If you prefer to start with PERFORM, you must include
* the code below in your subroutine pool so that SOAR can instantiate your local class or call static methods.
*
* Below are 4 excerpts from <https://help.sap.com/doc/abapdocu_757_index_htm/7.57/en-US/index.htm?file=abapgenerate_subroutine_pool.htm>
* which explain that the classic way is to use PERFORM, but two examples are given which explain that the absolute
* type name of a local class can be used instead of PERFORM (either "CALL METHOD (class)=>meth", or
* "CREATE OBJECT oref TYPE (class)" followed by "CALL METHOD oref->('METH')").
*
* 1)
* Subroutines defined in the source code of the subroutine pool can be called from all programs that are
* loaded in the same internal session by specifying the program name prog using the statement PERFORM.
*
* 2)
* Hints
*   • Since subroutines are now obsolete as a method of program modularization, a temporary subroutine pool
*     created using GENERATE SUBROUTINE POOL should only contain a single initial subroutine that calls a
*     method of a local class and does not contain any other functional code.
*
* 3)
* Example
*   Creates and dynamically generates a subroutine pool that implements a local class. The static method meth
*   of the class can be called using the absolute type name of the class.
*
*   [...]
*
*   GENERATE SUBROUTINE POOL itab NAME FINAL(prog).
*   class = `\PROGRAM=` && prog && `\CLASS=MAIN`.
*   CALL METHOD (class)=>meth.
*
* 4)
* Example
*   Creates and dynamically generates a subroutine pool that implements a local class. The class is
*   instantiated using its absolute type name, and the instance method meth is called dynamically.
*
*   [...]
*
*   GENERATE SUBROUTINE POOL itab NAME FINAL(prog).
*   class = `\PROGRAM=` && prog && `\CLASS=MAIN`.
*   CREATE OBJECT oref TYPE (class).
*   CALL METHOD oref->('METH').


FORM call_static_method
    USING
      class_name  TYPE seoclsname
      method_name TYPE seomtdname
      parameters  TYPE abap_parmbind_tab
      exceptions  TYPE abap_excpbind_tab
    RAISING
      cx_static_check
      cx_dynamic_check.

  CALL METHOD (class_name)=>(method_name)
    PARAMETER-TABLE parameters
    EXCEPTION-TABLE exceptions.

ENDFORM.


FORM create_object
    USING
      class_name  TYPE seoclsname
      parameters  TYPE abap_parmbind_tab
      exceptions  TYPE abap_excpbind_tab
    CHANGING
      result      TYPE REF TO object
    RAISING
      cx_static_check
      cx_dynamic_check.

  " EXCEPTION-TABLE is required by kernels before note 2866213, even if no specific need.
  " NB: note 2866213 - ABAP short dump RUNT_ILLEGAL_SWITCH at CREATE OBJECT ... PARAMETER-TABLE
  " at https://me.sap.com/notes/2866213/E.
  CREATE OBJECT result TYPE (class_name)
    PARAMETER-TABLE parameters
    EXCEPTION-TABLE exceptions.

ENDFORM.
