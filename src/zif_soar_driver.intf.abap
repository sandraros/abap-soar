INTERFACE zif_soar_driver
  PUBLIC .

*  METHODS generate_subroutine_pool
*    IMPORTING
*      class_name    TYPE seoclsname
*    RETURNING
*      VALUE(result) TYPE REF TO zif_soar_gensrp
*    RAISING
*      zcx_soar_gensrp.

  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  METHODS get_abap_source_code
    IMPORTING
      class_name    TYPE seoclsname
    RETURNING
      VALUE(result) TYPE ty_abap_source_code.

  METHODS get_classes
    RETURNING
      VALUE(result) TYPE zif_soar_manager=>ty_classes.

  METHODS instantiate
    IMPORTING
      gensrp            TYPE REF TO zif_soar_gensrp
      public_class_name TYPE seoclsname
    RETURNING
      VALUE(result)     TYPE REF TO object
    RAISING
      cx_static_check
      cx_dynamic_check.

ENDINTERFACE.
