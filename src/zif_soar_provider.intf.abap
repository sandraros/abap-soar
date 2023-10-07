INTERFACE zif_soar_provider
  PUBLIC .

*  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  METHODS get_abap_source_code
    IMPORTING
      srp_id        type zsoar_srp_id
*      class_name    TYPE seoclsname
    RETURNING
      VALUE(result) TYPE zif_soar_manager=>ty_abap_source_code.

  METHODS get_classes
    RETURNING
      VALUE(result) TYPE zif_soar_manager=>ty_classes.

*  METHODS instantiate
*    IMPORTING
*      srp_oo            TYPE REF TO zif_soar_srpoo
*      public_class_name TYPE seoclsname
*    RETURNING
*      VALUE(result)     TYPE REF TO object
*    RAISING
*      cx_static_check
*      cx_dynamic_check.

ENDINTERFACE.
