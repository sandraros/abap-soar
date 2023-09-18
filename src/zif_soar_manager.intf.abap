INTERFACE zif_soar_manager
  PUBLIC .

  TYPES: BEGIN OF ty_class,
           interface_name            TYPE seoclsname,
           public_class_name         TYPE seoclsname,
           in_development_class_name TYPE seoclsname,
           hash_key                  TYPE seoclsname,
         END OF ty_class.
  TYPES ty_classes TYPE STANDARD TABLE OF ty_class WITH EMPTY KEY.

  CLASS-METHODS create
    IMPORTING
      driver        TYPE REF TO zif_soar_driver
    RETURNING
      VALUE(result) TYPE REF TO zif_soar_manager.

  METHODS instantiate
    IMPORTING
      public_class_name TYPE seoclsname
    RETURNING
      VALUE(result)     TYPE REF TO object
    RAISING
      zcx_soar.

  METHODS set_outsourced_classes
    IMPORTING
      classes TYPE ty_classes.

ENDINTERFACE.
