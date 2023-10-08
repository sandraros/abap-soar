INTERFACE zif_soar_manager_test
  PUBLIC .

  CLASS-METHODS create
    RETURNING
      VALUE(result) TYPE REF TO zif_soar_manager_test.

  METHODS square
    IMPORTING
      number        TYPE numeric
    RETURNING
      VALUE(result) TYPE decfloat34.

ENDINTERFACE.
