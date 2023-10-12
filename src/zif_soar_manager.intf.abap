**********************************************************************
*
* https://github.com/sandraros/abap-soar
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
INTERFACE zif_soar_manager
  PUBLIC .

  TYPES:
    "! Description of the RETURNING parameter of a class method if the sense it's
    "! expected to be a factory method).
    BEGIN OF ty_result_object,
      parameter_name TYPE string,
      bound_optional TYPE abap_bool,
    END OF ty_result_object.
  TYPES: BEGIN OF ty_generate_subroutine_pool,
           name         TYPE syrepid,
           message      TYPE string,
           line         TYPE i,
           word         TYPE string,
           include      TYPE string,
           message_id   TYPE trmsg_key,
           offset       TYPE i,
           shortdump_id TYPE string,
           trace_file   TYPE string,
         END OF ty_generate_subroutine_pool.

  DATA srp_name TYPE ty_generate_subroutine_pool-name READ-ONLY.

  METHODS call_static_method
    IMPORTING
      class_name    TYPE seoclsname
      method_name   TYPE seomtdname
      parameters    TYPE abap_parmbind_tab OPTIONAL
      exceptions_   TYPE abap_excpbind_tab OPTIONAL
      result_object TYPE ty_result_object OPTIONAL
      via_perform   TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE REF TO object
    RAISING
      zcx_soar.

  CLASS-METHODS create
    IMPORTING
      srp_id        TYPE zsoar_srp_id
      provider      TYPE REF TO zif_soar_provider
    RETURNING
      VALUE(result) TYPE REF TO zif_soar_manager
    RAISING
      zcx_soar.

  METHODS create_object
    IMPORTING
      class_name    TYPE seoclsname
      parameters    TYPE abap_parmbind_tab OPTIONAL
      exceptions_   TYPE abap_excpbind_tab OPTIONAL
      via_perform   TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE REF TO object
    RAISING
      zcx_soar.

  METHODS get_last_generate_subr_pool
    RETURNING
      VALUE(result) TYPE ty_generate_subroutine_pool.

ENDINTERFACE.
