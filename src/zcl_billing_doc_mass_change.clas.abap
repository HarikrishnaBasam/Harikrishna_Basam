CLASS zcl_billing_doc_mass_change DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES:
      BEGIN OF ty_billing_doc,
        billingdocument TYPE vbeln,
        new_status      TYPE char1_run_type,
      END OF ty_billing_doc,
      ty_billing_docs TYPE TABLE OF ty_billing_doc.

    METHODS:
      schedule_mass_change_job
        IMPORTING
          it_billing_docs  TYPE ty_billing_docs
        RETURNING
          VALUE(rv_job_id) TYPE cl_apj_rt_api=>ty_job_info
        RAISING
          cx_apj_rt.

  PROTECTED SECTION.
  PRIVATE SECTION.
*    METHODS:
*      create_job_template
*        RETURNING
*          VALUE(ro_job_template) TYPE REF TO if_apj_rt_job_template
*        RAISING
*          cx_apj_rt.
ENDCLASS.



CLASS ZCL_BILLING_DOC_MASS_CHANGE IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA: lt_billing_docs TYPE ty_billing_docs.

    " Example: Populate billing documents for mass change
    lt_billing_docs = VALUE #(
      ( billingdocument = '90000128' new_status = 'C' )
*      ( billingdocument = '90000002' new_status = 'C' )
    ).

    TRY.
        DATA(lv_job_id) = schedule_mass_change_job( lt_billing_docs ).
*        out->write( |Job { lv_job_id } scheduled successfully| ).
      CATCH cx_apj_rt INTO DATA(lx_error).
        out->write( |Error scheduling job: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.


  METHOD schedule_mass_change_job.
    DATA:lt_parameters TYPE cl_apj_rt_api=>tt_job_parameter_value.
    TYPES: BEGIN OF ty_start_info ,
             start_immediately TYPE abap_boolean,
             timestamp         TYPE timestamp,
           END OF ty_start_info.

    DATA: job_start_info TYPE ty_start_info,
          lv_jobname(32) TYPE c,
          lv_jobcount(8) TYPE c,
          lt_error       TYPE TABLE OF bapiret2.


    GET TIME STAMP FIELD DATA(lv_tstmp).
    job_start_info-timestamp =  cl_abap_tstmp=>add_to_short(
                                  tstmp = lv_tstmp
                                  secs  = 5
                                ).
  types:
    BEGIN OF ty_value_range,
           sign   TYPE tvarv_sign,
           option TYPE tvarv_opti,
           low    TYPE char256,
           high   TYPE char256,
         END OF ty_value_range .

  DATA:  tt_value_range TYPE STANDARD TABLE OF ty_value_range WITH EMPTY KEY .
  DATA: wa_data TYPE ty_value_range.
     wa_data-sign = 'I'.
     wa_data-option = 'EQ'.
     wa_data-Low = '90000128'.
     wa_data-high = ''.
     APPEND wa_data to tt_value_range.


    " Prepare job parameters
    lt_parameters = VALUE #(
      ( name = 'BILLING_DOCS' t_value = tt_value_range )
    ).


    TRY.
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name          = 'BILLING_DOCUMENTS_OC_MASS_CHANGE'
            iv_job_text                   =  |{ 'Mass Change of Billing Documents Output' }| && | | && cl_abap_context_info=>get_system_date( ) && cl_abap_context_info=>get_system_time( )
            is_start_info                 = VALUE #( timestamp = cl_abap_tstmp=>add_to_short(
                                                                        tstmp = lv_tstmp
                                                                        secs  = 5
                                                                                          ) )
            iv_username                   = sy-uname
*        is_end_info                   =
*        is_scheduling_info            =
        it_job_parameter_value        = lt_parameters
*        it_job_parameter_value_simple =
*        iv_jobname                    =
*        iv_jobcount                   =
*        iv_with_message_table         = abap_false
    IMPORTING
      ev_jobname                    = lv_jobname
      ev_jobcount                   = lv_jobcount
      et_message                    = lt_error
        ).
      CATCH cx_apj_rt INTO DATA(lv_msg).
      lv_msg->get_text(
        RECEIVING
          result =  DATA(lv_result)
      ).

        CLEAR: lv_jobname.
        "handle exception
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
