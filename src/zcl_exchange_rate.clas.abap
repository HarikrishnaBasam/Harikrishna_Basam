CLASS zcl_exchange_rate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES : if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EXCHANGE_RATE IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: r_invocie   TYPE RANGE OF vbeln,
          lv_exchange TYPE dmbtr,
          lv_price    TYPE dmbtr.
    DATA lt_original_data TYPE TABLE OF zfi_i_einvoice_ewaybill WITH DEFAULT KEY.
    lt_original_data = CORRESPONDING #( it_original_data ).
    r_invocie = VALUE #( FOR wa IN lt_original_data (
                         sign = 'I'
                         option = 'EQ'
                         low = wa-invoice
                         ) ).
    IF lt_original_data IS NOT INITIAL.
      SELECT  BillingDocument,TotalNetAmount
             FROM I_BillingDocument
             WHERE BillingDocument IN @r_invocie
               INTO TABLE @DATA(lt_billing).
      SELECT  BillingDocument,PriceDetnExchangeRate
             FROM zfi_billing_exchange_price
             WHERE BillingDocument IN @r_invocie
               INTO TABLE @DATA(lt_billing_Item).
    ENDIF.
    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CLEAR:lv_price, lv_exchange.
      READ TABLE lt_billing INTO DATA(wa_billing) WITH KEY BillingDocument = <ls_data>-invoice.
      READ TABLE lt_billing_Item INTO DATA(wa_price) WITH KEY BillingDocument = <ls_data>-invoice.
      IF sy-subrc = 0 AND <ls_data>-trans_currency NE 'INR'.
        lv_exchange = wa_price-PriceDetnExchangeRate * 100.
        <ls_data>-total_amount = wa_billing-TotalNetAmount * lv_exchange.
      ELSE.
        <ls_data>-total_amount = wa_billing-TotalNetAmount.
      ENDIF.
      <ls_data>-total_Value = <ls_data>-total_amount + <ls_data>-total_tax_amount.
    ENDLOOP.
    MOVE-CORRESPONDING lt_original_data TO ct_calculated_data.
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.
ENDCLASS.
