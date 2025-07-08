CLASS lhc__Invoice DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_Item,
        SlNo       TYPE string,
        PrdDesc    TYPE string,
        HsnCd      TYPE string,
        Qty        TYPE p LENGTH 6 DECIMALS 3,
        Unit       TYPE string,
        UnitPrice  TYPE p LENGTH 15 DECIMALS 2,
        TotAmt     TYPE p LENGTH 15 DECIMALS 2,
        GstRt      TYPE p LENGTH 5 DECIMALS 2,
        IgstAmt    TYPE p LENGTH 15 DECIMALS 2,
        CgstAmt    TYPE p LENGTH 15 DECIMALS 2,
        SgstAmt    TYPE p LENGTH 15 DECIMALS 2,
        AssAmt     TYPE p LENGTH 15 DECIMALS 2,
        TotItemVal TYPE p LENGTH 15 DECIMALS 2,
        Discount   TYPE p LENGTH 15 DECIMALS 2,
        OthChrg    TYPE p LENGTH 15 DECIMALS 2,
      END OF ty_Item,

      BEGIN OF ty_ValDtls,
        AssVal      TYPE p LENGTH 15 DECIMALS 2,
        SgstVal     TYPE p LENGTH 15 DECIMALS 2,
        CgstVal     TYPE p LENGTH 15 DECIMALS 2,
        IgstVal     TYPE p LENGTH 15 DECIMALS 2,
        TotInvVal   TYPE p LENGTH 15 DECIMALS 2,
        TotInvValFc TYPE p LENGTH 15 DECIMALS 2,
        Discount    TYPE p LENGTH 15 DECIMALS 2,
        OthChrg     TYPE p LENGTH 15 DECIMALS 2,
        RndOffAmt   TYPE p LENGTH 15 DECIMALS 2,
      END OF ty_ValDtls,

      BEGIN OF ty_SellerDtls,
        Gstin TYPE string,
        LglNm TYPE string,
        Addr1 TYPE string,
        Loc   TYPE string,
        Pin   TYPE string,
        Stcd  TYPE string,
      END OF ty_SellerDtls,

      BEGIN OF ty_BuyerDtls,
        Gstin TYPE string,
        LglNm TYPE string,
        Addr1 TYPE string,
        Loc   TYPE string,
        Pin   TYPE string,
        Stcd  TYPE string,
      END OF ty_BuyerDtls,

      BEGIN OF ty_EwbDtls,
        TransId    TYPE string,
        TransMode  TYPE string,
        Distance   TYPE string,
        TransName  TYPE string,
        TransDocNo TYPE string,
        TransDocDt TYPE string,
        VehNo      TYPE string,
        VehType    TYPE string,
      END OF ty_EwbDtls,

      BEGIN OF ty_TxnHdr,
        txnId     TYPE string,
        userGstin TYPE string,
        action    TYPE string,
        docnum    TYPE string,
        docdate   TYPE string,
      END OF ty_TxnHdr,

      BEGIN OF ty_TranDtl,
        TaxSch   TYPE string,
        SupTyp   TYPE string,
        RegRev   TYPE string,
        EcmGstin TYPE string,
      END OF ty_TranDtl,

      BEGIN OF ty_DocDtls,
        Typ TYPE string,
        No  TYPE string,
        Dt  TYPE string,
      END OF ty_DocDtls,

      BEGIN OF ty_exp_dtls,
        forcur  TYPE string,
        cntcode TYPE string,
      END OF ty_exp_dtls,

      BEGIN OF ty_TxnDocData,
        Version    TYPE string,
        TranDtls   TYPE ty_TranDtl, " string,  " To be detailed if necessary
        DocDtls    TYPE ty_DocDtls, "string,   " To be detailed if necessary
        SellerDtls TYPE ty_SellerDtls,
        BuyerDtls  TYPE ty_BuyerDtls,
        ValDtls    TYPE ty_ValDtls,
        ItemList   TYPE STANDARD TABLE OF  ty_Item WITH EMPTY KEY,
*        EwbDtls    TYPE ty_EwbDtls,
        expdtls    TYPE ty_exp_dtls,
      END OF ty_TxnDocData,

      BEGIN OF ty_FullData,
        HeaderData TYPE ty_TxnHdr,
        docData    TYPE ty_TxnDocData,
      END OF ty_FullData,

      BEGIN OF name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF name_mapping .

    TYPES: BEGIN OF ty_system_response,
             ackno          TYPE string,
             ackdt          TYPE string,
             irn            TYPE string,
             signed_invoice TYPE string,
             signed_qr_code TYPE string,
             status         TYPE string,
             remarks        TYPE string,
           END OF ty_system_response.

    TYPES: BEGIN OF ty_header_data,
             txn_id               TYPE string,    " Transaction ID
             user_gstin           TYPE string,    " GSTIN of the user
             action               TYPE string,    " Action (e.g., Cancel)
             irn                  TYPE string,    " IRN (Invoice Reference Number)
             service_provider     TYPE string,    " Service provider (IRP)
             date                 TYPE string,    " Date
             number_of_line_items TYPE string, " Number of line items (?)
             get_gstin            TYPE string,    " GSTIN for the request
             doctype              TYPE string,    " Document Type (e.g., INV)
             docnum               TYPE string,    " Document Number
             docdate              TYPE string,    " Document Date
           END OF ty_header_data.
    TYPES: BEGIN OF ty_cancel_request,
             irn     TYPE string,   " IRN to be canceled
             cnl_rsn TYPE string,   " Cancellation Reason (1)
             cnl_rem TYPE string,   " Cancellation Remarks (Data wrong entry)
           END OF ty_cancel_request.

    TYPES: BEGIN OF ty_cancel_doc_request,
             header_data    TYPE ty_header_data,    " Header Data
             cancel_request TYPE ty_cancel_request, " Cancel Request
           END OF ty_cancel_doc_request.

    TYPES: BEGIN OF ty_header_data1,
             txn_id           TYPE string,
             user_gstin       TYPE string,
             action           TYPE string,
             ewb_no           TYPE string,
             service_provider TYPE string,
             date             TYPE string,
           END OF ty_header_data1.

    TYPES: BEGIN OF ty_item_list,
             product_name   TYPE string,
             product_desc   TYPE string,
             hsn_code       TYPE string,
             quantity       TYPE p LENGTH 6 DECIMALS 3,
             qty_unit       TYPE string,
             taxable_amount TYPE p LENGTH 15 DECIMALS 2,
             sgst_rate      TYPE p LENGTH 15 DECIMALS 2,
             cgst_rate      TYPE p LENGTH 15 DECIMALS 2,
             igst_rate      TYPE p LENGTH 15 DECIMALS 2,
             cess_rate      TYPE p LENGTH 15 DECIMALS 2,
             iamt           TYPE p LENGTH 15 DECIMALS 2,
             camt           TYPE p LENGTH 15 DECIMALS 2,
             samt           TYPE p LENGTH 15 DECIMALS 2,
             csamt          TYPE p LENGTH 15 DECIMALS 2,
             rt             TYPE p LENGTH 15 DECIMALS 2,
           END OF ty_item_list.
    TYPES: BEGIN OF ty_doc_data,
             supply_type              TYPE string,
             sub_supply_type          TYPE string,
             sub_supply_desc          TYPE string,
             doc_type                 TYPE string,
             doc_no                   TYPE string,
             doc_date                 TYPE string,
             from_gstin               TYPE string,
             from_trd_name            TYPE string,
             from_addr1               TYPE string,
             from_addr2               TYPE string,
             from_place               TYPE string,
             from_pincode             TYPE string,
             act_from_state_cd        TYPE string,
             from_state_cd            TYPE string,
             to_gstin                 TYPE string,
             to_trd_name              TYPE string,
             to_addr1                 TYPE string,
             to_addr2                 TYPE string,
             to_place                 TYPE string,
             to_pincode               TYPE string,
             act_to_state_cd          TYPE string,
             to_state_cd              TYPE string,
             total_value              TYPE p LENGTH 15 DECIMALS 2,
             cgst_value               TYPE p LENGTH 15 DECIMALS 2,
             sgst_value               TYPE p LENGTH 15 DECIMALS 2,
             igst_value               TYPE p LENGTH 15 DECIMALS 2,
             cess_value               TYPE p LENGTH 15 DECIMALS 2,
*             tot_inv_value            TYPE p LENGTH 15 DECIMALS 2,
             trans_mode               TYPE string,
             trans_distance           TYPE string,
             transporter_name         TYPE string,
             transporter_id           TYPE string,
             trans_doc_no             TYPE string,
             trans_doc_date           TYPE string,
             vehicle_no               TYPE string,
             vehicle_type             TYPE string,
             item_list                TYPE STANDARD TABLE OF ty_item_list WITH EMPTY KEY,
             transaction_type         TYPE string,
             dispatch_from_gstin      TYPE string,
             dispatch_from_trade_name TYPE string,
             ship_to_gstin            TYPE string,
             ship_to_trade_name       TYPE string,
             other_value              TYPE p LENGTH 15 DECIMALS 2,
           END OF ty_doc_data.

    TYPES: BEGIN OF ty_header_data_eway,
             txn_id           TYPE string,
             user_gstin       TYPE string,
             action           TYPE string,
             ewb_no           TYPE string,
             service_provider TYPE string,
             date             TYPE string,
           END OF ty_header_data_eway.

    TYPES: BEGIN OF ty_cancel_request_eway,
             ewb_no          TYPE string,
             cancel_rsn_code TYPE string,
             cancel_rmrk     TYPE string,
           END OF ty_cancel_request_eway.

    TYPES: BEGIN OF ty_cancel_doc_request_eway,
             header_data    TYPE ty_header_data_eway,
             cancel_request TYPE ty_cancel_request_eway,
           END OF ty_cancel_doc_request_eway.

    DATA: ls_cancel_doc_request TYPE ty_cancel_doc_request_eway.
    DATA lv_roundof    TYPE p LENGTH 15 DECIMALS 2.

    TYPES: BEGIN OF ty_ewaybill,
             header_data TYPE ty_header_data1,
             doc_data    TYPE ty_doc_data,
           END OF ty_ewaybill.

    DATA: ls_ewaybill TYPE ty_ewaybill.
    DATA: lv_dis   TYPE dmbtr,
          lv_oth   TYPE dmbtr,
          lv_round TYPE dmbtr,
          lv_ass   TYPE dmbtr,
          lv_total TYPE dmbtr,
          lv_cgst  TYPE dmbtr,
          lv_sgst  TYPE dmbtr,
          lv_igst  TYPE dmbtr,
          lv_tot   TYPE dmbtr.

    DATA l_xml TYPE string.
    DATA: lv_cancel_doc_request TYPE ty_cancel_doc_request.

    DATA: lt_response TYPE TABLE OF ty_system_response,
          ls_response TYPE ty_system_response.

    DATA: lv_invoice    TYPE ty_FullData,
          lv_exchange   TYPE dmbtr,
          name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap,
          it_item       TYPE TABLE OF ty_Item,
          wa_item       TYPE ty_Item,
          it_eway_item  TYPE TABLE OF ty_item_list,
          wa_eway_item  TYPE ty_item_list.
    DATA: lv_url          TYPE string,
          l_zuser_id_pass TYPE zuser_id_pass,
          user_id         TYPE string,
          password        TYPE string,
          lv_xml          TYPE string,
          lo_http_client  TYPE REF TO if_web_http_client.
    DATA: lv_einv  TYPE zeinvoice_eway,
          it_einv  TYPE STANDARD TABLE OF zeinvoice_eway,
          lv_trans TYPE ztrans_detail,
          it_trans TYPE STANDARD TABLE OF ztrans_detail.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR _Invoice RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _Invoice RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ _Invoice RESULT result .

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK _Invoice.

    METHODS IRN_Cancel FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~IRN_Cancel RESULT result.

    METHODS IRN_Generate FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~IRN_Generate RESULT result.

    METHODS EWAY_Cancel FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~EWAY_Cancel RESULT result.

    METHODS EWAY_Generate FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~EWAY_Generate RESULT result.

    METHODS Trans_Detail FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~Trans_Detail RESULT result.

    METHODS GetDefaultsForTrans_Detail FOR READ
      IMPORTING keys FOR FUNCTION _Invoice~GetDefaultsForTrans_Detail RESULT result.
    METHODS GET_EWAY_Bill FOR MODIFY
      IMPORTING keys FOR ACTION _Invoice~GET_EWAY_Bill RESULT result.

ENDCLASS.

CLASS lhc__Invoice IMPLEMENTATION.

  METHOD get_instance_features.
    MOVE-CORRESPONDING keys TO result.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
     ENTITY _Invoice
     FIELDS ( status ) WITH CORRESPONDING #( keys ) RESULT DATA(lt_data) FAILED failed.
    result = VALUE #(  FOR ls_data IN lt_data

*           """""""""""""""""""""""""""""""""""""""""""""""""""""""" let ls_final = VALUE #( result[ %tky = ls_data-%tky ] OPTIONAL )
           (  %action-IRN_Generate = COND #( WHEN ls_data-status EQ '1' OR ls_data-status EQ '2'
                                              OR ls_data-doc_type EQ 'JSN' OR ls_data-doc_type EQ 'JSP'
                                            THEN if_abap_behv=>fc-o-disabled
                                            ELSE if_abap_behv=>fc-o-enabled )
              %action-IRN_Cancel = COND #( WHEN ls_data-status EQ '' OR ls_data-status EQ '2' OR ls_data-eway_status = '1' THEN if_abap_behv=>fc-o-disabled
                                            ELSE if_abap_behv=>fc-o-enabled )
              %action-EWAY_Generate = COND #( WHEN (  ls_data-eway_status = '1' OR ls_data-status EQ ''
                                              OR ls_data-status EQ '2' OR ls_data-eway_status = ' '
                                               OR ls_data-eway_status = '2' ) AND ( ls_data-trans_status = ' ' OR ls_data-eway_status = '1' )
                                             THEN if_abap_behv=>fc-o-disabled
                                        ELSE if_abap_behv=>fc-o-enabled )
              %action-Trans_Detail = COND #( WHEN  ( ls_data-status EQ '' OR ls_data-status EQ '2' "OR ls_data-eway_status = '1'
                                               ) AND  ls_data-doc_type NE 'JSN' AND   "OR ls_data-eway_status = '2'
                                                ls_data-doc_type NE 'JSP'
                                             THEN if_abap_behv=>fc-o-disabled
                                        ELSE if_abap_behv=>fc-o-enabled )
              %action-EWAY_Cancel = COND #( WHEN ls_data-eway_status = ' ' OR ls_data-eway_status = '2' THEN if_abap_behv=>fc-o-disabled
                                ELSE if_abap_behv=>fc-o-enabled )
              %action-GET_EWAY_Bill = COND #( WHEN ls_data-eway_status = ' ' OR ls_data-eway_status = '1' THEN if_abap_behv=>fc-o-disabled
                                ELSE if_abap_behv=>fc-o-enabled )
              %tky = ls_data-%tky
               )
    ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
    MOVE-CORRESPONDING keys TO result.
    SELECT * FROM zfi_i_einvoice_ewaybill FOR ALL ENTRIES IN @keys
   WHERE invoice = @keys-invoice INTO TABLE @DATA(lt_result).
    LOOP AT result[] ASSIGNING FIELD-SYMBOL(<fs_result>).
      READ TABLE lt_result INTO DATA(wa_result) WITH KEY invoice = <fs_result>-invoice.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_result TO <fs_result>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD IRN_Cancel.

    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
ENTITY _Invoice
ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata)
FAILED DATA(lv_failed)
REPORTED DATA(lv_REPORTED) .
    IF lt_key IS NOT INITIAL.
      SELECT invoice,FiscalYear,CompanyCode,doc_type,com_gst,billing_date,Odn_numer,status,irnnumber FROM zfi_i_einvoice_ewaybill
               FOR ALL ENTRIES IN @lt_key
               WHERE invoice = @lt_key-invoice
               INTO TABLE @DATA(lt_Inv_can).
    ENDIF.
    DATA(lv_irn_can_reason) = lt_key[ 1 ]-%param-irn_cancel.
    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE lt_inv_can INTO DATA(wa_inv_can) WITH KEY invoice = <fs_keys>-invoice status = '1'.
      IF sy-subrc = 0.
        lv_cancel_doc_request-header_data-txn_id = |{ wa_inv_can-invoice }{ wa_inv_can-FiscalYear }{ wa_inv_can-CompanyCode }| .
        lv_cancel_doc_request-header_data-user_gstin = wa_inv_can-com_gst." '37ABLPK6554F002'.
        lv_cancel_doc_request-header_data-action = 'Cancel'.
        lv_cancel_doc_request-header_data-irn = wa_inv_can-irnnumber.
        lv_cancel_doc_request-header_data-service_provider = ' '.
        REPLACE ALL OCCURRENCES OF '.'  IN wa_inv_can-billing_date WITH  '/' .
        lv_cancel_doc_request-header_data-date = wa_inv_can-billing_date.
        DATA(lines) = lines( lt_Inv_can ).
        lv_cancel_doc_request-header_data-number_of_line_items = lines.
        lv_cancel_doc_request-header_data-get_gstin = wa_inv_can-com_gst."'37ABLPK6554F002'.
*        lv_cancel_doc_request-header_data-doctype = 'INV'.
        IF wa_inv_can-doc_type = 'G2' OR wa_inv_can-doc_type = 'CBRE'.
          lv_cancel_doc_request-header_data-doctype = |CRN|.
        ELSEIF wa_inv_can-doc_type = 'L2'.
          lv_cancel_doc_request-header_data-doctype = |DBN|.
        ELSE.
          lv_cancel_doc_request-header_data-doctype = |INV|.
        ENDIF.
        lv_cancel_doc_request-header_data-docnum = wa_inv_can-Odn_numer.
        lv_cancel_doc_request-header_data-docdate = wa_inv_can-billing_date.
        lv_cancel_doc_request-cancel_request-irn = wa_inv_can-irnnumber.
        lv_cancel_doc_request-cancel_request-cnl_rsn = '1'.   " Reason for cancellation
        lv_cancel_doc_request-cancel_request-cnl_rem = lv_irn_can_reason. "'Data wrong entry'.   " Remarks

        CALL TRANSFORMATION id SOURCE data = lv_cancel_doc_request  RESULT XML l_xml  .
        CONDENSE l_xml NO-GAPS.

        DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
        | xmlns:ns="http://www.sap.com/eDocument/India/eInvoice/schema/xsd/1.0">| &&
        |<soapenv:Header/>| &&
        |<soapenv:Body>| && |{ l_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.

        REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:cancelDocRequest>' .
        REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:cancelDocRequest>' .
        REPLACE ALL OCCURRENCES OF '</asx:values>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF '</asx:abap>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF   '<HEADER_DATA>' IN lv_xml_file WITH '<ns:HeaderData>' .
        REPLACE ALL OCCURRENCES OF   '</HEADER_DATA>' IN lv_xml_file WITH '</ns:HeaderData>' .
        REPLACE ALL OCCURRENCES OF   '<TXN_ID>' IN lv_xml_file WITH '<ns:txnId>' .
        REPLACE ALL OCCURRENCES OF   '</TXN_ID>' IN lv_xml_file WITH '</ns:txnId>' .
        REPLACE ALL OCCURRENCES OF   '<USER_GSTIN>' IN lv_xml_file WITH '<ns:userGstin>' .
        REPLACE ALL OCCURRENCES OF   '</USER_GSTIN>' IN lv_xml_file WITH '</ns:userGstin>' .
        REPLACE ALL OCCURRENCES OF   'ACTION' IN lv_xml_file WITH 'ns:action' .
        REPLACE ALL OCCURRENCES OF   'IRN' IN lv_xml_file WITH 'ns:Irn' .
        REPLACE ALL OCCURRENCES OF   'SERVICE_PROVIDER' IN lv_xml_file WITH 'ns:serviceProvider' .
        REPLACE ALL OCCURRENCES OF   '<DATE>' IN lv_xml_file WITH '<ns:date>' .
        REPLACE ALL OCCURRENCES OF   '</DATE>' IN lv_xml_file WITH '</ns:date>' .
        REPLACE ALL OCCURRENCES OF   'NUMBER_OF_LINE_ITEMS' IN lv_xml_file WITH 'ns:numberOfLineItems' .
        REPLACE ALL OCCURRENCES OF   'GET_GSTIN' IN lv_xml_file WITH 'ns:getGSTIN' .
        REPLACE ALL OCCURRENCES OF   'DOCTYPE' IN lv_xml_file WITH 'ns:doctype' .
        REPLACE ALL OCCURRENCES OF   'DOCNUM' IN lv_xml_file WITH 'ns:docnum' .
        REPLACE ALL OCCURRENCES OF   '<DOCDATE>' IN lv_xml_file WITH '<ns:docdate>' .
        REPLACE ALL OCCURRENCES OF   '</DOCDATE>' IN lv_xml_file WITH '</ns:docdate>' .
        REPLACE ALL OCCURRENCES OF '<CANCEL_REQUEST><ns:Irn>' IN lv_xml_file WITH '<ns:cancelRequest><ns:irn>'.
        REPLACE ALL OCCURRENCES OF '</ns:Irn><CNL_RSN>' IN lv_xml_file WITH '</ns:irn><ns:CnlRsn>'.
        REPLACE ALL OCCURRENCES OF   'CANCEL_REQUEST' IN lv_xml_file WITH 'ns:cancelRequest' .
        REPLACE ALL OCCURRENCES OF   'CNL_RSN' IN lv_xml_file WITH 'ns:CnlRsn' .
        REPLACE ALL OCCURRENCES OF   'CNL_REM' IN lv_xml_file WITH 'ns:CnlRem' .
        CLEAR:lv_url,user_id,password, l_zuser_id_pass.
        SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'EINVOICE' INTO @l_zuser_id_pass.
        IF sy-subrc = 0.
          lv_url = l_zuser_id_pass-url.
          user_id = l_zuser_id_pass-user_id.
          password = l_zuser_id_pass-password.
        ENDIF.


        TRY.
            lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url )  ).
          CATCH cx_web_http_client_error cx_http_dest_provider_error INTO DATA(lv_handle).
            DATA(lv_string) = lv_handle->get_text( ).
            "handle exception
        ENDTRY.
        DATA(lo_request) = lo_http_client->get_http_request(  ).
        lo_request->set_content_type( content_type = 'text/xml' ).


        lo_request->set_text(
          EXPORTING
            i_text   = lv_xml_file
        ).
        lo_request->set_authorization_basic(
     EXPORTING
       i_username = user_id  "'7eeaf493-0b69-4549-8eab-347d81c117a3'
       i_password = password  "'09ec775d-412e-444c-b0ae-a00867d311f2'
     RECEIVING
       r_value    = DATA(l_value)
   ).
        TRY.
            DATA(lv_response) = lo_http_client->execute(
                                  i_method  =  if_web_http_client=>post ).
          CATCH cx_web_http_client_error cx_web_message_error INTO lv_handle.
            lv_string = lv_handle->get_text( ).
            "handle exception
        ENDTRY.
        lv_response->get_text(
          RECEIVING
            r_value = DATA(t_value)
        ).
        IF t_value NS 'error'.
          FIND REGEX '<ns:CancelDate>([^<]+)</ns:CancelDate>' IN t_value SUBMATCHES DATA(lv_CancelDate).
          IF lv_CancelDate IS NOT INITIAL.
            UPDATE zeinvoice_eway SET status = '2', irn_cancel_dt = @lv_CancelDate
             WHERE invoice = @wa_inv_can-invoice AND fiscalyear = @wa_inv_can-FiscalYear.
          ENDIF.
        ELSE.
          FIND REGEX '<ns:ErrorCode>([^<]+)</ns:ErrorCode>' IN t_value SUBMATCHES DATA(lv_ErrorCode).
          FIND REGEX '<ns:ErrorMessage>([^<]+)</ns:ErrorMessage>' IN t_value SUBMATCHES DATA(lv_ErrorMessage).
          IF lv_ErrorMessage IS NOT INITIAL.
            UPDATE zeinvoice_eway SET status = '1', error_code = @lv_ErrorCode, error_msg = @lv_ErrorMessage
            WHERE invoice = @wa_inv_can-invoice AND fiscalyear = @wa_inv_can-FiscalYear.
          ENDIF.
        ENDIF.
        IF lv_CancelDate IS NOT INITIAL.
          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                          %cid = <fs_keys>-%cid_ref
                     %msg                 = new_message(
                                              id       = 'ZEINVOICE'
                                              number   = '005'
                                              severity = if_abap_behv_message=>severity-success
                                              v1       = lv_CancelDate )
                                            ) TO reported-_invoice.
        ELSE.
          APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                       %state_area = 'IRN Cancled Error'
                                        %msg      = new_message(
                                         id       = 'ZEINVOICE'
                                         number   = '002'
                                         severity = if_abap_behv_message=>severity-error
                                         v1       = lv_ErrorMessage )
                                  %element-invoice = if_abap_behv=>mk-on
                                       ) TO reported-_invoice.
        ENDIF.
      ENDIF..
    ENDLOOP..
    result = VALUE #( FOR invdata IN invoicedata
         ( %tky = invdata-%tky %param = invdata ) ).
  ENDMETHOD.

  METHOD IRN_Generate.

    DATA(lt_key) = keys.

    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
    ENTITY _Invoice
    ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata)
    FAILED DATA(lv_failed)
    REPORTED DATA(lv_REPORTED) .
    IF lt_key IS NOT INITIAL.
      SELECT * FROM zfi_i_einvoice_ewaybill FOR ALL ENTRIES IN @lt_key
      WHERE invoice = @lt_key-invoice INTO TABLE @DATA(lt_Invoice).
    ENDIF.
    SELECT 'I' AS sign,'EQ' AS option, ConditionType AS low, ConditionType AS high  FROM I_SlsPricingConditionType
    WHERE ConditionClass = 'A' AND ConditionCategory = 'F' INTO TABLE @DATA(lt_other).
    SELECT 'I' AS sign,'EQ' AS option, ConditionType AS low, ConditionType AS high  FROM I_SlsPricingConditionType
    WHERE ConditionClass = 'A'
    AND ConditionCategory IN ( ' ','E' )  AND ConditionCalculationType IN ( 'A','B','C ' ) INTO TABLE @DATA(lt_discount).
    DATA: r_invocie TYPE RANGE OF vbeln.
    DATA: r_product TYPE RANGE OF matnr.
    DATA: r_plant TYPE RANGE OF werks_d.
    DATA: r_meins TYPE RANGE OF meins.
    r_invocie = VALUE #( FOR wa IN lt_invoice (
                         sign = 'I'
                         option = 'EQ'
                         low = wa-invoice
                         ) ).

    IF lt_invoice IS NOT INITIAL.
      SELECT BillingDocument,SDPricingProcedure
             FROM I_BillingDocument
             WHERE BillingDocument IN @r_invocie
             INTO TABLE @DATA(lt_billing).
      SELECT  BillingDocument,BillingDocumentItem,Product,plant,
             BillingDocumentItemText,BillingQuantity,BillingQuantityUnit,PriceDetnExchangeRate
             FROM I_BillingDocumentItem
             WHERE BillingDocument IN @r_invocie
               INTO TABLE @DATA(lt_billing_Item).

    ENDIF.
    IF lt_billing_Item IS NOT INITIAL.
      SELECT BillingDocument,BillingDocumentItem,
             PricingProcedureStep,PricingProcedureCounter,ConditionType,ConditionRateValue,
             ConditionAmount
               FROM I_BillingDocumentItemPrcgElmnt
               WHERE BillingDocument IN @r_invocie
               INTO TABLE @DATA(lt_price)               .
    ENDIF.
    CLEAR:r_product,r_plant,r_meins.
    r_product = VALUE #( FOR wa1 IN lt_billing_Item (
                     sign = 'I'
                     option = 'EQ'
                     low = wa1-Product
                     ) ).
    r_plant = VALUE #( FOR wa2 IN lt_billing_Item (
             sign = 'I'
             option = 'EQ'
             low = wa2-Plant
             ) ).
    r_meins = VALUE #( FOR wa2 IN lt_billing_Item (
         sign = 'I'
         option = 'EQ'
         low = wa2-BillingQuantityUnit
         ) ).
    IF lt_billing_Item IS NOT INITIAL.
      SELECT FROM I_ProductPlantBasic FIELDS Product,Plant,ConsumptionTaxCtrlCode
                WHERE Product IN @r_product
               AND Plant IN @r_plant
               INTO TABLE @DATA(lt_HSN). .

      SELECT * FROM ZI_EinvoiceUnits
               WHERE Units IN @r_meins
               INTO TABLE @DATA(t_unit).
    ENDIF.

    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE lt_Invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WITH KEY invoice = <fs_keys>-invoice.
      IF <fs_invoice>-irnnumber IS INITIAL AND <fs_invoice>-acc_doc_number IS ASSIGNED .
        lv_invoice-headerdata-txnid = |{ <fs_invoice>-invoice }{ <fs_invoice>-FiscalYear }{ <fs_invoice>-CompanyCode }| .
        lv_invoice-headerdata-usergstin = <fs_invoice>-com_gst. ".|37ABLPK6554F002|."
        lv_invoice-headerdata-action = |Invoice|.
        DATA(lv_invoice_num) = <fs_invoice>-Odn_numer."|{ <fs_invoice>-invoice ALPHA = OUT }|.
        lv_invoice-headerdata-docnum = lv_invoice_num."<fs_invoice>-invoice.
        lv_invoice-headerdata-docdate = <fs_invoice>-billing_date.
        REPLACE ALL OCCURRENCES OF '.'  IN lv_invoice-headerdata-docdate WITH  '/' .
        lv_invoice-docdata-version = |1.01|.

        """""""""""""Transactional Details
        READ TABLE lt_billing INTO DATA(wa_billing) WITH KEY BillingDocument = <fs_invoice>-invoice.
        IF wa_billing-SDPricingProcedure = 'ZIGNA2'.
          lv_invoice-docdata-trandtls-suptyp = |EXPWOP|.
        ELSE.
          lv_invoice-docdata-trandtls-suptyp = |B2B|.
        ENDIF.
        lv_invoice-docdata-trandtls-taxsch = |GST|.
        lv_invoice-docdata-trandtls-regrev = |N|.
*      lv_invoice-docdata-trandtls-ecmgstin = | |.

        """""""""""""Document Details
        IF <fs_invoice>-doc_type = 'G2' OR <fs_invoice>-doc_type = 'CBRE'.
          lv_invoice-docdata-docdtls-typ = |CRN|.
        ELSEIF <fs_invoice>-doc_type = 'L2'.
          lv_invoice-docdata-docdtls-typ = |DBN|.
        ELSE.
          lv_invoice-docdata-docdtls-typ = |INV|.
        ENDIF.
        lv_invoice-docdata-docdtls-no = lv_invoice_num. "<fs_invoice>-invoice.
        lv_invoice-docdata-docdtls-dt = lv_invoice-headerdata-docdate.



        """""""""""""Seller Details
        lv_invoice-docdata-sellerdtls-gstin =  <fs_invoice>-com_gst. "|37ABLPK6554F002|. "
        lv_invoice-docdata-sellerdtls-lglnm = <fs_invoice>-CompanyCodeName.
        lv_invoice-docdata-sellerdtls-addr1 = |Phagwara Hoshiarpur Rd|.
        lv_invoice-docdata-sellerdtls-loc = |Mehtiana|.
        lv_invoice-docdata-sellerdtls-pin = |146001|."|521456|. "
        lv_invoice-docdata-sellerdtls-stcd = lv_invoice-docdata-sellerdtls-gstin+0(2).
        """""""""""""Buyer Details
        IF lv_invoice-docdata-trandtls-suptyp = |B2B|.
          lv_invoice-docdata-buyerdtls-gstin = <fs_invoice>-cust_gst.
          lv_invoice-docdata-buyerdtls-lglnm = <fs_invoice>-customername.
          lv_invoice-docdata-buyerdtls-addr1 = <fs_invoice>-StreetName.
          lv_invoice-docdata-buyerdtls-loc = <fs_invoice>-CityName.
          lv_invoice-docdata-buyerdtls-pin = <fs_invoice>-PostalCode.
          IF lv_invoice-docdata-buyerdtls-gstin IS NOT INITIAL.
            lv_invoice-docdata-buyerdtls-stcd = <fs_invoice>-cust_gst+0(2).
          ENDIF.
        ELSE.
          lv_invoice-docdata-buyerdtls-gstin = |URP|.
          lv_invoice-docdata-buyerdtls-lglnm = <fs_invoice>-customername.
          lv_invoice-docdata-buyerdtls-addr1 = <fs_invoice>-StreetName.
          lv_invoice-docdata-buyerdtls-loc = <fs_invoice>-CityName.
          lv_invoice-docdata-buyerdtls-pin = |999999|.
          lv_invoice-docdata-buyerdtls-stcd = |96|.
*          lv_invoice-docdata-expdtls-forcur = <fs_invoice>-trans_currency.
*          lv_invoice-docdata-expdtls-cntcode = |AE|.
        ENDIF.

        """""""""""""Item Details
        CLEAR: lv_dis,lv_tot,lv_round.
        CLEAR: lv_sgst,lv_cgst,lv_igst,lv_total,lv_ass.
        LOOP AT lt_billing_Item INTO DATA(wa_billing_Item) WHERE BillingDocument = <fs_invoice>-invoice.
          wa_item-slno = wa_billing_item-BillingDocumentItem.
          wa_item-prddesc = wa_billing_item-BillingDocumentItemText.
          wa_item-qty = wa_billing_item-BillingQuantity.
          READ TABLE t_unit INTO DATA(w_unit) WITH KEY Units = wa_billing_item-BillingQuantityUnit.
          IF sy-subrc = 0.
            wa_item-unit = w_unit-Text_000.
          ENDIF.
          READ TABLE lt_HSN INTO DATA(wa_HSN) WITH KEY Product = wa_billing_Item-Product Plant = wa_billing_Item-Plant.
          IF sy-subrc = 0.
            wa_item-hsncd = wa_hsn-ConsumptionTaxCtrlCode.
          ENDIF.
          READ TABLE lt_price INTO DATA(wa_price) WITH KEY BillingDocument = <fs_invoice>-invoice
            BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'PPR0'.
          IF sy-subrc = 0.
            wa_item-unitprice = wa_price-ConditionRateValue.
            IF lv_invoice-docdata-trandtls-suptyp = |B2B|.
              wa_item-totamt =  wa_price-ConditionAmount.
            ELSE.
              CLEAR lv_exchange.
              lv_exchange = wa_billing_Item-PriceDetnExchangeRate * 100.
              wa_item-totamt =  wa_price-ConditionAmount * lv_exchange.
            ENDIF.
          ENDIF.
          CLEAR wa_price.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
            BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOCG'.
          IF sy-subrc = 0.
            wa_item-gstrt =  wa_item-gstrt + wa_price-ConditionRateValue.
            wa_item-CgstAmt = wa_price-ConditionAmount.
            lv_cgst = lv_cgst + wa_item-CgstAmt.
          ENDIF.
          CLEAR wa_price.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
            BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOSG'.
          IF sy-subrc = 0.
            wa_item-gstrt =  wa_item-gstrt + wa_price-ConditionRateValue.
            wa_item-SgstAmt = wa_price-ConditionAmount.
            lv_sgst = lv_sgst + wa_item-sgstAmt.
          ENDIF.
          CLEAR wa_price.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
            BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOIG'.
          IF sy-subrc = 0.
            wa_item-gstrt =   wa_item-gstrt + wa_price-ConditionRateValue.
            IF lv_invoice-docdata-trandtls-suptyp = |B2B|.
              wa_item-IgstAmt =  wa_price-ConditionAmount.
            ELSE.
              CLEAR lv_exchange.
              lv_exchange = wa_billing_Item-PriceDetnExchangeRate * 100.
              wa_item-IgstAmt =  wa_price-ConditionAmount * lv_exchange.
            ENDIF.
            lv_igst = lv_igst + wa_item-IgstAmt.
          ENDIF.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
                      BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'DRD1'.
          IF sy-subrc = 0.
            lv_roundof =    wa_price-ConditionAmount.
          ENDIF.

          wa_item-discount = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                                 WHERE ( (  ConditionType IN lt_discount ) AND BillingDocument = <fs_invoice>-invoice
                                 AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                                NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .
          wa_item-othchrg = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                       WHERE ( (  ConditionType IN lt_other ) AND BillingDocument = <fs_invoice>-invoice
                       AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                      NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .
          CLEAR lv_round.
          lv_round = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                    WHERE ( (  ConditionType EQ 'DRD1' ) AND BillingDocument = <fs_invoice>-invoice
                     AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                       NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .

          wa_item-totamt = wa_item-totamt + wa_item-othchrg.
          wa_item-assamt = wa_item-totamt + wa_item-discount.
          lv_ass = lv_ass + wa_item-assamt .
          IF wa_item-discount < 0.
            wa_item-discount = wa_item-discount * -1.
          ENDIF.
          lv_dis = lv_dis + wa_item-discount.
          wa_item-othchrg = ' '.
*           wa_item-assamt = wa_item-assamt + wa_item-othchrg.
          wa_item-totitemval = wa_item-assamt + wa_item-CgstAmt + wa_item-SgstAmt +
                               wa_item-IgstAmt + lv_round .
          lv_tot = lv_tot + wa_item-totitemval.
          APPEND wa_item TO it_item.
          CLEAR wa_item.
        ENDLOOP.
        """""""""""""Value Details
        lv_invoice-docdata-valdtls-igstval = lv_igst.
        lv_invoice-docdata-valdtls-cgstval = lv_cgst.
        lv_invoice-docdata-valdtls-sgstval = lv_sgst.
*        lv_invoice-docdata-valdtls-discount = lv_dis.
        lv_invoice-docdata-valdtls-othchrg =  ' '.
        lv_invoice-docdata-valdtls-assval   =  lv_ass.
        lv_invoice-docdata-valdtls-totinvval = lv_tot ."- lv_dis.
        lv_invoice-docdata-itemlist = it_item[].
        CLEAR it_item.

        READ TABLE lt_billing INTO wa_billing WITH KEY BillingDocument = <fs_invoice>-invoice.
        IF wa_billing-SDPricingProcedure = 'ZIGNA2'.
          IF lv_invoice-docdata-valdtls-igstval IS INITIAL.
            lv_invoice-docdata-trandtls-suptyp = |EXPWOP|.
          ELSE.
            lv_invoice-docdata-trandtls-suptyp = |EXPWP|.
          ENDIF.
          lv_invoice-docdata-expdtls-forcur = |INR|."<fs_invoice>-trans_currency.
          lv_invoice-docdata-expdtls-cntcode = |AE|.
        ELSE.
          lv_invoice-docdata-trandtls-suptyp = |B2B|.
        ENDIF.

        CALL TRANSFORMATION id SOURCE data = lv_invoice  RESULT XML lv_xml  .
        CONDENSE lv_xml NO-GAPS.



        DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
        | xmlns:tns="http://www.sap.com/GS/eDocument/India/eInvoice">| &&
        |<soapenv:Header/>| &&
        |<soapenv:Body>| && |{ lv_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.

        REPLACE ALL OCCURRENCES OF  'SLNO' IN lv_xml_file WITH 'SlNo' .
        REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<tns:submitDocumentRequest>' .
        REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</tns:submitDocumentRequest>' .
        REPLACE ALL OCCURRENCES OF '</asx:values>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF '</asx:abap>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF   'HEADERDATA' IN lv_xml_file WITH 'HeaderData' .
        REPLACE ALL OCCURRENCES OF   'DOCDATA' IN lv_xml_file WITH 'docData' .
        REPLACE ALL OCCURRENCES OF   'TRANDTLS' IN lv_xml_file WITH 'TranDtls' .
        REPLACE ALL OCCURRENCES OF   'DOCDTLS' IN lv_xml_file WITH 'DocDtls' .
        REPLACE ALL OCCURRENCES OF   'SELLERDTLS' IN lv_xml_file WITH 'SellerDtls' .
        REPLACE ALL OCCURRENCES OF   'BUYERDTLS' IN lv_xml_file WITH 'BuyerDtls' .
        REPLACE ALL OCCURRENCES OF   'VALDTLS' IN lv_xml_file WITH 'ValDtls' .
        REPLACE ALL OCCURRENCES OF   'ITEMLIST' IN lv_xml_file WITH 'ItemList' .
        REPLACE ALL OCCURRENCES OF   'item' IN lv_xml_file WITH 'Item' .
        REPLACE ALL OCCURRENCES OF 'EWBDTLS' IN lv_xml_file WITH 'EwbDtls' .
        REPLACE ALL OCCURRENCES OF   'TXNID' IN lv_xml_file WITH 'txnId' .
        REPLACE ALL OCCURRENCES OF  'USERGSTIN' IN lv_xml_file WITH 'userGstin' .
        REPLACE ALL OCCURRENCES OF 'ACTION' IN lv_xml_file WITH 'action' .
        REPLACE ALL OCCURRENCES OF  'DOCNUM' IN lv_xml_file WITH 'docnum' .
        REPLACE ALL OCCURRENCES OF  'DOCDATE' IN lv_xml_file WITH 'docdate' .
        REPLACE ALL OCCURRENCES OF  'VERSION' IN lv_xml_file WITH 'Version' .
        REPLACE ALL OCCURRENCES OF  'TAXSCH' IN lv_xml_file WITH 'TaxSch' .
        REPLACE ALL OCCURRENCES OF  'SUPTYP' IN lv_xml_file WITH 'SupTyp' .
        REPLACE ALL OCCURRENCES OF   'REGREV' IN lv_xml_file WITH 'RegRev' .
        REPLACE ALL OCCURRENCES OF  'ECMGSTIN' IN lv_xml_file WITH 'EcmGstin' .
        REPLACE ALL OCCURRENCES OF  'TYP' IN lv_xml_file WITH 'Typ' .
        REPLACE ALL OCCURRENCES OF  'NO' IN lv_xml_file WITH 'No' .
        REPLACE ALL OCCURRENCES OF  'DT' IN lv_xml_file WITH 'Dt' .
        REPLACE ALL OCCURRENCES OF  'GSTIN' IN lv_xml_file WITH 'Gstin' .
        REPLACE ALL OCCURRENCES OF  'LGLNM' IN lv_xml_file WITH 'LglNm' .
        REPLACE ALL OCCURRENCES OF  'ADDR1' IN lv_xml_file WITH 'Addr1' .
        REPLACE ALL OCCURRENCES OF  'LOC' IN lv_xml_file WITH 'Loc' .
        REPLACE ALL OCCURRENCES OF  'PIN' IN lv_xml_file WITH 'Pin' .
        REPLACE ALL OCCURRENCES OF  'STCD' IN lv_xml_file WITH 'Stcd' .
        REPLACE ALL OCCURRENCES OF  'ASSVAL' IN lv_xml_file WITH 'AssVal' .
        REPLACE ALL OCCURRENCES OF  'IGSTVAL' IN lv_xml_file WITH 'IgstVal' .
        REPLACE ALL OCCURRENCES OF  'CGSTVAL' IN lv_xml_file WITH 'CgstVal' .
        REPLACE ALL OCCURRENCES OF  'SGSTVAL' IN lv_xml_file WITH 'SgstVal' .
        REPLACE ALL OCCURRENCES OF  'TOTINVVAL' IN lv_xml_file WITH 'TotInvVal' .
        REPLACE ALL OCCURRENCES OF  'PRDDESC' IN lv_xml_file WITH 'PrdDesc' .
        REPLACE ALL OCCURRENCES OF  'HSNCD' IN lv_xml_file WITH 'HsnCd' .
        REPLACE ALL OCCURRENCES OF  'QTY' IN lv_xml_file WITH 'Qty' .
        REPLACE ALL OCCURRENCES OF  'UNIT' IN lv_xml_file WITH 'Unit' .
        REPLACE ALL OCCURRENCES OF  'UNITPRICE' IN lv_xml_file WITH 'UnitPrice' .
        REPLACE ALL OCCURRENCES OF 'UnitPRICE' IN lv_xml_file WITH 'UnitPrice' .
        REPLACE ALL OCCURRENCES OF  'TOTAMT' IN lv_xml_file WITH 'TotAmt' .
        REPLACE ALL OCCURRENCES OF  'GSTRT' IN lv_xml_file WITH 'GstRt' .
        REPLACE ALL OCCURRENCES OF  'IGSTAMT' IN lv_xml_file WITH 'IgstAmt' .
        REPLACE ALL OCCURRENCES OF  'SGSTAMT' IN lv_xml_file WITH 'SgstAmt' .
        REPLACE ALL OCCURRENCES OF  'CGSTAMT' IN lv_xml_file WITH 'CgstAmt' .
        REPLACE ALL OCCURRENCES OF  'ASSAMT' IN lv_xml_file WITH 'AssAmt' .
        REPLACE ALL OCCURRENCES OF  'TOTITEMVAL' IN lv_xml_file WITH 'TotItemVal' .
        REPLACE ALL OCCURRENCES OF  'TRANSID' IN lv_xml_file WITH 'TransId' .
        REPLACE ALL OCCURRENCES OF  'TRANSMODE' IN lv_xml_file WITH 'TransMode' .
        REPLACE ALL OCCURRENCES OF  'DISTANCE' IN lv_xml_file WITH 'Distance' .
        REPLACE ALL OCCURRENCES OF  'TRANSNAME' IN lv_xml_file WITH 'TransName' .
        REPLACE ALL OCCURRENCES OF  'TRANSDOCNO' IN lv_xml_file WITH 'TransDocNo' .
        REPLACE ALL OCCURRENCES OF  'TRANSDOCDT' IN lv_xml_file WITH 'TransDocDt' .
        REPLACE ALL OCCURRENCES OF  'VEHNO' IN lv_xml_file WITH 'VehNo' .
        REPLACE ALL OCCURRENCES OF  'RNDOFFAMT' IN lv_xml_file WITH 'RndOffAmt' .
        REPLACE ALL OCCURRENCES OF  'DISCOUNT' IN lv_xml_file WITH 'Discount' .
        REPLACE ALL OCCURRENCES OF  'OTHCHRG' IN lv_xml_file WITH 'OthChrg' .
        REPLACE ALL OCCURRENCES OF  'TOTINVVALFC' IN lv_xml_file WITH 'TotInvValFc' .
        REPLACE ALL OCCURRENCES OF  'FORCUR' IN lv_xml_file WITH 'ForCur' .
        REPLACE ALL OCCURRENCES OF  'CNTCODE' IN lv_xml_file WITH 'CntCode' .
        REPLACE ALL OCCURRENCES OF  'EXPDtLS' IN lv_xml_file WITH 'ExpDtls' .

        CLEAR:lv_url,user_id,password, l_zuser_id_pass.
        SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'EINVOICE' INTO @l_zuser_id_pass.
        IF sy-subrc = 0.
          lv_url = l_zuser_id_pass-url.
          user_id = l_zuser_id_pass-user_id.
          password = l_zuser_id_pass-password.
        ENDIF.

        TRY.
            lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url )  ).
          CATCH cx_web_http_client_error cx_http_dest_provider_error INTO DATA(lv_handle).
            DATA(lv_string) = lv_handle->get_text( ).
            "handle exception
        ENDTRY.
        DATA(lo_request) = lo_http_client->get_http_request(  ).
        lo_request->set_content_type( content_type = 'text/xml' ).

        lo_request->set_text(
          EXPORTING
            i_text   = lv_xml_file
        ).
*       CATCH cx_web_message_error.

        lo_request->set_authorization_basic(
     EXPORTING
       i_username = user_id  "'7eeaf493-0b69-4549-8eab-347d81c117a3'
       i_password = password  "'09ec775d-412e-444c-b0ae-a00867d311f2'
     RECEIVING
       r_value    = DATA(l_value)
   ).
        TRY.
            DATA(lv_response) = lo_http_client->execute(
                                  i_method  =  if_web_http_client=>post ).
          CATCH cx_web_http_client_error cx_web_message_error INTO lv_handle.
            lv_string = lv_handle->get_text( ).
            "handle exception
        ENDTRY.

        lv_response->get_text(
          RECEIVING
            r_value = DATA(t_value)
        ).

        IF t_value NS 'error'.
          CLEAR lv_einv.
          FIND REGEX '<ns:status>([^<]+)</ns:status>' IN t_value SUBMATCHES DATA(lv_status).
          FIND REGEX '<ns:AckNo>([^<]+)</ns:AckNo>' IN t_value SUBMATCHES DATA(lv_Ackno).
          FIND REGEX '<ns:AckDt>([^<]+)</ns:AckDt>' IN t_value SUBMATCHES DATA(lv_Ackdt).
          FIND REGEX '<ns:Irn>([^<]+)</ns:Irn>' IN t_value SUBMATCHES DATA(lv_irn).
          FIND REGEX '<ns:SignedInvoice>([^<]+)</ns:SignedInvoice>' IN t_value SUBMATCHES DATA(lv_SignedInvoice).
          FIND REGEX '<ns:SignedQRCode>([^<]+)</ns:SignedQRCode>' IN t_value SUBMATCHES DATA(lv_SignedQRCode).
          FIND REGEX '<ns:EwbNo>([^<]+)</ns:EwbNo>' IN t_value SUBMATCHES DATA(lv_EwbNo).
          FIND REGEX '<ns:EwbDt>([^<]+)</ns:EwbDt>' IN t_value SUBMATCHES DATA(lv_EwbDt).
          FIND REGEX '<ns:EwbValidTill>([^<]+)</ns:EwbValidTill>' IN t_value SUBMATCHES DATA(lv_EwbValidTill).
          FIND REGEX '<ns:Remarks>([^<]+)</ns:Remarks>' IN t_value SUBMATCHES DATA(lv_Remarks).
          IF lv_status IS NOT INITIAL.
            lv_einv-invoice = <fs_invoice>-invoice.
            lv_einv-fiscalyear = <fs_invoice>-FiscalYear.
            lv_einv-status = lv_status.
            lv_einv-ackno = lv_Ackno.
            lv_einv-ackdt = lv_Ackdt.
            lv_einv-ack_dt = |{ lv_Ackdt+0(4) }| && |{ lv_Ackdt+5(2) }| && |{ lv_Ackdt+8(2) }|.
            lv_einv-irn = lv_irn.
            lv_einv-signed_invoice = lv_SignedInvoice.
            lv_einv-signed_qr_code = lv_SignedQRCode.
            lv_einv-ewbno = lv_EwbNo.
            lv_einv-ewbdt = lv_EwbDt.
            lv_einv-ewbvalidtill = lv_EwbValidTill.
            lv_einv-remarks = lv_Remarks.
            lv_einv-irn_cancel_dt  = ' '.
            lv_einv-error_msg = ''.
            lv_einv-error_code = ''.
            APPEND lv_einv TO it_einv.
            MODIFY zeinvoice_eway FROM TABLE @it_einv.
          ENDIF.
        ELSE.
          CLEAR lv_einv.
          FIND REGEX '<ns:status>([^<]+)</ns:status>' IN t_value SUBMATCHES lv_status.
          FIND REGEX '<ns:ErrorCode>([^<]+)</ns:ErrorCode>' IN t_value SUBMATCHES DATA(lv_ErrorCode).
          FIND REGEX '<ns:ErrorMessage>([^<]+)</ns:ErrorMessage>' IN t_value SUBMATCHES DATA(lv_ErrorMessage).
          CONDENSE lv_ErrorMessage NO-GAPS.
          lv_einv-invoice = <fs_invoice>-invoice.
          DATA(l_inv) = <fs_invoice>-invoice.
          lv_einv-fiscalyear = <fs_invoice>-FiscalYear.
*          lv_einv-status = lv_status.
          lv_einv-error_msg = lv_ErrorMessage.
          lv_einv-error_code = lv_ErrorCode.
          DATA(l_len) = strlen( lv_ErrorMessage ).
          DATA(l_div) = l_len / 4.
          DATA(l_val) = lv_ErrorMessage+0(l_div).
          DATA(l_va2) = lv_ErrorMessage+l_div(l_div).
          DATA(l_val1) = l_div + l_div.
          DATA(l_va3) = lv_ErrorMessage+l_val1(l_div).
          DATA(l_val2) = l_val1 + l_div.
          DATA(l_val5) = l_val2 + l_div.
          IF l_val5 GT l_len.
            DATA(l_fin) = l_val5 - l_len.
            l_div = l_div - l_fin.
          ENDIF.
          DATA(l_va4) = lv_ErrorMessage+l_val2(l_div).
          APPEND lv_einv TO it_einv.
          MODIFY zeinvoice_eway FROM TABLE @it_einv.
        ENDIF.
      ENDIF.
      IF lv_irn IS NOT INITIAL.
*        APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

        APPEND VALUE #( %tky                 = <fs_keys>-%tky
                        %cid = <fs_keys>-%cid_ref
                   %msg                 = new_message(
                                            id       = 'ZEINVOICE'
                                            number   = '001'
                                            severity = if_abap_behv_message=>severity-success
                                            v1       = lv_irn )
                                          ) TO reported-_invoice.
      ELSE.
        APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.
        IF l_val IS INITIAL.
          l_val = 'Server Issue'.
        ENDIF.
        APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                     %state_area = 'IRN Generate Error'
                                      %msg      = new_message(
                                       id       = 'ZEINVOICE'
                                       number   = '002'
                                       severity = if_abap_behv_message=>severity-error
                                           v1       = l_val
                                           v2       = l_va2
                                           v3       = l_va3
                                           v4       = l_va4
                                       )
                                %element-invoice = if_abap_behv=>mk-on
                                     ) TO reported-_invoice.
      ENDIF.
      CLEAR:lv_status,lv_ErrorCode,lv_ErrorMessage.
      CLEAR: lv_einv,lv_Ackno,lv_Ackdt,lv_irn,lv_SignedInvoice,lv_SignedQRCode,lv_EwbNo,lv_EwbDt,lv_EwbValidTill,lv_Remarks.
      CLEAR it_einv.
    ENDLOOP.
*
    result = VALUE #( FOR invdata IN invoicedata
             ( %tky = invdata-%tky %param = invdata ) ).

  ENDMETHOD.


  METHOD EWAY_Cancel.
    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
       ENTITY _Invoice
       ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata) FAILED DATA(lv_failed) REPORTED DATA(lv_REPORTED) .
    DATA(lv_eway_can_reason) = lt_key[ 1 ]-%param-eway_CANCEL.
    SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'GST' INTO @l_zuser_id_pass.
    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE invoicedata INTO DATA(wa_invoicedata) WITH KEY invoice = <fs_keys>-invoice  eway_status = '1'.
      IF sy-subrc = 0.
        ls_cancel_doc_request-header_data-txn_id = |{ wa_invoicedata-invoice }{ wa_invoicedata-FiscalYear }{ wa_invoicedata-CompanyCode }| .
        IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
          ls_cancel_doc_request-header_data-user_gstin = l_zuser_id_pass-user_id.
        ELSE.
          ls_cancel_doc_request-header_data-user_gstin = wa_invoicedata-com_gst."|29AAACW2913K1Z5|.
        ENDIF.
        ls_cancel_doc_request-header_data-action = |CANEWB|.
        ls_cancel_doc_request-header_data-ewb_no = ||." wa_invoicedata-ewbno.
        ls_cancel_doc_request-header_data-service_provider = |API|.
        cl_abap_context_info=>get_system_date(
  RECEIVING
    rv_date = DATA(l_date)
).
        DATA(lv_date) = |{ l_date+6(2) }-| && |{ l_date+4(2) }-| && |{ l_date+0(4) }|.
        ls_cancel_doc_request-header_data-date = lv_date.
        ls_cancel_doc_request-cancel_request-ewb_no = wa_invoicedata-ewbno.
        ls_cancel_doc_request-cancel_request-cancel_rsn_code = |4|.
        ls_cancel_doc_request-cancel_request-cancel_rmrk = lv_eway_can_reason."|OVER LOAD|.

        CALL TRANSFORMATION id SOURCE data = ls_cancel_doc_request  RESULT XML l_xml  .
        CONDENSE l_xml NO-GAPS.

        DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
                              | xmlns:ns="http://www.sap.com/eDocument/India/eWayBill/schema/xsd/1.0">| &&
                              |<soapenv:Header/>| &&
                              |<soapenv:Body>| && |{ l_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.


        REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:cancelDocRequest>' .
        REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:cancelDocRequest>' .
        REPLACE ALL OCCURRENCES OF '</asx:values>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF '</asx:abap>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF   'HEADER_DATA' IN lv_xml_file WITH 'ns:HeaderData' .
        REPLACE ALL OCCURRENCES OF   'TXN_ID' IN lv_xml_file WITH 'ns:txnId' .
        REPLACE ALL OCCURRENCES OF   'USER_GSTIN' IN lv_xml_file WITH 'ns:userGstin' .
        REPLACE ALL OCCURRENCES OF   '<ACTION>' IN lv_xml_file WITH '<ns:action>' .
        REPLACE ALL OCCURRENCES OF   '</ACTION>' IN lv_xml_file WITH '</ns:action>' .
        REPLACE ALL OCCURRENCES OF   'EWB_NO' IN lv_xml_file WITH 'ns:ewbNo' .
        REPLACE ALL OCCURRENCES OF   'SERVICE_PROVIDER' IN lv_xml_file WITH 'ns:serviceProvider' .
        REPLACE ALL OCCURRENCES OF   '<DATE>' IN lv_xml_file WITH '<ns:date>' .
        REPLACE ALL OCCURRENCES OF   '</DATE>' IN lv_xml_file WITH '</ns:date>' .
        REPLACE ALL OCCURRENCES OF   'CANCEL_REQUEST' IN lv_xml_file WITH 'ns:cancelRequest' .
        REPLACE ALL OCCURRENCES OF   'CANCEL_RSN_CODE' IN lv_xml_file WITH 'ns:cancelRsnCode' .
        REPLACE ALL OCCURRENCES OF   'CANCEL_RMRK' IN lv_xml_file WITH 'ns:cancelRmrk' .

        CLEAR:lv_url,user_id,password, l_zuser_id_pass.
        SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'EWAYBILL' INTO @l_zuser_id_pass.
        IF sy-subrc = 0.
          lv_url = l_zuser_id_pass-url.
          user_id = l_zuser_id_pass-user_id.
          password = l_zuser_id_pass-password.
        ENDIF.

        TRY.
            lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url )  ).
          CATCH cx_web_http_client_error cx_http_dest_provider_error INTO DATA(lv_handle).
            DATA(lv_string) = lv_handle->get_text( ).
            "handle exception
        ENDTRY.
        DATA(lo_request) = lo_http_client->get_http_request(  ).
        lo_request->set_content_type( content_type = 'text/xml' ).

        lo_request->set_text(
          EXPORTING
            i_text   = lv_xml_file
        ).
*       CATCH cx_web_message_error.

        lo_request->set_authorization_basic(
     EXPORTING
       i_username = user_id  "'2e98993d-9e52-4e85-a7a4-38ab9e899cb8'
       i_password = password "'4ef845ba-9ab7-4686-9712-0aa19301d54e'
     RECEIVING
       r_value    = DATA(l_value)
   ).

        TRY.
            DATA(lv_response) = lo_http_client->execute(
                                  i_method  =  if_web_http_client=>post ).
          CATCH cx_web_http_client_error cx_web_message_error INTO lv_handle.
            lv_string = lv_handle->get_text( ).
            "handle exception
        ENDTRY.

        lv_response->get_text(
          RECEIVING
            r_value = DATA(t_value)
        ).
        IF t_value NS 'error'.
          FIND REGEX '<ns2:status>([^<]+)</ns2:status>' IN t_value SUBMATCHES DATA(lv_status).
          FIND REGEX '<ns2:cancelDate>([^<]+)</ns2:cancelDate>' IN t_value SUBMATCHES DATA(lv_eway_can).
          IF lv_status = '1'.
            UPDATE zeinvoice_eway SET ewb_cancel_dt = @lv_eway_can, eway_status = '2',ewb_error_msg = ' '
             WHERE invoice = @wa_invoicedata-invoice AND fiscalyear = @wa_invoicedata-FiscalYear AND ewbno = @wa_invoicedata-ewbno.
          ENDIF.
        ELSE.
          FIND REGEX '<ns2:errorCodes>([^<]+)</ns2:errorCodes>' IN t_value SUBMATCHES DATA(lv_eway_error).
          IF lv_eway_error IS NOT INITIAL.
            UPDATE zeinvoice_eway SET ewb_error_msg = @lv_eway_error
              WHERE invoice = @wa_invoicedata-invoice AND fiscalyear = @wa_invoicedata-FiscalYear AND ewbno = @wa_invoicedata-ewbno.
          ENDIF.
        ENDIF.
        IF lv_eway_can IS NOT INITIAL.
          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                          %cid = <fs_keys>-%cid_ref
                     %msg                 = new_message(
                                              id       = 'ZEINVOICE'
                                              number   = '004'
                                              severity = if_abap_behv_message=>severity-success
                                              v1       = lv_eway_can )
                                            ) TO reported-_invoice.
        ELSE.
          APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                       %state_area = 'E-WAY Bill Cancled Error'
                                        %msg      = new_message(
                                         id       = 'ZEINVOICE'
                                         number   = '002'
                                         severity = if_abap_behv_message=>severity-error
                                         v1       = lv_eway_error )
                                  %element-invoice = if_abap_behv=>mk-on
                                       ) TO reported-_invoice.
        ENDIF.
      ENDIF..
    ENDLOOP.
    result = VALUE #( FOR invdata IN invoicedata
             ( %tky = invdata-%tky %param = invdata ) ).
  ENDMETHOD.

  METHOD EWAY_Generate.
    DATA: r_invocie TYPE RANGE OF vbeln.
    DATA: r_product TYPE RANGE OF matnr.
    DATA: r_plant TYPE RANGE OF werks_d.
    DATA: r_meins TYPE RANGE OF meins.
    DATA: r_year TYPE RANGE OF gjahr.
    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
       ENTITY _Invoice
       ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata) FAILED DATA(lv_failed) REPORTED DATA(lv_REPORTED) .

    SELECT * FROM zfi_i_einvoice_ewaybill FOR ALL ENTRIES IN @lt_key WHERE invoice = @lt_key-invoice INTO TABLE @DATA(lt_Invoice).
    SELECT 'I' AS sign,'EQ' AS option, ConditionType AS low, ConditionType AS high  FROM I_SlsPricingConditionType
   WHERE ConditionClass = 'A' AND ConditionCategory = 'F' INTO TABLE @DATA(lt_other).
    SELECT 'I' AS sign,'EQ' AS option, ConditionType AS low, ConditionType AS high  FROM I_SlsPricingConditionType
    WHERE ConditionClass = 'A'
    AND ConditionCategory IN ( ' ','E' )  AND ConditionCalculationType IN ( 'A','B','C ' ) INTO TABLE @DATA(lt_discount).
    r_invocie = VALUE #( FOR wa3 IN lt_invoice (
                          sign = 'I'
                          option = 'EQ'
                          low = wa3-invoice
                          ) ).
    r_year = VALUE #( FOR wa5 IN lt_invoice (
                      sign = 'I'
                      option = 'EQ'
                      low = wa5-FiscalYear
                      ) ).
    IF lt_invoice IS NOT INITIAL.
      SELECT BillingDocument,BillingDocumentItem,Product,plant,
             BillingDocumentItemText,BillingQuantity,BillingQuantityUnit,PriceDetnExchangeRate
      FROM I_BillingDocumentItem
      WHERE BillingDocument IN @r_invocie
      INTO TABLE @DATA(lt_billing_Item).
      SELECT * FROM ztrans_detail
      WHERE invoice IN @r_invocie
      AND fiscalyear IN @r_year
               INTO TABLE @DATA(lt_trans).
      SELECT * FROM zi_customer_data
               WHERE BillingDocument IN @r_invocie INTO TABLE @DATA(lt_ship_to_party).
    ENDIF.
    IF lt_billing_Item IS NOT INITIAL.
      SELECT BillingDocument,BillingDocumentItem,PricingProcedureStep,
             PricingProcedureCounter,ConditionType,ConditionRateValue,
             ConditionAmount
       FROM I_BillingDocumentItemPrcgElmnt
       WHERE BillingDocument IN @r_invocie
        INTO TABLE @DATA(lt_price).
      r_product = VALUE #( FOR wa1 IN lt_billing_Item (
                     sign = 'I'
                     option = 'EQ'
                     low = wa1-Product
                     ) ).
      r_plant = VALUE #( FOR wa2 IN lt_billing_Item (
               sign = 'I'
               option = 'EQ'
               low = wa2-Plant
               ) ).
      r_meins = VALUE #( FOR wa2 IN lt_billing_Item (
                 sign = 'I'
                 option = 'EQ'
                 low = wa2-BillingQuantityUnit
     ) ).

      IF lt_billing_Item IS NOT INITIAL.
        SELECT Product,Plant,ConsumptionTaxCtrlCode FROM I_ProductPlantBasic
                 WHERE Product IN @r_product
                 AND Plant IN @r_plant
                 INTO TABLE @DATA(lt_HSN). .
        SELECT * FROM ZI_EinvoiceUnits
                 WHERE Units IN @r_meins
                 INTO TABLE @DATA(t_unit).
      ENDIF.
    ENDIF.
    CLEAR l_zuser_id_pass. """Below Sclect Query Eway Bill Company Code GST Number Purpose
    SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'GST' INTO @l_zuser_id_pass.
    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE lt_Invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WITH KEY invoice = <fs_keys>-invoice.
      IF sy-subrc = 0.
        READ TABLE lt_billing_Item INTO DATA(wa_bill) WITH KEY BillingDocument = <fs_invoice>-invoice.
        ls_ewaybill-header_data-txn_id = |{ <fs_invoice>-invoice }{ <fs_invoice>-FiscalYear }{ <fs_invoice>-CompanyCode }| .
        IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
          ls_ewaybill-header_data-user_gstin = l_zuser_id_pass-user_id.
        ELSE.
          ls_ewaybill-header_data-user_gstin = <fs_invoice>-com_gst. "|29AAACW2913K1Z5|."
        ENDIF.
        ls_ewaybill-header_data-action = |GENEWAYBILL|.
        ls_ewaybill-header_data-ewb_no = ''.
        ls_ewaybill-header_data-service_provider = |API|.
        cl_abap_context_info=>get_system_date(
          RECEIVING
            rv_date = DATA(l_date)
        ).
        DATA(lv_date) = |{ l_date+6(2) }-| && |{ l_date+4(2) }-| && |{ l_date+0(4) }|.
        ls_ewaybill-header_data-date =  lv_date.

        """""""""""""""""Document Data
        ls_ewaybill-doc_data-supply_type = |O|.

        ls_ewaybill-doc_data-sub_supply_desc = wa_bill-BillingDocumentItemText."|Trading material|.
        IF <fs_invoice>-doc_type = 'JSP' OR <fs_invoice>-doc_type = 'JSN'.
          ls_ewaybill-doc_data-doc_type = |CHL|.
          ls_ewaybill-doc_data-sub_supply_type = |4|.
        ELSE.
          ls_ewaybill-doc_data-doc_type = |INV|.
          ls_ewaybill-doc_data-sub_supply_type = |1|.

        ENDIF.
        DATA(lv_invoice) = <fs_invoice>-Odn_numer." |{ <fs_invoice>-invoice ALPHA = OUT }|.
        CONDENSE lv_invoice.
        ls_ewaybill-doc_data-doc_no = lv_invoice.
        ls_ewaybill-doc_data-doc_date = <fs_invoice>-billing_date.
        REPLACE ALL OCCURRENCES OF '.' IN ls_ewaybill-doc_data-doc_date WITH '-'.
        """""""""""""""Company Details
        ls_ewaybill-doc_data-from_trd_name = <fs_invoice>-CompanyCodeName.
        ls_ewaybill-doc_data-from_addr1 = |Phagwara Hoshiarpur Rd|.
        ls_ewaybill-doc_data-from_addr2 = ''.
        ls_ewaybill-doc_data-from_place = |Mehtiana|."
        IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
          ls_ewaybill-doc_data-from_gstin = l_zuser_id_pass-user_id.
          ls_ewaybill-doc_data-from_pincode = l_zuser_id_pass-password.
        ELSE.
          ls_ewaybill-doc_data-from_gstin = <fs_invoice>-com_gst. "|29AAACW2913K1Z5|.
          ls_ewaybill-doc_data-from_pincode = |146001|.
        ENDIF.
        IF ls_ewaybill-doc_data-from_gstin IS NOT INITIAL.
          ls_ewaybill-doc_data-act_from_state_cd = ls_ewaybill-doc_data-from_gstin+0(2).
          ls_ewaybill-doc_data-from_state_cd = ls_ewaybill-doc_data-from_gstin+0(2).
        ENDIF.
        """"""""""""Customer Details
        READ TABLE lt_ship_to_party INTO DATA(wa_ship_to_party) WITH KEY BillingDocument = <fs_invoice>-invoice.
        IF <fs_invoice>-country  NE 'IN' ."trans_currency NE 'INR'.
          ls_ewaybill-doc_data-to_gstin = |URP|.
        ELSE.
          IF <fs_invoice>-doc_type NE 'F2'.
            ls_ewaybill-doc_data-to_gstin = <fs_invoice>-cust_gst.
          ELSE.
            ls_ewaybill-doc_data-to_gstin = wa_ship_to_party-cust_gst.
          ENDIF.
        ENDIF.
        IF <fs_invoice>-doc_type NE 'F2'.
          ls_ewaybill-doc_data-to_trd_name = <fs_invoice>-customername.
          ls_ewaybill-doc_data-to_addr1 = <fs_invoice>-StreetName.
          ls_ewaybill-doc_data-to_addr2 = ''.
        ELSE.
          ls_ewaybill-doc_data-to_trd_name = wa_ship_to_party-customername.
          ls_ewaybill-doc_data-to_addr1 = wa_ship_to_party-StreetName.
          ls_ewaybill-doc_data-to_addr2 = ''.
        ENDIF.
        IF ls_ewaybill-doc_data-to_gstin IS NOT INITIAL AND ls_ewaybill-doc_data-to_gstin NE |URP|.
          IF <fs_invoice>-doc_type NE 'F2'.
            ls_ewaybill-doc_data-to_place = <fs_invoice>-CityName.
            ls_ewaybill-doc_data-to_pincode =  <fs_invoice>-PostalCode.
          ELSE.
            ls_ewaybill-doc_data-to_place = wa_ship_to_party-CityName.
            ls_ewaybill-doc_data-to_pincode =  wa_ship_to_party-PostalCode.
          ENDIF.
          ls_ewaybill-doc_data-act_to_state_cd = ls_ewaybill-doc_data-to_gstin+0(2).
          ls_ewaybill-doc_data-to_state_cd = ls_ewaybill-doc_data-to_gstin+0(2).
        ELSEIF ls_ewaybill-doc_data-to_gstin EQ |URP|.
          ls_ewaybill-doc_data-to_place = <fs_invoice>-CityName.
          ls_ewaybill-doc_data-to_pincode =  |999999|.
          ls_ewaybill-doc_data-act_to_state_cd = |97|.
          ls_ewaybill-doc_data-to_state_cd = |97|.
          READ TABLE lt_trans INTO DATA(wa_1) WITH KEY invoice = <fs_keys>-invoice.
          IF wa_1-pin_code IS NOT INITIAL.
            ls_ewaybill-doc_data-to_pincode = wa_1-pin_code.
          ENDIF.
          IF wa_1-city IS NOT INITIAL.
            ls_ewaybill-doc_data-to_place = wa_1-city.
          ENDIF.
        ENDIF.
        """"""""""""""""Transport Details

        LOOP AT lt_trans INTO DATA(wa) WHERE invoice = <fs_keys>-invoice.
          ls_ewaybill-doc_data-transporter_id = wa-transporter_id.
          ls_ewaybill-doc_data-trans_distance = wa-trans_distance.
          ls_ewaybill-doc_data-trans_doc_no = wa-trans_doc_no.
*          ls_ewaybill-doc_data-trans_doc_date = wa-trans_doc_date.
          ls_ewaybill-doc_data-trans_doc_date =  |{ wa-trans_doc_date+6(2) }-| && |{ wa-trans_doc_date+4(2) }-| && |{ wa-trans_doc_date+0(4) }|..
          ls_ewaybill-doc_data-vehicle_type = wa-vehicle_type.
          ls_ewaybill-doc_data-transporter_name = wa-transporter_name.
          ls_ewaybill-doc_data-vehicle_no = wa-vehicle_no.
          ls_ewaybill-doc_data-trans_mode = wa-transport_mode.
        ENDLOOP.

        """"""""""""""""""""""" Item Details
        CLEAR: lv_sgst,lv_cgst,lv_igst,lv_total.
        LOOP AT lt_billing_Item INTO DATA(wa_billing_Item) WHERE BillingDocument = <fs_invoice>-invoice.
          wa_eway_item-product_name = wa_billing_Item-Product.
          wa_eway_item-product_desc = wa_billing_Item-BillingDocumentItemText.
          READ TABLE lt_HSN INTO DATA(wa_HSN) WITH KEY Product = wa_billing_Item-Product Plant = wa_billing_Item-Plant.
          IF sy-subrc = 0.
            wa_eway_item-hsn_code = wa_hsn-ConsumptionTaxCtrlCode.
          ENDIF.
          wa_eway_item-quantity = wa_billing_item-BillingQuantity.
          READ TABLE t_unit INTO DATA(w_unit) WITH KEY Units =  wa_billing_item-BillingQuantityUnit.
          IF sy-subrc = 0.
            wa_eway_item-qty_unit = w_unit-Text_000.
          ENDIF.

          READ TABLE lt_price INTO DATA(wa_price) WITH KEY BillingDocument = <fs_invoice>-invoice
                      BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOSG'.
          IF sy-subrc = 0.
            wa_eway_item-sgst_rate =  wa_price-ConditionRateValue.
            wa_eway_item-samt = wa_price-ConditionAmount.
            wa_eway_item-rt = wa_eway_item-rt + wa_price-ConditionRateValue.
            lv_sgst = lv_sgst + wa_price-ConditionAmount.
          ENDIF.
          CLEAR wa_price.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
                  BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOCG'.
          IF sy-subrc = 0.
            wa_eway_item-cgst_rate = wa_price-ConditionRateValue.
            wa_eway_item-camt = wa_price-ConditionAmount.
            wa_eway_item-rt = wa_eway_item-rt + wa_price-ConditionRateValue.
            lv_cgst = lv_cgst + wa_price-ConditionAmount.
          ENDIF..
          CLEAR wa_price.
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
                  BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'JOIG'.
          IF sy-subrc = 0.
            wa_eway_item-igst_rate = wa_price-ConditionRateValue.
            wa_eway_item-iamt = wa_price-ConditionAmount.
            wa_eway_item-rt = wa_eway_item-rt + wa_price-ConditionRateValue.
            IF <fs_invoice>-trans_currency NE 'INR'.
              CLEAR:wa_eway_item-igst_rate,lv_exchange.
              lv_exchange = wa_billing_Item-PriceDetnExchangeRate * 100.
              wa_eway_item-iamt = wa_eway_item-iamt * lv_exchange.
            ENDIF.
            lv_igst = lv_igst + wa_eway_item-iamt.
          ENDIF..
          READ TABLE lt_price INTO wa_price WITH KEY BillingDocument = <fs_invoice>-invoice
                     BillingDocumentItem = wa_billing_Item-BillingDocumentItem ConditionType = 'PPR0'.
          IF sy-subrc = 0.
            wa_eway_item-taxable_amount =  wa_price-ConditionAmount.
            IF <fs_invoice>-trans_currency NE 'INR'.
              CLEAR wa_eway_item-taxable_amount.
              CLEAR lv_exchange.
              lv_exchange = wa_billing_Item-PriceDetnExchangeRate * 100.
              wa_eway_item-taxable_amount = wa_price-ConditionAmount * lv_exchange.
            ENDIF.
          ENDIF.
          CLEAR:lv_dis,lv_oth.
          lv_dis = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                        WHERE ( (  ConditionType IN lt_discount ) AND BillingDocument = <fs_invoice>-invoice
                        AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                       NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .
          lv_oth = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                       WHERE ( (  ConditionType IN lt_other ) AND BillingDocument = <fs_invoice>-invoice
                       AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                      NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .
          CLEAR lv_round.
          lv_round = REDUCE dmbtr( INIT basic_cost TYPE dmbtr FOR w_price IN lt_price
                    WHERE ( (  ConditionType EQ 'DRD1' ) AND BillingDocument = <fs_invoice>-invoice
                     AND  BillingDocumentItem = wa_billing_Item-BillingDocumentItem )
                       NEXT basic_cost = basic_cost + w_price-ConditionAmount ) .

          wa_eway_item-taxable_amount = wa_eway_item-taxable_amount + lv_oth.
          wa_eway_item-taxable_amount = wa_eway_item-taxable_amount + lv_dis.
          wa_eway_item-taxable_amount = wa_eway_item-taxable_amount + lv_round.
          lv_total = lv_total + wa_eway_item-taxable_amount.
*          wa_eway_item-taxable_amount =  wa_eway_item-taxable_amount + wa_eway_item-samt + wa_eway_item-camt + wa_eway_item-iamt.
          APPEND wa_eway_item TO it_eway_item.
          CLEAR wa_eway_item.
        ENDLOOP.
        ls_ewaybill-doc_data-item_list = it_eway_item[].
        CLEAR it_eway_item.
        """""""""""""""""Price Details
        ls_ewaybill-doc_data-cgst_value = lv_cgst.
        ls_ewaybill-doc_data-sgst_value = lv_sgst.
        ls_ewaybill-doc_data-igst_value = lv_igst.
        ls_ewaybill-doc_data-total_value = lv_total + lv_cgst + lv_igst + lv_sgst.
*        ls_ewaybill-doc_data-tot_inv_value = lv_total + lv_cgst + lv_igst + lv_sgst.

        ls_ewaybill-doc_data-transaction_type = |1|.
        IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
          ls_ewaybill-doc_data-dispatch_from_gstin = l_zuser_id_pass-user_id.
        ELSE.
          ls_ewaybill-doc_data-dispatch_from_gstin = <fs_invoice>-com_gst ."|29AAACW2913K1Z5|.
        ENDIF.
        ls_ewaybill-doc_data-dispatch_from_trade_name = <fs_invoice>-CompanyCodeName.
        IF <fs_invoice>-doc_type NE 'F2'.
          ls_ewaybill-doc_data-ship_to_gstin = <fs_invoice>-cust_gst."|29AAACW2913K1Z5|.
          ls_ewaybill-doc_data-ship_to_trade_name = <fs_invoice>-customername..
        ELSE.
          ls_ewaybill-doc_data-ship_to_gstin = wa_ship_to_party-cust_gst.
          ls_ewaybill-doc_data-ship_to_trade_name = wa_ship_to_party-customername.
        ENDIF.
        ls_ewaybill-doc_data-other_value = ||.

        CALL TRANSFORMATION id SOURCE data = ls_ewaybill  RESULT XML lv_xml  .
        CONDENSE lv_xml NO-GAPS.

        DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
        | xmlns:ns="http://www.sap.com/eDocument/India/eWayBill/schema/xsd/1.0">| &&
        |<soapenv:Header/>| &&
        |<soapenv:Body>| && |{ lv_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.


        REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:submitDocumentRequest>' .
        REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:submitDocumentRequest>' .
        REPLACE ALL OCCURRENCES OF '</asx:values>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF '</asx:abap>'  IN lv_xml_file WITH  '' .
        REPLACE ALL OCCURRENCES OF   'HEADER_DATA' IN lv_xml_file WITH 'ns:HeaderData' .
        REPLACE ALL OCCURRENCES OF   'TXN_ID' IN lv_xml_file WITH 'ns:txnId' .
        REPLACE ALL OCCURRENCES OF   'USER_GSTIN' IN lv_xml_file WITH 'ns:userGstin' .
        REPLACE ALL OCCURRENCES OF   '<ACTION>' IN lv_xml_file WITH '<ns:action>' .
        REPLACE ALL OCCURRENCES OF   '</ACTION>' IN lv_xml_file WITH '</ns:action>' .
        REPLACE ALL OCCURRENCES OF   'EWB_NO' IN lv_xml_file WITH 'ns:ewbNo' .
        REPLACE ALL OCCURRENCES OF   'SERVICE_PROVIDER' IN lv_xml_file WITH 'ns:serviceProvider' .
        REPLACE ALL OCCURRENCES OF   '<DATE>' IN lv_xml_file WITH '<ns:date>' .
        REPLACE ALL OCCURRENCES OF   '</DATE>' IN lv_xml_file WITH '</ns:date>' .
        REPLACE ALL OCCURRENCES OF   'DOC_DATA' IN lv_xml_file WITH 'ns:docData' .
        REPLACE ALL OCCURRENCES OF   '<SUPPLY_TYPE>' IN lv_xml_file WITH '<ns:supplyType>' .
        REPLACE ALL OCCURRENCES OF   '</SUPPLY_TYPE>' IN lv_xml_file WITH '</ns:supplyType>' .
        REPLACE ALL OCCURRENCES OF   '<SUB_SUPPLY_TYPE>' IN lv_xml_file WITH '<ns:subSupplyType>' .
        REPLACE ALL OCCURRENCES OF   '</SUB_SUPPLY_TYPE>' IN lv_xml_file WITH '</ns:subSupplyType>' .
        REPLACE ALL OCCURRENCES OF   'SUB_SUPPLY_DESC' IN lv_xml_file WITH 'ns:subSupplyDesc' .
        REPLACE ALL OCCURRENCES OF   'DOC_TYPE' IN lv_xml_file WITH 'ns:docType' .
        REPLACE ALL OCCURRENCES OF   '<DOC_NO>' IN lv_xml_file WITH '<ns:docNo>' .
        REPLACE ALL OCCURRENCES OF   '</DOC_NO>' IN lv_xml_file WITH '</ns:docNo>' .
        REPLACE ALL OCCURRENCES OF   '<DOC_DATE>' IN lv_xml_file WITH '<ns:docDate>' .
        REPLACE ALL OCCURRENCES OF   '</DOC_DATE>' IN lv_xml_file WITH '</ns:docDate>' .
        REPLACE ALL OCCURRENCES OF   '<FROM_GSTIN>' IN lv_xml_file WITH '<ns:fromGstin>' .
        REPLACE ALL OCCURRENCES OF   '</FROM_GSTIN>' IN lv_xml_file WITH '</ns:fromGstin>' .
        REPLACE ALL OCCURRENCES OF   'FROM_TRD_NAME' IN lv_xml_file WITH 'ns:fromTrdName' .
        REPLACE ALL OCCURRENCES OF   'FROM_ADDR1' IN lv_xml_file WITH 'ns:fromAddr1' .
        REPLACE ALL OCCURRENCES OF   'FROM_ADDR2' IN lv_xml_file WITH 'ns:fromAddr2' .
        REPLACE ALL OCCURRENCES OF   'FROM_PLACE' IN lv_xml_file WITH 'ns:fromPlace' .
        REPLACE ALL OCCURRENCES OF   'FROM_PINCODE' IN lv_xml_file WITH 'ns:fromPincode' .
        REPLACE ALL OCCURRENCES OF   'ACT_FROM_STATE_CD' IN lv_xml_file WITH 'ns:actFromStateCode' .
        REPLACE ALL OCCURRENCES OF   'FROM_STATE_CD' IN lv_xml_file WITH 'ns:fromStateCode' .
        REPLACE ALL OCCURRENCES OF   '<TO_GSTIN>' IN lv_xml_file WITH '<ns:toGstin>' .
        REPLACE ALL OCCURRENCES OF   '</TO_GSTIN>' IN lv_xml_file WITH '</ns:toGstin>' .
        REPLACE ALL OCCURRENCES OF   '<TO_TRD_NAME>' IN lv_xml_file WITH '<ns:toTrdName>' .
        REPLACE ALL OCCURRENCES OF   '</TO_TRD_NAME>' IN lv_xml_file WITH '</ns:toTrdName>' .
        REPLACE ALL OCCURRENCES OF   'TO_ADDR1' IN lv_xml_file WITH 'ns:toAddr1' .
        REPLACE ALL OCCURRENCES OF   'TO_ADDR2' IN lv_xml_file WITH 'ns:toAddr2' .
        REPLACE ALL OCCURRENCES OF   'TO_PLACE' IN lv_xml_file WITH 'ns:toPlace' .
        REPLACE ALL OCCURRENCES OF   'TO_PINCODE' IN lv_xml_file WITH 'ns:toPincode' .
        REPLACE ALL OCCURRENCES OF   'ACT_TO_STATE_CD' IN lv_xml_file WITH 'ns:actToStateCode' .
        REPLACE ALL OCCURRENCES OF   'TO_STATE_CD' IN lv_xml_file WITH 'ns:toStateCode' .
        REPLACE ALL OCCURRENCES OF   'TOTAL_VALUE' IN lv_xml_file WITH 'ns:totalValue' .
        REPLACE ALL OCCURRENCES OF   'CGST_VALUE' IN lv_xml_file WITH 'ns:cgstValue' .
        REPLACE ALL OCCURRENCES OF   'SGST_VALUE' IN lv_xml_file WITH 'ns:sgstValue' .
        REPLACE ALL OCCURRENCES OF   'IGST_VALUE' IN lv_xml_file WITH 'ns:igstValue' .
        REPLACE ALL OCCURRENCES OF   'CESS_VALUE' IN lv_xml_file WITH 'ns:cessValue' .
        REPLACE ALL OCCURRENCES OF   'TOT_INV_VALUE' IN lv_xml_file WITH 'ns:totInvValue' .
        REPLACE ALL OCCURRENCES OF   'TRANS_MODE' IN lv_xml_file WITH 'ns:transMode' .
        REPLACE ALL OCCURRENCES OF   'TRANS_DISTANCE' IN lv_xml_file WITH 'ns:transDistance' .
        REPLACE ALL OCCURRENCES OF   'TRANSPORTER_NAME' IN lv_xml_file WITH 'ns:transporterName' .
        REPLACE ALL OCCURRENCES OF   'TRANSPORTER_ID' IN lv_xml_file WITH 'ns:transporterId' .
        REPLACE ALL OCCURRENCES OF   'TRANS_DOC_NO' IN lv_xml_file WITH 'ns:transDocNo' .
        REPLACE ALL OCCURRENCES OF   'TRANS_DOC_DATE' IN lv_xml_file WITH 'ns:transDocDate' .
        REPLACE ALL OCCURRENCES OF   'VEHICLE_NO' IN lv_xml_file WITH 'ns:vehicleNo' .
        REPLACE ALL OCCURRENCES OF   'VEHICLE_TYPE' IN lv_xml_file WITH 'ns:vehicleType' .
        REPLACE ALL OCCURRENCES OF   '<item>' IN lv_xml_file WITH '' .
        REPLACE ALL OCCURRENCES OF   '</item>' IN lv_xml_file WITH '' .
        REPLACE ALL OCCURRENCES OF   'ITEM_LIST' IN lv_xml_file WITH 'ns:itemList' .
        REPLACE ALL OCCURRENCES OF   'PRODUCT_NAME' IN lv_xml_file WITH 'ns:productName' .
        REPLACE ALL OCCURRENCES OF   'PRODUCT_DESC' IN lv_xml_file WITH 'ns:productDesc' .
        REPLACE ALL OCCURRENCES OF   'HSN_CODE' IN lv_xml_file WITH 'ns:hsnCode' .
        REPLACE ALL OCCURRENCES OF   'QUANTITY' IN lv_xml_file WITH 'ns:quantity' .
        REPLACE ALL OCCURRENCES OF   'QTY_UNIT' IN lv_xml_file WITH 'ns:qtyUnit' .
        REPLACE ALL OCCURRENCES OF   'TAXABLE_AMOUNT' IN lv_xml_file WITH 'ns:taxableAmount' .
        REPLACE ALL OCCURRENCES OF   'SGST_RATE' IN lv_xml_file WITH 'ns:sgstRate' .
        REPLACE ALL OCCURRENCES OF   'CGST_RATE' IN lv_xml_file WITH 'ns:cgstRate' .
        REPLACE ALL OCCURRENCES OF   'IGST_RATE' IN lv_xml_file WITH 'ns:igstRate' .
        REPLACE ALL OCCURRENCES OF   'CESS_RATE' IN lv_xml_file WITH 'ns:cessRate' .
        REPLACE ALL OCCURRENCES OF   'IAMT' IN lv_xml_file WITH 'ns:iamt' .
        REPLACE ALL OCCURRENCES OF   'CAMT' IN lv_xml_file WITH 'ns:camt' .
        REPLACE ALL OCCURRENCES OF   '<SAMT>' IN lv_xml_file WITH '<ns:samt>' .
        REPLACE ALL OCCURRENCES OF   '</SAMT>' IN lv_xml_file WITH '</ns:samt>' .
        REPLACE ALL OCCURRENCES OF   '<CSAMT>' IN lv_xml_file WITH '<ns:csamt>' .
        REPLACE ALL OCCURRENCES OF   '</CSAMT>' IN lv_xml_file WITH '</ns:csamt>' .
        REPLACE ALL OCCURRENCES OF   'RT' IN lv_xml_file WITH 'ns:rt' .
        REPLACE ALL OCCURRENCES OF   '<TRANSACTION_TYPE>' IN lv_xml_file WITH '<ns:transactionType>' .
        REPLACE ALL OCCURRENCES OF   '</TRANSACTION_TYPE>' IN lv_xml_file WITH '</ns:transactionType>' .
        REPLACE ALL OCCURRENCES OF   'DISPATCH_FROM_GSTIN' IN lv_xml_file WITH 'ns:dispatchFromGSTIN' .
        REPLACE ALL OCCURRENCES OF   'DISPATCH_FROM_TRADE_NAME' IN lv_xml_file WITH 'ns:dispatchFromTradeName' .
        REPLACE ALL OCCURRENCES OF   'SHIP_TO_GSTIN' IN lv_xml_file WITH 'ns:shipToGSTIN' .
        REPLACE ALL OCCURRENCES OF   'SHIP_TO_TRADE_NAME' IN lv_xml_file WITH 'ns:shipToTradeName' .
        REPLACE ALL OCCURRENCES OF   'OTHER_VALUE' IN lv_xml_file WITH 'ns:otherValue' .

        CLEAR:lv_url,user_id,password, l_zuser_id_pass.
        SELECT SINGLE * FROM zuser_id_pass WHERE channel = 'EWAYBILL' INTO @l_zuser_id_pass.
        IF sy-subrc = 0.
          lv_url = l_zuser_id_pass-url.
          user_id = l_zuser_id_pass-user_id.
          password = l_zuser_id_pass-password.
        ENDIF.

        TRY.
            lo_http_client = cl_web_http_client_manager=>create_by_http_destination(
              i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url )  ).
          CATCH cx_web_http_client_error cx_http_dest_provider_error INTO DATA(lv_handle).
            DATA(lv_string) = lv_handle->get_text( ).
            "handle exception
        ENDTRY.
        DATA(lo_request) = lo_http_client->get_http_request(  ).
        lo_request->set_content_type( content_type = 'text/xml' ).

        lo_request->set_text(
          EXPORTING
            i_text   = lv_xml_file
        ).
*       CATCH cx_web_message_error.

        lo_request->set_authorization_basic(
     EXPORTING
       i_username = user_id  "'2e98993d-9e52-4e85-a7a4-38ab9e899cb8'
       i_password = password  "'4ef845ba-9ab7-4686-9712-0aa19301d54e'
     RECEIVING
       r_value    = DATA(l_value)
   ).

        TRY.
            DATA(lv_response) = lo_http_client->execute(
                                  i_method  =  if_web_http_client=>post ).
          CATCH cx_web_http_client_error cx_web_message_error INTO lv_handle.
            lv_string = lv_handle->get_text( ).
            "handle exception
        ENDTRY.

        lv_response->get_text(
          RECEIVING
            r_value = DATA(t_value)
        ).
        CLEAR: it_einv.
        IF t_value NS 'error'.
          FIND REGEX '<ns2:status>([^<]+)</ns2:status>' IN t_value SUBMATCHES DATA(lv_status).
          FIND REGEX '<ns2:docNo>([^<]+)</ns2:docNo>' IN t_value SUBMATCHES DATA(lv_docNo).
          FIND REGEX '<ns2:ewayBillNo>([^<]+)</ns2:ewayBillNo>' IN t_value SUBMATCHES DATA(lv_ewayBillNo).
          FIND REGEX '<ns2:ewayBilldate>([^<]+)</ns2:ewayBilldate>' IN t_value SUBMATCHES DATA(lv_ewayBilldate).
          FIND REGEX '<ns2:validUpto>([^<]+)</ns2:validUpto>' IN t_value SUBMATCHES DATA(lv_validUpto).
          IF lv_status = '1'.
            DATA(lv_eway_date) =  |{ lv_ewayBilldate+6(4) }| && |{ lv_ewayBilldate+3(2) }| && |{ lv_ewayBilldate+0(2) }|.
            IF <fs_invoice>-doc_type = 'JSP' OR <fs_invoice>-doc_type = 'JSN'.
              lv_einv-invoice = <fs_invoice>-invoice.
              lv_einv-fiscalyear = <fs_invoice>-FiscalYear.
              lv_einv-ewbno = lv_ewayBillNo.
              lv_einv-eway_status = lv_status.
              lv_einv-ewbdt = lv_eway_date.
              lv_einv-ewbvalidtill = lv_ewayBilldate.
              lv_einv-remarks = ' '.
              lv_einv-irn_cancel_dt  = ' '.
              lv_einv-error_msg = ''.
              lv_einv-error_code = ''.
              APPEND lv_einv TO it_einv.
              MODIFY zeinvoice_eway FROM TABLE @it_einv.
            ELSE.
              UPDATE zeinvoice_eway SET eway_status = @lv_status, ewbno = @lv_ewayBillNo ,ewb_error_msg = ' ',
                                        ewbdt = @lv_ewayBilldate, ewbbill_dt = @lv_eway_date, ewbvalidtill = @lv_validUpto
                                        WHERE invoice = @<fs_invoice>-invoice AND fiscalyear = @<fs_invoice>-FiscalYear.
            ENDIF.
          ENDIF.
        ELSE.
          FIND REGEX '<ns2:errorCodes>([^<]+)</ns2:errorCodes>' IN t_value SUBMATCHES DATA(lv_ErrorMessage).
          IF lv_ErrorMessage IS INITIAL.
            FIND REGEX '<ns:ErrorMessage>([^<]+)</ns:ErrorMessage>' IN t_value SUBMATCHES lv_ErrorMessage.
          ENDIF.
          UPDATE zeinvoice_eway SET ewb_error_msg = @lv_ErrorMessage
           WHERE invoice = @<fs_invoice>-invoice AND fiscalyear = @<fs_invoice>-FiscalYear.
          DATA(l_len) = strlen( lv_ErrorMessage ).
          DATA(l_div) = l_len / 4.
          DATA(l_val) = lv_ErrorMessage+0(l_div).
          DATA(l_va2) = lv_ErrorMessage+l_div(l_div).
          DATA(l_val1) = l_div + l_div.
          DATA(l_va3) = lv_ErrorMessage+l_val1(l_div).
          DATA(l_val2) = l_val1 + l_div.
          DATA(l_val5) = l_val2 + l_div.
          IF l_val5 GT l_len.
            DATA(l_fin) = l_val5 - l_len.
            l_div = l_div - l_fin.
          ENDIF.
          DATA(l_va4) = lv_ErrorMessage+l_val2(l_div).
        ENDIF.
        IF lv_ErrorMessage IS INITIAL AND lv_ewayBillNo IS INITIAL.
          lv_ErrorMessage = 'No Response'.
          l_val = 'No Response'.
        ENDIF.
        IF lv_ewayBillNo IS NOT INITIAL.
          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                          %cid = <fs_keys>-%cid_ref
                     %msg                 = new_message(
                                              id       = 'ZEINVOICE'
                                              number   = '003'
                                              severity = if_abap_behv_message=>severity-success
                                              v1       = lv_ewayBillNo )
                                            ) TO reported-_invoice.
        ELSE.
          APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                       %state_area = 'E-WAY Bill Error'
                                        %msg      = new_message(
                                         id       = 'ZEINVOICE'
                                         number   = '002'
                                         severity = if_abap_behv_message=>severity-error
                                           v1       = l_val
                                           v2       = l_va2
                                           v3       = l_va3
                                           v4       = l_va4
                                         )
                                  %element-invoice = if_abap_behv=>mk-on
                                       ) TO reported-_invoice.
        ENDIF.
      ENDIF..
    ENDLOOP.
    result = VALUE #( FOR invdata IN invoicedata
             ( %tky = invdata-%tky %param = invdata ) ).
  ENDMETHOD.

  METHOD Trans_Detail.
    DATA: lv_gst            TYPE string,
          lv_vehicle_no     TYPE string,
          lv_transporter_id TYPE string.

    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
       ENTITY _Invoice
       ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata) FAILED DATA(lv_failed) REPORTED DATA(lv_REPORTED) .
    DATA(trans_distance) = lt_key[ 1 ]-%param-trans_distance.
    DATA(trans_doc_no) = lt_key[ 1 ]-%param-trans_doc_no.
    DATA(trans_doc_date) = lt_key[ 1 ]-%param-trans_doc_date.
    DATA(vehicle_type) = lt_key[ 1 ]-%param-vehicle_type.
    DATA(transporter_name) = lt_key[ 1 ]-%param-transporter_name.
    DATA(vehicle_no) = lt_key[ 1 ]-%param-vehicle_no.
    DATA(transporter_id) = lt_key[ 1 ]-%param-transporter_id.
    DATA(transport_mode) = lt_key[ 1 ]-%param-transport_mode.
    DATA(pincode) = lt_key[ 1 ]-%param-pin_code.
    DATA(City) = lt_key[ 1 ]-%param-city.
    CLEAR:lv_vehicle_no,lv_transporter_id.
    SELECT SINGLE channel,user_id FROM zuser_id_pass WHERE channel = 'GST' INTO @DATA(l_zuser_id_pass).
    SELECT invoice,FiscalYear,com_gst,ewbno FROM zfi_i_einvoice_ewaybill FOR ALL ENTRIES IN @lt_key WHERE invoice = @lt_key-invoice INTO TABLE @DATA(lt_Invoice).
    SELECT invoice,FiscalYear,vehicle_no,transporter_id FROM ztrans_detail FOR ALL ENTRIES IN @lt_key WHERE invoice = @lt_key-invoice INTO TABLE @DATA(lt_transport).
    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE lt_Invoice INTO DATA(wa_invoice) WITH KEY invoice = <fs_keys>-invoice.
      IF sy-subrc = 0.
        lv_trans-client = sy-mandt.
        lv_trans-invoice = wa_invoice-invoice.
        lv_trans-fiscalyear = wa_invoice-FiscalYear.
        lv_trans-transporter_name = transporter_name.
        lv_trans-trans_distance = trans_distance.
        lv_trans-trans_doc_no = trans_doc_no.
        lv_trans-trans_doc_date = trans_doc_date.
        lv_trans-vehicle_type = vehicle_type.
        lv_trans-vehicle_no = vehicle_no.
        lv_vehicle_no = vehicle_no.
        lv_trans-transporter_id = transporter_id.
        lv_transporter_id = transporter_id.
        lv_trans-transport_mode = transport_mode.
        lv_trans-pin_code = pincode.
        lv_trans-city = city.
        APPEND lv_trans TO it_trans.
        CLEAR lv_trans.
      ENDIF..

      IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
        lv_gst = l_zuser_id_pass-user_id.
      ELSE.
        lv_gst  = wa_invoice-com_gst. "|29AAACW2913K1Z5|."
      ENDIF.
      READ TABLE lt_transport INTO DATA(wa_transport) WITH KEY invoice =  wa_invoice-invoice fiscalyear = wa_invoice-FiscalYear.
      IF sy-subrc = 0 AND wa_transport-vehicle_no IS NOT INITIAL AND wa_transport-vehicle_no NE lv_vehicle_no
      AND wa_invoice-ewbno NE 'null'.
        zcl_update_vehicle_number=>update_vehicle_number(
          EXPORTING
            usergstin   = lv_gst
            billing     =  wa_invoice-invoice
            gjahr       = wa_invoice-FiscalYear
            vehicle     = lv_vehicle_no
          IMPORTING
            status      =  DATA(l_status)
            status1      =  DATA(l_status1)
            status_code = DATA(l_status_code)
        ).
      ELSEIF wa_transport-transporter_id NE lv_transporter_id AND wa_invoice-ewbno NE 'null'.
        zcl_update_transport_details=>update_vehicle_number(
          EXPORTING
            usergstin     =  lv_gst
            billing       = wa_invoice-invoice
            gjahr         =   wa_invoice-FiscalYear
            transporterid = lv_transporter_id
          IMPORTING
            status        = l_status
            status1        = l_status1
            status_code   = l_status_code
        ).
      ENDIF.


      IF it_trans IS NOT INITIAL AND l_status_code IS INITIAL.
        IF l_status IS INITIAL.
          l_status = 'Data Update successfully'.
        ENDIF.
        MODIFY ztrans_detail FROM TABLE @it_trans.
        APPEND VALUE #( %tky                 = <fs_keys>-%tky
                        %cid = <fs_keys>-%cid_ref
                   %msg                 = new_message(
                                            id       = 'ZEINVOICE'
                                            number   = '001'
                                            severity = if_abap_behv_message=>severity-success
                                            v1       = l_status
                                            v2       = l_status1 )
                                          ) TO reported-_invoice.
      ELSE.
        APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

        APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                      %msg      = new_message(
                                       id       = 'ZEINVOICE'
                                       number   = '002'
                                       severity = if_abap_behv_message=>severity-error
                                       v1       = l_status_code )
                                %element-invoice = if_abap_behv=>mk-on
                                     ) TO reported-_invoice.
      ENDIF.
      CLEAR it_einv.
    ENDLOOP.
    result = VALUE #( FOR invdata IN invoicedata
             ( %tky = invdata-%tky %param = invdata ) ).
  ENDMETHOD.


  METHOD GetDefaultsForTrans_Detail.
    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
       ENTITY _Invoice
       ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata) .
    IF invoicedata IS NOT INITIAL.
      SELECT * FROM ztrans_detail FOR ALL ENTRIES IN @invoicedata
               WHERE invoice = @invoicedata-invoice AND fiscalyear = @invoicedata-FiscalYear
               INTO TABLE @DATA(it_trans).
    ENDIF.
    READ TABLE lt_key INTO DATA(key) INDEX 1.
    READ TABLE it_trans INTO DATA(wa_trans) WITH KEY invoice = key-invoice.
    IF sy-subrc = 0.
      INSERT VALUE #( %tky = key-%tky ) INTO TABLE result REFERENCE INTO DATA(new_line).
      new_line->%param-trans_distance = wa_trans-trans_distance.
      new_line->%param-vehicle_no = wa_trans-vehicle_no.
      new_line->%param-transporter_name = wa_trans-transporter_name.
      new_line->%param-transporter_id = wa_trans-transporter_id.
      new_line->%param-trans_doc_date = wa_trans-trans_doc_date.
      new_line->%param-trans_doc_no = wa_trans-trans_doc_no.
      new_line->%param-vehicle_type = wa_trans-vehicle_type.
      new_line->%param-transport_mode = wa_trans-transport_mode.
      new_line->%param-pin_code = wa_trans-pin_code.
      new_line->%param-city = wa_trans-city.
    ELSE.
      cl_abap_context_info=>get_system_date(
    RECEIVING
      rv_date = DATA(l_date)
  ).
      wa_trans-trans_doc_date = l_date.
      INSERT VALUE #( %tky = key-%tky ) INTO TABLE result REFERENCE INTO new_line.
      new_line->%param-trans_distance = wa_trans-trans_distance.
      new_line->%param-vehicle_no = wa_trans-vehicle_no.
      new_line->%param-transporter_name = wa_trans-transporter_name.
      new_line->%param-transporter_id = wa_trans-transporter_id.
      new_line->%param-trans_doc_date = wa_trans-trans_doc_date.
      new_line->%param-trans_doc_no = wa_trans-trans_doc_no.
      new_line->%param-vehicle_type = wa_trans-vehicle_type.
      new_line->%param-transport_mode = wa_trans-transport_mode.
      new_line->%param-pin_code = wa_trans-pin_code.
      new_line->%param-city = wa_trans-city.
    ENDIF..


  ENDMETHOD.

  METHOD GET_EWAY_Bill.
    DATA :lv_gst TYPE string,
          lv_ewb TYPE string.
    DATA(lt_key) = keys.
    READ ENTITIES OF zfi_i_einvoice_ewaybill IN LOCAL MODE
       ENTITY _Invoice
       ALL FIELDS WITH CORRESPONDING #( keys ) RESULT DATA(invoicedata)
       FAILED DATA(lv_failed) REPORTED DATA(lv_REPORTED) .
    IF lt_key IS NOT INITIAL.
      SELECT invoice,com_gst,ewbno FROM zfi_i_einvoice_ewaybill FOR ALL ENTRIES IN @lt_key
 WHERE invoice = @lt_key-invoice INTO TABLE @DATA(lt_Invoice).
    ENDIF.
    SELECT SINGLE channel,user_id FROM zuser_id_pass WHERE channel = 'GST' INTO @DATA(l_zuser_id_pass).
    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<fs_keys>).
      READ TABLE lt_Invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WITH KEY invoice = <fs_keys>-invoice.
      IF sy-subrc = 0.
        IF l_zuser_id_pass-user_id IS NOT INITIAL AND l_zuser_id_pass-channel = 'GST'.
          lv_gst = l_zuser_id_pass-user_id.
        ELSE.
          lv_gst  = <fs_invoice>-com_gst. "|29AAACW2913K1Z5|."
        ENDIF.
        lv_ewb = <fs_invoice>-ewbno.
        zcl_get_eway_bill=>get_eway_bill_details(
          EXPORTING
            usergstin = lv_gst
            ewbno     = lv_ewb
          IMPORTING
            status    = DATA(l_string)
        ).
        IF l_string IS NOT INITIAL.
          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                          %cid = <fs_keys>-%cid_ref
                     %msg                 = new_message(
                                              id       = 'ZEINVOICE'
                                              number   = '003'
                                              severity = if_abap_behv_message=>severity-success
                                              v1       = l_string )
                                            ) TO reported-_invoice.
        ELSE.
          APPEND VALUE #( %tky = <fs_keys>-%tky ) TO failed-_invoice.

          APPEND VALUE #( %tky                 = <fs_keys>-%tky
                                       %state_area = 'E-WAY Bill Error'
                                        %msg      = new_message(
                                         id       = 'ZEINVOICE'
                                         number   = '002'
                                         severity = if_abap_behv_message=>severity-error
                                           v1       = l_string
                                         )
                                  %element-invoice = if_abap_behv=>mk-on
                                       ) TO reported-_invoice.
        ENDIF.
      ENDIF.
    ENDLOOP.
    result = VALUE #( FOR invdata IN invoicedata
         ( %tky = invdata-%tky %param = invdata ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZFI_I_EINVOICE_EWAYBILL DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION..

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZFI_I_EINVOICE_EWAYBILL IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
