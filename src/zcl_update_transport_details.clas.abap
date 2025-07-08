CLASS zcl_update_transport_details DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_header_data,
             txnId           TYPE string,
             userGstin       TYPE string,
             action          TYPE string,
             ewbNo           TYPE string,
             serviceProvider TYPE string,
             date            TYPE string,
           END OF ty_header_data.

    TYPES: BEGIN OF ty_update_trans_request,
             ewbNo         TYPE string,
             transporterId TYPE string,
           END OF ty_update_trans_request.

    TYPES: BEGIN OF ty_update_trans_doc_request,
             HeaderData         TYPE ty_header_data,
             updateTransRequest TYPE ty_update_trans_request,
           END OF ty_update_trans_doc_request.

    TYPES: BEGIN OF ty_soap_body,
             updateTransDocRequest TYPE ty_update_trans_doc_request,
           END OF ty_soap_body.

    CLASS-DATA: lv_transport TYPE ty_soap_body.

    CLASS-DATA: l_xml           TYPE string,
                lv_url          TYPE string,
                l_zuser_id_pass TYPE zuser_id_pass,
                user_id         TYPE string,
                password        TYPE string,
                lo_http_client  TYPE REF TO if_web_http_client.

    CLASS-METHODS: update_vehicle_number
      IMPORTING
        userGstin     TYPE string
        Billing       TYPE vbeln
        gjahr         TYPE gjahr
        transporterId TYPE string
      EXPORTING
        status        TYPE string
        status1        TYPE string
        status_code   TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UPDATE_TRANSPORT_DETAILS IMPLEMENTATION.


  METHOD update_vehicle_number.

    SELECT SINGLE invoice,ewbno,ewbdt FROM zfi_i_einvoice_ewaybill
                    WHERE invoice = @Billing
                      AND FiscalYear = @gjahr   INTO @DATA(l_invoice).
*    SELECT SINGLE * FROM ztrans_detail
*                    WHERE invoice = @Billing
*                      AND FiscalYear = @gjahr   INTO @DATA(l_transport).

    lv_transport-updatetransdocrequest-headerdata-txnid = ''.
    lv_transport-updatetransdocrequest-headerdata-usergstin = userGstin.
    lv_transport-updatetransdocrequest-headerdata-action = 'GENEWAYBILL'.
    lv_transport-updatetransdocrequest-headerdata-ewbno = ''.
    lv_transport-updatetransdocrequest-headerdata-serviceprovider = 'API'.
    DATA(lv_date) = |{ l_invoice-ewbdt+6(2) }-| && |{ l_invoice-ewbdt+4(2) }-| && |{ l_invoice-ewbdt+0(4) }|.
    lv_transport-updatetransdocrequest-headerdata-date = l_invoice-ewbdt+0(10).

    lv_transport-updatetransdocrequest-updatetransrequest-ewbno = l_invoice-ewbno.
    lv_transport-updatetransdocrequest-updatetransrequest-transporterid = transporterId.

    CALL TRANSFORMATION id SOURCE data = lv_transport  RESULT XML l_xml  .
    CONDENSE l_xml NO-GAPS.

    DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
                      | xmlns:ns="http://www.sap.com/eDocument/India/eWayBill/schema/xsd/1.0">| &&
                      |<soapenv:Header/>| &&
                      |<soapenv:Body>| && |{ l_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.


    REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:updateTransDocRequest>' .
    REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:updateTransDocRequest>' .
    REPLACE ALL OCCURRENCES OF '</asx:values>'  IN lv_xml_file WITH  '' .
    REPLACE ALL OCCURRENCES OF '</asx:abap>'  IN lv_xml_file WITH  '' .
    REPLACE ALL OCCURRENCES OF   'HEADERDATA' IN lv_xml_file WITH 'ns:HeaderData' .
    REPLACE ALL OCCURRENCES OF   'TXNID' IN lv_xml_file WITH 'ns:txnId' .
    REPLACE ALL OCCURRENCES OF   'USERGSTIN' IN lv_xml_file WITH 'ns:userGstin' .
    REPLACE ALL OCCURRENCES OF   '<ACTION>' IN lv_xml_file WITH '<ns:action>' .
    REPLACE ALL OCCURRENCES OF   '</ACTION>' IN lv_xml_file WITH '</ns:action>' .
    REPLACE ALL OCCURRENCES OF   'EWBNO' IN lv_xml_file WITH 'ns:ewbNo' .
    REPLACE ALL OCCURRENCES OF   'SERVICEPROVIDER' IN lv_xml_file WITH 'ns:serviceProvider' .
    REPLACE ALL OCCURRENCES OF   '<DATE>' IN lv_xml_file WITH '<ns:date>' .
    REPLACE ALL OCCURRENCES OF   '</DATE>' IN lv_xml_file WITH '</ns:date>' .
    REPLACE ALL OCCURRENCES OF   '<DATE/>' IN lv_xml_file WITH '<ns:date/>' .


    REPLACE ALL OCCURRENCES OF   '<UPDATETRANSREQUEST>' IN lv_xml_file WITH '<ns:updateTransRequest>' .
    REPLACE ALL OCCURRENCES OF   '<UPDATETRANSDOCREQUEST>' IN lv_xml_file WITH '' .
    REPLACE ALL OCCURRENCES OF   '</UPDATETRANSREQUEST>' IN lv_xml_file WITH '</ns:updateTransRequest>' .
    REPLACE ALL OCCURRENCES OF   '</UPDATETRANSDOCREQUEST>' IN lv_xml_file WITH '' .
    REPLACE ALL OCCURRENCES OF   '<TRANSPORTERID>' IN lv_xml_file WITH '<ns:transporterId>' .
    REPLACE ALL OCCURRENCES OF   '</TRANSPORTERID>' IN lv_xml_file WITH '</ns:transporterId>' .

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
      status = 'Transport GST Number'.
      status1 = 'Update successfully'.
    ELSE.
      FIND REGEX '<ns2:errorCodes>([^<]+)</ns2:errorCodes>' IN t_value SUBMATCHES DATA(lv_eway_error).
      FIND REGEX '<ns:ErrorMessage>([^<]+)</ns:ErrorMessage>' IN t_value SUBMATCHES DATA(lv_eway_error1).
      status = lv_eway_error.
      if lv_eway_error Is INITIAL .
      if lv_eway_error1 is INITIAL.
      status_code = 'No Response'.
      ELSE.
       status_code = lv_eway_error1.
      ENDIF.
      ELSE.
      status_code = lv_eway_error.
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
