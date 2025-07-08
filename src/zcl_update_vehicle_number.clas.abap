CLASS zcl_update_vehicle_number DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_headerdata,
             txnid           TYPE string,
             usergstin       TYPE string,
             action          TYPE string,
             ewbno           TYPE string,
             serviceprovider TYPE string,
             date            TYPE string,
           END OF ty_headerdata.

    TYPES: BEGIN OF ty_updaterequest,
             ewbno        TYPE string,
             vehicleno    TYPE string,
             fromplace    TYPE string,
             fromstate    TYPE string,
             reasoncode   TYPE string,
             reasonrem    TYPE string,
             transdocno   TYPE string,
             transdocdate TYPE string,
             transmode    TYPE string,
             vehicletype  TYPE string,
           END OF ty_updaterequest.

    TYPES: BEGIN OF ty_updatedocrequest,
             headerdata    TYPE ty_headerdata,
             updaterequest TYPE ty_updaterequest,
           END OF ty_updatedocrequest.
    CLASS-DATA: ls_headerdata    TYPE ty_headerdata,
                ls_updaterequest TYPE ty_updaterequest,
                ls_updatedocreq  TYPE ty_updatedocrequest.
    CLASS-DATA: l_xml           TYPE string,
                lv_url          TYPE string,
                l_zuser_id_pass TYPE zuser_id_pass,
                user_id         TYPE string,
                password        TYPE string,
                lo_http_client  TYPE REF TO if_web_http_client.
    CLASS-METHODS: update_vehicle_number
      IMPORTING
        userGstin   TYPE string
        Billing     TYPE vbeln
        gjahr       TYPE gjahr
        vehicle     TYPE string
      EXPORTING
        status      TYPE string
        status1      TYPE string
        status_code TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UPDATE_VEHICLE_NUMBER IMPLEMENTATION.


  METHOD update_vehicle_number.
    SELECT SINGLE invoice,CityName,cust_gst,ewbno,ewbdt FROM zfi_i_einvoice_ewaybill
                    WHERE invoice = @Billing
                      AND FiscalYear = @gjahr   INTO @DATA(l_invoice).
    SELECT SINGLE * FROM ztrans_detail
                    WHERE invoice = @Billing
                      AND FiscalYear = @gjahr   INTO @DATA(l_transport).

    ls_updatedocreq-headerdata-txnid = ''.
    ls_updatedocreq-headerdata-action = 'VEHEWB'.
    ls_updatedocreq-headerdata-usergstin = userGstin.
    ls_updatedocreq-headerdata-ewbno = l_invoice-ewbno.
    DATA(lv_date) = |{ l_invoice-ewbdt+6(2) }-| && |{ l_invoice-ewbdt+4(2) }-| && |{ l_invoice-ewbdt+0(4) }|.
    ls_updatedocreq-headerdata-date = lv_date.
    ls_updatedocreq-headerdata-serviceprovider = ''.


    ls_updatedocreq-updaterequest-ewbno = l_invoice-ewbno.
    ls_updatedocreq-updaterequest-vehicleno = vehicle.
    ls_updatedocreq-updaterequest-fromplace = l_invoice-CityName.
    ls_updatedocreq-updaterequest-fromstate = l_invoice-cust_gst+0(2).
    ls_updatedocreq-updaterequest-reasoncode = '1'.
    ls_updatedocreq-updaterequest-reasonrem = 'break down'.
    CLEAR lv_date.
    lv_date = |{ l_transport-trans_doc_date+6(2) }-| && |{ l_transport-trans_doc_date+4(2) }-| && |{ l_transport-trans_doc_date+0(4) }|.
    ls_updatedocreq-updaterequest-transdocdate = lv_date.
    ls_updatedocreq-updaterequest-transdocno = l_transport-trans_doc_no.
    ls_updatedocreq-updaterequest-transmode = l_transport-transport_mode.
    ls_updatedocreq-updaterequest-vehicletype = l_transport-vehicle_type.

    CALL TRANSFORMATION id SOURCE data = ls_updatedocreq  RESULT XML l_xml  .
    CONDENSE l_xml NO-GAPS.

    DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
                      | xmlns:ns="http://www.sap.com/eDocument/India/eWayBill/schema/xsd/1.0">| &&
                      |<soapenv:Header/>| &&
                      |<soapenv:Body>| && |{ l_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.

    REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:updateDocRequest>' .
    REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:updateDocRequest>' .
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

    REPLACE ALL OCCURRENCES OF   '<UPDATEREQUEST>' IN lv_xml_file WITH '<ns:updateRequest>' .
    REPLACE ALL OCCURRENCES OF   '</UPDATEREQUEST>' IN lv_xml_file WITH '</ns:updateRequest>' .
    REPLACE ALL OCCURRENCES OF   '<VEHICLENO>' IN lv_xml_file WITH '<ns:vehicleNo>' .
    REPLACE ALL OCCURRENCES OF   '</VEHICLENO>' IN lv_xml_file WITH '</ns:vehicleNo>' .
    REPLACE ALL OCCURRENCES OF   '<FROMPLACE>' IN lv_xml_file WITH '<ns:fromPlace>' .
    REPLACE ALL OCCURRENCES OF   '</FROMPLACE>' IN lv_xml_file WITH '</ns:fromPlace>' .
    REPLACE ALL OCCURRENCES OF   '<FROMSTATE>' IN lv_xml_file WITH '<ns:fromState>' .
    REPLACE ALL OCCURRENCES OF   '</FROMSTATE>' IN lv_xml_file WITH '</ns:fromState>' .
    REPLACE ALL OCCURRENCES OF   '<REASONCODE>' IN lv_xml_file WITH '<ns:reasonCode>' .
    REPLACE ALL OCCURRENCES OF   '</REASONCODE>' IN lv_xml_file WITH '</ns:reasonCode>' .
    REPLACE ALL OCCURRENCES OF   '<REASONREM>' IN lv_xml_file WITH '<ns:reasonRem>' .
    REPLACE ALL OCCURRENCES OF   '</REASONREM>' IN lv_xml_file WITH '</ns:reasonRem>' .
    REPLACE ALL OCCURRENCES OF   '<TRANSDOCNO>' IN lv_xml_file WITH '<ns:transDocNo>' .
    REPLACE ALL OCCURRENCES OF   '</TRANSDOCNO>' IN lv_xml_file WITH '</ns:transDocNo>' .
    REPLACE ALL OCCURRENCES OF   '<TRANSDOCDATE>' IN lv_xml_file WITH '<ns:transDocDate>' .
    REPLACE ALL OCCURRENCES OF   '</TRANSDOCDATE>' IN lv_xml_file WITH '</ns:transDocDate>' .
    REPLACE ALL OCCURRENCES OF   '<TRANSMODE>' IN lv_xml_file WITH '<ns:transMode>' .
    REPLACE ALL OCCURRENCES OF   '</TRANSMODE>' IN lv_xml_file WITH '</ns:transMode>' .
    REPLACE ALL OCCURRENCES OF   '<VEHICLETYPE>' IN lv_xml_file WITH '<ns:vehicleType>' .
    REPLACE ALL OCCURRENCES OF   '</VEHICLETYPE>' IN lv_xml_file WITH '</ns:vehicleType>' .

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
      status = 'Vehicle Number'.
      status1 = 'Update successfully'.
    ELSE.
      FIND REGEX '<ns2:errorCodes>([^<]+)</ns2:errorCodes>' IN t_value SUBMATCHES DATA(lv_eway_error).
      status = lv_eway_error.
      if lv_eway_error Is INITIAL.
      status_code = 'No Response'.
      ELSE.
      status_code = lv_eway_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
