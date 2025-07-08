CLASS zcl_get_eway_bill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_header_data,
             txnid           TYPE string,
             usergstin       TYPE string,
             action          TYPE string,
             ewbno           TYPE string,
             serviceprovider TYPE string,
             date            TYPE string, "ISO 8601 format: YYYY-MM-DDTHH:MM:SS
           END OF ty_header_data.

    TYPES: BEGIN OF ty_getewaybills,
             headerdata TYPE ty_header_data,
           END OF ty_getewaybills.
    CLASS-DATA lv_request TYPE ty_getewaybills.
    CLASS-DATA: l_xml           TYPE string,
                lv_url          TYPE string,
                l_zuser_id_pass TYPE zuser_id_pass,
                user_id         TYPE string,
                password        TYPE string,
                lo_http_client  TYPE REF TO if_web_http_client.


    CLASS-METHODS: Get_eway_bill_details
      IMPORTING
        userGstin TYPE string
        ewbNo     TYPE string
      EXPORTING
        status    TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GET_EWAY_BILL IMPLEMENTATION.


  METHOD get_eway_bill_details.
    lv_request-headerdata-txnid = ''.
    lv_request-headerdata-usergstin = userGstin.
    lv_request-headerdata-action = 'GetEwayBill'.
    lv_request-headerdata-ewbno = ewbNo.
    lv_request-headerdata-serviceprovider = ''.
    lv_request-headerdata-date = ''.

    CALL TRANSFORMATION id SOURCE data = lv_request  RESULT XML l_xml  .
    CONDENSE l_xml NO-GAPS.

    DATA(lv_xml_file) =   |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"| &&
                          | xmlns:ns="http://www.sap.com/eDocument/India/eWayBill/schema/xsd/1.0">| &&
                          |<soapenv:Header/>| &&
                          |<soapenv:Body>| && |{ l_xml+111 }| && |</soapenv:Body>|  && |</soapenv:Envelope>|.

    REPLACE ALL OCCURRENCES OF '<DATA>'  IN lv_xml_file WITH  '<ns:geteWayBills>' .
    REPLACE ALL OCCURRENCES OF '</DATA>'  IN lv_xml_file WITH  '</ns:geteWayBills>' .
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
    status = 'Eway Bill Details get successfully'.
  ELSE.
  FIND REGEX '<ns2:errorCodes>([^<]+)</ns2:errorCodes>' IN t_value SUBMATCHES DATA(lv_eway_error).
   status = lv_eway_error.
  ENDIF.




  ENDMETHOD.
ENDCLASS.
