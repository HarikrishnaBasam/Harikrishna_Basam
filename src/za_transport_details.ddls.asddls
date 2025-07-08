@EndUserText.label: 'Transport Details'
@Metadata.allowExtensions: true
define root abstract entity ZA_Transport_Details
  //  with parameters parameter_name : parameter_type
{
  //  @UI.defaultValue                                                                                                           :                                                      #( 'ZFI_I_EINVOICE_EWAYBILL: invoice' )
  //  trans_mode
  @EndUserText.label:'Vehicle Number'
  vehicle_no       : abap.char( 20 );
  @EndUserText.label:'Transport Distance'
  trans_distance   : abap.char(8);
  @EndUserText.label:'Export Pincode'
  pin_code         : abap.char(6);
  @EndUserText.label:'Export City'
  city             : abap.char(50);
  @EndUserText.label:'Transport Name'
  transporter_name : abap.char( 60 );
  @EndUserText.label:'Transport GST ID'
  transporter_id   : abap.char( 20 );
  @EndUserText.label:'Transport Doc Date'
  trans_doc_date   : abap.dats;
  @EndUserText.label:'Transport Document No'
  trans_doc_no     : abap.numc( 10 );
  @Consumption.valueHelpDefinition: [{
  entity           : {
    name           : 'ZI_VEHICLE_TYPE',
    element        : 'vehicle_type'
  },
  useForValidation : true,
  label            : 'Vehicle Type'
  }]
  vehicle_type     : zvehicle_type;
  @Consumption.valueHelpDefinition: [{
  entity           : {
    name           : 'ZI_TRANSPORT_MODE',
    element        : 'Trans_Mode'
  },
  useForValidation : true,
  label            : 'Transport Mode'
  }]
  transport_mode   : ztransport_mode;

}
