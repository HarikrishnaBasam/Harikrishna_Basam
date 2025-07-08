@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'EInvoice And EWAY Bill'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZFI_C_EINVOICE_EWAYBILL
  provider contract transactional_query
  as projection on ZFI_I_EINVOICE_EWAYBILL
{
      @EndUserText.label: 'Invoice Number'
  key invoice,
      @EndUserText.label: 'Billing Type'
      doc_type,
      @EndUserText.label: 'Billing Date'
      billing_date,
      @EndUserText.label: 'ODN Number'
      Odn_numer,
      @EndUserText.label: 'Account Document Number'
      acc_doc_number,
      @EndUserText.label: 'Fiscal Year'
      FiscalYear,
      @EndUserText.label: 'Companycode Currency'
      stat_currency,
      @EndUserText.label: 'Transaction Currency'
      trans_currency,
      @EndUserText.label: 'Total Amount'
      @Semantics.amount.currencyCode: 'stat_currency'
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_EXCHANGE_RATE'
      total_amount,
      @EndUserText.label: 'Total Tax Amount'
      @Semantics.amount.currencyCode: 'stat_currency'
      total_tax_amount,
      @EndUserText.label: 'Total Amount With Tax Amount'
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_EXCHANGE_RATE'
      total_Value,
      @EndUserText.label: 'Customer Code'
      SoldToParty,
      @EndUserText.label: 'Ship-To-Party'
      SoldToParty1,
      @EndUserText.label: 'Customer Name'
      customername,
      @EndUserText.label: 'Street'
      StreetName,
      @EndUserText.label: 'City'
      CityName,
      @EndUserText.label: 'Country'
      country,
      @EndUserText.label: 'Region'
      Region,
      @EndUserText.label: 'Postal Code'
      PostalCode,
      @EndUserText.label: 'Customer GST Number'
      cust_gst,
      @EndUserText.label: 'Company Code'
      CompanyCode,
      @EndUserText.label: 'Company Name'
      CompanyCodeName,
      @EndUserText.label: 'Company GST Number'
      com_gst,
      Criticality,
      @EndUserText.label: 'E-Invocie Status'
      status,
      @EndUserText.label: 'IRN Number'
      irnnumber,
      @EndUserText.label: 'IRN Date'
      ackdt,
      @EndUserText.label: 'IRN Status'
      remarks,
      Criticality_ewy,
      @EndUserText.label: 'E-WAY Bill Status'
      eway_status,
      @EndUserText.label: 'IRN Errors'
      errormsg,
      @EndUserText.label: 'E-WAY Bill Number'
      ewbno,
      @EndUserText.label: 'E-Way Bill Date'
      ewbdt,
      @EndUserText.label: 'E-Way Bill error'
      ewb_error_msg,
      @EndUserText.label: 'Transport Status'
      @UI.hidden: true
      trans_status
}
