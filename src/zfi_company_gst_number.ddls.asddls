@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Company GST Number'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZFI_COMPANY_GST_NUMBER
  as select from I_BillingDocumentItem as _billingitem
  association [0..1] to I_IN_BusinessPlaceTaxDetail as _Comgst on  _billingitem.CompanyCode = _Comgst.CompanyCode
                                                               and _Comgst.BusinessPlace    = '1100'
{
  key _billingitem.BillingDocument       as billing_dco,
      _Comgst.IN_GSTIdentificationNumber as com_gst
}
group by
  _billingitem.BillingDocument,
  _Comgst.IN_GSTIdentificationNumber
