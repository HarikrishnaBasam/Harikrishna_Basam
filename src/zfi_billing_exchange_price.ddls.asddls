@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Billing Exchange Price'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZFI_BILLING_EXCHANGE_PRICE
  as select from I_BillingDocumentItem
{
  key BillingDocument,
      PriceDetnExchangeRate
}

group by
  BillingDocument,
  PriceDetnExchangeRate
