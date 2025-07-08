@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Billing Pricing Data'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZBILLING_PRICING_DATA
  as select from I_BillingDocumentItemPrcgElmnt
{
  key BillingDocument,
  key BillingDocumentItem,
  key PricingProcedureStep,
  key PricingProcedureCounter,
      ConditionType,
      ConditionRateValue,
      TransactionCurrency,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      ConditionAmount
}
