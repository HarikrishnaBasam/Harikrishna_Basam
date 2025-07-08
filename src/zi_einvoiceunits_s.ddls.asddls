@EndUserText.label: 'EInvoice Units Singleton'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.semanticKey: [ 'SingletonID' ]
@UI: {
  headerInfo: {
    typeName: 'EinvoiceUnitsAll'
  }
}
define root view entity ZI_EinvoiceUnits_S
  as select from I_Language
    left outer join I_CstmBizConfignLastChgd on I_CstmBizConfignLastChgd.ViewEntityName = 'ZI_EINVOICEUNITS'
  composition [0..*] of ZI_EinvoiceUnits as _EinvoiceUnits
{
  @UI.facet: [ {
    id: 'ZI_EinvoiceUnits', 
    purpose: #STANDARD, 
    type: #LINEITEM_REFERENCE, 
    label: 'EInvoice Units', 
    position: 1 , 
    targetElement: '_EinvoiceUnits'
  } ]
  @UI.lineItem: [ {
    position: 1 
  } ]
  key 1 as SingletonID,
  _EinvoiceUnits,
  @UI.hidden: true
  I_CstmBizConfignLastChgd.LastChangedDateTime as LastChangedAtMax
  
}
where I_Language.Language = $session.system_language
