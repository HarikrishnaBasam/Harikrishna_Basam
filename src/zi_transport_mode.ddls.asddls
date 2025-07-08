@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Transport Mode'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_TRANSPORT_MODE
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZTRANSPORT_MODE' )
{
  key value_low as Trans_Mode,
      @Semantics.language: true
  key language,
      @Semantics.text: true
      text      as Trans_Modedes
}
