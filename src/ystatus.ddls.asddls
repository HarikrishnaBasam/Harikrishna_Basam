@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity YSTATUS
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'YSTATUS' )
{
  key value_low as status,
  key language,
      text      as statusdes
}
