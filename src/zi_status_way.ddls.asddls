@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Eway Bill Status'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_STATUS_WAY
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZSTATUS_EWAY' )
{
      //  key cast( value_low as zgender ) as gender ,
  key value_low as status,
      //  key value_position,
      @Semantics.language: true
  key language,
      //  value_low,
      @Semantics.text: true
      text      as statusdes
}
