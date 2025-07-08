@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vehicle Type'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_vehicle_type
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZVEHICLE_TYPE' )
{
  key value_low as vehicle_type,
      @Semantics.language: true
  key language,
      @Semantics.text: true
      text      as vehicle_typedes
}
