@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Data'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CUSTOMER_DATA
  as select from I_SalesOrderPartner as _partner
  association [0..1] to I_BillingDocumentItem     as _billItem on $projection.salesorder = _billItem.SalesDocument
  association [0..1] to I_Customer_VH             as _custadd  on $projection.Customer = _custadd.Customer
  association [0..1] to I_BusinessPartnerCustomer as _region   on $projection.Customer = _region.Customer
  association [0..*] to I_Customer                as _gstnno   on $projection.Customer = _gstnno.Customer
{
  key _partner.SalesOrder       as salesorder,
  key _partner.PartnerFunction  as PartnerFunction,
  key _billItem.BillingDocument as BillingDocument,
      _partner.Customer         as Customer,
      _custadd.CustomerName     as customername,
      _custadd.StreetName       as StreetName,
      _custadd.CityName         as CityName,
      _custadd.Country          as country,
      _region.Region            as Region,
      _gstnno.PostalCode        as PostalCode,
      _gstnno.TaxNumber3        as cust_gst
}
where
      _partner.PartnerFunction  = 'WE'
  and _billItem.BillingDocument is not initial

group by
  SalesOrder,
  PartnerFunction,
  _billItem.BillingDocument,
  _partner.Customer,
  _custadd.CustomerName,
  _custadd.Country,
  _custadd.StreetName,
  _custadd.CityName,
  _region.Region,
  _gstnno.PostalCode,
  _gstnno.TaxNumber3
