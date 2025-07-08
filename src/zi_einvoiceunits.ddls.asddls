@EndUserText.label: 'EInvoice Units'
@AccessControl.authorizationCheck: #NOT_ALLOWED
@Metadata.allowExtensions: true
define view entity ZI_EinvoiceUnits
  as select from zunits
  association to parent ZI_EinvoiceUnits_S as _EinvoiceUnitsAll on $projection.SingletonID = _EinvoiceUnitsAll.SingletonID
{
  key units          as Units,
  key einvoice_units as EinvoiceUnits,
      text           as Text_000,
      @Consumption.hidden: true
      1              as SingletonID,
      _EinvoiceUnitsAll

}
