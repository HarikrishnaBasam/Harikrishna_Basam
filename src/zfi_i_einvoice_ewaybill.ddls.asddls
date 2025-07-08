@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'EInvoice And EWAY Bill'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZFI_I_EINVOICE_EWAYBILL
  as select from I_BillingDocument as _billing
  association [0..*] to I_Customer                 as _gstnno       on  $projection.SoldToParty = _gstnno.Customer
  association [0..1] to I_Customer_VH              as _custadd      on  $projection.SoldToParty = _custadd.Customer
  association [0..1] to I_BusinessPartnerCustomer  as _region       on  $projection.SoldToParty = _region.Customer
  association [0..1] to ZI_CUSTOMER_DATA           as _CustomerData on  $projection.invoice = _CustomerData.BillingDocument
  association [0..1] to I_CompanyCode              as _company      on  $projection.CompanyCode = _company.CompanyCode
  association [0..1] to ZFI_COMPANY_GST_NUMBER     as _Comgst       on  $projection.invoice = _Comgst.billing_dco
  association [0..1] to zeinvoice_eway             as _einv         on  $projection.invoice    = _einv.invoice
                                                                    and $projection.FiscalYear = _einv.fiscalyear
  //  association [0..1] to YSTATUS                   as _Status  on  $projection.status = _Status.status
  association [0..1] to ztrans_detail              as _Trans        on  $projection.invoice    = _Trans.invoice
                                                                    and $projection.FiscalYear = _Trans.fiscalyear
  association [0..1] to ZFI_BILLING_EXCHANGE_PRICE as _billingitem  on  $projection.invoice = _billingitem.BillingDocument

{
       //       @Search.defaultSearchElement: true
  key  _billing.BillingDocument     as invoice,
       _billing.BillingDocumentType as doc_type,
       concat(
       concat(
       substring(_billing.BillingDocumentDate, 7, 2),
       '.'
       ),
       concat(
       concat(
         substring(_billing.BillingDocumentDate, 5, 2),
         '.'
       ),
       substring(_billing.BillingDocumentDate, 1, 4)
       )
       )                            as billing_date,
       _billing.DocumentReferenceID as Odn_numer,
       _billing.AccountingDocument  as acc_doc_number,
       _billing.FiscalYear          as FiscalYear,
       _billing.StatisticsCurrency  as stat_currency,
       _billing.TransactionCurrency as trans_currency,
       //       case
       //       when _billing.TransactionCurrency <> 'INR'
       //                 then        cast( (
       //              coalesce( cast(_billingitem.PriceDetnExchangeRate as abap.fltp ), 0.00 )  *
       //              coalesce( cast( 100.00 as abap.fltp ), 0.00 )
       //             ) as abap.dec(23,2))
       //                 when _billing.TransactionCurrency = 'INR'
       //              then cast(_billingitem.PriceDetnExchangeRate as abap.fltp)
       //         else cast(0 as abap.fltp)
       //        end                         as exchangerate,
       @Semantics.amount.currencyCode: 'stat_currency'
       _billing.TotalNetAmount      as amount,
       @Semantics.amount.currencyCode: 'stat_currency'
       //       case
       //         when _billing.TransactionCurrency <> 'INR'
       //       //              then cast(_billingitem.PriceDetnExchangeRate * _billing.TotalNetAmount as abap.fltp)
       //          then        cast( (
       //              coalesce( cast(_billing.TotalNetAmount as abap.fltp ), 0.00 )  *
       //              coalesce( cast(_billingitem.PriceDetnExchangeRate as abap.fltp ), 0.00 )
       //             ) as abap.dec(23,2))
       //
       //         when _billing.TransactionCurrency = 'INR'
       //              then cast(_billing.TotalNetAmount as abap.fltp)
       //         else cast(0 as abap.fltp)
       //       end                          as total_amount,
       _billing.TotalNetAmount      as total_amount,
       @Semantics.amount.currencyCode: 'stat_currency'
       _billing.TotalTaxAmount      as total_tax_amount,
       //       @DefaultAggregation: #SUM
       cast( (
       //              coalesce( cast(_billing.TotalNetAmount as abap.fltp ), 0.00 )  +
              coalesce( cast( $projection.total_amount as abap.fltp ), 0.00 )  +
              coalesce( cast(_billing.TotalTaxAmount as abap.fltp ), 0.00 )
             ) as abap.dec(23,2))   as total_Value,
       _billing.SoldToParty         as SoldToParty,
       _CustomerData.Customer       as SoldToParty1,
       _custadd.CustomerName        as customername,
       _custadd.StreetName          as StreetName,
       _custadd.CityName            as CityName,
       _custadd.Country             as country,
       _region.Region               as Region,
       _gstnno.PostalCode           as PostalCode,
       _gstnno.TaxNumber3           as cust_gst,
       _billing.CompanyCode         as CompanyCode,
       _company.CompanyCodeName     as CompanyCodeName,
       _Comgst.com_gst              as com_gst,
       //       _einv.status                 as status,
       _einv.irn                    as irnnumber,

       case _einv.status
           when '1' then '3'  -- Greeen
           when '2' then '1'  -- Red
           else ' ' end             as Criticality,
       case
         when _einv.status = '1' then '1' //sap-icon://accept'
         when _einv.status = '2' then '2'  //sap-icon://decline'
         else ' ' end               as status,

       _einv.ackdt                  as ackdt,
       _einv.remarks                as remarks,
       case _einv.eway_status
       when '1' then '3'      -- Greeen
       when '2' then '1'      -- Red
       else ' ' end                 as Criticality_ewy,
       case
         when _einv.eway_status = '1' then '1'
         when _einv.eway_status = '2' then '2'
         else ' ' end               as eway_status,
       //       _einv.eway_status            as eway_status,
       _einv.error_msg              as errormsg,
       _einv.ewbno                  as ewbno,
       _einv.ewbdt                  as ewbdt,
       _einv.ewb_error_msg          as ewb_error_msg,
       case
        when _Trans.invoice is not initial then '1'
        when _Trans.invoice is  initial then ' '
        else ' ' end                as trans_status


}
where
       _billing.AccountingDocument         is not initial
  and  _billing.CancelledBillingDocument   =  ' '
  and  _billing.BillingDocumentIsCancelled =  ' '
  and  _billing.BillingDocumentType        <> 'S1'
  and  _billing.BillingDocumentType        <> 'S2'
  or(
       _billing.BillingDocumentType        =  'JSN'
    or _billing.BillingDocumentType        =  'JSP'
  )
