@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'User Id Password'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity Z_C_USER_ID_PASS
  provider contract transactional_query
  as projection on Z_I_USER_ID_PASS

{
      @EndUserText.label: 'Channel'
  key Channel,
      @EndUserText.label: 'User Id'
  key UserId,
      @EndUserText.label: 'Password'
      Password,
      @EndUserText.label: 'URL'
      Url
}
