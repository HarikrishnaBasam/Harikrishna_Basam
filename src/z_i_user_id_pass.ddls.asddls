@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'User Id Password'
@Metadata.ignorePropagatedAnnotations: true
define root view entity Z_I_USER_ID_PASS
  as select from zuser_id_pass
{
  key channel  as Channel,
  key user_id  as UserId,
      password as Password,
      url      as Url
}
