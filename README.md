## Generating localhost TLS certificate for the backend

The backend is always served via HTTPS, even when doing development. To generate the TLS certificate, run `tools/certify`. This will create a certificate authority and let it issue a certificate for `localhost`. You can assign the certificate and certificate authority any information you want, but I recommend to set at least the organization name so it is clear that you signed the certificate. Then, add `backend/cert/authority.pem` to your browser's trusted certificate authorities.

## Running

```
stack build
stack exec app
```
