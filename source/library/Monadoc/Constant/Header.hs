module Monadoc.Constant.Header where

import qualified Network.HTTP.Types as Http

contentRange :: Http.HeaderName
contentRange = "Content-Range"

contentSecurityPolicy :: Http.HeaderName
contentSecurityPolicy = "Content-Security-Policy"

contentTypeOptions :: Http.HeaderName
contentTypeOptions = "X-Content-Type-Options"

eTag :: Http.HeaderName
eTag = "ETag"

frameOptions :: Http.HeaderName
frameOptions = "X-Frame-Options"

link :: Http.HeaderName
link = "Link"

referrerPolicy :: Http.HeaderName
referrerPolicy = "Referrer-Policy"

strictTransportSecurity :: Http.HeaderName
strictTransportSecurity = "Strict-Transport-Security"

xssProtection :: Http.HeaderName
xssProtection = "X-XSS-Protection"
