module Monadoc.Constant.Header where

import qualified Network.HTTP.Types as Http

contentRange :: Http.HeaderName
contentRange = "Content-Range"

contentSecurityPolicy :: Http.HeaderName
contentSecurityPolicy = "Content-Security-Policy"

contentTypeOptions :: Http.HeaderName
contentTypeOptions = "X-Content-Type-Options"

frameOptions :: Http.HeaderName
frameOptions = "X-Frame-Options"

referrerPolicy :: Http.HeaderName
referrerPolicy = "Referrer-Policy"

requestId :: Http.HeaderName
requestId = "Monadoc-Request-Id"

strictTransportSecurity :: Http.HeaderName
strictTransportSecurity = "Strict-Transport-Security"

xssProtection :: Http.HeaderName
xssProtection = "X-XSS-Protection"
