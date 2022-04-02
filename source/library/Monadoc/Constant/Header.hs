module Monadoc.Constant.Header where

import qualified Network.HTTP.Types as Http

contentSecurityPolicy :: Http.HeaderName
contentSecurityPolicy = "Content-Security-Policy"

contentTypeOptions :: Http.HeaderName
contentTypeOptions = "X-Content-Type-Options"

frameOptions :: Http.HeaderName
frameOptions = "X-Frame-Options"

link :: Http.HeaderName
link = "Link"

referrerPolicy :: Http.HeaderName
referrerPolicy = "Referrer-Policy"

xssProtection :: Http.HeaderName
xssProtection = "X-XSS-Protection"
