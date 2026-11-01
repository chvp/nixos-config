from ldaptor.protocols import pureldap, pureber
from ldaptor.protocols.ldap.ldapclient import LDAPClient
from ldaptor.protocols.ldap.ldapconnector import connectToLDAPEndpoint
from ldaptor.protocols.ldap.proxybase import ProxyBase
from twisted.application.service import Application, Service
from twisted.internet import defer, protocol, reactor
from twisted.internet.endpoints import serverFromString
from twisted.python import log
from functools import partial

alias_map = {
    b"android@accentor.tech": b"charlotte@vanpetegem.be",
    b"@chvp.be": b"charlotte@vanpetegem.be",
    b"@cvpetegem.be": b"charlotte@vanpetegem.be",
    b"charlotte@vanpetegem.me": b"charlotte@vanpetegem.be",

    b"team@accentor.tech": [
        b"charlotte@vanpetegem.be",
        b"robbe@vanpetegem.be",
    ],

    b"webmaster@vanpetegem.me": b"webmaster@vanpetegem.be",
}

def replaceMailAliasInFilter(filter):
    if isinstance(filter, pureldap.LDAPFilter_and):
        return pureldap.LDAPFilter_and(value=list(map(lambda elem: replaceMailAliasInFilter(elem), filter.data)))
    elif isinstance(filter, pureldap.LDAPFilter_or):
        return pureldap.LDAPFilter_or(value=list(map(lambda elem: replaceMailAliasInFilter(elem), filter.data)))
    elif isinstance(filter, pureldap.LDAPFilter_not):
        return pureldap.LDAPFilter_not(value=replaceMailAliasInFilter(filter.data))
    elif isinstance(filter, pureldap.LDAPFilter_equalityMatch):
        if filter.attributeDesc.value in [b"mailAlias", b"mailalias"]:
            filter.attributeDesc.value = b"mail"
            if filter.assertionValue.value in alias_map:
                if isinstance(alias_map[filter.assertionValue.value], list):
                    return pureldap.LDAPFilter_or(value=list(map(lambda elem: pureldap.LDAPFilter_equalityMatch(
                        attributeDesc=pureber.BEROctetString(value=filter.attributeDesc.value),
                        assertionValue=pureber.BEROctetString(value=elem),
                    ), alias_map[filter.assertionValue.value])))
                else:
                    filter.assertionValue.value = alias_map[filter.assertionValue.value]
        return filter
    return filter

class LoggingProxy(ProxyBase):
    def handleBeforeForwardRequest(self, request, controls, reply):
        if not isinstance(request, pureldap.LDAPSearchRequest):
            return defer.succeed((request, controls))
        request.filter = replaceMailAliasInFilter(request.filter)
        return defer.succeed((request, controls))


class LoggingProxyService(Service):
    endpointStr = "tcp:1636"
    proxiedEndpointStr = 'ssl:sso.vanpetegem.be:636'

    def startService(self):
        factory = protocol.ServerFactory()
        use_tls = False
        proxiedEndpointStr = 'tcp:host=localhost:port=1636'
        clientConnector = partial(
            connectToLDAPEndpoint,
            reactor,
            self.proxiedEndpointStr,
            LDAPClient)

        def buildProtocol():
            proto = LoggingProxy()
            proto.clientConnector = clientConnector
            proto.use_tls = use_tls
            return proto

        factory.protocol = buildProtocol
        ep = serverFromString(reactor, self.endpointStr)
        d = ep.listen(factory)
        d.addCallback(self.setListeningPort)
        d.addErrback(log.err)

    def setListeningPort(self, port):
        self.port_ = port

    def stopService(self):
        async_tasks = []
        if self.port_ is not None:
            async_tasks.append(self.port_.stopListening())
        if len(async_tasks) > 0:
            return defer.DeferredList(async_tasks, consumeErrors=True)


application = Application("Logging LDAP Proxy")
service = LoggingProxyService()
service.setServiceParent(application)
