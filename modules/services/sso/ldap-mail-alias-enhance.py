from ldaptor.protocols import pureldap
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
    b"webmaster@vanpetegem.me": b"webmaster@vanpetegem.be",
}

def replaceMailAliasInFilter(filter):
    if isinstance(filter, pureldap.LDAPFilter_and) or isinstance(filter, pureldap.LDAPFilter_or):
        for elem in filter.data:
            replaceMailAliasInFilter(elem)
    elif isinstance(filter, pureldap.LDAPFilter_not):
        replaceMailAliasInFilter(filter.data)
    elif isinstance(filter, pureldap.LDAPFilter_equalityMatch):
        if filter.attributeDesc.value in [b"mailAlias", b"mailalias"]:
            filter.attributeDesc.value = b"mail"
            if filter.assertionValue.value in alias_map:
                filter.assertionValue.value = alias_map[filter.assertionValue.value]

class LoggingProxy(ProxyBase):
    def handleBeforeForwardRequest(self, request, controls, reply):
        if not isinstance(request, pureldap.LDAPSearchRequest):
            return defer.succeed((request, controls))
        replaceMailAliasInFilter(request.filter)
        return defer.succeed((request, controls))


def ldapBindRequestRepr(self):
    l=[]
    l.append('version={0}'.format(self.version))
    l.append('dn={0}'.format(repr(self.dn)))
    l.append('auth=****')
    if self.tag!=self.__class__.tag:
        l.append('tag={0}'.format(self.tag))
    l.append('sasl={0}'.format(repr(self.sasl)))
    return self.__class__.__name__+'('+', '.join(l)+')'

pureldap.LDAPBindRequest.__repr__ = ldapBindRequestRepr


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
