ASCIIDOC := /usr/bin/asciidoc
XMLTO := /usr/bin/xmlto

%.xml: %.txt
	$(ASCIIDOC) -b docbook -d manpage $<

%.1: %.1.xml
	$(XMLTO) man $<

%.5: %.5.xml
	$(XMLTO) man $<
