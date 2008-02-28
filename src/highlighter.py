from pygments            import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers     import get_lexer_by_name

class HtmlLineFormatter(HtmlFormatter):

    def __init__(self, **options):
        HtmlFormatter.__init__(self, **options)
        self.pasteid = options.get('pasteid',0)

    def wrap(self, source, outfile):
        return self._wrap_lines(HtmlFormatter.wrap(self, source, outfile))

    def _wrap_lines(self, source):
        n = 1
        p = self.pasteid
        for i, t in source:
            if i == 1:
                t = '<span id="li-' + `p` + '-' + `n` + '">' + t + '</span>'
                n += 1
            yield i, t

def hl(code,lang,p):
    lexer = get_lexer_by_name(lang)
    formatter = HtmlLineFormatter(pasteid=p,linenos=True)
    return highlight(code,lexer,formatter)
