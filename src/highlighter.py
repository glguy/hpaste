from pygments            import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers     import get_lexer_by_name
from pygments.lexers     import get_all_lexers
from pygments.util       import ClassNotFound

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
    try:
        lexer = get_lexer_by_name(lang,encoding='utf8')
    except ClassNotFound:
        lexer = get_lexer_by_name('text',encoding='utf8')
    formatter = HtmlLineFormatter(pasteid=p,linenos=True,encoding='utf8')
    return highlight(code,lexer,formatter)
