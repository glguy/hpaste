from pygments import highlight
from pygments.lexers import get_lexer_by_name

from pygments.formatters import HtmlFormatter
class HtmlLineFormatter(HtmlFormatter):

    def __init__(self, **options):
        HtmlFormatter.__init__(self, **options)
        self.pasteid = options.get('pasteid','0')

    def wrap(self, source, outfile):
        return self._wrap_lines(HtmlFormatter.wrap(self, source, outfile))

    def _wrap_lines(self, source):
        n = 1
        p = self.pasteid
        for i, t in source:
            if i == 1:
                # it's a line of formatted code
                t = '<span onclick="toghl(this,' + p + ',' + `n` + ')" class="li-' + `n` + '">' + t + '</span>'
                n += 1
            yield i, t

def hl(code,lang,p):
 lexer = get_lexer_by_name(lang)
 return highlight(code,lexer,HtmlLineFormatter(pasteid = p ,linenos = True))

def test():
 print hl('class F f where', 'haskell' , '7')
