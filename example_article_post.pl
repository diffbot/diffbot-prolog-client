% Diffbot API article post example.
%
%  USAGE
%  =====
%  Assign valid values for token and F 
%  type  consult('/media/9CC4E676C4E651D6/Documents/workspace/prolog/article.pl'). in the prolog console

:-use_module(library(diffbot)).
:-diffbot_defaults([token='c2e72be8cx0c1370535b6ef8bbd8daa']).
:-diffbot_defaults([token='c2e72be8c7c0c1370535b6ef8bbd8daa']).
:-F='/home/cosmin/downloads/textfile.txt',
  exists_file(F),
  diffbot('http://www.diffbot.com/products/automatic/article',file(F),[api=article, timeout=7000],J,v2),
  show_json(J).
